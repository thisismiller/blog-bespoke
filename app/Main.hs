import System.Environment
import System.Console.GetOpt
import System.Process
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Markdown
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe ( fromMaybe, fromJust )
import Data.Either (fromRight)
import Data.List ( partition, isPrefixOf )
import Data.List.Split ( splitOn )
import qualified Data.YAML as YAML
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))

newtype ShakeOutDir = ShakeOutDir String
newtype ShakeInDir = ShakeInDir String

getShakeOutDir :: Action String
getShakeOutDir = do
  (ShakeOutDir outdir) <- fromJust <$> getShakeExtra
  return outdir

getShakeInDir :: Action String
getShakeInDir = do
  (ShakeInDir indir) <- fromJust <$> getShakeExtra
  return indir

data Rewrite = Rewrite
  { rewriteExec :: Bool
  , rewriteBuild :: Bool
  , rewriteNeeds :: [String]
  , rewriteProvides :: [String]
  , rewriteInclude :: Bool
  , rewriteFilename :: String
  , rewriteVegaLite :: Bool
  }
defaultRewrite = Rewrite{rewriteExec=False, rewriteBuild=False, rewriteNeeds=[], rewriteProvides=[],
                         rewriteInclude=False, rewriteFilename="", rewriteVegaLite=False}

{- HLINT ignore "Use tuple-section" -}
attrsToRewrite :: Attr -> (Rewrite, Attr)
attrsToRewrite attrs@(ident, classes, kvs) =
  let (rewrite', newkvs) = foldl kvFold (defaultRewrite, []) kvs
      (rewrite, newcls) = foldl classFold (rewrite', []) classes
      kvFold (z, newkvs) elem@(k,v)
        | k == "needs" = (z{rewriteNeeds=splitOn "," v}, newkvs)
        | k == "provides" = (z{rewriteProvides=splitOn "," v}, newkvs)
        | k == "filename" = (z{rewriteFilename=v}, newkvs)
        | otherwise = (z, elem:newkvs)
      classFold (z, newcls) elem
        | elem == "exec" = (z{rewriteExec=True}, newcls)
        | elem == "build" = (z{rewriteBuild=True}, newcls)
        | elem == "vegalite" = (z{rewriteVegaLite=True}, elem:newcls)
        | elem == "include" = (z{rewriteInclude=True}, newcls)
        | otherwise = (z, elem:newcls)
      in (rewrite, (ident, newcls, newkvs))

pandocOptions = def{readerExtensions=pandocExtensions}

textToBlocks :: Text.Text -> IO [Block]
textToBlocks text = do
  newpandoc <- runIOorExplode $ readMarkdown pandocOptions text
  let (Pandoc _ newblocks) = newpandoc
  return newblocks

{- HLINT ignore "Used otherwise as a pattern" -}
{- HLINT ignore "Redundant do" -}
processAST :: (String -> Action Template) -> String -> Block -> Action Block
processAST getTemplate cwd div@(Div attrs blocks) =
  let (rewrites, newattrs) = attrsToRewrite attrs
   in doDivRewrites getTemplate cwd rewrites $ Div newattrs blocks
processAST getTemplate cwd code@(CodeBlock attrs contents) =
  let (rewrites, newattrs) = attrsToRewrite attrs
   in doCodeRewrites getTemplate cwd rewrites $ CodeBlock newattrs contents
processAST getTemplate cwd block = return block

doDivRewrites getTemplate cwd rewrites div@(Div attrs blocks) =
  case rewrites of
    Rewrite{rewriteInclude=True, rewriteFilename=filename} -> do
      let includePath = cwd </> filename
      rawcontents <- readFile' includePath
      (Pandoc _ newblocks) <- processMarkdownString getTemplate (dropFileName includePath) rawcontents
      doDivRewrites getTemplate cwd rewrites{rewriteInclude=False} $ Div attrs newblocks
    Rewrite{rewriteExec=True, rewriteNeeds=deps, rewriteProvides=outs} -> do
      doDivRewrites getTemplate cwd rewrites{rewriteExec=False} $ Div attrs blocks
    otherwise -> return div

doCodeRewrites getTemplate cwd rewrites code@(CodeBlock attrs contents) =
  case rewrites of
    Rewrite{rewriteInclude=True, rewriteFilename=filename} -> do
      let includePath = cwd </> filename
      newcontents <- readFile' includePath
      doCodeRewrites getTemplate (dropFileName includePath) rewrites{rewriteInclude=False} $ CodeBlock attrs newcontents
    Rewrite{rewriteBuild=True} -> do
      newcontents <- liftIO $ readCreateProcess (shell contents){cwd=Just cwd} ""
      return Null
    Rewrite{rewriteExec=True, rewriteNeeds=deps, rewriteProvides=outs} -> do
      newcontents <- liftIO $ readCreateProcess (shell contents){cwd=Just cwd} ""
      doCodeRewrites getTemplate cwd rewrites{rewriteExec=False} $ CodeBlock attrs newcontents
    Rewrite{rewriteVegaLite=True} -> do
      vegaTemplate <- getTemplate "vegalite"
      let figId = (\(x, _, _) -> x) attrs
      let vegahtml = renderTemplate vegaTemplate $ varListToJSON [("id", figId), ("spec", contents)]
      return $ Div attrs [Plain [RawInline (Format "html") vegahtml]]
    otherwise -> return code

isNote :: Inline -> Bool
isNote (Note _) = True
isNote _ = False

noteToDiv :: Inline -> Block
noteToDiv (Note blocks) = Div ("", ["sidenote"], []) blocks

footnotesToSidenotes :: [Block] -> [Block]
footnotesToSidenotes ((Para blocks):xs) =
  let (notes, parablocks) = partition isNote blocks
   in [Para parablocks] ++ map noteToDiv notes ++ footnotesToSidenotes xs
footnotesToSidenotes (x:xs) = x : footnotesToSidenotes xs
footnotesToSidenotes [] = []

processMarkdownString :: (String -> Action Template) -> String -> String -> Action Pandoc
processMarkdownString getTemplate cwd contents = do
  ast <- liftIO $ runIOorExplode $ readMarkdown pandocOptions $ Text.pack contents
  walkM (processAST getTemplate cwd) $ walk footnotesToSidenotes ast

processMarkdownFile :: (String -> Action Template) -> String -> String -> Action Text.Text
processMarkdownFile getTemplate mdfile htmlfile = do
  contents <- readFile' mdfile
  expanded <- processMarkdownString getTemplate (dropFileName mdfile) contents
  liftIO $ runIOorExplode $ writeHtml5String def expanded

data Options = Options
  { optPostDir :: Maybe String
  , optSiteDir :: Maybe String
  , optBuildDir :: Maybe String
  , optTemplateDir :: Maybe String
  , optAssetDir :: Maybe String
  }

defaultOptions = Options
  { optPostDir = Just "."
  , optSiteDir = Just "_site"
  , optBuildDir = Just "_build"
  , optTemplateDir = Just "templates"
  , optAssetDir = Just "assets"
  }

options :: [OptDescr (Either String (Options -> Options))]
options =
  [ Option ['p'] ["posts"] (OptArg (\f -> Right (\opts -> opts{optPostDir=Just $ fromMaybe "posts" f})) "DIR") "Directory of markdown posts"
  , Option ['s'] ["site"] (OptArg (\f -> Right (\opts -> opts{optSiteDir=Just $ fromMaybe "_site" f})) "DIR") "Final static site"
  , Option ['b'] ["build"] (OptArg (\f -> Right (\opts -> opts{optBuildDir=Just $ fromMaybe "_build" f})) "DIR") "Directory for intermediate build outputs"
  , Option ['t'] ["templates"] (OptArg (\f -> Right (\opts -> opts{optTemplateDir=Just $ fromMaybe "templates" f})) "DIR") "Directory for templates"
  , Option ['a'] ["assets"] (OptArg (\f -> Right (\opts -> opts{optAssetDir=Just $ fromMaybe "assets" f})) "DIR") "Directory for static assets"
  ]

main :: IO ()
main = do
  build

isPostNotKeyword :: String -> (FilePath -> Action Meta) -> String -> Action Bool
isPostNotKeyword keyword getMetadata path = do
  meta <- getMetadata path
  let metavalue = fromMaybe (MetaBool False) $ lookupMeta keyword meta
  let ret = metavalue == (MetaBool True)
  return $ not ret

isPostNotIgnored = isPostNotKeyword "ignore"
isPostNotHidden = isPostNotKeyword "hidden"

getTitle :: (FilePath -> Action Meta) -> String -> Action String
getTitle getMetadata path = do
  meta <- getMetadata path
  let MetaInlines title = fromMaybe (MetaInlines [Str path]) $ lookupMeta "title" meta
  rendered <- liftIO $ runIOorExplode $ writeHtml5String def (Pandoc nullMeta [Plain title])
  return $ Text.unpack rendered
getSummary :: (FilePath -> Action Meta) -> String -> Action String
getSummary getMetadata path = do
  meta <- getMetadata path
  let MetaInlines summary = fromMaybe (MetaInlines [Str "ibid."]) $ lookupMeta "summary" meta
  rendered <- liftIO $ runIOorExplode $ writeHtml5String def (Pandoc nullMeta [Plain summary])
  return $ Text.unpack rendered

getMetaObject :: (FilePath -> Action Meta) -> String -> Action Aeson.Value
getMetaObject getMetadata path = do
  title <- getTitle getMetadata path
  summary <- getSummary getMetadata path
  let link = path -<.> "html"
  return $ Aeson.object [ (Text.pack "title") .= title,
                          (Text.pack "summary") .= summary,
                          (Text.pack "link") .= link ]

metadataCache :: String -> Rules (FilePath -> Action Meta)
metadataCache postDir = newCache $ \mdpath -> do
  contents <- readFile' $ postDir </> mdpath
  let header' = take 1 $ drop 1 $ splitOn "---" contents
  let header = if null header' then "   " else header' !! 0
  meta <- liftIO $ runIOorExplode $ yamlToMeta pandocOptions $ BSLU.fromString header
  return meta

buildAllMdFilesIn :: (FilePath -> Action Meta) -> String -> Rules ()
buildAllMdFilesIn getMetadata dirpath = action $ do
  posts <- getDirectoryFiles dirpath ["//*.md"]
  postsToBuild <- filterM (isPostNotIgnored getMetadata) posts
  outdir <- getShakeOutDir
  need $ (outdir </> "index.html"):[ outdir </> filename -<.> "html" | filename <- postsToBuild ]

copyAssets :: String -> Rules ()
copyAssets assetDir = action $ do
  files <- getDirectoryFiles assetDir ["//*"]
  outdir <- getShakeOutDir
  need $ [outdir </> "assets" </> file | file <- files, not $ "." `isPrefixOf` file]

buildRules :: (FilePath -> Action Meta) -> String -> String -> String -> String -> String -> Rules ()
buildRules getMetadata postDir buildDir tmplDir assetDir siteDir = do
  phony "clean" $ do
    putInfo $ "Cleaning files in "++buildDir++" and "++siteDir
    removeFilesAfter buildDir ["//*"]
    removeFilesAfter siteDir ["//*"]

  getTemplate <- newCache $ \name -> do
    let templatePath = tmplDir </> name <.> "pdt"
    need [templatePath]
    templateText <- liftIO $ TextIO.readFile templatePath
    let Right x = compileTemplate templateText in return x

  siteDir </> "index.html" %> \out -> do
    template <- getTemplate "index"
    mdposts <- getDirectoryFiles postDir ["//*.md"]
    need [postDir </> post | post <- mdposts]
    putInfo $ "Building "++siteDir </> "index.html"
    mdpostsToBuild' <- filterM (isPostNotIgnored getMetadata) $ mdposts
    mdpostsToBuild <- filterM (isPostNotHidden getMetadata) $ mdpostsToBuild'
    postdata <- mapM (getMetaObject getMetadata) mdpostsToBuild
    let foo = renderTemplate template $ Aeson.object [ (Text.pack "posts") .= postdata ]
    writeFile' out foo

  siteDir <//> "*.html" %> \out -> do
    let mdfile = postDir </> makeRelative siteDir out -<.> "md"
    need [mdfile]
    putInfo $ "Building "++out++" from "++mdfile
    htmlcontents <- processMarkdownFile getTemplate mdfile out
    template <- getTemplate "post"
    let foo = renderTemplate template $ varListToJSON [("body", Text.unpack htmlcontents)]
    writeFile' out foo

  siteDir </> "assets" <//> "*" %> \out -> do
    let assetfile = assetDir </> makeRelative (siteDir </> "assets") out
    need [assetfile]
    putInfo $ "Building "++out++" from "++assetfile
    copyFile' assetfile out

build :: IO ()
build = do
  shakeExe <- getExecutablePath
  versionHash <- getHashedShakeVersion [shakeExe]
  shakeArgsOptionsWith shakeOptions options $ \skopts rawFlags targets ->
    let flags = foldl (flip id) defaultOptions rawFlags
        postDir = fromJust $ optPostDir flags
        siteDir = fromJust $ optSiteDir flags
        buildDir = fromJust $ optBuildDir flags
        templateDir = fromJust $ optTemplateDir flags
        assetDir = fromJust $ optAssetDir flags
        newShakeOpts = skopts{
            shakeFiles = buildDir,
            shakeExtra = addShakeExtra (ShakeInDir postDir) $
                         addShakeExtra (ShakeOutDir siteDir) (shakeExtra skopts),
            shakeVersion = versionHash } in
          return $ Just (newShakeOpts, do
            getMetadata <- metadataCache postDir
            let rules = buildAllMdFilesIn getMetadata postDir >> copyAssets assetDir >> buildRules getMetadata postDir buildDir templateDir assetDir siteDir
            if null targets then rules else want targets >> withoutActions rules)

