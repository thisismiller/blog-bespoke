import System.Environment
import System.Console.GetOpt
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe ( fromMaybe, fromJust )
import Data.Either (fromRight)
import Data.List ( partition )
import Data.List.Split ( splitOn )

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
  , rewriteNeeds :: [String]
  , rewriteProvides :: [String]
  , rewriteInclude :: Bool
  , rewriteFilename :: String
  }
defaultRewrite = Rewrite{rewriteExec=False, rewriteNeeds=[], rewriteProvides=[],
                         rewriteInclude=False, rewriteFilename=""}

attrsToRewrite :: Attr -> (Rewrite, Attr)
attrsToRewrite attrs@(ident, classes, kvs) =
  let (rewrite, newkvs) = foldl foldFn (defaultRewrite, []) kvs
        where foldFn (z, newkvs) elem@(k,v)
                | k == "exec" = (z{rewriteExec=True}, newkvs)
                | k == "needs" = (z{rewriteNeeds=splitOn "," v}, newkvs)
                | k == "provides" = (z{rewriteProvides=splitOn "," v}, newkvs)
                | k == "include" = (z{rewriteInclude=True}, newkvs)
                | k == "filename" = (z{rewriteFilename=v}, newkvs)
                | otherwise = (z, elem:newkvs)
          in (rewrite, (ident, classes, newkvs))

pandocOptions = def{readerExtensions=pandocExtensions}

textToBlocks :: Text.Text -> IO [Block]
textToBlocks text = do
  newpandoc <- runIOorExplode $ readMarkdown pandocOptions text
  let (Pandoc _ newblocks) = newpandoc
  return newblocks

processAST :: Block -> Action Block
processAST div@(Div attrs blocks) =
  case attrsToRewrite attrs of
    (Rewrite{rewriteInclude=True, rewriteFilename=filename}, newattrs) -> do
      newcontents <- readFile' filename
      newblocks <- liftIO $ textToBlocks $ Text.pack newcontents
      return $ Div newattrs newblocks
    (Rewrite{rewriteExec=True, rewriteNeeds=deps, rewriteProvides=outs}, newattrs) -> do
      return $ Div newattrs blocks
    otherwise -> return div
processAST code@(CodeBlock attrs contents) = unsafePerformIO $ do
  print code
  return $ case attrsToRewrite attrs of
      (Rewrite{rewriteInclude=True, rewriteFilename=filename}, newattrs) -> do
        newcontents <- readFile' filename
        processAST $ CodeBlock newattrs newcontents
      (Rewrite{rewriteExec=True, rewriteNeeds=deps, rewriteProvides=outs}, newattrs) -> do
        processAST $ CodeBlock newattrs contents
      otherwise -> return code
processAST block = return block

isNote :: Inline -> Bool
isNote (Note _) = True
isNote _ = False

noteToDiv :: Inline -> Block
noteToDiv (Note blocks) = Div ("", ["sidenote"], []) blocks

footnotesToSidenotes :: [Block] -> [Block]
footnotesToSidenotes ((Para blocks):xs) =
  let (notes, parablocks) = partition isNote blocks
   in [Para parablocks]++(map noteToDiv notes)++(footnotesToSidenotes xs)
footnotesToSidenotes (x:xs) = x : (footnotesToSidenotes xs)
footnotesToSidenotes [] = []

processMarkdownFile :: String -> String -> Action Text.Text
processMarkdownFile mdfile htmlfile = do
  contents <- readFile' mdfile
  ast <- liftIO $ runIOorExplode $ readMarkdown pandocOptions $ Text.pack contents
  expanded <- walkM processAST $ walk footnotesToSidenotes ast
  liftIO $ runIOorExplode $ writeHtml5String def expanded

data Options = Options
  { optPostDir :: Maybe String
  , optSiteDir :: Maybe String
  , optBuildDir :: Maybe String
  , optTemplateDir :: Maybe String
  }

defaultOptions = Options
  { optPostDir = Just "."
  , optSiteDir = Just "_site"
  , optBuildDir = Just "_build"
  , optTemplateDir = Just "templates"
  }

options :: [OptDescr (Either String (Options -> Options))]
options =
  [ Option ['p'] ["posts"] (OptArg (\f -> Right (\opts -> opts{optPostDir=Just $ fromMaybe "posts" f})) "DIR") "Directory of markdown posts"
  , Option ['s'] ["site"] (OptArg (\f -> Right (\opts -> opts{optSiteDir=Just $ fromMaybe "_site" f})) "DIR") "Final static site"
  , Option ['b'] ["build"] (OptArg (\f -> Right (\opts -> opts{optBuildDir=Just $ fromMaybe "_build" f})) "DIR") "Directory for intermediate build outputs"
  , Option ['t'] ["templates"] (OptArg (\f -> Right (\opts -> opts{optTemplateDir=Just $ fromMaybe "templates" f})) "DIR") "Directory for intermediate build outputs"
  ]

main :: IO ()
main = do
  build

buildAllMdFilesIn :: String -> Rules ()
buildAllMdFilesIn dirpath = action $ do
  posts <- getDirectoryFiles dirpath ["//*.md"]
  outdir <- getShakeOutDir
  need $ (outdir </> "index.html"):[ outdir </> filename -<.> "html" | filename <- posts ]

buildRules :: String -> String -> String -> String -> Rules ()
buildRules postDir buildDir tmplDir siteDir = do
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
    writeFile' out "<html><head></head><body><a href=\"fifo_vs_lifo_queues.html\">post</a></body></html>"

  siteDir <//> "*.html" %> \out -> do
    let mdfile = postDir </> makeRelative siteDir out -<.> "md"
    need [mdfile]
    putInfo $ "Building "++out++" from "++mdfile
    htmlcontents <- processMarkdownFile mdfile out
    template <- getTemplate "post"
    let foo = renderTemplate template $ varListToJSON [("body", Text.unpack htmlcontents)]
    writeFile' out foo

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
        newShakeOpts = skopts{
            shakeFiles = buildDir,
            shakeExtra = addShakeExtra (ShakeInDir postDir) $
                         addShakeExtra (ShakeOutDir siteDir) (shakeExtra skopts),
            shakeVersion = versionHash } in
          return $ Just (newShakeOpts, do
            let rules = buildAllMdFilesIn postDir >> buildRules postDir buildDir templateDir siteDir
            if null targets then rules else want targets >> withoutActions rules)

