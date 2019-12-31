import System.Environment
import System.Console.GetOpt
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe ( fromMaybe, fromJust )
import Data.Either (fromRight)

newtype ShakeOutDir = ShakeOutDir String

getShakeOutDir :: Action String
getShakeOutDir = do
  (ShakeOutDir outdir) <- fromJust <$> getShakeExtra
  return outdir

data Rewrite = Include String | Exec String | NoOp

foldFn :: Rewrite -> (String, String) -> Rewrite
foldFn z elem@(k,v)
  | k == "include" = Include v
  | k == "exec" = Exec v
  | otherwise = z

attrsToRewrite attrs@(_, _, kvs) = foldl foldFn NoOp kvs

pandocOptions = def{readerExtensions=pandocExtensions}

textToBlocks :: Text.Text -> IO [Block]
textToBlocks text = do
  newpandoc <- runIOorExplode $ readMarkdown pandocOptions text
  let (Pandoc _ newblocks) = newpandoc
  return newblocks

processAST :: Block -> Action Block
processAST div@(Div attrs blocks) =
  case attrsToRewrite attrs of
    Include filename -> do
      newcontents <- readFile' filename
      newblocks <- liftIO $ textToBlocks $ Text.pack $ newcontents
      return $ Div attrs newblocks
    Exec command -> do
      return div
    NoOp -> return div
processAST code@(CodeBlock attrs contents) = 
  case attrsToRewrite attrs of
    Include filename -> do
      newcontents <- readFile' filename
      return $ CodeBlock attrs newcontents
    Exec command -> do
      return code
    NoOp -> return code
processAST block = return block

processMarkdownFile :: String -> Action ()
processMarkdownFile filename = do
  contents <- readFile' filename
  ast <- liftIO $ runIOorExplode $ readMarkdown pandocOptions $ Text.pack contents
  expanded <- walkM processAST ast
  htmlcontents <- liftIO $ runIOorExplode $ writeHtml5String def expanded
  outdir <- getShakeOutDir
  writeFile' (outdir </> filename -<.> "html") $ Text.unpack htmlcontents

data Options = Options
  { optInDir :: Maybe String
  , optOutDir :: Maybe String
  }

defaultOptions = Options
  { optInDir = Just "."
  , optOutDir = Just "_build"
  }

options :: [OptDescr (Either String (Options -> Options))]
options =
  [ Option ['o'] ["out"] (OptArg (\f -> Right $ (\opts -> opts{optOutDir=Just $ fromMaybe "_build" f})) "DIR") "Output site directory"
  , Option ['f'] ["from"] (OptArg (\f -> Right $ (\opts -> opts{optInDir=Just $ fromMaybe "." f})) "DIR") "Post tree"
  ]

main :: IO ()
main = do
  build

buildAllMdFilesIn :: String -> Rules ()
buildAllMdFilesIn dirpath = action $ do
  posts <- getDirectoryFiles dirpath ["//*.md"]
  outdir <- getShakeOutDir
  need [ outdir </> filename -<.> "html" | filename <- posts ]

buildRules :: String -> String -> Rules ()
buildRules inDir outDir = do
  buildAllMdFilesIn inDir

  phony "clean" $ do
    putInfo $ "Cleaning files in "++outDir
    removeFilesAfter outDir ["//*"]

  outDir <//> "*.html" %> \out -> do
    let mdfile = dropDirectory1 $ out -<.> "md"
    need [mdfile]
    putInfo $ "Building "++out++" from "++mdfile
    processMarkdownFile mdfile

build :: IO ()
build = shakeArgsOptionsWith shakeOptions{shakeFiles="."} options $ \skopts rawFlags targets ->
  let flags = foldl (flip id) defaultOptions rawFlags
      inDir = fromJust $ optInDir flags
      outDir = fromJust $ optOutDir flags
      newShakeOpts = skopts{shakeExtra = addShakeExtra (ShakeOutDir outDir) (shakeExtra skopts)} in
        return $ Just (newShakeOpts, do
          let rules = buildRules inDir outDir
          if null targets then rules else want targets >> withoutActions rules)

