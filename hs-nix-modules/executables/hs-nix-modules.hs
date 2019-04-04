{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ApplicativeDo, BlockArguments, EmptyDataDecls,
    LambdaCase, OverloadedStrings, ScopedTypeVariables,
    TypeApplications, ViewPatterns #-}

module Main (main) where

-- base
import Control.Applicative
import Control.Monad
import Data.Foldable
import Prelude hiding (FilePath, pi)
import System.IO hiding (FilePath)

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- haskell-src-exts
import qualified Language.Haskell.Exts as HS

-- optparse-applicative
import qualified Options.Applicative as Opt

-- pipes
import Pipes

-- directory
import System.Directory

-- filepath
import System.FilePath hiding (FilePath)


------------------------------------------------------------
--  Miscellaneous concepts
------------------------------------------------------------

type FilePath = Text

-- All the output paths are relative to the base directory
type Base = FilePath

-- Name of a Haskell module
type ModuleName = Text

data Flag a = Yes | No

data OutputStdout

data StopAtLine


------------------------------------------------------------
--  Command-line options
------------------------------------------------------------

data Opt =
  Opt
    { opt_inputPaths :: [FilePath]
    , opt_outputStdout :: Flag OutputStdout
    , opt_outputFile :: Maybe FilePath
    , opt_stopAtLine :: Flag StopAtLine
    }


------------------------------------------------------------
--  Parsing the command-line options
------------------------------------------------------------

getOpt :: IO Opt
getOpt = Opt.execParser parserInfo

parserInfo :: Opt.ParserInfo Opt
parserInfo =
  Opt.info (pOpt <**> Opt.helper)
    (Opt.progDesc
      "Generates a Nix file that summarizes your \
      \Haskell source files and their imports.")

pOpt :: Opt.Parser Opt
pOpt =
  do
    inputPaths <- many (pInputPath)
    destStdout <- pStdout
    destFile <- optional pDestFile
    stopAtLine <- pStopAtLine

    return Opt
      { opt_inputPaths = inputPaths
      , opt_outputStdout = destStdout
      , opt_outputFile = destFile
      , opt_stopAtLine = stopAtLine
      }

pInputPath :: Opt.Parser (FilePath)
pInputPath = Opt.option readFilePath $
  Opt.long "input" <>
  Opt.help
    "A Haskell source file, or a directory containing \
    \ Haskell source files"

pStdout :: Opt.Parser (Flag OutputStdout)
pStdout = Opt.flag No Yes $
  Opt.long "stdout" <>
  Opt.help
    "Write the resulting Nix expression to stdout"

pDestFile :: Opt.Parser FilePath
pDestFile = Opt.option readFilePath $
  Opt.long "outfile" <>
  Opt.metavar "OUTFILE" <>
  Opt.help
    "Write the resulting Nix expression to OUTFILE"

pStopAtLine :: Opt.Parser (Flag StopAtLine)
pStopAtLine = Opt.flag No Yes $
  Opt.long "stop-at-line" <>
  Opt.help
    "Stop reading source files at the first horizontal line \
    \(a line that consists of three or more dashes). \
    \If you separate your imports from the rest of your code \
    \with a line and use this flag, then this program can \
    \work even if it can't parse the entire file."

readFilePath :: Opt.ReadM FilePath
readFilePath = Opt.maybeReader \s -> Just (Text.pack s)


------------------------------------------------------------
--  Destination
------------------------------------------------------------

data Destination = WriteToStdout | WriteToFile FilePath

destination_base :: Destination -> Base
destination_base =
  \case
    WriteToStdout -> "."
    WriteToFile x -> Text.dropWhileEnd (/= '/') x

destination_withOutputHandle ::
  Destination -> (Handle -> IO a) -> IO a
destination_withOutputHandle =
  \case
    WriteToStdout -> \f -> f stdout
    WriteToFile (Text.unpack -> fp) -> withFile fp WriteMode


------------------------------------------------------------
--  Discerning a Destination from the Opts
------------------------------------------------------------

opt_destination :: Opt -> IO Destination
opt_destination opt =
  case (opt_outputStdout opt :: Flag OutputStdout, opt_outputFile opt) of

    (Yes, Nothing) -> return WriteToStdout

    (No, Just x) -> return (WriteToFile x)

    (No, Nothing) ->
      fail "Must specify either --stdout or --outfile"

    (Yes, Just _) ->
      fail "Cannot use both --stdout and --outfile"


------------------------------------------------------------
--  Configuration
------------------------------------------------------------

data Conf =
  Conf
    { conf_inputPaths :: Set FilePath
    , conf_destination :: Destination
    , conf_stopAtLine :: Flag StopAtLine
    }


------------------------------------------------------------
--  Turning the Opts into a configuration
------------------------------------------------------------

opt_conf :: Opt -> IO Conf
opt_conf opt =
  do
    let inputPaths = Set.fromList (opt_inputPaths opt)

    destination <- opt_destination opt

    return Conf
      { conf_inputPaths = inputPaths
      , conf_destination = destination
      , conf_stopAtLine = opt_stopAtLine opt
      }


------------------------------------------------------------
--  Main
------------------------------------------------------------

main :: IO ()
main = mainConf =<< opt_conf =<< getOpt

mainConf :: Conf -> IO ()
mainConf conf =
  do
    let
      destination = conf_destination conf
      base = destination_base destination
      inputPaths = conf_inputPaths conf
      withOutputHandle = destination_withOutputHandle destination
      stopAtLine = conf_stopAtLine conf

    withOutputHandle \h ->
      do
        hPutStrLn h "{"
        runEffect
          (
            pipe_findSourceFiles inputPaths >->
            pipe_parseSourceFiles stopAtLine >->
            pipe_renderNixText base >->
            pipe_write h
          )
        hPutStrLn h "}"


------------------------------------------------------------
--  Searching for Haskell source files
------------------------------------------------------------

pipe_findSourceFiles :: Set FilePath -> Producer FilePath IO ()
pipe_findSourceFiles q =
  case Set.minView q of
    Nothing -> return ()
    Just (x, q') ->
      lift (getFileType x) >>= \case
        File ->
          do
            when (".hs" `Text.isSuffixOf` x) (yield x)
            pipe_findSourceFiles q'
        Dir ->
          do
            cs <- lift (listDirectory (Text.unpack x))
            let q'' = Set.fromList (map (\c -> x <> "/" <> Text.pack c) cs)
            pipe_findSourceFiles (Set.union q' q'')

data FileType = File | Dir

getFileType :: FilePath -> IO FileType
getFileType (Text.unpack -> fp) =
  doesPathExist fp >>= \case
    False -> fail ("Does not exist: " <> fp)
    True ->
      doesFileExist fp >>= \case
        True -> return File
        False ->
          doesDirectoryExist fp >>= \case
            True -> return Dir
            False -> fail ("Unrecognized file type: " <> fp)


------------------------------------------------------------
--  Parsing Haskell source files
------------------------------------------------------------

pipe_parseSourceFiles :: Flag StopAtLine ->
    Pipe FilePath (FilePath, Module) IO ()
pipe_parseSourceFiles stopAtLine =
  forever do
    fp <- await
    m <- lift (parseSourceFile stopAtLine fp)
    yield (fp, m)

data Module =
  Module
    { mod_name :: ModuleName
    , mod_imports :: Set ModuleName
    }

parseSourceFile :: Flag StopAtLine -> FilePath -> IO Module
parseSourceFile stopAtLine fp@(Text.unpack -> fp') =
  do
    r <- case stopAtLine :: Flag StopAtLine of

      No ->
          HS.parseFile fp'

      Yes ->
        do
          t <- Text.readFile fp'
          let s = Text.unpack (takeUntilDivider t)
          return (HS.parseFileContents s)

    parseResultModule fp r

takeUntilDivider :: Text -> Text
takeUntilDivider =
    Text.unlines . takeWhile (not . isDivider) . Text.lines

isDivider :: Text -> Bool
isDivider x =
    (Text.all (== '-') x) && (Text.length x >= 3)

parseResultModule :: FilePath -> HS.ParseResult (HS.Module l) -> IO Module
parseResultModule fp =
  \case
    HS.ParseFailed _ e -> parseFail fp e
    HS.ParseOk m       -> hsModule fp m

hsModule :: FilePath -> HS.Module l -> IO Module
hsModule fp =
  \case
    HS.Module _ (Just (HS.ModuleHead _ n _ _)) _ ims _ ->
        return (Module (hsModuleName n) (hsImports ims))
    HS.Module _ Nothing _ _ _ ->
        parseFail fp "Missing module head"
    HS.XmlPage _ _ _ _ _ _ _ ->
        parseFail fp "Don't know how to parse XmlPage"
    HS.XmlHybrid _ (Just (HS.ModuleHead _ n _ _)) _ ims _ _ _ _ _ ->
        return (Module (hsModuleName n) (hsImports ims))
    HS.XmlHybrid _ Nothing _ _ _ _ _ _ _ ->
        parseFail fp "Missing module head"

parseFail :: FilePath -> String -> IO a
parseFail (Text.unpack -> fp) e = fail ("<" ++ fp ++ "> " ++ e)

hsModuleName :: HS.ModuleName l -> ModuleName
hsModuleName (HS.ModuleName _ n) = Text.pack n

hsImports :: [HS.ImportDecl l] -> Set ModuleName
hsImports = Set.fromList . map (hsModuleName . HS.importModule)


------------------------------------------------------------
--  Converting the result to a Nix expression
------------------------------------------------------------

pipe_renderNixText :: Base -> Pipe (FilePath, Module) Text IO ()
pipe_renderNixText base =
  forever do
    (fp, m) <- await
    yield "  \""
    yield (mod_name m )
    yield "\" = {\n    src = ./"
    yield (Text.pack (makeRelative (Text.unpack base) (Text.unpack fp)))
    yield ";\n"
    case Set.toList (mod_imports m) of
      [] -> return ()
      xs -> do
        yield "    imports = [\n"
        for_ xs \i ->
          do
            yield "      \""
            yield i
            yield "\"\n"
        yield "    ];\n"
    yield "  };\n"

pipe_write :: Handle -> Consumer Text IO ()
pipe_write h =
  forever do
    t <- await
    lift (Text.hPutStr h t)
