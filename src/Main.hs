{-# LANGUAGE NondecreasingIndentation #-}

-- -----------------------------------------------------------------------------
--
-- Main.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- ----------------------------------------------------------------------------}

module Main (main) where

import AbsSyn
import CharSet
import DFA
import DFAMin
import NFA
import Info
import Output
import ParseMonad            ( runP, Warning(..) )
import Parser
import Scan
import Util                  ( hline )
import Paths_alex            ( version, getDataDir )

import Control.Exception     ( bracketOnError )
import Control.Monad         ( when, liftM )
import Data.Char             ( chr )
import Data.List             ( intercalate, isSuffixOf, nub )
import Data.Map              ( Map )
import Data.Version          ( showVersion )
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..) )
import System.Directory      ( removeFile )
import System.Environment    ( getProgName, getArgs )
import System.Exit           ( ExitCode(..), exitWith )
import System.IO             ( stderr, Handle, IOMode(..), openFile, hClose, hPutStr, hPutStrLn
                             , hGetContents, hSetEncoding, utf8 )
import qualified Data.Map    as Map

-- We need to force every file we open to be read in
-- as UTF8
alexReadFile :: FilePath -> IO String
alexReadFile file = hGetContents =<< alexOpenFile file ReadMode

-- We need to force every file we write to be written
-- to as UTF8
alexOpenFile :: FilePath -> IOMode -> IO Handle
alexOpenFile file mode = do
  h <- openFile file mode
  hSetEncoding h utf8
  return h

-- `main' decodes the command line arguments and calls `alex'.

main:: IO ()
main = do
  args <- getArgs
  case getOpt Permute argInfo args of
    (cli,_,[]) | DumpHelp `elem` cli -> do
        prog <- getProgramName
        bye (usageInfo (usageHeader prog) argInfo)
    (cli,_,[]) | DumpVersion `elem` cli ->
        bye copyright
    (cli,_,[]) | DumpNumericVersion `elem` cli ->
        bye projectVersion
    (cli,_,[]) | OptVerbose `elem` cli ->
        failure "Option '--verbose' not yet implemented"
    (cli,[file],[]) ->
        runAlex cli file
    (_,_,errors) ->
        failure $ concat errors
  where
    failure err = do
      prog <- getProgramName
      die (err ++ usageInfo (usageHeader prog) argInfo)

projectVersion :: String
projectVersion = showVersion version

copyright :: String
copyright = "Alex version " ++ projectVersion ++ ", (c) 2003 Chris Dornan and Simon Marlow\n"

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file\n"

runAlex :: [CLIFlags] -> FilePath -> IO ()
runAlex cli file = do
  basename <- case (reverse file) of
                'x':'.':r -> return (reverse r)
                _         -> die (file ++ ": filename must end in \'.x\'\n")

  prg <- alexReadFile file
  script <- parseScript file prg
  alex cli file basename script

parseScript :: FilePath -> String
  -> IO (Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code))
parseScript file prg =
  case runP prg initialParserEnv parse of
        Left (Just (AlexPn _ line col),err) ->
                die (file ++ ":" ++ show line ++ ":" ++ show col
                                 ++ ": " ++ err ++ "\n")
        Left (Nothing, err) ->
                die (file ++ ": " ++ err ++ "\n")

        Right (warnings, script@(_, _, scanner, _)) -> do
          -- issue 46: give proper error when lexer definition is empty
          when (null $ scannerTokens scanner) $
            dieAlex $ file ++ " contains no lexer rules\n"
          -- issue 71: warn about nullable regular expressions
          mapM_ printWarning warnings
          return script
  where
  printWarning (WarnNullableRExp (AlexPn _ line col) msg) =
    hPutStrLn stderr $ concat
      [ "Warning: "
      , file , ":", show line , ":" , show col , ": "
      , msg
      ]

alex :: [CLIFlags]
     -> FilePath
     -> FilePath
     -> (Maybe (AlexPosn, Code), [Directive], Scanner, Maybe (AlexPosn, Code))
     -> IO ()
alex cli file basename script = do
   (put_info, finish_info) <-
      case [ f | OptInfoFile f <- cli ] of
           []  -> return (\_ -> return (), return ())
           [Nothing] -> infoStart file (basename ++ ".info")
           [Just f]  -> infoStart file f
           _   -> dieAlex "multiple -i/--info options"

   o_file <- case [ f | OptOutputFile f <- cli ] of
                []  -> return (basename ++ ".hs")
                [f] -> return f
                _   -> dieAlex "multiple -o/--outfile options"

   tab_size <- case [ s | OptTabSize s <- cli ] of
                []  -> return (8 :: Int)
                [s] -> case reads s of
                        [(n,"")] -> return n
                        _        -> dieAlex "-s/--tab-size option is not a valid integer"
                _   -> dieAlex "multiple -s/--tab-size options"

   let target
        | OptGhcTarget `elem` cli = GhcTarget
        | otherwise               = HaskellTarget

   let encodingsCli
        | OptLatin1 `elem` cli = [Latin1]
        | otherwise            = []

   template_dir  <- templateDir getDataDir cli

   let maybe_header, maybe_footer :: Maybe (AlexPosn, Code)
       directives                 :: [Directive]
       scanner1                   :: Scanner
       (maybe_header, directives, scanner1, maybe_footer) = script

   scheme <- getScheme directives

   -- open the output file; remove it if we encounter an error
   bracketOnError
        (alexOpenFile o_file WriteMode)
        (\h -> do hClose h; removeFile o_file)
        $ \out_h -> do

   let   scanner2, scanner_final :: Scanner
         scs                     :: [StartCode]
         sc_hdr, actions         :: ShowS
         encodingsScript         :: [Encoding]

         (scanner2, scs, sc_hdr) = encodeStartCodes scanner1
         (scanner_final, actions) = extractActions scheme scanner2
         encodingsScript = [ e | EncodingDirective e <- directives ]

   encoding <- case nub (encodingsCli ++ encodingsScript) of
     []  -> return UTF8 -- default
     [e] -> return e
     _ | null encodingsCli -> dieAlex "conflicting %encoding directives"
       | otherwise -> dieAlex "--latin1 flag conflicts with %encoding directive"

   mapM_ (hPutStrLn out_h) (optsToInject target cli)
   injectCode maybe_header file out_h

   hPutStr out_h (importsToInject target cli)

   -- add the wrapper, if necessary
   case wrapperCppDefs scheme of
     Nothing -> return ()
     Just cppDefs -> do
       let wrapper_name = template_dir ++ "/AlexWrappers.hs"
       mapM_ (hPutStrLn out_h)
         [ unwords ["#define", i, "1"]
         | i <- cppDefs ]
       str <- alexReadFile wrapper_name
       hPutStr out_h str

   -- Inject the tab size
   hPutStrLn out_h $ "alex_tab_size :: Int"
   hPutStrLn out_h $ "alex_tab_size = " ++ show (tab_size :: Int)

   let dfa = scanner2dfa encoding scanner_final scs
       min_dfa = minimizeDFA dfa
       nm  = scannerName scanner_final
       usespreds = usesPreds min_dfa


   put_info "\nStart codes\n"
   put_info (show $ scs)
   put_info "\nScanner\n"
   put_info (show $ scanner_final)
   put_info "\nNFA\n"
   put_info (show $ scanner2nfa encoding scanner_final scs)
   put_info "\nDFA"
   put_info (infoDFA 1 nm dfa "")
   put_info "\nMinimized DFA"
   put_info (infoDFA 1 nm min_dfa "")
   hPutStr out_h (outputDFA target 1 nm scheme min_dfa "")

   hPutStr out_h (sc_hdr "")
   hPutStr out_h (actions "")

   -- add the template
   do
     let cppDefs = templateCppDefs target encoding usespreds cli
     mapM_ (hPutStrLn out_h)
       [ unwords ["#define", i, "1"]
       | i <- cppDefs ]
     tmplt <- alexReadFile $ template_dir ++ "/AlexTemplate.hs"
     hPutStr out_h tmplt

   injectCode maybe_footer file out_h

   hClose out_h
   finish_info

getScheme :: [Directive] -> IO Scheme
getScheme directives =
  do
    token <- case [ ty | TokenType ty <- directives ] of
      [] -> return Nothing
      [res] -> return (Just res)
      _ -> dieAlex "multiple %token directives"

    action <- case [ ty | ActionType ty <- directives ] of
      [] -> return Nothing
      [res] -> return (Just res)
      _ -> dieAlex "multiple %action directives"

    typeclass <- case [ tyclass | TypeClass tyclass <- directives ] of
      [] -> return Nothing
      [res] -> return (Just res)
      _ -> dieAlex "multiple %typeclass directives"

    case [ f | WrapperDirective f <- directives ] of
        []  ->
          case (typeclass, token, action) of
            (Nothing, Nothing, Nothing) ->
              return Default { defaultTypeInfo = Nothing }
            (Nothing, Nothing, Just actionty) ->
              return Default { defaultTypeInfo = Just (Nothing, actionty) }
            (Just _, Nothing, Just actionty) ->
              return Default { defaultTypeInfo = Just (typeclass, actionty) }
            (_, Just _, _) ->
              dieAlex "%token directive only allowed with a wrapper"
            (Just _, Nothing, Nothing) ->
              dieAlex "%typeclass directive without %token directive"
        [single]
          | single == "gscan" ->
            case (typeclass, token, action) of
              (Nothing, Nothing, Nothing) ->
                return GScan { gscanTypeInfo = Nothing }
              (Nothing, Just tokenty, Nothing) ->
                return GScan { gscanTypeInfo = Just (Nothing, tokenty) }
              (Just _, Just tokenty, Nothing) ->
                return GScan { gscanTypeInfo = Just (typeclass, tokenty) }
              (_, _, Just _) ->
                dieAlex "%action directive not allowed with a wrapper"
              (Just _, Nothing, Nothing) ->
                dieAlex "%typeclass directive without %token directive"
          | single == "basic" || single == "basic-bytestring" ||
            single == "strict-bytestring" || single == "strict-text" ->
            let
              strty = case single of
                "basic" -> Str
                "basic-bytestring" -> Lazy
                "strict-bytestring" -> Strict
                "strict-text" -> StrictText
                _ -> error "Impossible case"
            in case (typeclass, token, action) of
              (Nothing, Nothing, Nothing) ->
                return Basic { basicStrType = strty,
                               basicTypeInfo = Nothing }
              (Nothing, Just tokenty, Nothing) ->
                return Basic { basicStrType = strty,
                               basicTypeInfo = Just (Nothing, tokenty) }
              (Just _, Just tokenty, Nothing) ->
                return Basic { basicStrType = strty,
                               basicTypeInfo = Just (typeclass, tokenty) }
              (_, _, Just _) ->
                dieAlex "%action directive not allowed with a wrapper"
              (Just _, Nothing, Nothing) ->
                dieAlex "%typeclass directive without %token directive"
          | single == "posn" || single == "posn-bytestring" || single == "posn-strict-text" ->
            let
              strty = case single of
                "posn" -> Str
                "posn-bytestring" -> Strict
                "posn-strict-text" -> StrictText
                _ -> error "invalid str type for posn"
            in case (typeclass, token, action) of
              (Nothing, Nothing, Nothing) ->
                return Posn { posnStrType = strty,
                              posnTypeInfo = Nothing }
              (Nothing, Just tokenty, Nothing) ->
                return Posn { posnStrType = strty,
                              posnTypeInfo = Just (Nothing, tokenty) }
              (Just _, Just tokenty, Nothing) ->
                return Posn { posnStrType = strty,
                              posnTypeInfo = Just (typeclass, tokenty) }
              (_, _, Just _) ->
                  dieAlex "%action directive not allowed with a wrapper"
              (Just _, Nothing, Nothing) ->
                dieAlex "%typeclass directive without %token directive"
          | single == "monad" || single == "monad-bytestring" || single == "monad-strict-text" ||
            single == "monadUserState" ||
            single == "monadUserState-bytestring" ||
            single == "monadUserState-strict-text" ->
            let
              isText = single == "monad-strict-text" ||
                       single == "monadUserState-strict-text"
              isByteString = single == "monad-bytestring" ||
                             single == "monadUserState-bytestring"
              userState = single == "monadUserState" ||
                          single == "monadUserState-bytestring" ||
                          single == "monadUserState-strict-text"
            in case (typeclass, token, action) of
              (Nothing, Nothing, Nothing) ->
                return Monad { monadStrType = if isByteString then Lazy else if isText then StrictText else Str,
                               monadUserState = userState,
                               monadTypeInfo = Nothing }
              (Nothing, Just tokenty, Nothing) ->
                return Monad { monadStrType = if isByteString then Lazy else if isText then StrictText else Str,
                               monadUserState = userState,
                               monadTypeInfo = Just (Nothing, tokenty) }
              (Just _, Just tokenty, Nothing) ->
                return Monad { monadStrType = if isByteString then Lazy else if isText then StrictText else Str,
                               monadUserState = userState,
                               monadTypeInfo = Just (typeclass, tokenty) }
              (_, _, Just _) ->
                  dieAlex "%action directive not allowed with a wrapper"
              (Just _, Nothing, Nothing) ->
                dieAlex "%typeclass directive without %token directive"
          | otherwise -> dieAlex ("unknown wrapper type " ++ single)
        _many -> dieAlex "multiple %wrapper directives"

-- inject some code, and add a {-# LINE #-} pragma at the top
injectCode :: Maybe (AlexPosn,Code) -> FilePath -> Handle -> IO ()
injectCode Nothing _ _ = return ()
injectCode (Just (AlexPn _ ln _,code)) filename hdl = do
  hPutStrLn hdl ("{-# LINE " ++ show ln ++ " \"" ++ filename ++ "\" #-}")
  hPutStrLn hdl code

optsToInject :: Target -> [CLIFlags] -> [String]
optsToInject target _ = concat
  [ optNoWarnings
  , [ "{-# LANGUAGE CPP #-}" ]
  , [ "{-# LANGUAGE MagicHash #-}" | target == GhcTarget ]
  ]

-- List here all harmless warnings caused by Alex-generated code.
--
-- These will be suppressed so that they are not printed
-- when users turn on @-Wall@ in their lexer project.
--
optNoWarnings :: [String]
optNoWarnings =
  map (("{-# OPTIONS_GHC -fno-warn-" ++) . (++ " #-}"))
    [ "missing-signatures"
    , "tabs"
    , "unused-binds"
    , "unused-imports"
    ]

importsToInject :: Target -> [CLIFlags] -> String
importsToInject _ cli = unlines $
  always_imports ++ debug_imports ++ glaexts_import
  where
        glaexts_import | OptGhcTarget `elem` cli    = import_glaexts
                       | otherwise                  = []

        debug_imports  | OptDebugParser `elem` cli = import_debug
                       | otherwise                 = []

-- We need to #include "ghcconfig.h" to get hold of
-- WORDS_BIGENDIAN (see AlexTemplate.hs).

always_imports :: [String]
always_imports =
  [ "#include \"ghcconfig.h\""
  , "import qualified Data.Array"
  ]

import_glaexts :: [String]
import_glaexts =
  [ "import Data.Array.Base (unsafeAt)"
  , "import GHC.Exts (" ++ imports ++ ")"
  , "import qualified GHC.Exts"
  ]
  where
    -- We can import anything mentioning # safely,
    -- assuming the user code does not make use of
    -- MagicHash.
    imports = intercalate ","
      [ "Addr#"
      , "Int#"
      , "Int(I#)"
      , "(*#)"
      , "(+#)"
      , "(-#)"
      , "(==#)"
      , "(>=#)"
      , "indexCharOffAddr#"
      , "indexInt16OffAddr#"
      , "indexInt32OffAddr#"
      , "int2Word#"
      , "narrow16Int#"
      , "narrow32Int#"
      , "negateInt#"
      , "or#"
      , "ord#"
      , "uncheckedShiftL#"
      , "word2Int#"
      ]

import_debug :: [String]
import_debug =
  [ "import Data.Char (chr)"
  , "import qualified Debug.Trace"
  ]

templateDir :: IO FilePath -> [CLIFlags] -> IO FilePath
templateDir def cli
  = case [ d | OptTemplateDir d <- cli ] of
      [] -> def
      ds -> return (last ds)

templateCppDefs :: Target -> Encoding -> UsesPreds -> [CLIFlags] -> [String]
templateCppDefs target encoding usespreds cli =
  map ("ALEX_" ++) $ concat
    [ [ "GHC"    | target == GhcTarget ]
    , [ "LATIN1" | encoding == Latin1  ]
    , [ "NOPRED" | usespreds == DoesntUsePreds  ]
    , [ "DEBUG"  | OptDebugParser `elem` cli  ]
    ]

infoStart :: FilePath -> FilePath -> IO (String -> IO (), IO ())
infoStart x_file info_file = do
  bracketOnError
        (alexOpenFile info_file WriteMode)
        (\h -> do hClose h; removeFile info_file)
        (\h -> do infoHeader h x_file
                  return (hPutStr h, hClose h)
        )

infoHeader :: Handle -> FilePath -> IO ()
infoHeader h file = do
--  hSetBuffering h NoBuffering
  hPutStrLn h ("Info file produced by Alex version " ++ projectVersion ++
                ", from " ++ file)
  hPutStrLn h hline
  hPutStr h "\n"

initialParserEnv :: (Map String CharSet, Map String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv :: Map String CharSet
initSetEnv = Map.fromList [("white", charSet " \t\n\v\f\r"),
                           ("printable", charSetRange (chr 32) (chr 0x10FFFF)), -- FIXME: Look it up the unicode standard
                           (".", charSetComplement emptyCharSet
                            `charSetMinus` charSetSingleton '\n')]

initREEnv :: Map String RExp
initREEnv = Map.empty

-- -----------------------------------------------------------------------------
-- Command-line flags

data CLIFlags
  = OptDebugParser
  | OptGhcTarget
  | OptOutputFile FilePath
  | OptInfoFile (Maybe FilePath)
  | OptTabSize String
  | OptTemplateDir FilePath
  | OptLatin1
  | OptVerbose
  | DumpHelp
  | DumpVersion
  | DumpNumericVersion
  deriving Eq

argInfo :: [OptDescr CLIFlags]
argInfo  = [
   Option ['o'] ["outfile"] (ReqArg OptOutputFile "FILE")
        "write the output to FILE (default: file.hs)",
   Option ['i'] ["info"] (OptArg OptInfoFile "FILE")
        "put detailed state-machine info in FILE (or file.info)",
   Option ['t'] ["template"] (ReqArg OptTemplateDir "DIR")
        "look in DIR for template files",
   Option ['g'] ["ghc"]    (NoArg OptGhcTarget)
        "use GHC extensions",
   Option ['l'] ["latin1"]    (NoArg OptLatin1)
        "generated lexer will use the Latin-1 encoding instead of UTF-8",
   Option ['s'] ["tab-size"] (ReqArg OptTabSize "NUMBER")
        "set tab size to be used in the generated lexer (default: 8)",
   Option ['d'] ["debug"] (NoArg OptDebugParser)
        "produce a debugging scanner",
   Option ['v'] ["verbose"] (NoArg OptVerbose)
        "be verbose (not yet implemented)",
   Option ['?'] ["help"] (NoArg DumpHelp)
        "display this help and exit",
   Option ['V'] ["version"] (NoArg DumpVersion)
        "output version information and exit"
  ,Option [] ["numeric-version"] (NoArg DumpNumericVersion)
        "output the version number and exit"
  ]

-- -----------------------------------------------------------------------------
-- Utils

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

dieAlex :: String -> IO a
dieAlex s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)
