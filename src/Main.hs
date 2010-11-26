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
import Info
import Map ( Map )
import qualified Map hiding ( Map )
import Output
import ParseMonad ( runP )
import Parser
import Scan
import Util ( hline )
import Paths_alex ( version, getDataDir )

#if __GLASGOW_HASKELL__ < 610
import Control.Exception as Exception ( block, unblock, catch, throw )
#endif
#if __GLASGOW_HASKELL__ >= 610
import Control.Exception ( bracketOnError )
#endif
import Control.Monad ( when, liftM )
import Data.Char ( chr )
import Data.List ( isSuffixOf )
import Data.Maybe ( isJust, fromJust )
import Data.Version ( showVersion )
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..) )
import System.Directory ( removeFile )
import System.Environment ( getProgName, getArgs )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr, Handle, IOMode(..), openFile, hClose, hPutStr, hPutStrLn )
#if __GLASGOW_HASKELL__ >= 612
import System.IO ( hGetContents, hSetEncoding, utf8 )
#endif

-- We need to force every file we open to be read in
-- as UTF8
alexReadFile :: FilePath -> IO String
#if __GLASGOW_HASKELL__ >= 612
alexReadFile file = do
  h <- alexOpenFile file ReadMode
  hGetContents h
#else
alexReadFile = readFile
#endif

-- We need to force every file we write to be written
-- to as UTF8
alexOpenFile :: FilePath -> IOMode -> IO Handle
#if __GLASGOW_HASKELL__ >= 612
alexOpenFile file mode = do
  h <- openFile file mode
  hSetEncoding h utf8
  return h
#else
alexOpenFile = openFile
#endif

-- `main' decodes the command line arguments and calls `alex'.  

main:: IO ()
main =	do
 args <- getArgs
 case getOpt Permute argInfo args of
    (cli,_,[]) | DumpHelp `elem` cli -> do
	prog <- getProgramName
        bye (usageInfo (usageHeader prog) argInfo)
    (cli,_,[]) | DumpVersion `elem` cli ->
	bye copyright
    (cli,[file],[]) -> 
	runAlex cli file
    (_,_,errors) -> do
	prog <- getProgramName
        die (concat errors ++ usageInfo (usageHeader prog) argInfo)

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

	Right script -> return script

alex :: [CLIFlags] -> FilePath -> FilePath
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
  
   let target 
	| OptGhcTarget `elem` cli = GhcTarget
	| otherwise               = HaskellTarget

   template_dir  <- templateDir getDataDir cli
   let template_name = templateFile template_dir target cli
		
   -- open the output file; remove it if we encounter an error
   bracketOnError 
        (alexOpenFile o_file WriteMode)
	(\h -> do hClose h; removeFile o_file)
	$ \out_h -> do

   let
	 (maybe_header, directives, scanner1, maybe_footer) = script
 	 (scanner2, scs, sc_hdr) = encodeStartCodes scanner1
	 (scanner_final, actions) = extractActions scanner2
 
   wrapper_name <- wrapperFile template_dir directives

   hPutStr out_h (optsToInject target cli)
   injectCode maybe_header file out_h

   hPutStr out_h (importsToInject target cli)

   -- add the wrapper, if necessary
   when (isJust wrapper_name) $
	do str <- alexReadFile (fromJust wrapper_name)
	   hPutStr out_h str

   let dfa = scanner2dfa scanner_final scs
       nm  = scannerName scanner_final

   put_info (infoDFA 1 nm dfa "")
   hPutStr out_h (outputDFA target 1 nm dfa "")

   injectCode maybe_footer file out_h

   hPutStr out_h (sc_hdr "")
   hPutStr out_h (actions "")

   -- add the template
   tmplt <- alexReadFile template_name
   hPutStr out_h tmplt

   hClose out_h
   finish_info

-- inject some code, and add a {-# LINE #-} pragma at the top
injectCode :: Maybe (AlexPosn,Code) -> FilePath -> Handle -> IO ()
injectCode Nothing _ _ = return ()
injectCode (Just (AlexPn _ ln _,code)) filename hdl = do
  hPutStrLn hdl ("{-# LINE " ++ show ln ++ " \"" ++ filename ++ "\" #-}")
  hPutStrLn hdl code

optsToInject :: Target -> [CLIFlags] -> String
optsToInject GhcTarget _ = "{-# LANGUAGE CPP,MagicHash #-}\n"
optsToInject _         _ = "{-# LANGUAGE CPP #-}\n"

importsToInject :: Target -> [CLIFlags] -> String
importsToInject _ cli = always_imports ++ debug_imports ++ glaexts_import
  where
	glaexts_import | OptGhcTarget `elem` cli    = import_glaexts
		       | otherwise                  = ""

	debug_imports  | OptDebugParser `elem` cli = import_debug
		       | otherwise		   = ""

-- CPP is turned on for -fglasogw-exts, so we can use conditional
-- compilation.  We need to #include "config.h" to get hold of
-- WORDS_BIGENDIAN (see GenericTemplate.hs).

always_imports :: String
always_imports = "#if __GLASGOW_HASKELL__ >= 603\n" ++
		 "#include \"ghcconfig.h\"\n" ++
		 "#elif defined(__GLASGOW_HASKELL__)\n" ++
		 "#include \"config.h\"\n" ++
		 "#endif\n" ++
		 "#if __GLASGOW_HASKELL__ >= 503\n" ++
		 "import Data.Array\n" ++
		 "import Data.Char (ord)\n" ++
		 "import Data.Array.Base (unsafeAt)\n" ++
		 "#else\n" ++
		 "import Array\n" ++
		 "import Char (ord)\n" ++
		 "#endif\n"

import_glaexts :: String
import_glaexts = "#if __GLASGOW_HASKELL__ >= 503\n" ++
		 "import GHC.Exts\n" ++
		 "#else\n" ++
		 "import GlaExts\n" ++
		 "#endif\n"

import_debug :: String
import_debug   = "#if __GLASGOW_HASKELL__ >= 503\n" ++
		 "import System.IO\n" ++
		 "import System.IO.Unsafe\n" ++
		 "import Debug.Trace\n" ++
		 "#else\n" ++
		 "import IO\n" ++
		 "import IOExts\n" ++
		 "#endif\n"

templateDir :: IO FilePath -> [CLIFlags] -> IO FilePath
templateDir def cli
  = case [ d | OptTemplateDir d <- cli ] of
      [] -> def
      ds -> return (last ds)

templateFile :: FilePath -> Target -> [CLIFlags] -> FilePath
templateFile dir target cli
  = dir ++ "/AlexTemplate" ++ maybe_ghc ++ maybe_debug
  where 
	maybe_ghc = case target of
                      GhcTarget -> "-ghc"
                      _         -> ""

	maybe_debug
	  | OptDebugParser `elem` cli  = "-debug"
	  | otherwise		       = ""

wrapperFile :: FilePath -> [Directive] -> IO (Maybe FilePath)
wrapperFile dir directives =
  case [ f | WrapperDirective f <- directives ] of
	[]  -> return Nothing
	[f] -> return (Just (dir ++ "/AlexWrapper-" ++ f))
	_many -> dieAlex "multiple %wrapper directives"

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
  hPutStrLn h ("Info file produced by Alex version " ++ projectVersion ++ 
		", from " ++ file)
  hPutStrLn h hline
  hPutStr h "\n"

initialParserEnv :: (Map String CharSet, Map String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv :: Map String CharSet
initSetEnv = Map.fromList [("white", charSet " \t\n\v\f\r"),
		           ("printable", charSet [chr 32 .. chr 126]),
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
  | OptTemplateDir FilePath
  | DumpHelp
  | DumpVersion
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
   Option ['d'] ["debug"] (NoArg OptDebugParser)
	"produce a debugging scanner",
   Option ['?'] ["help"] (NoArg DumpHelp)
	"display this help and exit",
   Option ['V','v'] ["version"] (NoArg DumpVersion)  -- ToDo: -v is deprecated!
	"output version information and exit"
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

#if __GLASGOW_HASKELL__ < 610
bracketOnError
	:: IO a		-- ^ computation to run first (\"acquire resource\")
	-> (a -> IO b)  -- ^ computation to run last (\"release resource\")
	-> (a -> IO c)	-- ^ computation to run in-between
	-> IO c		-- returns the value from the in-between computation
bracketOnError before after thing =
  block (do
    a <- before 
    r <- Exception.catch 
	   (unblock (thing a))
	   (\e -> do { after a; throw e })
    return r
 )
#endif
