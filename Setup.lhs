#!/usr/bin/runhaskell

\begin{code}
module Main where

import Control.Exception ( finally )
import Distribution.PackageDescription ( PackageDescription )
import Distribution.Setup ( BuildFlags, CleanFlags, CopyDest(..), CopyFlags(..), InstallFlags )
import Distribution.Simple ( defaultMainWithHooks, defaultUserHooks, UserHooks(..), Args, compilerPath )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), mkDataDir )
import System.Directory
import System.Exit ( ExitCode(..) )
import System.IO
import System.IO.Error
import System.Process
import System.Cmd
import Text.Printf ( printf )

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{ postBuild = myPostBuild,
					      postClean = myPostClean,
					      postCopy  = myPostCopy,
					      postInst  = myPostInstall }

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
myPostBuild _ _ _ lbi =
  excursion "templates" $ do
  let cpp_template src dst opts = do
	let dst_pp = dst ++ ".hspp"
	    ghc = compilerPath (compiler lbi)
	    ghc_args = ["-o", dst_pp, "-E", "-cpp", src] ++ opts
		-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
		-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
	    perl_args = ["-pe", "s/^#\\s+(\\d+)\\s+(\"[^\"]*\")/{-# LINE \\1 \\2 #-}/g;s/\\$(Id:.*)\\$/\\1/g", dst_pp]
	mb_perl <- findExecutable "perl"
	perl <- case mb_perl of
		  Nothing -> ioError (userError "You need \"perl\" installed and on your PATH to complete the build")
		  Just path -> return path
	do_cmd ghc ghc_args `cmd_seq` do_cmd_out perl perl_args dst

  cmd_seqs ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
  	    [ cpp_template "wrappers.hs"        dst opts | (dst,opts) <- wrappers ])

myPostClean :: Args -> CleanFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO ExitCode
myPostClean _ _ _ _ =
  excursion "templates" $ do
  sequence [ try (removeFile f) >> try (removeFile (f ++ ".hspp"))
	   | (f,_) <- all_templates]
  return ExitSuccess

myPostInstall :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
myPostInstall _ _ pkg_descr lbi =
  install pkg_descr lbi NoCopyDest

myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
myPostCopy _ copy_flags pkg_descr lbi = 
  install pkg_descr lbi (copyDest copy_flags)

install :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ExitCode
install pkg_descr lbi copy_dest =
  excursion "templates" $ do
  let dataDir = mkDataDir pkg_descr lbi copy_dest
  createDirectoryIfMissing True dataDir
  sequence [ copyFile f (dataDir ++ '/':f) | (f,_) <- all_templates ]  
	-- XXX: joinFileName
  return ExitSuccess

all_templates :: [(FilePath,[String])]
all_templates = templates ++ wrappers

templates :: [(FilePath,[String])]
templates = [
  ("AlexTemplate",           []),
  ("AlexTemplate-ghc",       ["-DALEX_GHC"]),
  ("AlexTemplate-ghc-debug", ["-DALEX_GHC","-DALEX_DEBUG"]),
  ("AlexTemplate-debug",     ["-DALEX_DEBUG"])
 ]

wrappers :: [(FilePath,[String])]
wrappers = [
  ("AlexWrapper-basic", ["-DALEX_BASIC"]),
  ("AlexWrapper-basic-bytestring", ["-DALEX_BASIC_BYTESTRING"]),
  ("AlexWrapper-posn",  ["-DALEX_POSN"]),
  ("AlexWrapper-posn-bytestring", ["-DALEX_POSN_BYTESTRING"]),
  ("AlexWrapper-monad", ["-DALEX_MONAD"]),
  ("AlexWrapper-monad-bytestring", ["-DALEX_MONAD_BYTESTRING"]),
  ("AlexWrapper-gscan", ["-DALEX_GSCAN"])
 ]

-- -----------------------------------------------------------------------------
-- Utils

do_cmd :: FilePath -> [String] -> IO ExitCode
do_cmd cmd args = do
  putStrLn (unwords (cmd:args))
  ph <- runProcess cmd args Nothing Nothing Nothing Nothing Nothing
  waitForProcess ph

do_cmd_out :: FilePath -> [String] -> FilePath -> IO ExitCode
do_cmd_out cmd args outfile = do
  putStrLn (unwords (cmd:args))
  outh <- openFile outfile WriteMode
  ph <- runProcess cmd args Nothing Nothing Nothing (Just outh) Nothing
  waitForProcess ph

cmd_seq :: IO ExitCode -> IO ExitCode -> IO ExitCode
cmd_seq c1 c2 = do
  e <- c1
  case e of
	ExitSuccess -> c2
	_	    -> return e

cmd_seqs :: [IO ExitCode] -> IO ExitCode
cmd_seqs = foldr cmd_seq (return ExitSuccess)

excursion :: FilePath -> IO a -> IO a
excursion d io = do
  cwd <- getCurrentDirectory
  (do setCurrentDirectory d; io) `finally` setCurrentDirectory cwd
\end{code}
