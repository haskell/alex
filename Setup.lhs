#!/usr/bin/runhaskell

\begin{code}
module Main where

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Setup ( BuildFlags(..), buildVerbose )
import Distribution.Simple ( defaultMainWithHooks, defaultUserHooks, UserHooks(..) )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program

import System.FilePath ((</>))
import System.IO.Error ( try )
import System.Directory (removeFile)

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{ hookedPrograms = [perlProgram],
					      postBuild = myPostBuild,
					      postClean = myPostClean,
					      copyHook  = myCopy,
					      instHook  = myInstall }

perlProgram = simpleProgram "perl"

-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
crazy_perl_regexp =
 "s/^#\\s+(\\d+)\\s+(\"[^\"]*\")/{-# LINE \\1 \\2 #-}/g;s/\\$(Id:.*)\\$/\\1/g"

myPostBuild _ flags _ lbi = do
  let runProgram p = rawSystemProgramConf (buildVerbose flags) p (withPrograms lbi)
      cpp_template src dst opts = do
        runProgram ghcProgram (["-o", dst, "-E", "-cpp", "templates" </> src] ++ opts)
	runProgram perlProgram ["-i.bak", "-pe", crazy_perl_regexp, dst]

  sequence_ ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
  	     [ cpp_template "wrappers.hs"        dst opts | (dst,opts) <- wrappers ])

myPostClean _ _ _ _ = mapM_ (try . removeFile) all_template_files

myInstall pkg_descr lbi hooks flags =
  instHook defaultUserHooks pkg_descr' lbi hooks flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

myCopy pkg_descr lbi hooks copy_flags =
  copyHook defaultUserHooks pkg_descr' lbi hooks copy_flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

all_template_files :: [FilePath]
all_template_files = map fst (templates ++ wrappers)

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
  ("AlexWrapper-strict-bytestring", ["-DALEX_STRICT_BYTESTRING"]),
  ("AlexWrapper-posn",  ["-DALEX_POSN"]),
  ("AlexWrapper-posn-bytestring", ["-DALEX_POSN_BYTESTRING"]),
  ("AlexWrapper-monad", ["-DALEX_MONAD"]),
  ("AlexWrapper-monad-bytestring", ["-DALEX_MONAD_BYTESTRING"]),
  ("AlexWrapper-monadUserState", ["-DALEX_MONAD", "-DALEX_MONAD_USER_STATE"]),
  ("AlexWrapper-monadUserState-bytestring", ["-DALEX_MONAD_BYTESTRING", "-DALEX_MONAD_USER_STATE"]),
  ("AlexWrapper-gscan", ["-DALEX_GSCAN"])
 ]

\end{code}
