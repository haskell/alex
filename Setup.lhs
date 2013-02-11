#!/usr/bin/runhaskell

\begin{code}
module Main where

import Distribution.Verbosity
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Setup ( BuildFlags(..), buildVerbosity, fromFlagOrDefault )
import Distribution.Simple ( defaultMainWithHooks, simpleUserHooks, UserHooks(..) )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program

import System.FilePath ((</>))
import Control.Exception ( IOException, try )
import System.Directory (removeFile)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{ postBuild = myPostBuild,
                                             postClean = myPostClean,
                                             copyHook  = myCopy,
                                             instHook  = myInstall }

-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
mungeLinePragma line = case symbols line of
 ["#", number, string] | length string >= 2
                      && head string == '"'
                      && last string == '"'
   -> case reads number of
        [(n, "")] -> "{-# LINE " ++ show (n :: Int) ++ " " ++ string ++ " #-}"
        _         -> line
 -- Also convert old-style CVS lines, no idea why we do this...
 ("--":"$":"Id":":":_) -> filter (/='$') line
 (     "$":"Id":":":_) -> filter (/='$') line
 _ -> line

symbols :: String -> [String]
symbols cs = case lex cs of
              (sym, cs'):_ | not (null sym) -> sym : symbols cs'
              _ -> []

myPostBuild _ flags _ lbi = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity flags)
      runProgram p = rawSystemProgramConf verbosity p (withPrograms lbi)
      cpp_template src dst opts = do
        let tmp = dst ++ ".tmp"
        runProgram ghcProgram (["-o", tmp, "-E", "-cpp", "templates" </> src] ++ opts)
        writeFile dst . unlines . map mungeLinePragma . lines =<< readFile tmp
        removeFile tmp

  sequence_ ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
  	     [ cpp_template "wrappers.hs"        dst opts | (dst,opts) <- wrappers ])

myPostClean _ _ _ _ = let try' = try :: IO a -> IO (Either IOException a)
                      in mapM_ (try' . removeFile) all_template_files

myInstall pkg_descr lbi hooks flags =
  instHook simpleUserHooks pkg_descr' lbi hooks flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

myCopy pkg_descr lbi hooks copy_flags =
  copyHook simpleUserHooks pkg_descr' lbi hooks copy_flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

all_template_files :: [FilePath]
all_template_files = map fst (templates ++ wrappers)

templates :: [(FilePath,[String])]
templates = [
  ("AlexTemplate",           []),
  ("AlexTemplate-ghc",       ["-DALEX_GHC"]),
  ("AlexTemplate-ghc-nopred",["-DALEX_GHC", "-DALEX_NOPRED"]),
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
