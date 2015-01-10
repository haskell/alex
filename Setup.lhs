#!/usr/bin/env runhaskell

\begin{code}
{-# OPTIONS -fwarn-unused-imports #-}
module Main where

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Setup (BuildFlags(..), buildVerbosity, fromFlagOrDefault)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program
import Distribution.Simple.Utils (notice)
import Distribution.Verbosity (normal)

import Control.Exception ( IOException, try )
import Control.Monad (unless, when)
import Data.Char
import Data.Maybe (isJust)
import System.Directory (removeFile, createDirectoryIfMissing, copyFile)
import System.FilePath (splitFileName, (</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    buildHook      = myBuild
  , postBuild      = myPostBuild
  , postClean      = myPostClean
  , copyHook       = myCopy
  , instHook       = myInstall
  }

-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into
-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
mungeLinePragma line = case symbols line of
 syms | Just prag <- getLinePrag syms  -> prag
 -- Also convert old-style CVS lines, no idea why we do this...
 ("--":"$":"Id":":":_) -> filter (/='$') line
 (     "$":"Id":":":_) -> filter (/='$') line
 _ -> line

getLinePrag :: [String] -> Maybe String
getLinePrag ("#" : n : string : rest)
  | length rest <= 1   -- clang puts an extra field
  , length string >= 2 && head string == '"' && last string == '"'
  , all isDigit n
  = Just $ "{-# LINE " ++ n ++ " " ++ string ++ " #-}"
getLinePrag other = Nothing

symbols :: String -> [String]
symbols cs = case lex cs of
              (sym, cs'):_ | not (null sym) -> sym : symbols cs'
              _ -> []

{-|
  Alex contains .x and .y files in its source code which need to be
  lexed by Alex and parsed by Happy, respectively.  In order to get
  Alex to compile on systems which don't already have Alex + Happy,
  we must break this circular dependency by putting build artifacts
  into the distribution tarball.  This hook looks for preparsed .hs
  files in the artifacts/ directory if Alex and Happy are not found
  on the system, and copies them to the right place before building
  so that Alex and Happy will not be called.  If Alex and Happy are
  found, then we will produce the build artifacts as usual and then
  copy them to the artifacts/ directory, in anticipation of a later
  run of cabal sdist by the maintainer to prepare a source tarball.
 -}
shippedArtifacts :: [FilePath]
shippedArtifacts =
  [ "alex" </> "alex-tmp" </> "Parser.hs"
  , "alex" </> "alex-tmp" </> "Scan.hs"
  ]

copyArtifact :: FilePath -> FilePath -> FilePath -> IO ()
copyArtifact src dst f = do
  createDirectoryIfMissing True $ dst </> (fst $ splitFileName f)
  copyFile (src </> f) (dst </> f)

myBuild pkg_descr lbi hooks flags = do
  let verbosity = fromFlagOrDefault normal $ buildVerbosity flags
      path      = getProgramSearchPath (withPrograms lbi)
  happyExistence <- programFindLocation happyProgram verbosity path
  alexExistence  <- programFindLocation  alexProgram verbosity path
  let happyAlexExistence = happyExistence >> alexExistence

  unless (isJust happyAlexExistence) $ do
    notice verbosity ("No existing happy parser and/or alex lexer; " ++
                      "using saved artifacts...")
    mapM_ (copyArtifact "artifacts" (buildDir lbi)) shippedArtifacts

  buildHook simpleUserHooks pkg_descr lbi hooks flags

  when (isJust happyAlexExistence) $ do
    notice verbosity "Saving build artifacts for future generations..."
    mapM_ (copyArtifact (buildDir lbi) "artifacts") shippedArtifacts

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
