module Main (main) where

import           Control.Monad
import           Language.Preprocessor.Cpphs
import           System.Directory
import           System.FilePath

main :: IO ()
main = do
  sequence_ ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
             [ cpp_template "wrappers.hs"        dst opts | (dst,opts) <- wrappers ])

  putStrLn ""
  putStrLn "-- fragment for alex.cabal file"
  putStrLn "data-dir: data/"
  putStrLn ""
  putStrLn "data-files:"
  forM_ all_template_files $ \fn -> putStrLn ("        " ++ fn)
  putStrLn "-- end of fragment"
  putStrLn ""
  putStrLn "You can invoke `cabal sdist` now"

cpp_template :: FilePath -> FilePath -> [String] -> IO ()
cpp_template src0 dst0 defs = do
    ex <- doesFileExist src
    unless ex $
      fail ("file " ++ show src ++ " not found; are you in the right directory?")

    putStrLn ("generating " ++ show dst ++ "  (from " ++ show src ++ ")...")
    createDirectoryIfMissing False "data"
    srcdat <- readFile src
    outdat <- runCpphs cppflags src =<< readFile src
    writeFile dst outdat

    return ()
  where
    src = "templates" </> src0
    dst = "data"      </> dst0

    cppflags = defaultCpphsOptions
      { defines = [(d,"1") | d <- defs ]
      , boolopts = defaultBoolOptions
                   { hashline  = False
                   , locations = True
                   , ansi      = False
                   , macros    = True
                   }
      }

all_template_files :: [FilePath]
all_template_files = map fst (templates ++ wrappers)

templates :: [(FilePath,[String])]
templates = [
  ("AlexTemplate",           []),
  ("AlexTemplate-ghc",       ["ALEX_GHC"]),
  ("AlexTemplate-ghc-nopred",["ALEX_GHC", "ALEX_NOPRED"]),
  ("AlexTemplate-ghc-debug", ["ALEX_GHC","ALEX_DEBUG"]),
  ("AlexTemplate-debug",     ["ALEX_DEBUG"])
 ]

wrappers :: [(FilePath,[String])]
wrappers = [
  ("AlexWrapper-basic",                     ["ALEX_BASIC"]),
  ("AlexWrapper-basic-bytestring",          ["ALEX_BASIC_BYTESTRING"]),
  ("AlexWrapper-strict-bytestring",         ["ALEX_STRICT_BYTESTRING"]),
  ("AlexWrapper-posn",                      ["ALEX_POSN"]),
  ("AlexWrapper-posn-bytestring",           ["ALEX_POSN_BYTESTRING"]),
  ("AlexWrapper-monad",                     ["ALEX_MONAD"]),
  ("AlexWrapper-monad-bytestring",          ["ALEX_MONAD_BYTESTRING"]),
  ("AlexWrapper-monadUserState",            ["ALEX_MONAD", "ALEX_MONAD_USER_STATE"]),
  ("AlexWrapper-monadUserState-bytestring", ["ALEX_MONAD_BYTESTRING", "ALEX_MONAD_USER_STATE"]),
  ("AlexWrapper-gscan",                     ["ALEX_GSCAN"])
 ]
