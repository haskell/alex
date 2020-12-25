{-# LANGUAGE CPP #-}
module Scan (lexToken) where

#if ALEX_BOOTSTRAP
import Scan.Bootstrapped (lexToken)
#else
import Scan.Oracle (lexToken)
#endif
