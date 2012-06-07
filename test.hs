import System.Cmd (system)
import System.Exit (exitWith)

main = system "make -k -C tests clean all" >>= exitWith

