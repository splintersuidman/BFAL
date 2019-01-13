module Main where

import System.Environment
import Target.Compiler

main :: IO ()
main =
  do args <- getArgs
     output <- either
       (\err -> ioError $ userError $ "Could not compile program: " ++ err ++ ".")
       return
       (compileProgram (args !! 0))
     mapM_ (putStr . show) output
     putStrLn ""
