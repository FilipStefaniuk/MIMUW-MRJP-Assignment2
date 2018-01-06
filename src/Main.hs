module Main where

  import System.Environment
  import System.Process   
  import System.Exit 
  import System.FilePath.Posix

  import LexLatte
  import ParLatte
  import ErrM

  import EmitLLVM
  import Frontend
  import Middleend

  main :: IO ()
  main = getArgs >>= handle

  handle :: [String] -> IO ()
  handle ["-h"] = putStrLn "Usage: latc_llvm [-h] [file...]"  
  handle [] = getContents >>= compile >>= putStr
  handle fs = mapM_ compileFile fs

  compileFile :: String -> IO ExitCode
  compileFile inputFilename = let outputFilename = replaceExtension inputFilename "ll" in
      readFile inputFilename >>= compile >>= writeFile outputFilename >>
      rawSystem "llvm-as" ["-o", "tmp.bc",  outputFilename] >>       
      rawSystem "llvm-link" ["-o", replaceExtension outputFilename "bc", "lib/runtime.bc", "tmp.bc"] >>
      rawSystem "rm" ["tmp.bc"]

  compile :: String -> IO String
  compile input = case pProgram $ myLexer input of
    Bad e -> exitWith (ExitFailure 1)
    Ok a -> case checkProgram a of
      Left e -> return $ show e
      Right _ -> transProgram a >>= return . emit 