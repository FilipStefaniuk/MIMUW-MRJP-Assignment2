{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Process   
import System.Exit 
import System.IO
import System.FilePath.Posix
import Control.Monad.Error

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Compiler
import Printer

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

parseProgram :: String -> ErrorT GenMError IO Program
parseProgram input = case pProgram $ myLexer input of
    Bad e -> throwError . GenMError $ e
    Ok a -> return a

compile :: String -> IO String
compile input = runErrorT (parseProgram input >>= gen) >>= \case
    Left e -> hPutStrLn stderr "ERROR" >> hPutStrLn stderr (show e) >> exitWith (ExitFailure 1)
    Right program -> hPutStrLn stderr "OK" >> (return . printProgram $ program)
