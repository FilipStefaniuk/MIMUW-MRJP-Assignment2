module Main where
    
    
    import System.IO ( stdin, hGetContents )
    import System.Environment ( getArgs, getProgName )
    import System.Exit ( exitFailure, exitSuccess )
    
    import LexLatte
    import ParLatte
    -- import SkelLatte
    import PrintLatte
    import AbsLatte
    
    import qualified PrintLLVM

    import Frontend
    import Middleend
    
    
    
    import ErrM
    
    type ParseFun a = [Token] -> Err a
    
    myLLexer = myLexer
    
    type Verbosity = Int
    
    putStrV :: Verbosity -> String -> IO ()
    putStrV v s = if v > 1 then putStrLn s else return ()
    
    runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
    runFile v p f = putStrLn f >> readFile f >>= run v p
    
    run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
    run v p s = let ts = myLLexer s in case pProgram ts of
               Bad s    -> do putStrLn "\nParse              Failed...\n"
                              putStrV v "Tokens:"
                              putStrV v $ show ts
                              putStrLn s
                              exitFailure
               Ok  tree -> case checkProgram tree of
                Left e -> putStrLn $ show e
                Right _ -> do
                  t <- transProgram tree
                  putStrV 2 $ PrintLLVM.printTree t
                -- Right _ -> putStrV 2 $ printTree tree
    
    showTree :: (Show a, Print a) => Int -> a -> IO ()
    showTree v tree
     = do
          putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
          putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
    
    usage :: IO ()
    usage = do
      putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (no arguments)  Parse stdin verbosely."
        , "  (files)         Parse content of files verbosely."
        , "  -s (files)      Silent mode. Parse content of files silently."
        ]
      exitFailure
    
    main :: IO ()
    main = do
      args <- getArgs
      case args of
        ["--help"] -> usage
        [] -> hGetContents stdin >>= run 2 pProgram
        "-s":fs -> mapM_ (runFile 0 pProgram) fs
        fs -> mapM_ (runFile 2 pProgram) fs