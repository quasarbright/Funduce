{-# LANGUAGE FlexibleContexts #-}

module Funduce.Repl(run) where

import Funduce.Parsing.ParseUtils(SS)
import Funduce.Parsing.ParseSexpr
import Funduce.Parsing.ConvertSexpr
import Funduce.Static.Desugar
import Funduce.Dynamic.Eval

import Data.List (isPrefixOf)

import System.Exit
import System.Environment
import System.Console.Repline
import Control.Monad.State.Strict
import Control.Monad.Except (runExcept)
import qualified Data.Map as Map

type IState = (Env SS, Store SS)

emptyState :: IState
emptyState = (emptyEnv, emptyStore)

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO . print $ err
  abort

---------------------------------------------------------------------
-- execution
---------------------------------------------------------------------

exec :: String -> Repl ()
exec source = do
    (env,store) <- get
    sexpr <- hoistErr $ parseSexpr "<stdin>" source
    -- TODO wf
    aexpr <- hoistErr $ runExcept (convertExpr sexpr)
--    liftIO $ print aexpr
    let cexpr = desugarExpr aexpr
--    liftIO $ print cexpr
    let (mVal, store') = interpretExpr env store cexpr
    val <- hoistErr mVal
    put (env,store')
    liftIO $ print val

printAnnots :: Repl ()
printAnnots = do
    (env,store) <- get
    let (mAnnots, _) = interpretEnv env store
    annots <- hoistErr mAnnots
    liftIO $ mapM_ putStrLn [x++" = "++show v | (x,v) <- Map.toList annots]

---------------------------------------------------------------------
-- commands
---------------------------------------------------------------------

load :: [String] -> Repl ()
load (file:_) = do
    contents <- liftIO $ readFile file
    (env,store) <- get
    sexprs <- hoistErr $ parseSexprs file contents
    aprog <- hoistErr $ runExcept (convertProgram sexprs)
--    liftIO $ print aprog
    let cprog = desugarProgram aprog
--    liftIO $ print cprog
    let (mEnv',store') = interpretProgram env store cprog
    env' <- hoistErr mEnv'
    put (env',store')
--    printAnnots [name | Decl name _ <- decls] envC'
load [] = liftIO $ putStrLn "must load a file"

browse :: a -> Repl ()
browse _ = printAnnots

quit :: a -> Repl ()
quit _ =  liftIO exitSuccess

---------------------------------------------------------------------
-- Interactive Shell
---------------------------------------------------------------------

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
    [ (":load", fileCompleter)
    ]

comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":quit"]
  (env,_) <- get
  let names = Map.keys env
  return $ filter (isPrefixOf n) (cmds ++ names)

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load"   , load)
  , ("quit"   , quit)
  , ("browse", browse)
  ]

---------------------------------------------------------------------
-- Entry Point
---------------------------------------------------------------------


completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT emptyState
     $ evalRepl (pure "Funduce> ") exec opts (Just ':') completer pre

run :: IO ()
run = do
    args <- getArgs
    case args of
        [] -> shell (return ())
        fname:_ -> shell (load [fname])
