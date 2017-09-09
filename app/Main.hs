{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.List                  (foldl', isPrefixOf)
import qualified Data.Set                   as Set
import qualified Data.Text.Lazy             as TL
import           System.Console.Repline
import           System.Exit
import           Text.Megaparsec

import           Language.Nuclear.Emit
import           Language.Nuclear.Eval
import           Language.Nuclear.Parser
import           Language.Nuclear.Syntax

type Repl a = HaskelineT (StateT IState IO) a

newtype IState = IState
  { termctx :: TermEnv
  }

initState :: IState
initState = IState emptyTermEnv

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

hoistParseErr ::
     (ShowErrorComponent e)
  => TL.Text
  -> Either (ParseError (Token TL.Text) e) a
  -> Repl a
hoistParseErr _ (Right val) = return val
hoistParseErr stream (Left err) = do
  liftIO $ putStr $ parseErrorPretty' stream err
  abort

instance ShowErrorComponent Decl where
  showErrorComponent = show

cmd :: String -> Repl ()
cmd = emit . TL.pack

emit :: TL.Text -> Repl ()
emit source = do
  st <- get
  modl <- hoistParseErr source $ runParseModule "<stdin>" source
  let st' = st {termctx = foldl' evalDef (termctx st) modl}
  put st'
  case lookup "it" modl of
    Nothing -> return ()
    Just ex -> do
      let ir = codegen initModule [ex]
      liftIO ir
      abort

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
  where
    (_, tmctx') = runEval env nm ex

comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":help", ":quit"]
  return $ filter (isPrefixOf n) cmds

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

help :: [String] -> Repl ()
help args = liftIO . print $ "Help: " ++ show args

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

opts :: [(String, [String] -> Repl ())]
opts = [("help", help), ("quit", quit)]

pre :: Repl ()
pre = liftIO $ putStrLn "Welcome!"

repl :: IO ()
repl = flip evalStateT initState $ evalRepl ">>> " cmd opts completer pre

main :: IO ()
main = repl
