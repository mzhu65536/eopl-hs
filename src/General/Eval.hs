{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module General.Eval where

import Control.Monad.Except
import Control.Monad.Reader

newtype Eval e w a = Eval { unEval :: ReaderT e (ExceptT w IO) a }
  deriving (Functor, Applicative, Monad, MonadReader e,
            MonadError w, MonadIO)

runEval :: Eval env warn val -> env -> IO (Either warn val)
runEval act env = runExceptT (runReaderT (unEval act) env)
