{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
module MVar where

import qualified Control.Concurrent as Concurrent

import Polysemy
import Polysemy.Internal (send)

data MVarConcurrency monad a where
  NewEmptyMVar :: MVarConcurrency monad (Concurrent.MVar a)
  PutMVar :: Concurrent.MVar a -> a -> MVarConcurrency monad ()
  TakeMVar :: Concurrent.MVar a -> MVarConcurrency monad a

newEmptyMVar :: Member MVarConcurrency r => Sem r (Concurrent.MVar a)
newEmptyMVar = send NewEmptyMVar

putMVar :: Member MVarConcurrency r => Concurrent.MVar a -> a -> Sem r ()
putMVar mvar value = send (PutMVar mvar value)

takeMVar :: Member MVarConcurrency r => Concurrent.MVar a -> Sem r a
takeMVar mvar = send (TakeMVar mvar)

mVarConcurrencyToIO :: Member (Embed IO) r => Sem (MVarConcurrency ': r) a -> Sem r a
mVarConcurrencyToIO = 
  interpretH
    (\case
        NewEmptyMVar ->
          do mvar <- embed Concurrent.newEmptyMVar
             pureT mvar
        PutMVar mvar value ->
          do embed (Concurrent.putMVar mvar value)
             pureT ()
        TakeMVar mvar ->
          do value <- embed (Concurrent.takeMVar mvar)
             pureT value)

runMVarConcurrency' :: Sem (MVarConcurrency ': r) a -> Sem (Embed IO ': r) a
runMVarConcurrency' =
  reinterpretH
    (\case
        NewEmptyMVar ->
          do mvar <- embed Concurrent.newEmptyMVar
             pureT mvar
        PutMVar mvar value ->
          do embed (Concurrent.putMVar mvar value)
             pureT ()
        TakeMVar mvar ->
          do value <- embed (Concurrent.takeMVar mvar)
             pureT value)

runMVarConcurrency :: Sem '[MVarConcurrency] a -> IO a
runMVarConcurrency = runM . runMVarConcurrency'

