{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RWT where

-- Monade, die I/O und Zustand kann

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Control.Monad.State.Class (MonadState, get, put)

import qualified Data.IORef as IORef
import Data.IORef (IORef)

data RWF state next =
    GetString (String -> next)
  | PutString String next
  | GetState (state -> next)
  | PutState state next

instance Functor (RWF state) where
  fmap f (GetString k) = GetString (f . k)
  fmap f (PutString string next) = PutString string (f next)
  fmap f (GetState k) = GetState (f . k)
  fmap f (PutState state next) = PutState state (f next)

type RWT' state = FreeT (RWF state)

runRWFIO :: (forall a . monad a -> IO a) -> IORef state -> RWT' state monad a -> IO a
runRWFIO lower ioref action =
  do x <- lower (runFreeT action)
     case x of
       Pure r -> return r
       Free (GetState k) ->
         do state <- IORef.readIORef ioref
            runRWFIO lower ioref (k state)
       Free (PutState state action') ->
         do IORef.writeIORef ioref state
            runRWFIO lower ioref action'
       Free (GetString k) ->
         do string <- getLine
            runRWFIO lower ioref (k string)
       Free (PutString string action') ->
         do putStrLn string
            runRWFIO lower ioref action'

newtype RWT state monad a = RWT { unRWT :: RWT' state monad a }
  deriving (Functor, Applicative, Monad, MonadTrans)

putString :: Monad monad => String -> RWT state monad ()
putString string = RWT (liftF (PutString string ()))

getString :: Monad monad => RWT state monad String
getString = RWT (liftF (GetString id))

getState :: Monad monad => RWT state monad state
getState = RWT (liftF (GetState id))

putState :: Monad monad => state -> RWT state monad ()
putState state = RWT (liftF (PutState state ()))

instance Monad monad => MonadState state (RWT state monad) where
  get = getState
  put = putState

-- RWT auf IO reduzieren
runRWTIO :: (forall a . monad a -> IO a) -> IORef state -> RWT state monad a -> IO a
runRWTIO lower ioref action = runRWFIO lower ioref (unRWT action)

-- Zahl einlesen
getNumber :: (Num a, Ord a, Read a, Show a, Monad monad) => (a, a) -> RWT state monad a
getNumber (lo, hi) = do
  s <- getString
  let input = read s
  if lo <= input && input <= hi
  then return input
  else
    do putString ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
       getNumber (lo, hi)
