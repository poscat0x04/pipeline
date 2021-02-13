{-# LANGUAGE DerivingVia #-}

module Pipeline where

import Control.Monad.Trans.Cont
import Data.Functor.Identity

newtype ContPipe r i o m a = MakePipe {runPipe :: (a -> Result r m i o) -> Result r m i o}
  deriving
    ( Functor,
      Applicative,
      Monad
    )
    via (Cont (Result r m i o))

type Result r m i o = InCont r m i -> OutCont r m o -> m r

newtype InCont r m i = MakeInCont {resumeIn :: OutCont r m i -> m r}

newtype OutCont r m o = MakeOutCont {resumeOut :: Maybe o -> InCont r m o -> m r}

suspendIn :: Result r m i o -> InCont r m i -> InCont r m o
suspendIn k ik = MakeInCont \ok -> k ik ok

suspendOut :: (Maybe i -> Result r m i o) -> OutCont r m o -> OutCont r m i
suspendOut k ok = MakeOutCont \v ik -> k v ik ok

emptyIk :: InCont r m a
emptyIk = MakeInCont \ok -> resumeOut ok Nothing emptyIk

sinkOk :: OutCont r m o
sinkOk = MakeOutCont \_ ik -> resumeIn ik sinkOk

await :: ContPipe r i o m (Maybe i)
await = MakePipe \k ik ok -> resumeIn ik (suspendOut k ok)

yield :: o -> ContPipe r i o m ()
yield v = MakePipe \k ik ok -> resumeOut ok (Just v) (suspendIn (k ()) ik)

(.|) :: forall r i e o m a. ContPipe r i e m () -> ContPipe r e o m a -> ContPipe r i o m a
p .| q = MakePipe \k ik ok ->
  runPipe
    q
    (\a _ ok' -> k a emptyIk ok')
    (suspendIn (runPipe p (\() -> f)) ik)
    ok
  where
    f :: Result r m i e
    f _ ok = resumeOut ok Nothing emptyIk

runContPipe :: Applicative m => ContPipe a () () m a -> m a
runContPipe p = runPipe p (\a _ _ -> pure a) ik ok
  where
    ik = MakeInCont \ok' -> resumeOut ok' (Just ()) ik
    ok = MakeOutCont \_ ik' -> resumeIn ik' ok
