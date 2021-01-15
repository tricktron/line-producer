
module Coroutine where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (MonadTrans (..))

newtype Coroutine suspendFun m res = Coroutine
  { resume :: m (Either (suspendFun (Coroutine suspendFun m res)) res)
  }

instance (Functor suspendFun, Functor f) => Functor (Coroutine suspendFun f) where
  fmap = fmap

instance (Functor suspendFun, Monad m) => Applicative (Coroutine suspendFun m) where
  pure = pure
  (<*>) = (<*>)

instance (Functor suspendFun, Monad m) => Monad (Coroutine suspendFun m) where
  return x = Coroutine $ return $ Right x
  co >>= f =
    Coroutine
      ( resume co
          >>= either
            (return . Left . fmap (>>= f))
            (resume . f)
      )

instance (Functor suspendFun) => MonadTrans (Coroutine suspendFun) where
  lift = Coroutine . liftM Right

suspend :: (Functor suspendFun, Monad m) => suspendFun (Coroutine suspendFun m res) -> Coroutine suspendFun m res
suspend suspendFun = Coroutine $ return $ Left suspendFun