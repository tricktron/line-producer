{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Producer where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Coroutine ( Coroutine(resume), suspend )

-- type Generator a m res = Coroutine ((,) a) m res

class Producer a m where
  yield :: Monad m => a -> Coroutine ((,) a) m ()
  yield a = suspend (a, return ())



instance Producer Int IO where
  yield :: Monad m => Int -> Coroutine ((,) Int) m ()
  yield i = suspend (i, return ())


simpleIntProducer :: (Eq t, Monad m, Producer t m, Num t) => t -> t -> Coroutine ((,) t) m ()
simpleIntProducer start end | start == end = do return ()
                            | otherwise    = do
                                               yield start
                                               simpleIntProducer (start + 1) end


runGenerator :: Monad m => Coroutine ((,) a) m res -> m ([a], res)
runGenerator = go id
  where
    go f gen = do
      eitherGen <- resume gen
      case eitherGen of
        (Left (x, gen)) -> go (f . (x:)) gen
        (Right x)  -> return (f [], x)


main :: IO ([Int], ())
main = do
  runGenerator (yield 3)