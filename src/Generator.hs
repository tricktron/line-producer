module Generator where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Coroutine ( Coroutine(resume), suspend )

type Generator a m res = Coroutine ((,) a) m res

runGenerator :: Monad m => Generator a m res -> m ([a], res)
runGenerator = go id
  where
    go f gen = do
      eitherGen <- resume gen
      case eitherGen of
        (Left (x, gen)) -> go (f . (x:)) gen
        (Right x)  -> return (f [], x)

yield :: Monad m => a -> Generator a m ()
yield a = suspend (a, return ())

main :: IO ([Integer], Integer)
main = do
  let generator = do
        lift $ putStr "I am first, "
        yield 1
        lift $ putStr "I am second, "
        yield 2
        lift $ putStr "I am third, "
        return 3
  runGenerator generator