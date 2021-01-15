module Generator where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Coroutine ( Coroutine(resume), suspend )

type Generator a m res = Coroutine ((,) a) m res

runGenerator :: Monad m => Generator a m res -> m ([a], res)
runGenerator = go id
  where
    go f g =
      resume g
        >>= either
          (\(a, cont) -> go (f . (a :)) cont)
          (\res -> return (f [], res))

yield :: (Monad m) => a -> Generator a m ()
yield a = suspend (a, return ())

main :: IO ([Integer], Integer)
main = do
  let gen = do
        lift $ putStr "I am first, "
        yield 1
        lift $ putStr "I am second, "
        yield 2
        lift $ putStr "I am third, "
        return 3
  runGenerator gen