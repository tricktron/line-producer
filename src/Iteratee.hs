module Iteratee where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Coroutine (Coroutine, resume, suspend)

type Iteratee a m res = Coroutine ((->) a) m res

await :: Monad m => Iteratee a m a
await = suspend return

runIteratee :: Monad m => [a] -> Iteratee a m res -> m res
runIteratee xs iter = do
  eitherCo <- resume iter
  case eitherCo of
    (Left co) ->
      case xs of
          [] -> runIteratee [] $ co $ error "No more values to process"
          (x:xs') -> runIteratee xs' $ co x
    (Right x) -> return x

main :: IO ()
main = do
  let iteratee = do
        lift $ putStr "Enter two numbers: "
        a <- await
        b <- await
        lift $ putStrLn $ "Sum is " ++ show (a + b)
  runIteratee [88, 42] iteratee