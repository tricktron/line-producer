module LineProducer where

import Coroutine
import System.IO (hGetLine, hIsEOF, hClose, IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import Control.Monad.Trans.Class (MonadTrans(lift))

type Consumer a m res = Coroutine ((->) a) m res
type Producer a m res = Coroutine ((,) a) m res

await :: Monad m => Consumer a m a
await = suspend return

yield :: Monad m => a -> Producer a m ()
yield a = suspend (a, return ())

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
    a <- ma
    b <- mb
    f a b

pipe2 :: Monad m => Producer a m x -> Consumer (Maybe a) m y -> m (x, y)
pipe2 gen iter = bindM2 go (resume gen) (resume iter) where
    go (Left (x, gen')) (Left funIter) = pipe2 gen' (funIter $ Just x)
    go (Right x) (Right y)             = return (x, y)
    go (Left (x, gen')) (Right y)      = pipe2 gen' $ return y
    go (Right x) (Left funIter)        = pipe2 (return x) (funIter Nothing)


pipe :: Monad m => Producer a m x -> Consumer a m y -> m (x, y)
pipe gen iter = bindM2 go (resume gen) (resume iter) where
    go (Left (x, gen')) (Left funIter) = pipe gen' (funIter x)
    go (Right x) (Right y)             = return (x, y)
    go (Left (x, gen')) (Right y)      = pipe gen' $ return y
    go (Right x) (Left funIter)        = error "The producer ended too soon"



class LineProducerConsumer a where
    onLine :: Monad m => Producer a m x -> Consumer (Maybe a) m y -> m (x, y)


data Person = Person {
  name :: String,
  age  :: Int
}

simplePersonProducer :: Producer Person IO ()
simplePersonProducer = do
    yield $ Person "Steve Jobs" 65
    yield $ Person "Elon Musk" 40
    return ()

simpleIntProducer :: Int -> Int -> Producer Int IO ()
simpleIntProducer start end | start == end = do return ()
                            | otherwise    = do
                                               yield start
                                               simpleIntProducer (start + 1) end

simplePersonConsumer :: Consumer Person IO Int
simplePersonConsumer = do
  p1 <- await
  p2 <- await
  return $ age p1 + age p2

eachLine :: String -> Producer String IO ()
eachLine filePath =
  do
    file <- lift $ openFile filePath ReadMode
    go file
    lift $ hClose file
  where
    go file = do
      done <- lift $ hIsEOF file
      if done
        then return Nothing
        else lift (hGetLine file) >>= yield >> go file

printEachLine :: Show a => Consumer (Maybe a) IO ()
printEachLine = do
    line <- await
    case line of
        (Just l) -> do
            lift $ putStrLn $ show l
            printEachLine
        (Nothing) -> return ()

main :: IO ((), ())
main = pipe2 (simpleIntProducer 0 10) printEachLine





