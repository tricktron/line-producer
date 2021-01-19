module Pipe where

import Generator (yield,  Generator )
import Iteratee (await,  Iteratee )
import Coroutine ( Coroutine(resume) )
import Control.Monad.Trans.Class (MonadTrans(lift))
import System.IO (hGetLine, hIsEOF, hClose, IOMode( ReadMode ), openFile)

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
    a <- ma
    b <- mb
    f a b

pipe :: Monad m => Generator a m x -> Iteratee a m y -> m (x, y)
pipe gen iter = bindM2 go (resume gen) (resume iter) where
    go (Left (x, gen')) (Left funIter) = pipe gen' (funIter x)
    go (Right x) (Right y)             = return (x, y)
    go (Left (x, gen')) (Right y)      = pipe gen' $ return y
    go (Right x) (Left funIter)        = error "The producer ended too soon"

pipe2 :: Monad m => Generator a m x -> Iteratee (Maybe a) m y -> m (x, y)
pipe2 gen iter = bindM2 go (resume gen) (resume iter) where
    go (Left (x, gen')) (Left funIter) = pipe2 gen' (funIter $ Just x)
    go (Right x) (Right y)             = return (x, y)
    go (Left (x, gen')) (Right y)      = pipe2 gen' $ return y
    go (Right x) (Left funIter)        = pipe2 (return x) (funIter Nothing)

simpleProducer :: Coroutine ((,) Integer) IO Integer
simpleProducer = do
    lift $ putStr "I am first, "
    yield 1
    lift $ putStr "I am second, "
    yield 2
    lift $ putStr "I am third, "
    return 3

eachLine :: Generator String IO ()
eachLine =
  do
    file <- lift $ openFile "./test.txt" ReadMode
    go file
    lift $ hClose file
  where
    go file = do
      done <- lift $ hIsEOF file
      if done
        then return ()
        else lift (hGetLine file) >>= yield >> go file


printEachLine :: Iteratee (Maybe String) IO ()
printEachLine = do
    line <- await
    case line of
        (Just l) -> do
            lift $ putStrLn l
            printEachLine
        (Nothing) -> return ()


simpleConsumer :: Iteratee Integer IO ()
simpleConsumer = do
        lift $ putStr "Enter two numbers: "
        a <- await
        b <- await
        lift $ putStrLn $ "Sum is " ++ show (a + b)

main :: IO ((), ())
main = do
    pipe2 eachLine printEachLine
