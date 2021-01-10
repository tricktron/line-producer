module Main where

import Control.Monad.Reader
  ( MonadIO,
    MonadReader (ask),
    MonadTrans (lift),
    ReaderT,
    liftIO,
    runReaderT,
  )
import System.IO (hGetLine, hIsEOF, hClose, IOMode( ReadMode ), openFile)

type Generator e m = ReaderT (e -> m ()) m

type Producer m e = Generator e m ()

type Consumer m e = e -> m ()

yield :: Monad m => e -> Producer m e
yield e = ask >>= \f -> lift $ f e

runGenerator :: Monad m => Producer m e -> Consumer m e -> m ()
runGenerator m f = runReaderT m f

eachLine :: MonadIO m => Producer m String
eachLine =
  do
    file <- liftIO $ openFile "./test.txt" ReadMode
    go file
    liftIO $ hClose file
  where
    go file = do
      done <- liftIO $ hIsEOF file
      if done
        then return ()
        else liftIO (hGetLine file) >>= yield >> go file

main :: IO ()
main = eachLine `runGenerator` putStrLn