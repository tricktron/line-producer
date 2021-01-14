module Coroutine where

newtype Coroutine suspendFun m res = Coroutine
  { resume :: m (Either (suspendFun (Coroutine suspendFun m res)) res)
  }