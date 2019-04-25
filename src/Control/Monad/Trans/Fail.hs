{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Fail where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.StrictIdentity
import Control.Monad.Trans.Class

-- | 'FailT' is a monad transformer which adds a 'MonadFail' instance, defining
-- 'MonadFail.fail' to be 'error'. Note that it is only valid if @m@ is a
-- strict 'Monad'.
newtype FailT m a = FailT { runFailT :: m a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix)

instance Monad m => MonadFail (FailT m) where
  fail = error

instance MonadTrans FailT where
  lift = FailT

-- | A monad which 'fail's with 'error'.
-- Note that this uses 'StrictIdentity', as the transformer 'FailT' is only
-- valid for strict 'Monad's.
type Fail = FailT StrictIdentity
