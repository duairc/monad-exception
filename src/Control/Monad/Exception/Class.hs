{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports the 'MonadException' type class.

-}

module Control.Monad.Exception.Class
    ( -- * The @MonadException@ class
      MonadException (..)
    )
where

import           Control.Exception (Exception, SomeException (..))
import qualified Control.Exception as E
import           Control.Monad.Base (liftBase)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Control
                     ( MonadBaseControl (..)
                     , MonadTransControl (..)
                     , Run
                     , control
                     )
import           Prelude hiding (catch)


------------------------------------------------------------------------------
-- | The 'MonadException' type class. Minimal complete definition: 'throw',
-- 'catch'.
class Monad m => MonadException m where
    -- | Generalized version of 'E.throwIO'.
    throw :: Exception e => e -> m a

    -- | Generalized version of 'E.catch'.
    catch :: Exception e => m a -> (e -> m a) -> m a

    -- | Generalized version of 'E.bracket'.
    bracket :: m a -> (a -> m b) -> (a -> m c) -> m c
    bracket open close thing = do
        a <- open
        b <- thing a `catch` \e@(SomeException _) -> close a >> throw e
        close a
        return b


------------------------------------------------------------------------------
instance MonadException (Either SomeException) where
    throw = Left . E.toException
    catch m h = either (\e -> maybe (Left e) h (E.fromException e)) Right m


------------------------------------------------------------------------------
instance MonadException IO where
    throw = E.throwIO
    catch = E.catch
    bracket = E.bracket


------------------------------------------------------------------------------
instance (MonadException b, MonadBaseControl b m) => MonadException m where
    throw = liftBase . throw
    catch m h = control $ \run -> catch (run m) (run . h)
    bracket a b c = control $ \run -> bracket (run a)
        (\a' -> run $ restoreM a' >>= b)
        (\a' -> run $ restoreM a' >>= c)


------------------------------------------------------------------------------
instance (MonadTransControl t, Monad (t m), MonadException m) => MonadException (t m) where
    throw = lift . throw
    catch m h = controlT $ \run -> catch (run m) (run . h)
    bracket a b c = controlT $ \run -> bracket (run a)
        (\a' -> run $ restoreT (return a') >>= b)
        (\a' -> run $ restoreT (return a') >>= c)


------------------------------------------------------------------------------
controlT
    :: (Monad m, Monad (t m), MonadTransControl t)
    => (Run t -> m (StT t a))
    -> t m a
controlT f = liftWith f >>= restoreT . return
