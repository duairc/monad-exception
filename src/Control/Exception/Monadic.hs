{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-|

This module is intended as a drop-in replacement for "Control.Exception".

-}

module Control.Exception.Monadic
    (
      MonadException (..)

    -- * Catching exceptions
    -- ** The @catch@ functions
    , catches
    , Handler (..)
    , catchJust

    -- ** The @handle@ functions
    , handle
    , handleJust

    -- ** The @try@ functions
    , try
    , tryJust

    -- * Utilities
    , bracket_
    , bracketOnError
    , finally
    , onException

    -- * The @evaluate@ functions
    , evaluate
    , unsafeEvaluate

    -- * The @mapException@ function
    , E.mapException

    -- * Asynchronous exceptions
    , throwTo
    , mask
    , mask_
    , uninterruptibleMask
    , uninterruptibleMask_
    , E.MaskingState (..)
    , getMaskingState
    , allowInterrupt

    -- * Exceptions (re-exported from Control.Exception)
    , E.SomeException (..)
    , E.Exception (..)
    , E.IOException
    , E.ArithException (..)
    , E.ArrayException (..)
    , E.AssertionFailed (..)
    , E.AsyncException (..)
    , E.NonTermination (..)
    , E.NestedAtomically (..)
    , E.BlockedIndefinitelyOnMVar (..)
    , E.BlockedIndefinitelyOnSTM (..)
    , E.Deadlock (..)
    , E.NoMethodError (..)
    , E.PatternMatchFail (..)
    , E.RecConError (..)
    , E.RecSelError (..)
    , E.RecUpdError (..)
    , E.ErrorCall (..)
    )
where

import           Control.Concurrent (ThreadId)
import           Control.Exception (Exception (..), SomeException)
import qualified Control.Exception as E
import           Control.Monad (liftM)
import           Control.Monad.Base (MonadBase (..))
import           Control.Monad.Exception.Class
import           Control.Monad.Trans.Control
                     ( MonadBaseControl
                     , liftBaseOp
                     , liftBaseOp_
                     )
import           Control.Monad.Trans.Exception
import           Prelude hiding (catch)
import           System.IO.Unsafe (unsafePerformIO)


------------------------------------------------------------------------------
-- | Generalized version of 'E.catches'.
catches :: MonadException m => m a -> [Handler m a] -> m a
catches m handlers = m `catch` go handlers
  where
    go [] e = throw e
    go (Handler handler:xs) e = maybe (go xs e) handler (fromException e)


------------------------------------------------------------------------------
-- | Generalized version of 'E.Handler'. You need this when using 'catches'.
data Handler m a = forall e. Exception e => Handler (e -> m a)


------------------------------------------------------------------------------
-- | Generalized version of 'E.catchJust'.
catchJust
    :: (MonadException m, Exception e)
    => (e -> Maybe b)
    -> m a
    -> (b -> m a)
    -> m a
catchJust p a handler = catch a (\e -> maybe (throw e) handler (p e))


------------------------------------------------------------------------------
-- | A version of 'catch' with the arguments swapped around. See 'E.handle'.
handle :: (MonadException m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch


------------------------------------------------------------------------------
-- | A version of 'catchJust' with the arguments swapped around. See
-- 'E.handleJust'.
handleJust
    :: (MonadException m, Exception e)
    => (e -> Maybe b)
    -> (b -> m a)
    -> m a
    -> m a
handleJust p = flip (catchJust p)


------------------------------------------------------------------------------
-- | A generalized version of 'E.try'.
try :: (MonadException m, Exception e) => m a -> m (Either e a)
try = handle (return . Left) . liftM Right


------------------------------------------------------------------------------
-- | A generalized version of 'E.tryJust'.
tryJust
    :: (MonadException m, Exception e)
    => (e -> Maybe b)
    -> m a
    -> m (Either b a)
tryJust p = handleJust p (return . Left) . liftM Right


------------------------------------------------------------------------------
-- | Generalized version of 'E.bracketOnError'.
bracketOnError :: MonadException m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError a b c = bracket
    (bracket
        a
        (const (return ()))
        (\a' -> liftM Right (c a') `catch` \e -> return (Left (e, a'))))
    (const (return ()))
    (\e -> case e of
        Right c' -> return c'
        Left (e@(E.SomeException _), a') -> bracket
            (b a')
            (const (return ()))
            (const (throw e)))


------------------------------------------------------------------------------
-- | Generalized version of 'E.bracket_'.
bracket_ :: MonadException m => m a -> m b -> m c -> m c
bracket_ a b c = bracket a (const b) (const c)


------------------------------------------------------------------------------
-- | Generalized version of 'E.finally'.
finally :: MonadException m => m a -> m b -> m a
finally a b = bracket_ (return ()) b a


------------------------------------------------------------------------------
-- | Generalized version of 'E.onException'.
onException :: MonadException m => m a -> m b -> m a
onException a b = a `catch` \e -> b >> throw (e :: SomeException)


------------------------------------------------------------------------------
-- | Generalized version of 'E.evaluate'. This only works on 'IO'-like monads.
-- See 'unsafeEvaluate' for a version that works on every 'MonadException'.
evaluate :: MonadBase IO m => a -> m a
evaluate = liftBase . E.evaluate


------------------------------------------------------------------------------
-- | Generalized version of 'E.evaluate'. This uses 'unsafePerformIO' behind
-- the scenes to do something kind of similar to what the @spoon@ package
-- does.
unsafeEvaluate :: MonadException m => a -> m a
unsafeEvaluate = either throw return . unsafePerformIO . try' . evaluate
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = E.try


------------------------------------------------------------------------------
-- | Generalized version of 'throwTo'.
throwTo :: (MonadBase IO m, Exception e) => ThreadId -> e -> m ()
throwTo tid e = liftBase $ E.throwTo tid e


------------------------------------------------------------------------------
-- | Generalized version of 'E.mask'.
mask
    :: MonadBaseControl IO m
    => ((forall n b. MonadBaseControl IO n => n b -> n b) -> m a)
    -> m a
mask = liftBaseOp E.mask . liftUnmask
  where
    liftUnmask
        :: ((forall m a. MonadBaseControl IO m => m a -> m a) -> b)
        -> (forall a. IO a -> IO a)
        -> b
    liftUnmask f unmask = f $ liftBaseOp_ unmask


------------------------------------------------------------------------------
-- | Generalized version of 'E.mask'.
mask_ :: MonadBaseControl IO m => m a -> m a
mask_ = mask . const


------------------------------------------------------------------------------
-- | Generalized version of 'E.mask'.
uninterruptibleMask
    :: MonadBaseControl IO m
    => ((forall n b. MonadBaseControl IO n => n b -> n b) -> m a)
    -> m a
uninterruptibleMask = liftBaseOp E.uninterruptibleMask . liftUnmask
  where
    liftUnmask
        :: ((forall m a. MonadBaseControl IO m => m a -> m a) -> b)
        -> (forall a. IO a -> IO a)
        -> b
    liftUnmask f unmask = f $ liftBaseOp_ unmask


------------------------------------------------------------------------------
-- | Generalized version of 'E.mask'.
uninterruptibleMask_ :: MonadBaseControl IO m => m a -> m a
uninterruptibleMask_ = uninterruptibleMask . const


------------------------------------------------------------------------------
-- | Generalized version of 'E.getMaskingState'.
getMaskingState :: MonadBase IO m => m E.MaskingState
getMaskingState = liftBase E.getMaskingState


------------------------------------------------------------------------------
-- | Generalized version of 'E.allowInterrupt'.
allowInterrupt :: MonadBase IO m => m ()
allowInterrupt = liftBase E.allowInterrupt
