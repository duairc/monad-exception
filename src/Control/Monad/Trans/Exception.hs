{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports the 'ExceptionT' monad transformer.

-}

module Control.Monad.Trans.Exception
    ( -- * The @ExceptionT@ transformer
      ExceptionT
    , runExceptionT
    )
where

import           Control.Applicative (Applicative (..), Alternative (..))
import           Control.Exception
                     ( SomeException (..)
                     , Exception (..)
                     , PatternMatchFail (..)
                     )
import           Control.Monad (MonadPlus (..), ap, liftM)
import           Control.Monad.Exception.Class (MonadException (..))
import           Control.Monad.Fix (fix)
import           Control.Monad.Instances.Evil ()
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Control
                     ( MonadBaseControl (..)
                     , MonadTransControl (..)
                     , ComposeSt
                     , defaultLiftBaseWith
                     , defaultRestoreM
                     )
import           Prelude hiding (catch)


------------------------------------------------------------------------------
-- | This is a variant of the 'Either' data type with an additional @Zero@
-- constructor, to allow the implementation of 'Alternative' and 'MonadPlus'
-- instances for 'ExceptionT'.
data Either' a b = Zero | Left' a | Right' b


------------------------------------------------------------------------------
-- | Case analysis for the 'Either'' type. If the value is 'Left'' @a@, apply
-- the first function to @a@; if it is 'Right'' @b@, apply the second function
-- to @b@. If the value is 'Zero', then apply the first function to
-- @fix@ 'SomeException'.
either' :: (SomeException -> c) -> (b -> c) -> Either' SomeException b -> c
either' f _ Zero = f (fix SomeException)
either' f _ (Left' e) = f e
either' _ f (Right' a) = f a


------------------------------------------------------------------------------
-- | The 'ExceptionT' monad transformer. This is can be used to add
-- 'MonadException' functionality otherwise pure monad stacks. If your monad
-- stack is built on top of 'IO' however, it already has 'MonadException'
-- functionality and you should use that instead, unless you have a good
-- reason not to. Pass-through instances for the @mtl@ type classes are
-- provided automatically by the @mtl-evil-instances@ package.
newtype ExceptionT m a = ExceptionT (m (Either' SomeException a))


------------------------------------------------------------------------------
instance MonadTrans ExceptionT where
    lift = ExceptionT . liftM Right'


------------------------------------------------------------------------------
instance MonadTransControl ExceptionT where
    data StT ExceptionT a = StExceptionT (Either' SomeException a)
    liftWith f = lift $ f (\(ExceptionT m) -> liftM StExceptionT m)
    restoreT = ExceptionT . liftM (\(StExceptionT e) -> e)


------------------------------------------------------------------------------
instance Monad m => Functor (ExceptionT m) where
    fmap = liftM


------------------------------------------------------------------------------
instance Monad m => Applicative (ExceptionT m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
instance Monad m => Alternative (ExceptionT m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
instance Monad m => Monad (ExceptionT m) where
    return = ExceptionT . return . Right'
    (ExceptionT m) >>= f = ExceptionT $ m >>= either' (return . Left') (\a ->
        let ExceptionT m' = f a in m')
    fail = ExceptionT . return . Left' . toException . PatternMatchFail


------------------------------------------------------------------------------
instance Monad m => MonadPlus (ExceptionT m) where
    mzero = ExceptionT $ return Zero
    mplus (ExceptionT m) (ExceptionT m') = ExceptionT $ m >>= \x -> case x of
        Zero -> m'
        Left' e -> m' >>= \x' -> case x' of
            Right' a -> return (Right' a)
            _ -> return (Left' e)
        Right' a -> return (Right' a)


------------------------------------------------------------------------------
instance MonadBaseControl b m => MonadBaseControl b (ExceptionT m) where
     newtype StM (ExceptionT m) a = StMT {unStMT :: ComposeSt ExceptionT m a}
     liftBaseWith = defaultLiftBaseWith StMT
     restoreM = defaultRestoreM unStMT


------------------------------------------------------------------------------
instance Monad m => MonadException (ExceptionT m) where
    throw = ExceptionT . return . Left' . toException
    catch (ExceptionT m) h = ExceptionT $ m >>= \a -> case a of
        Zero -> case fromException (fix SomeException) of
            Nothing -> return a
            Just e' -> let ExceptionT m' = h e' in m'
        Left' e -> case fromException e of
            Nothing -> return a
            Just e' -> let ExceptionT m' = h e' in m'
        _ -> return a


------------------------------------------------------------------------------
-- | Run the 'ExceptionT' monad transformer. This returns 'Right' if
-- everything goes okay, and 'Left' in the case of an error, with the
-- exception wrapped up in a 'SomeException'.
runExceptionT :: Monad m => ExceptionT m a -> m (Either SomeException a)
runExceptionT (ExceptionT m) = m >>= either' (return . Left) (return . Right)
