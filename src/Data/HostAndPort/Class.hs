{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef GENERIC_LENS
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
#endif

-- |
-- Module:      Data.HostAndPort.Class
-- Description: Generic access to host and port stored in various data types.
-- Copyright:   (c) 2017-2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generic access to host and port stored in various data types via type class
-- mechanism that allows usage of simple functions or lenses.
module Data.HostAndPort.Class
    (
    -- * HasHost
      HasHost(..)
    , getHost
    , setHost

    -- * HasPort
    , HasPort(..)
    , getPort
    , setPort

    -- * Host and Port
    , getHostAndPort
    , setHostAndPort
    )
  where

import Control.Arrow ((&&&))
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Kind (Type)

#ifdef GENERIC_LENS
import Data.Generics.Product.Typed (HasType, typed)
#endif


-- | Class for data types that contain @('Host' a)@ in them.
class HasHost a where

    -- | Type used for host. Reason for using polymorphic value is to support
    -- different data types for different scenarios. For example,
    -- @streaming-commons@ library uses 'Data.Streaming.Network.HostPreference'
    -- for server side (listening) and 'Data.ByteString.ByteString' for client
    -- side (connecting to server).
    type Host a :: Type

    -- | Lens for accessing @('Host' a)@ stored in type @a :: 'Type'@.
    host :: Functor f => (Host a -> f (Host a)) -> a -> f a

#ifdef GENERIC_LENS
    default host
        :: (Functor f, HasType (Host a) a)
        => (Host a -> f (Host a))
        -> a -> f a
    host = typed
    {-# INLINE host #-}
#endif

-- | Class for data types that contain @('Port' a)@ in them.
class HasPort a where

    -- | Type used for port number. Reason for using polymorphic value is to
    -- support different data types for different scenarios. For example,
    -- 'Data.Int.Int', or some custom newtype on top of integral type.
    type Port a :: Type

    -- | Lens for accessing @('Port' a)@ stored in type @a :: 'Type'@.
    port :: Functor f => (Port a -> f (Port a)) -> a -> f a

#ifdef GENERIC_LENS
    default port
        :: (Functor f, HasType (Port a) a)
        => (Port a -> f (Port a))
        -> a -> f a
    port = typed
    {-# INLINE port #-}
#endif

getHost :: HasHost a => a -> Host a
getHost s = getConst (host Const s)
{-# INLINE getHost #-}

setHost :: HasHost a => Host a -> a -> a
setHost h s = runIdentity (host (\_ -> Identity h) s)
{-# INLINE setHost #-}

getPort :: HasPort a => a -> Port a
getPort s = getConst (port Const s)
{-# INLINE getPort #-}

setPort :: HasPort a => Port a -> a -> a
setPort h s = runIdentity (port (\_ -> Identity h) s)
{-# INLINE setPort #-}

getHostAndPort
    :: (HasHost a, HasPort a)
    => a
    -> (Host a, Port a)
getHostAndPort = getHost &&& getPort
{-# INLINE getHostAndPort #-}

setHostAndPort
    :: (HasHost a, HasPort a)
    => Host a
    -> Port a
    -> a
    -> a
setHostAndPort h p = setPort p . setHost h
{-# INLINE setHostAndPort #-}
