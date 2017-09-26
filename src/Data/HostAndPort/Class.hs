{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.HostAndPort.Class
-- Description: Generic access to host and port stored in various data types.
-- Copyright:   (c) 2017 Peter Trško
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


-- | Class for data types that contain @('Host' a ~ host)@ in them.
class HasHost a where

    -- | Type used for host. Reason for using polymorphic value is to support
    -- different data types for different scenarios. For example,
    -- @streaming-commons@ library uses 'Data.Streaming.Network.HostPreference'
    -- for server side (listening) and 'Data.ByteString.ByteString' for client
    -- side (connecting to server).
    type Host a :: *

    -- | Lens for accessing @('Host' a ~ host)@ stored in type @a :: *@.
    host :: (Functor f, Host a ~ host) => (host -> f host) -> a -> f a

-- | Class for data types that contain @('Port' a ~ port)@ in them.
class HasPort a where

    -- | Type used for port number. Reason for using polymorphic value is to
    -- support different data types for different scenarios. For example,
    -- 'Data.Int.Int', or some custom newtype on top of integral type.
    type Port a :: *

    -- | Lens for accessing @('Port' a ~ port)@ stored in type @a :: *@.
    port :: (Functor f, Port a ~ port) => (port -> f port) -> a -> f a

getHost :: (Host a ~ host, HasHost a) => a -> host
getHost s = getConst (host Const s)
{-# INLINE getHost #-}

setHost :: (Host a ~ host, HasHost a) => host -> a -> a
setHost h s = runIdentity (host (\_ -> Identity h) s)
{-# INLINE setHost #-}

getPort :: (Port a ~ port, HasPort a) => a -> port
getPort s = getConst (port Const s)
{-# INLINE getPort #-}

setPort :: (Port a ~ port, HasPort a) => port -> a -> a
setPort h s = runIdentity (port (\_ -> Identity h) s)
{-# INLINE setPort #-}

getHostAndPort
    :: (Host a ~ host, Port a ~ port, HasHost a, HasPort a)
    => a
    -> (host, port)
getHostAndPort = getHost &&& getPort
{-# INLINE getHostAndPort #-}

setHostAndPort
    :: (Host a ~ host, Port a ~ port, HasHost a, HasPort a)
    => host
    -> port
    -> a
    -> a
setHostAndPort h p = setPort p . setHost h
{-# INLINE setHostAndPort #-}
