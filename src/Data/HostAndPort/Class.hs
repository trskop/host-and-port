{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.HostAndPort.Class
-- Description: Host and port used for connecting to server or listening for
--              client connections.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Host and port used for connecting to server or listening for client
-- connections.
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


class HasHost a where
    type Host a :: *
    host :: Functor f => (Host a -> f (Host a)) -> a -> f a

class HasPort a where
    type Port a :: *
    port :: Functor f => (Port a -> f (Port a)) -> a -> f a

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
