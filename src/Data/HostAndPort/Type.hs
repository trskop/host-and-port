{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.HostAndPort.Type
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
module Data.HostAndPort.Type
    (
    -- * Host and Port
      HostAndPort(..)

    -- * Listen or Connect
    , ListenOrConnect(..)

    -- ** Listen
    , ListenFor
    , pattern ListenFor
    , listenHost
    , listenPort

    -- ** Connect
    , ConnectTo
    , pattern ConnectTo
    , connectHost
    , connectPort
    )
  where

import Prelude (Bounded, Enum, (+))

import Data.Eq (Eq)
import Data.Function (($), (.), flip)
import Data.Functor (Functor, fmap)
import Data.Int (Int)
import Data.Ord ((>))
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep, showsTypeRep)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), showChar, showParen, showString)
import Text.Read (Read)

import qualified Data.Streaming.Network as Streaming (HasPort(portLens))

import qualified Data.HostAndPort.Class as Class
    ( HasHost(Host, host)
    , HasPort(Port, port)
    )


-- | Used as a tag for 'HostAndPort' data type to determine if the
-- @(host, port)@ pair is used on server side (listening for connections) or on
-- client side (connecting to server).
data ListenOrConnect
    = Listen
    -- ^ Listen for connections.
    | Connect
    -- ^ Connect to server.
  deriving (Bounded, Enum, Eq, Generic, Show, Read)

type ListenFor (t :: k) host port = HostAndPort 'Listen t host port
type ConnectTo (t :: k) host port = HostAndPort 'Connect t host port

pattern ListenFor :: host -> port -> ListenFor (t :: k) host port
pattern ListenFor{listenHost, listenPort} = HostAndPort
    { _host = listenHost
    , _port = listenPort
    }

pattern ConnectTo :: host -> port -> ConnectTo t host port
pattern ConnectTo{connectHost, connectPort} = HostAndPort
    { _host = connectHost
    , _port = connectPort
    }

data HostAndPort (t1 :: k1) (t2 :: k2) host port = HostAndPort
    { _host :: host
    , _port :: port
    }
  deriving (Generic)

instance
    ( Show host, Show port, Typeable t
    ) => Show (HostAndPort 'Listen t host port)
  where
    showsPrec d HostAndPort{..} = showParen (d > appPrecedence)
        $ showString "ListenFor @" . showsTypeRep (typeRep (Proxy @t))
        . showChar ' '
        . showsPrec (appPrecedence + 1) _host
        . showChar ' '
        . showsPrec (appPrecedence + 1) _port
      where
        appPrecedence = 10
    {-# INLINEABLE showsPrec #-}

instance Class.HasHost (HostAndPort t1 t2 host port) where
    type Host (HostAndPort t1 t2 host port) = host
    host f s@HostAndPort{_host} = f _host <&> \b -> s{_host = b}
    {-# INLINE host #-}

instance Class.HasPort (HostAndPort t1 t2 host port) where
    type Port (HostAndPort t1 t2 host port) = port
    port f s@HostAndPort{_port} = f _port <&> \b -> s{_port = b}
    {-# INLINE port #-}

instance (port ~ Int) => Streaming.HasPort (HostAndPort t1 t2 host port) where
    portLens = Class.port
    {-# INLINE portLens #-}

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
{-# INLINE (<&>) #-}
