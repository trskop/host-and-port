{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- Module:      Data.HostAndPort.Type
-- Description: Data type representing host and port pair used for connecting
--              to server or listening for client connections.
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type representing host and port pair used for connecting to server or
-- listening for client connections.
module Data.HostAndPort.Type
    (
    -- * Host and Port
      HostAndPort(..)
    , eqHostAndPortWith
    , compareHostAndPortWith
    , showsPrecHostAndPortWith

    -- * Listen and Connect
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

    -- ** Dhall
#ifdef DHALL
    , HostOrPortField(..)
    , interpretDhall
    , hostAndPort
    , listenOn
    , connectTo
#else
    -- | Dhall support is currently disabled, recompile with @dhall@
    -- compilation option enabled.
#endif
    )
  where

import Prelude (Bounded, Enum)

import Control.Applicative ((<*>))
import Data.Bool (Bool, (&&))
import Data.Eq (Eq)
import Data.Function
    ( ($)
    , (.)
#if !MIN_VERSION_base(4,11,0)
    , flip
#endif
    )
import Data.Functor
#if MIN_VERSION_base(4,11,0)
    ( (<&>)
#else
    ( Functor
    , fmap
#endif
    , (<$>)
    )
import Data.Functor.Classes
    ( Eq2(liftEq2)
    , Ord2(liftCompare2)
    , Show2(liftShowsPrec2)
    )
import Data.Int (Int)
import Data.Ord (Ordering(EQ), (>=))
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep, showsTypeRep)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), ShowS, showChar, showParen, showString)
import Text.Read (Read)

#ifdef DHALL
import qualified Dhall (Decoder, field, record)
#endif

import qualified Data.Streaming.Network as Streaming (HasPort(portLens))
import Data.Text (Text)

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
  deriving stock (Bounded, Enum, Eq, Generic, Show, Read)

-- | Host and port pair used on the server side when binding port on which it
-- will listen for connections. Type @t :: k@ is a phantom type used to
-- indicate service\/api name so that multiple services\/apis can coexist in
-- one application.
--
-- Usage example:
--
-- @
-- type ListenFileApi =
--     'ListenFor' \"file-api\" 'Data.Streaming.Network.HostPreference' 'Int'
-- @
type ListenFor = HostAndPort 'Listen

-- | Host and port pair used on the client side when connectiong to a server
-- which provides service\/API on specified host and port. Type @t :: k@ is a
-- phantom type used to indicate service\/api name so that multiple clients can
-- coexist in one application.
--
-- Usage example:
--
-- @
-- type ConnectFileApi =
--     'ConnectTo' \"file-api\" 'Data.ByteString.ByteString' 'Int'
-- @
type ConnectTo = HostAndPort 'Connect

-- | Behaves as a record data constructor for 'ListenFor' type.
--
-- Usage examples:
--
-- @
-- defaultListenFileApi :: ListenFileApi
-- defaultListenFileApi = 'ListenFor'
--     { 'listenHost' = \"example.com\"
--     , 'listenPort' = 12345
--     }
--
-- connectToFileApi
--     :: ListenFileApi
--     -> ('Data.Streaming.Network.AppData' -> 'System.IO.IO' ())
--     -> 'System.IO.IO' a
-- connectToFileApi 'ListenFor'{..} = 'Data.Streaming.Network.runTCPServer' cfg
--   where
--     cfg = 'Data.Streaming.Network.serverSettingsTCP' 'listenPort' 'listenHost'
--
-- @
pattern ListenFor :: host -> port -> ListenFor (t :: k) host port
pattern ListenFor{listenHost, listenPort} = HostAndPort
    { host = listenHost
    , port = listenPort
    }

-- | Behaves as a record data constructor for 'ConnectTo' type.
--
-- Usage examples:
--
-- @
-- defaultConnectFileApi :: ConnectFileApi
-- defaultConnectFileApi = 'ConnectTo'
--     { 'connectHost' = \"example.com\"
--     , 'connectPort' = 12345
--     }
--
-- connectToFileApi
--     :: ConnectFileApi
--     -> ('Data.Streaming.Network.AppData' -> 'System.IO.IO' a)
--     -> 'System.IO.IO' a
-- connectToFileApi 'ConnectTo'{..} = 'Data.Streaming.Network.runTCPClient' cfg
--   where
--     cfg = 'Data.Streaming.Network.clientSettingsTCP' 'connectPort' 'connectHost'
-- @
pattern ConnectTo :: host -> port -> ConnectTo t host port
pattern ConnectTo{connectHost, connectPort} = HostAndPort
    { host = connectHost
    , port = connectPort
    }

-- | Data type that represents pair @(host, port)@ which uses two additional
-- poly-kinded phantom types @tag1 :: k1@ and @tag2 :: k2@, which have
-- following meaning:
--
-- * @tag1 :: k1@ - specifies connection side, e.g. 'Listen' (server, see
--   'ListenFor') or 'Connect' (client, see 'ConnectTo').
--
-- * @tag2 :: k2@ - specifies protocol\/API which is available on that host and
--   port.
--
-- There are also specialised type aliases:
--
-- * @'ListenFor' = 'HostAndPort' ''Listen'@
--
-- * @'ConnectTo' = 'HostAndPort' ''Connect'@
data HostAndPort (tag1 :: k1) (tag2 :: k2) host port = HostAndPort
    { host :: !host
    , port :: !port
    }
  deriving stock (Generic)

-- |
-- @
-- instance (Show host, Show port, Typeable tag) => 'ListenFor' tag host port
-- @
instance
    ( Show host, Show port, Typeable tag
    ) => Show (HostAndPort 'Listen tag host port)
  where
    showsPrec :: Int -> HostAndPort 'Listen tag host port -> ShowS
    showsPrec = showsPrecHostAndPortWith showTypeName showsPrec showsPrec
      where
        showTypeName _ tag =
            showString "ListenFor @" . showsTypeRep (typeRep tag)
    {-# INLINEABLE showsPrec #-}

-- |
-- @
-- instance (Show host, Show port, Typeable tag) => 'ConnectTo' tag host port
-- @
instance
    ( Show host, Show port, Typeable tag
    ) => Show (HostAndPort 'Connect tag host port)
  where
    showsPrec :: Int -> HostAndPort 'Connect tag host port -> ShowS
    showsPrec = showsPrecHostAndPortWith showTypeName showsPrec showsPrec
      where
        showTypeName _ tag =
            showString "ConnectTo @" . showsTypeRep (typeRep tag)
    {-# INLINEABLE showsPrec #-}

instance (Typeable tag1, Typeable tag2) => Show2 (HostAndPort tag1 tag2) where
    liftShowsPrec2
        :: (Int -> a -> ShowS)
        -> ([a] -> ShowS)
        -> (Int -> b -> ShowS)
        -> ([b] -> ShowS)
        -> Int
        -> HostAndPort tag1 tag2 a b
        -> ShowS
    liftShowsPrec2 showsPrecHost _ showsPrecPort _ =
        showsPrecHostAndPortWith showTypeName showsPrecHost showsPrecPort
      where
        showTypeName tag1 tag2 = showString "HostAndPort"
            . showString " @" . showsTypeRep (typeRep tag1)
            . showString " @" . showsTypeRep (typeRep tag2)
    {-# INLINEABLE liftShowsPrec2 #-}

instance Eq2 (HostAndPort tag1 tag2) where
    liftEq2
        :: (a -> b -> Bool)
        -> (c -> d -> Bool)
        -> HostAndPort tag1 tag2 a c
        -> HostAndPort tag1 tag2 b d
        -> Bool
    liftEq2 = eqHostAndPortWith
    {-# INLINE liftEq2 #-}

instance Ord2 (HostAndPort tag1 tag2) where
    liftCompare2
        :: (a -> b -> Ordering)
        -> (c -> d -> Ordering)
        -> HostAndPort tag1 tag2 a c
        -> HostAndPort tag1 tag2 b d
        -> Ordering
    liftCompare2 = compareHostAndPortWith
    {-# INLINE liftCompare2 #-}

showsPrecHostAndPortWith
    :: forall host port tag1 tag2
    .  (Proxy tag1 -> Proxy tag2 -> ShowS)
    -- ^ Function for \"showing\" type.  For example:
    --
    -- @
    -- showTypeName
    --     :: ('Typeable' tag1, 'Typeable' tag2)
    --     => 'Proxy' tag1
    --     -> 'Proxy' tag2
    --     -> 'ShowS'
    -- showTypeName tag1 tag2 = 'showString' \"HostAndPort\"
    --     . 'showString' \" \@\" . 'showsTypeRep' ('typeRep' tag1)
    --     . 'showString' \" \@\" . 'showsTypeRep' ('typeRep' tag2)
    -- @
    -> (Int -> host -> ShowS)
    -> (Int -> port -> ShowS)
    -> Int
    -> HostAndPort tag1 tag2 host port
    -> ShowS
showsPrecHostAndPortWith showsPrecTypeName showsPrecHost showsPrecPort d
  HostAndPort{host, port} =
    showParen (d >= appPrecedence)
        $ showsPrecTypeName Proxy Proxy
        . showChar ' ' . showsPrecHost appPrecedence host
        . showChar ' ' . showsPrecPort appPrecedence port
  where
    appPrecedence = 11
{-# INLINEABLE showsPrecHostAndPortWith #-}

eqHostAndPortWith
    :: (host1 -> host2 -> Bool)
    -> (port1 -> port2 -> Bool)
    -> HostAndPort tag1 tag2 host1 port1
    -> HostAndPort tag1 tag2 host2 port2
    -> Bool
eqHostAndPortWith eqHosts eqPorts
  HostAndPort{host = host1, port = port1}
  HostAndPort{host = host2, port = port2} =
    eqHosts host1 host2 && eqPorts port1 port2
{-# INLINEABLE eqHostAndPortWith #-}

-- | Hostname is considered to be more significant than port number.
compareHostAndPortWith
    :: (host1 -> host2 -> Ordering)
    -> (port1 -> port2 -> Ordering)
    -> HostAndPort tag1 tag2 host1 port1
    -> HostAndPort tag1 tag2 host2 port2
    -> Ordering
compareHostAndPortWith compareHosts comparePorts
  HostAndPort{host = host1, port = port1}
  HostAndPort{host = host2, port = port2} =
    case compareHosts host1 host2 of
        EQ -> comparePorts port1 port2
        r -> r
{-# INLINEABLE compareHostAndPortWith #-}

instance Class.HasHost (HostAndPort t1 t2 host port) where
    type Host (HostAndPort t1 t2 host port) = host
    host f s@HostAndPort{host} = f host <&> \b -> s{host = b}
    {-# INLINE host #-}

instance Class.HasPort (HostAndPort t1 t2 host port) where
    type Port (HostAndPort t1 t2 host port) = port
    port f s@HostAndPort{port} = f port <&> \b -> s{port = b}
    {-# INLINE port #-}

instance (port ~ Int) => Streaming.HasPort (HostAndPort t1 t2 host port) where
    portLens = Class.port
    {-# INLINE portLens #-}

#if !MIN_VERSION_base(4,11,0)
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
{-# INLINE (<&>) #-}
#endif

#ifdef DHALL
-- | Enum that represents fields of a 'HostAndPort' record in an abstract way.
--
-- See 'interpretDhall' for more details.
data HostOrPortField = HostField | PortField
  deriving stock (Eq, Generic, Show)

-- | Generic parser for Dhall record that has two fields.
--
-- @
-- 'hostAndPort' = 'interpretDhall' $ \\case
--      'HostField' -> \"host\"
--      'PortField' -> \"port\"
-- @
interpretDhall
    :: forall tag1 tag2 host port
    .  (HostOrPortField -> Text)
    -- ^ Get record field names.
    -> Dhall.Decoder host
    -> Dhall.Decoder port
    -> Dhall.Decoder (HostAndPort tag1 tag2 host port)
interpretDhall fieldName hostType portType = Dhall.record
    $ HostAndPort
        <$> Dhall.field (fieldName HostField) hostType
        <*> Dhall.field (fieldName PortField) portType

-- | Parse Dhall record of the form:
--
-- > { host : Host
-- > , port : Port
-- > }
hostAndPort
    :: forall tag1 tag2 host port
    .  Dhall.Decoder host
    -> Dhall.Decoder port
    -> Dhall.Decoder (HostAndPort tag1 tag2 host port)
hostAndPort = interpretDhall \case
    HostField -> "host"
    PortField -> "port"

-- | Parse Dhall record of the form:
--
-- > { listenHost : Host
-- > , listenPort : Port
-- > }
listenOn
    :: forall tag host port
    .  Dhall.Decoder host
    -> Dhall.Decoder port
    -> Dhall.Decoder (ListenFor tag host port)
listenOn = interpretDhall \case
    HostField -> "listenHost"
    PortField -> "listenPort"

-- | Parse Dhall record of the form:
--
-- > { connectHost : Host
-- > , connectPort : Port
-- > }
connectTo
    :: forall tag host port
    .  Dhall.Decoder host
    -> Dhall.Decoder port
    -> Dhall.Decoder (ConnectTo tag host port)
connectTo = interpretDhall \case
    HostField -> "connectHost"
    PortField -> "connectPort"
#endif
