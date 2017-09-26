{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.HostAndPort.Parse
-- Description: Parse String into host and port pair.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parse 'String' into host and port pair.
module Data.HostAndPort.Parse
    (
    -- * Parse Host and Port
      ParsedHost(..)
    , parseListen
    , parseConnect

    -- * Utilities
    , modifyHostAndPortWith
    , hostPreference
    , invalidPortNumber
    , invalidHostName

    -- * Simpler Parsers
    , parseIp
    , parseIpv6
    , parseIpv4
    , parseHostName
    , parsePort
    )
  where

import Control.Applicative ((<*>), (<|>), pure)
import Control.Monad ((>=>))
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq, (==))
import Data.Function (($), (.), const, id)
import Data.Functor (Functor, (<$>), fmap)
import qualified Data.List as List (break, drop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Word (Word)
import GHC.Generics (Generic, Generic1)
import Text.Read (readMaybe)
import Text.Show (Show, show)

import Data.IP (IP(IPv4, IPv6))
import Data.Streaming.Network.Internal
    ( HostPreference(Host, HostAny, HostIPv4Only)
    )
import Text.Hostname (validHostname)

import Data.HostAndPort.Class (HasHost, HasPort, Host, Port, setHost, setPort)


-- | Hots are specified either by their 'IpAddress' or by their 'HostName'.
data ParsedHost s
    = HostName s
    -- ^ Host name as defined by <https://tools.ietf.org/html/rfc1123 RFC 1123>.
    | IpAddress IP
    -- ^ IPv4 or IPv6 address.
  deriving (Eq, Functor, Generic, Generic1, Show)

-- | Parse string in the form of either @HOST[:PORT]@ or @[HOST]:PORT@. In
-- other words, either @HOST@ or @PORT@ has to be provided. Where @PORT@ is a
-- TCP/UDP port number, and @HOST@ is one of:
--
-- * @*@ - Listen on all interfaces, both IPv4 and IPv6.
-- * @0.0.0.0@ - Listen on all interfaces, but IPv4 only.
-- * @[::]@ - Listen on all interfaces, both IPv6 and IPv4.
-- * Valid IPv4 address, e.g. @127.0.0.1@ for IPv4 loopback.
-- * Valid IPv6 address in square brackets, e.g. @[::1]@ for IPv6 loopback.
-- * Valid hostname as defined by
--   <https://tools.ietf.org/html/rfc1123 RFC 1123>, e.g. @\"example.com\"@.
parseListen
    :: String
    -> Either String (Maybe (ParsedHost String), Maybe Word)
parseListen = \case
    "" -> Left "Expected HOST:PORT, or HOST, or :PORT."

    -- "[" IPV6_ADDR "]:" PORT
    '[' : s -> case List.break (== ']') s of
        ("", _) -> Left "Expected IPv6 address after '['."
        (_, "") -> Left "Missing ']' after IPv6 address."
        (s1, s2) -> (,)
            <$> parseIpv6' s1
            <*> parsePort' (List.drop 1 s2)

    -- ":" PORT
    s@(':' : _) -> (Nothing, ) <$> parsePort' s

    -- (HOST_NAME | IP_ADDR) (":" PORT)? | ":" PORT
    s ->
        let (s1, s2) = List.break (== ':') s
        in (,) <$> parseHost s1 <*> parsePort' s2
  where
    parsePort' = \case
        ""      -> Right Nothing
        ':' : s -> Just <$> parsePort s
        s       -> Left $ "Expected :PORT, but got: " <> show s
            -- This is an indication of a bug in code that is calling
            -- parsePort'.

    parseHost s =
        maybe (Left notAHostName) (Right . Just) $ parseIp' <|> parseHostName'
      where
        notAHostName = "Neither host name nor IP address: " <> show s

        parseIp' = either (const Nothing) (Just . IpAddress) $ parseIp s

        parseHostName' =
            either (const Nothing) (Just . HostName) $ parseHostName s

    parseIpv6' s = Just . IpAddress <$> parseIpv6 s

-- | Same as 'parseListen', but:
--
-- * will reject @HOST@ when it's @\*@, @::@, or @0.0.0.0@;
-- * and it will reject @PORT@ when it's set to @0@.
parseConnect
    :: String
    -> Either String (Maybe (ParsedHost String), Maybe Word)
parseConnect = parseListen >=> onlyValidIp >=> onlyValidPort
  where
    onlyValidIp = \case
        r@(Just (IpAddress ip), _) -> case ip of
            IPv6 v@"::"      -> invalidAddress "6" v
            IPv4 v@"0.0.0.0" -> invalidAddress "4" v
            _                -> Right r
        r -> Right r

    onlyValidPort = \case
        r@(_, Just p)
          | p == 0    -> invalidPortNumber "Zero isn't allowed"
          | otherwise -> Right r
        r -> Right r

    invalidAddress n v = Left $ "Invalid IPv" <> n <> " address: " <> show v

-- {{{ Simple Parsers ---------------------------------------------------------

-- | Parse port number, i.e. value in range @[0, 65535]@.
parsePort :: String -> Either String Word
parsePort s = maybe (Left notAPortNumber) validatePortNumber $ readMaybe s
  where
    notAPortNumber = "Not a port number: " <> show s

    validatePortNumber p
      | p > 65535 = invalidPortNumber "Values higher than 65535 aren't allowed"
      | otherwise = Right p

-- | Parse 'IP' address, either IPv4 or IPv6.
parseIp :: String -> Either String IP
parseIp s = maybe (Left notIpAddr) Right $ readMaybe s
  where
    notIpAddr = "Not an IP address: " <> show s

-- | Parse IPv6 address only.
parseIpv6 :: String -> Either String IP
parseIpv6 "*" = Right "::"
parseIpv6 s   = maybe (Left notIpv6Addr) (Right . IPv6) $ readMaybe s
  where
    notIpv6Addr = "Not an IPv6 addres: " <> show s

-- | Parse IPv4 address only.
parseIpv4 :: String -> Either String IP
parseIpv4 s = maybe (Left notIpv4Addr) (Right . IPv4) $ readMaybe s
  where
    notIpv4Addr = "Not an IPv4 addres: " <> show s

-- | Parse host name as defined by
-- <https://tools.ietf.org/html/rfc1123 RFC 1123>.
parseHostName :: String -> Either String String
parseHostName s
  | validHostname (fromString s) = Right s
  | otherwise                    = invalidHostName $ show s

-- }}} Simple Parsers ---------------------------------------------------------

-- {{{ Utilities --------------------------------------------------------------

-- | Interpret result of 'parseListen' or 'parseConnect' as modification of a
-- data type @a :: *@. Value 'Nothing' is interpreted as no change (i.e.
-- identity).
--
-- Usage examples:
--
-- @
-- 'parseListen' '>=>' 'modifyHostAndPortWith' convertHost convertPort
--     :: ('Host' a ~ host, 'Port' a ~ port, 'HasHost' a, 'HasPort' a)
--     => 'String'
--     -> 'Either' 'String' (a -> a)
-- @
modifyHostAndPortWith
    :: (Host a ~ host, Port a ~ port, HasHost a, HasPort a)
    => (ParsedHost String -> Either String host)
    -- ^ Convert 'ParsedHost' into @host :: *@ or fail with an error message.
    -> (Word -> Either String port)
    -- ^ Convert 'Word' into @port :: *@ or fail with an error message.
    -> (Maybe (ParsedHost String), Maybe Word)
    -- ^ Result of either 'parseListen' or 'parseConnect'.
    -> Either String (a -> a)
    -- ^ Either failed with an error message or returned a function that will
    -- set host and port in the type @a :: *@.
modifyHostAndPortWith convertHost convertPort (possiblyHost, possiblyPort) =
    (.) <$> (setHost `after` convertHost) possiblyHost
        <*> (setPort `after` convertPort) possiblyPort
  where
    after setter f = maybe (pure id) (fmap setter . f)

-- | Interpret @'ParsedHost' 'String'@ as a listening preference
-- ('HostPreference') as used by "Data.Streaming.Network".
--
-- Interpretation of 'ParsedHost' is:
--
-- @
-- \\case
--     -- Listen on all interfaces, but IPv4 only.
--     'IpAddress' \"0.0.0.0\" -> 'fromString' \"!4\"
--
--     -- Listen on all interfaces, both IPv4 and IPv6.
--     'IpAddress' \"::\"      -> 'fromString' \"*\"
--
--     -- Listen only on specified interface.
--     'IpAddress' ip        -> 'fromString' ('show' ip)
--     'HostName' name       -> 'fromString' name
-- @
--
-- For more details see documentation of 'HostPreference'.
hostPreference :: ParsedHost String -> HostPreference
hostPreference = \case
    IpAddress "0.0.0.0" -> HostIPv4Only
    IpAddress "::"      -> HostAny
    IpAddress ip        -> Host (show ip)
    HostName hostName   -> Host hostName

-- | Fail on inalid port number.
--
-- >>> invalidPortNumber "Value 12345 was prohibited! Just because."
-- Left "Invalid port number: Value 12345 was prohibited! Just because."
invalidPortNumber :: String -> Either String a
invalidPortNumber msg = Left $ "Invalid port number: " <> msg

-- | Fail on inalid host name.
--
-- >>> invalidPortNumber $ "\"example.com\" is reserved for example code."
-- Left "Invalid port number: \"example.com\" is reserved for example code."
invalidHostName :: String -> Either String a
invalidHostName msg = Left $ "Invalid host name: " <> msg

-- }}} Utilities --------------------------------------------------------------
