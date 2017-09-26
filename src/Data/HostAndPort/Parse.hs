{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.HostAndPort.Parse
-- Description: Parse string into host and port pair.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parse string into host and port pair.
module Data.HostAndPort.Parse
    ( ParsedHost(..)
    , parseListen
    , parseConnect

    --
    , parseIp
    , parseIpv6
    , parseIpv4
    , parseHostName
    , parsePort
    )
  where

import Control.Applicative ((<*>), (<|>))
import Control.Monad ((>=>))
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq, (==))
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>))
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
import Text.Hostname (validHostname)


data ParsedHost s
    = HostName s
    | IpAddress IP
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

    -- HOST (":" PORT)? | ":" PORT
    s ->
        let (s1, s2) = List.break (== ':') s
        in (,) <$> parseHost s1 <*> parsePort' s2
  where
    parsePort' :: String -> Either String (Maybe Word)
    parsePort' = \case
        "" -> Right Nothing
        ':' : s -> Just <$> parsePort s
        s -> Left $ "Expected :PORT, but got: " <> show s
            -- This is an indication of a bug in code that is calling
            -- parsePort'.

    parseHost :: String -> Either String (Maybe (ParsedHost String))
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

-- | Parse port number, i.e. value in range @[0, 65535]@.
parsePort :: String -> Either String Word
parsePort s = maybe (Left notAPortNumber) validatePortNumber $ readMaybe s
  where
    notAPortNumber = "Not a port number: " <> show s

    validatePortNumber p
      | p > 65535 = invalidPortNumber "Values higher than 65535 aren't allowed"
      | otherwise = Right p

parseIp :: String -> Either String IP
parseIp s = maybe (Left notIpAddr) Right $ readMaybe s
  where
    notIpAddr = "Not an IP address: " <> show s

parseIpv6 :: String -> Either String IP
parseIpv6 "*" = Right "::"
parseIpv6 s   = maybe (Left notIpv6Addr) (Right . IPv6) $ readMaybe s
  where
    notIpv6Addr = "Not an IPv6 addres: " <> show s

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
