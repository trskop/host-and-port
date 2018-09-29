{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
-- |
-- Module:      Data.HostAndPort
-- Description: Host and port pair used for connecting to server or listening
--              for client connections.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Host and port pair used for connecting to server or listening for client
-- connections.
module Data.HostAndPort
    (
    -- * Host and Port Pair
    --
    -- | Data type representing host and port pair used for connecting to
    -- server or listening for client connections.
      module Data.HostAndPort.Type

    -- * Parse Host and Port
    --
    -- | Parse 'Data.String.String' into host and port pair, especially command
    -- line arguments.
    , module Data.HostAndPort.Parse

    -- * Generic Access to Host and Port
    --
    -- | Generic access to host and port stored in various data types via type
    -- class mechanism that allows usage of simple functions or lenses.
    --
    -- @
    -- import "Data.HostAndPort.Class"
    -- @
    --
    -- Be aware that definitions of 'host' and 'port' fields clash with
    -- 'Data.HostAndPort.Class.host' and 'Data.HostAndPort.Class.port' methods.
    )
  where

import Data.HostAndPort.Type
import Data.HostAndPort.Parse
