{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
-- |
-- Module:      Data.HostAndPort
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
module Data.HostAndPort
    ( module Data.HostAndPort.Type
    , module Data.HostAndPort.Class
    , module Data.HostAndPort.Parse
    )
  where

import Data.HostAndPort.Type
import Data.HostAndPort.Class
import Data.HostAndPort.Parse
