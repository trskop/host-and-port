# host-and-port

## Description

Data type and utilities for handling host name and port number pairs.

This Haskell package provides:

* Generic data type for storing host name and port number that uses phantom
  types to distinguish between server/client, and various
  protocols/services/APIs.
* Type classes for accessing host name and port number in a data type using
  lenses.
* Generic parsing of host name and port number from e.g. command line
  arguments.
