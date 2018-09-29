  let
    Types = ./HostAndPort/Types.dhall

in let
    -- Smart constructor for `ListenTo` which is useful to for building smart
    -- constructors that use default port number, e.g.:
    --
    -- ```
    -- connectToPostgresql
    --   : ∀(host : Text) → Types.ConnectTo
    --   = connectTo 5432
    -- ```
    connectTo : ∀(port : Types.Port) → ∀(host : Types.Host) → Types.ConnectTo
      = λ(port : Types.Port)
      → λ(host : Types.Host)
      → { connectHost = host
        , connectPort = port
        }

in let
    -- Smart constructor for `ConnectTo` which is useful to for building smart
    -- constructors that use default port number, e.g.:
    --
    -- ```
    -- webserverListenOn
    --   : ∀(host : Text) → Types.ListenOn
    --   = listenOn 80
    -- ```
    listenOn : ∀(port : Types.Port) → ∀(host : Types.Host) → Types.ListenOn
      = λ(port : Types.Port)
      → λ(host : Types.Host)
      → { listenHost = host
        , listenPort = port
        }

in
    { connectTo = connectTo
    , listenOn = listenOn
    }
