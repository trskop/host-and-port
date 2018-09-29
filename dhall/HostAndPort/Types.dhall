  let
    Host : Type = Text

in let
    Port : Type = Natural

in let
    HostAndPort : Type =
      { host : Host
      , port : Port
      }

in let
    ConnectTo : Type =
      { connectHost : Host
      , connectPort : Port
      }

in let
    ListenOn : Type =
      { listenHost : Host
      , listenPort : Port
      }

in
    { Host = Host
    , Port = Port
    , HostAndPort = HostAndPort
    , ConnectTo = ConnectTo
    , ListenOn = ListenOn
    }
