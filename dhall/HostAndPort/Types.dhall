let Host : Type = Text

let Port : Type = Natural

in  { Host = Host
    , Port = Port

    , HostAndPort =
        { host : Host
        , port : Port
        }

    , ConnectTo =
        { connectHost : Host
        , connectPort : Port
        }

    , ListenOn =
        { listenHost : Host
        , listenPort : Port
        }
    }
