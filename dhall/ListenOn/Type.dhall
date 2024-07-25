let Host =
        missing
          sha256:b9c75dfe7b1571f8b606d709a1103d67f86f16e04e63aa0de9856cd00904d4a2
      ? ../HostAndPort/Host.dhall

let Port =
        missing
          sha256:41a29012a364e1ac961fb75ab56e82d100426911e6839fb1d731a67e1e4dc713
      ? ../HostAndPort/Port.dhall

let ListenOn = { listenHost : Host, listenPort : Port }

in  ListenOn : Type
