-- | Smart constructor for `ListenOn` which is useful to for building smart
-- constructors that use default port number, e.g.:
--
-- ```
-- webserverListenOn
--   : ∀(host : Text) → ListenOn
--   = listenOn 80
-- ```
let Host =
        missing
          sha256:b9c75dfe7b1571f8b606d709a1103d67f86f16e04e63aa0de9856cd00904d4a2
      ? ../HostAndPort/Host.dhall

let Port =
        missing
          sha256:41a29012a364e1ac961fb75ab56e82d100426911e6839fb1d731a67e1e4dc713
      ? ../HostAndPort/Port.dhall

let ListenOn =
        missing
          sha256:ebe5ef242699e30b10494eb1859779d9826acdf8ba8fb0c8f06213eb0e8691a2
      ? ./Type.dhall

let listenOn =
      λ(port : Port) → λ(host : Host) → { listenHost = host, listenPort = port }

in  listenOn : ∀(port : Port) → ∀(host : Host) → ListenOn
