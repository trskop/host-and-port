-- | Smart constructor for `ConnectTo` which is useful to for building smart
-- constructors that use default port number, e.g.:
--
-- ```
-- connectToPostgresql
--   : ∀(host : Text) → ConnectTo
--   = connectTo 5432
-- ```
let Host =
        missing
          sha256:b9c75dfe7b1571f8b606d709a1103d67f86f16e04e63aa0de9856cd00904d4a2
      ? ../HostAndPort/Host.dhall

let Port =
        missing
          sha256:41a29012a364e1ac961fb75ab56e82d100426911e6839fb1d731a67e1e4dc713
      ? ../HostAndPort/Port.dhall

let ConnectTo =
        missing d62664f961c64165fa61f75d2988d0ce6fab67fae0938ea16d50942ff7f
      ? (   missing
              sha256:257acd62664f961c64165fa61f75d2988d0ce6fab67fae0938ea16d50942ff7f
          ? ./Type.dhall
        )

let connectTo =
      λ(port : Port) →
      λ(host : Host) →
        { connectHost = host, connectPort = port }

in  connectTo : ∀(port : Port) → ∀(host : Host) → ConnectTo
