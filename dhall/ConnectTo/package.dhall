-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:257acd62664f961c64165fa61f75d2988d0ce6fab67fae0938ea16d50942ff7f
    ? ./Type.dhall
, listenOn =
      ./connectTo.dhall sha256:7fd8e528cf23c63f0ae926881aea4bbda721ec5664a0d3feb98eb17237b56603
    ? ./connectTo.dhall
}
