-- vim: filetype=dhall

{ HostAndPort =
      ./HostAndPort/package.dhall sha256:536c9e93d547948c1940011bb59cf34363af25a501d93a9530d4bc098a4fcff3
    ? ./HostAndPort/package.dhall
, ListenOn =
      ./ListenOn/package.dhall sha256:8bf1820ae404b1f71ca917254a4a0ab1d78bf03cd6d0b50d46702f8309bee453
    ? ./ListenOn/package.dhall
, ConnectTo =
      ./ConnectTo/package.dhall sha256:446b620c4b49b95a57fffc96b8bba6e78486d7b6e03d95e79f4ab4a24b2a754a
    ? ./ConnectTo/package.dhall
}
