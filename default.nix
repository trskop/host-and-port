{ pkgs ? import <nixpkgs> { } }:

pkgs.haskellPackages.callCabal2nix "host-and-port" ./. { }
