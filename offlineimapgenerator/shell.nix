# This file is NixOS specific and allows to use `nix-shell` to load up
# Haskell context (e.g. hpack) for this project. Also see the Makefile.
#
{ pkgs ? import <nixpkgs> {} }:

(pkgs.haskellPackages.callCabal2nix "offlineimapgenerator" ./. {}).env
