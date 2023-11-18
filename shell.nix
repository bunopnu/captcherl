{pkgs ? import <nixpkgs> {}}: let
  erl = pkgs.beam.packages.erlangR26;
in
  pkgs.mkShell {
    packages = [
      # Use this as Nix formatter while editing this file:
      pkgs.alejandra
      # Erlang setup:
      erl.erlang
      erl.rebar3
    ];
  }
