########################################################################
# default.nix -- The top-level nix build file for cardano-faucet.
#
# This file defines various attributes that are used for building and
# developing cardano-faucet.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   cardano-faucet: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs cardano-faucet;
  project = cardano-faucet.haskell.project;
in
{
  inherit pkgs cardano-faucet;

  inherit project;
}
