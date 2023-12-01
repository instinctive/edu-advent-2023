# https://github.com/mhwombat/nix-for-numbskulls
#
# $ cd <project-dir>
# $ ln -s path/to/default.nix
# $ nix-shell --argstr myroot `pwd`

{ myroot ? ./. }:
let
  pkgs = import <nixpkgs> {};
in
  pkgs.haskellPackages.developPackage {
    root = myroot;
  }
