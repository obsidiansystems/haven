{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , filepath, http-conduit, http-types, mtl, process, SHA, stdenv
      , temporary, transformers, xml
      , maven
      }:
      mkDerivation {
        pname = "haven";
        version = "0.1.0.0";
        sha256 = "161m2msqr30460m9k1s67w3d05gil9d9gdhizl8rshll8pjhl6hg";
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring containers directory filepath http-conduit
          http-types mtl process SHA temporary transformers xml
        ];
        executableSystemDepends = [ maven ];
        description = "Recursively retrieve maven dependencies";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
