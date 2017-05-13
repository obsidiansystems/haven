# haven
## Recursively retrieve maven dependencies

A haskell project that retrieves maven package dependencies recursively given a starting set of packages. The primary output format is a list of [nix sets](http://nixos.org/nix/manual/#idm140737318096432) describing the maven packages that could be used in a nix-based gradle build.
