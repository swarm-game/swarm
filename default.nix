{ compiler ? "ghc8107" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hls = import sources.haskell-hls-nix {};

  myHaskellPackages = with pkgs.haskell.lib; pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      hsnoise = hself.callCabal2nix "hsnoise" sources.hsnoise {};
      th-extras = hself.callCabal2nix "th-extras" sources.th-extras {};
      dependent-sum = hself.callCabal2nix "dependent-sum" "${sources.dependent}/dependent-sum" {};
      dependent-sum-template = hself.callCabal2nix "dependent-sum-template" "${sources.dependent}/dependent-sum-template" {};
      base16 = dontCheck (hself.callHackage "base16" "0.3.0.1" {});
      some = hself.callHackage "some" "1.0.2" {};
      murmur3 = dontCheck (hself.callHackageDirect {pkg = "murmur3"; ver = "1.0.4"; sha256 = "0f0a2j6hn133ijryd7631kwjizmkcaxhdyqnkp16z86fap77dqbx";} {});
      brick = hself.callHackageDirect {pkg = "brick"; ver = "0.64"; sha256 = "0y6qbixm0i1rg6gcxkl29bylajq0vmdnbv8sqynjqwxj5zs8s982";} {};
      linear = hself.callHackageDirect {pkg = "linear"; ver = "1.21.6"; sha256 = "1vyh33k14b6plk5ic2yrkl8z618ivd2gjz6qll2xrsswrkdfwxhr";} {};
      unordered-containers = hself.callHackageDirect {pkg = "unordered-containers"; ver = "0.2.14.0"; sha256 = "1n4z4sh11jya0v55rhs2fmq787sj8qw4g1v6m0jj3z0p8jkm1mzw";} {};
      random = dontCheck (hself.callHackageDirect {pkg = "random"; ver = "1.2.0"; sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";} {});
      hashable = dontCheck (hself.callHackageDirect {pkg = "hashable"; ver = "1.3.4.1"; sha256 = "13f2hy8jr519avnv9kg5hfx2n5s1l5d7786zfyj6w3ax77nsi8bm";} {});
      witch = hself.callHackageDirect  {pkg = "witch"; ver = "0.3.4.0"; sha256 = "17yff9l18c1y0kxw8kv1z2cb7amzsrd745z90pcw9k5vaqsi66ic";} {};
      word-wrap = hself.callHackageDirect {pkg = "word-wrap"; ver = "0.5"; sha256 = "1665rgiv8fgywwky6fnd60b3ignyhwblcwfj8jp0naafqrix9dfy";} {};
      clock = dontCheck hself.clock_0_8_2;
      swarm =
        hself.callCabal2nix
          "swarm"
          ( pkgs.lib.sourceFilesBySuffices ./. [
          "swarm.cabal"
          "cabal.project"
          ".yaml"
          "LICENSE"
          ".hs"
          ])
          {};
      };
    };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.swarm
    ];
    buildInputs = with pkgs.haskellPackages; [
      myHaskellPackages.cabal-install
      ghcid
      fourmolu
      hlint
      hls.hls
      (import sources.niv {}).niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."swarm");

  docker = pkgs.dockerTools.buildImage {
    name = "swarm";
    tag = "latest";
    created = "now";
    config = {
      # Volumes = {
      #   # "/etc/swarm" = {}; 
      # };
      Entrypoint = ["${exe}/bin/swarm"];
    };
    # docker run -it swarm:latest
  };

  # This is an attempt to get a static executable working, currently ghc8107 is failing to build as a cross compiler
  myMuslHaskellPackages = with pkgs.haskell.lib; pkgs.pkgsMusl.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      base16 = dontCheck (hself.callHackage "base16" "0.3.0.1" {});
      some = hself.callHackage "some" "1.0.2" {};
      murmur3 = dontCheck (hself.callHackageDirect {pkg = "murmur3"; ver = "1.0.4"; sha256 = "0f0a2j6hn133ijryd7631kwjizmkcaxhdyqnkp16z86fap77dqbx";} {});
      brick = hself.callHackageDirect {pkg = "brick"; ver = "0.64"; sha256 = "0y6qbixm0i1rg6gcxkl29bylajq0vmdnbv8sqynjqwxj5zs8s982";} {};
      linear = hself.callHackageDirect {pkg = "linear"; ver = "1.21.6"; sha256 = "1vyh33k14b6plk5ic2yrkl8z618ivd2gjz6qll2xrsswrkdfwxhr";} {};
      unordered-containers = hself.callHackageDirect {pkg = "unordered-containers"; ver = "0.2.14.0"; sha256 = "1n4z4sh11jya0v55rhs2fmq787sj8qw4g1v6m0jj3z0p8jkm1mzw";} {};
      random = dontCheck (hself.callHackageDirect {pkg = "random"; ver = "1.2.0"; sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";} {});
      hashable = dontCheck (hself.callHackageDirect {pkg = "hashable"; ver = "1.3.4.1"; sha256 = "13f2hy8jr519avnv9kg5hfx2n5s1l5d7786zfyj6w3ax77nsi8bm";} {});
      witch = hself.callHackageDirect  {pkg = "witch"; ver = "0.3.4.0"; sha256 = "17yff9l18c1y0kxw8kv1z2cb7amzsrd745z90pcw9k5vaqsi66ic";} {};
      word-wrap = hself.callHackageDirect {pkg = "word-wrap"; ver = "0.5"; sha256 = "1665rgiv8fgywwky6fnd60b3ignyhwblcwfj8jp0naafqrix9dfy";} {};
      clock = dontCheck hself.clock_0_8_2;
      swarm =
        hself.callCabal2nix
          "swarm"
          gitignore
        {
          configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.pkgsMusl.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.pkgsMusl.ncurses6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            ] ++ pkgs.pkgsMusl.lib.optionals (!strip) [
              "--disable-executable-stripping"
            ] ;
        };
      };
    };
  exeMusl = pkgs.haskell.lib.justStaticExecutables (myMuslHaskellPackages."swarm");
in
  {
     inherit pkgs;
    inherit shell;
    inherit exe;
    inherit docker;
    inherit myHaskellPackages;
    inherit hls;
    swarm = myHaskellPackages.swarm;
    src = sources.nixpkgs;
  }
