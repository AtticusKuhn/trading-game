{
  description = "Haskell development environment with QuickSpec and lexi-lambda/eff";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # eff's ghc-9.6 branch uses the PromptTag# API shipped by GHC 9.6.
    eff-src = {
      url = "github:lexi-lambda/eff/a6ad3c7d7c62d21cf088af109ceb1d10a56b7125";
      flake = false;
    };

    # QuickSpec 2.2.1 supports the newer twee-lib API.
    quickspec-src = {
      url = "github:nick8325/quickspec/94d67d3ca40cc57c2b96d7e176a74308049dbc8c";
      flake = false;
    };

    # This revision is twee-lib 2.6.2, needed by QuickSpec 2.2.1.
    twee-src = {
      url = "github:nick8325/twee/9f56b26a9b09ea508ff944702b88d60db48c4cb9";
      flake = false;
    };
  };

  outputs = inputs@{ nixpkgs, eff-src, quickspec-src, twee-src, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      projectFor = system:
        let
          pkgs = import nixpkgs { inherit system; };
          hlib = pkgs.haskell.lib;

          haskellPackages = pkgs.haskell.packages.ghc96.override {
            overrides = hself: _hsuper: {
              # Upstream asks for primitive < 0.8, but it builds with 0.9.
              eff = hlib.dontCheck (hlib.doJailbreak
                (hself.callCabal2nix "eff" "${eff-src}/eff" { }));

              # This revision targets random >= 1.3. The only API it needs here
              # is splitGen, whose random-1.2 equivalent is split.
              twee-lib = (hlib.doJailbreak
                (hself.callCabal2nix "twee-lib" "${twee-src}/src" { }))
                .overrideAttrs (old: {
                  postPatch = (old.postPatch or "") + ''
                    substituteInPlace Twee.hs \
                      --replace-fail Random.splitGen Random.split
                  '';
                });

              quickspec = hself.callCabal2nix
                "quickspec" quickspec-src { };
            };
          };

          ghc = haskellPackages.ghcWithPackages (hp: [
            hp.eff
            hp.quickspec
          ]);

          hello = pkgs.writeShellApplication {
            name = "quickspec-eff-hello";
            runtimeInputs = [ ghc ];
            text = ''
              runghc ${./hello.hs} "$@"
            '';
          };
        in {
          inherit pkgs haskellPackages ghc hello;
        };
    in {
      packages = forAllSystems (system:
        let project = projectFor system;
        in {
          default = project.hello;
          inherit (project) ghc;
          inherit (project.haskellPackages) eff quickspec;
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${(projectFor system).hello}/bin/quickspec-eff-hello";
        };
      });

      devShells = forAllSystems (system:
        let project = projectFor system;
        in {
          default = project.pkgs.mkShell {
            packages = [ project.ghc ];
          };
        });

      checks = forAllSystems (system:
        let project = projectFor system;
        in {
          hello = project.pkgs.runCommand "quickspec-eff-hello-check" { } ''
            ${project.hello}/bin/quickspec-eff-hello > "$out"
            grep -F "Hello, world (from eff)!" "$out"
            grep -F "QuickSpec" "$out"
          '';
        });
    };
}
