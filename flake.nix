{
  inputs = {
    plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/c1ff6245fbb5a7ce835c0100b2fc2ee01a0024b6;
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    plutonomy = {
      url = github:well-typed/plutonomy/6c01302ba8cf3be4f71617e106cd5ef7ed10fc63;
      flake = false;
    };
    haskell-nix.follows = "plutip/haskell-nix";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, ... }:
    let
      # GENERAL
      # supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          haskell-nix.overlay
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
          cardano-transaction-lib.overlays.spago
        ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          nativeBuildInputs = [
            pkgs'.fd
            pkgs'.git
            pkgs'.nixpkgs-fmt
            pkgs.easy-ps.purs-tidy
            pkgs'.haskell.packages.${onchain.ghcVersion}.cabal-fmt
            pkgs'.haskell.packages.${onchain.ghcVersion}.fourmolu
          ];
          inherit (pkgs'.lib) concatStringsSep;
          otherBuildInputs = [ pkgs'.bash pkgs'.coreutils pkgs'.findutils pkgs'.gnumake pkgs'.nix ];
          format = pkgs.writeScript "format"
            ''
              export PATH=${concatStringsSep ":" (map (b: "${b}/bin") (otherBuildInputs ++ nativeBuildInputs))}
              export FOURMOLU_EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
              set -x
              purs-tidy format-in-place $(fd -epurs)
              fourmolu $FOURMOLU_EXTENSIONS --mode inplace --check-idempotence $(find onchain/{exporter,src} -iregex ".*.hs")
              nixpkgs-fmt $(fd -enix)
              cabal-fmt --inplace $(fd -ecabal)
            '';
        in
        {
          inherit format;
        }
      ;

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "ghc8107";

        inherit (plutip.inputs) nixpkgs haskell-nix;

        nixpkgsFor = system: import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${plutip.inputs.iohk-nix}/overlays/crypto")
          ];
          inherit (haskell-nix) config;
        };
        nixpkgsFor' = system: import nixpkgs { inherit system; inherit (haskell-nix) config; };

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
          in
          pkgs.haskell-nix.cabalProject {
            src = ./onchain;
            compiler-nix-name = ghcVersion;
            index-state = "2022-05-25T00:00:00Z";
            cabalProject = ''
              package plutonomy
                flags: +plutus-f680ac697

              packages: ./.
            '';
            inherit (plutip) cabalProjectLocal;
            extraSources = plutip.extraSources ++ [
              {
                src = "${inputs.plutonomy}";
                subdirs = [ "." ];
              }
              {
                src = "${plutip}";
                subdirs = [ "." ];
              }
            ];
            modules = plutip.haskellModules;
            shell = {
              withHoogle = true;
              exactDeps = true;
              nativeBuildInputs = with pkgs'; [
                git
                haskellPackages.apply-refact
                fd
                cabal-install
                hlint
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
                nixpkgs-fmt
              ];
              tools.haskell-language-server = { };
              additional = ps:
                with ps; [
                  cardano-api
                  plutus-ledger
                  plutus-ledger-api
                  plutus-script-utils
                  plutus-tx
                  plutus-tx-plugin
                  plutonomy
                  serialise
                ];
            };
          };

        script-exporter = system:
          let
            pkgs' = nixpkgsFor' system;
            exporter = ((projectFor system).flake { }).packages."cardano-racers-onchain:exe:exporter";
          in
          pkgs'.runCommandLocal "script-exporter" { }
            ''
              ln -s ${exporter}/bin/exporter $out
            '';

        exported-scripts = system:
          let
            pkgs' = nixpkgsFor' system;
            exporter = ((projectFor system).flake { }).packages."cardano-racers-onchain:exe:exporter";
          in
          pkgs'.runCommand "exported-scripts" { }
            ''
              set -e
              mkdir $out
              ${exporter}/bin/exporter
            '';
      };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            exporter = ((onchain.projectFor system).flake { }).packages."cardano-racers-onchain:exe:exporter";
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "cardano-racers-project";
            strictComp = false; # TODO: this should be eventually removed
            src = pkgs.runCommandLocal "generated-source" { }
              ''
                set -e
                cp -r ${./offchain} $out
                chmod -R +w $out
                ${exporter}/bin/exporter $out/src
              '';
            packageJson = ./offchain/package.json;
            packageLock = ./offchain/package-lock.json;
            nodejs = pkgs.nodejs-18_x;
            shell = {
              withRuntime = true;
              packageLockOnly = true;
              packages = with pkgs; [
                bashInteractive
                docker
                fd
                nodePackages.eslint
                nodePackages.prettier
                ogmios
                plutip-server
                postgresql
              ];
              shellHook =
                ''
                  export LC_CTYPE=C.UTF-8
                  export LC_ALL=C.UTF-8
                  export LANG=C.UTF-8
                '';
            };
          };
      };


      bundlesFor = system:
      let
        pkgs = nixpkgsFor system;
        project = (offchain.projectFor system);
        builtPursProject = project.buildPursProject {};
        createEntrypoint = eName: pkgs.writeText "${eName}-text" ''
          "use strict";
          import("./output.js").then(m => window.racers${eName} = m);
          console.log("racers${eName} ready");
        '';
        wrapWithCustomEntrypoint = eName: b: b.overrideAttrs (_: prev: {
          buildCommand = ''
            cp ${(createEntrypoint eName)} ${pkgs.lib.toLower eName}-entry.js
            ${prev.buildCommand}
            '';
        });
        adminBundledPursProject = wrapWithCustomEntrypoint "Admin" (project.bundlePursProject {
          main = "Lib.CardanoRacers.AdminFFI";
          entrypoint = "admin-entry.js";
          browserRuntime = true;
        });
        clientBundledPursProject = wrapWithCustomEntrypoint "Client" (project.bundlePursProject {
          main = "Lib.CardanoRacers.ClientFFI";
          entrypoint = "client-entry.js";
          browserRuntime = true;
        });
      in pkgs.runCommand "admin-bundle-cmd" {
          buildInputs = [
            pkgs.nodejs
            project.nodeModules
            builtPursProject
          ];
          nativeBuildInputs = [
            project.purs
            pkgs.easy-ps.spago
          ];
        }
        ''
        export HOME=$TMP
        export NODE_PATH="${project.nodeModules}/lib/node_modules"
        export PATH="${project.nodeModules}/bin:$PATH"

        mkdir -p $out/dist/racers-admin $out/dist/racers-client $out/dist/racers-bot
        cp -r ${adminBundledPursProject}/dist/* $out/dist/racers-admin
        cp -r ${clientBundledPursProject}/dist/* $out/dist/racers-client

        cp -r ${builtPursProject}/output $out/dist/racers-bot/
        cp ${builtPursProject}/build/package.json $out/dist/racers-bot/
        cp ${builtPursProject}/build/package-lock.json $out/dist/racers-bot/
        cp ${builtPursProject}/build/index.js $out/dist/racers-bot/
        cp ${builtPursProject}/build/index.d.ts $out/dist/racers-bot/
        '';

      gzippedBundlesFor = system:
        let
          pkgs = nixpkgsFor system;
          bundles = bundlesFor system;
        in pkgs.runCommand "gzipped-bundles" {
            buildInputs = [
              pkgs.gnutar
              bundles
            ];
          }
          ''
            mkdir -p $out
            mkdir -p ./admin
            mkdir -p ./client
            mkdir -p ./bot
            cp -r ${bundles}/dist/racers-admin/* ./admin
            cp -r ${bundles}/dist/racers-client/* ./client
            cp -r ${bundles}/dist/racers-bot/* ./bot
            chmod -R 755 ./admin ./client ./bot
            tar -czf $out/admin-browser-bundle.tar.gz -C ./admin .
            tar -czf $out/client-browser-bundle.tar.gz -C ./client .
            tar -czf $out/bot-bundle.tar.gz -C ./bot .
          '';

    in
    {
      inherit nixpkgsFor;

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // {
          script-exporter = onchain.script-exporter system;
          exported-scripts = onchain.exported-scripts system;
          bundles = bundlesFor system;
          gzipped-bundles = gzippedBundlesFor system;
        }
      );
      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // {
          cardano-racers = self.offchain.project.${system}.runPlutipTest { testMain = "Test"; };
        }
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.project.${system}.devShell;
      });

      apps = perSystem (system: {
        docs = self.offchain.project.${system}.launchSearchablePursDocs { };
        ctl-docs = cardano-transaction-lib.apps.${system}.docs;

        runtime = (nixpkgsFor system).launchCtlRuntime {};
        script-exporter = {
          # nix run .#script-exporter -- offchain/src
          type = "app";
          program = (onchain.script-exporter system).outPath;
        };
        format = {
          type = "app";
          program = (formatCheckFor system).format.outPath;
        };
      });
    };
}
