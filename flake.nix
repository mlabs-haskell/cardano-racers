{
  inputs = {
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # offchain
    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      rev = "723052ce847c0fd9625243cd188af035b8a91b87";
    };
    cardano-node.follows = "cardano-transaction-lib/cardano-node";
    hydra.follows = "hydra-sdk/hydra";
    hydra-sdk = {
      url = "github:mlabs-haskell/purescript-hydra-sdk/dshuiski/v3";
      inputs.ctl.follows = "cardano-transaction-lib";
    };

    # onchain
    # TODO: https://github.com/mlabs-haskell/cardano-racers/issues/22
    plutip.url = "github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b";
    haskell-nix.follows = "plutip/haskell-nix";
    iohk-nix.follows = "plutip/iohk-nix";
    plutonomy = {
      url = "github:well-typed/plutonomy/6c01302ba8cf3be4f71617e106cd5ef7ed10fc63";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, haskell-nix, hydra, iohk-nix, cardano-transaction-lib, plutip, ... }:
    let
      # GENERAL
      # supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
          cardano-transaction-lib.overlays.spago
        ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };
      nixpkgsUnstableFor = system: import nixpkgs-unstable { inherit system; };

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
        nixpkgsFor' = system: import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
        };
  
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

      # HYDRA

      hydraSourceFor = system: pkgs:
        let
          offchainSource = offchainSourceFor system pkgs; 
        in
        pkgs.runCommandLocal "cardano-racers-hydra-src" { }
          ''
            set -e
            mkdir $out
            cp -r ${offchainSource} $out/offchain
            cp -r ${./hydra}/* $out
          '';

      hydraApp = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgsUnstable = nixpkgsUnstableFor system;
            src = hydraSourceFor system pkgs;
          in
          pkgs.purescriptProject {
            inherit pkgs src;
            projectName = "cardano-racers-hydra";
            strictComp = true;
            packageJson = ./hydra/package.json;
            packageLock = ./hydra/package-lock.json;
            nodejs = pkgs.nodejs-18_x;
            shell = {
              withRuntime = true;
              packageLockOnly = true;
              packages = with pkgs; [
                fd
                hydra.packages.${system}.hydra-node
                nodePackages.eslint
                nodePackages.prettier
                nodePackages.purs-tidy
                (pkgsUnstable.steam.override { privateTmp = false; }).run-free # steam-run
              ];
              shellHook =
                ''
                  echo "hydra-node version: $(hydra-node --version)"
                '';
            };
          };
      };

      # OFFCHAIN / Testnet, Cardano, ...

      offchainSourceFor = system: pkgs:
        let
          exporter = ((onchain.projectFor system).flake { }).packages."cardano-racers-onchain:exe:exporter";
        in
        pkgs.runCommandLocal "offchain-src" {}
          ''
            set -e
            cp -r ${./offchain} $out
            chmod -R +w $out
            ${exporter}/bin/exporter $out/src
          '';

      offchain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            src = offchainSourceFor system pkgs; 
          in
          pkgs.purescriptProject {
            inherit pkgs src;
            projectName = "cardano-racers-offchain";
            strictComp = true;
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
                postgresql
                zip
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

      gzippedBundlesFor = system:
        let
          pkgs = nixpkgsFor system;
          project = (offchain.projectFor system);
          builtPursProject = project.buildPursProject {};
          makeBundleInstructionsFor = cname:
            let 
              name = pkgs.lib.toLower cname;
            in
            ''
            mkdir -p $out/${name}

            echo '"use strict";'  > $out/${name}/entrypoint.js
            echo \
                    'import("../output/Lib.CardanoRacers.${cname}FFI/index.js").then(m => window.racers${cname} = m);' \
                    >> $out/${name}/entrypoint.js
            echo 'console.log("racers${cname} ready");' >> $out/${name}/entrypoint.js

            BROWSER_RUNTIME=1 webpack --mode=production \
                    -c $src/offchain/webpack.config.cjs \
                    -o $out/${name}/ --env entry=$out/${name}/entrypoint.js
            cp $src/offchain/index.html $out/${name}/

            rm $out/${name}/entrypoint.js
            chmod -R 755 $out/${name}
            tar -czf $out/${name}-browser-bundle.tar.gz -C $out/${name} .
            '';
        in pkgs.runCommand "gzipped-bundles" {
            src = ./.;
            buildInputs = [
              pkgs.gnutar
              pkgs.zip
              pkgs.gnumake
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

          mkdir -p $out $out/bot

          cp -r ${builtPursProject}/output $out/output

          ${makeBundleInstructionsFor "Client"}
          ${makeBundleInstructionsFor "Admin"}

          cp -r ${builtPursProject}/output $out/bot/
          cp -r $src/offchain/build/* $out/bot/
          tar -czf $out/bot-bundle.tar.gz -C $out/bot .

          zip -j $out/bundles.zip $out/admin-browser-bundle.tar.gz \
                  $out/bot-bundle.tar.gz \
                  $out/client-browser-bundle.tar.gz

          # rm -rf $out/output/
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

      hydraApp = {
        project = perSystem hydraApp.projectFor;
        flake = perSystem (system: (hydraApp.projectFor system).flake { });
      };

      packages = perSystem (system:
        {
          script-exporter = onchain.script-exporter system;
          exported-scripts = onchain.exported-scripts system;
          gzipped-bundles = gzippedBundlesFor system; 
        }
      );

      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // {
          cardano-racers-offchain-localnet-tests = self.offchain.project.${system}.runLocalTestnetTest {
            testMain = "Test.CardanoRacers.Main";
          };
          cardano-racers-offchain-unit-tests = self.offchain.project.${system}.runPursTest {
            testMain = "Test.CardanoRacers.Unit";
          };
          cardano-racers-hydra-tests = self.hydraApp.project.${system}.runLocalTestnetTest {
            testMain = "Test.CardanoRacers.Hydra.Main";
            buildInputs = [
              hydra.packages.${system}.hydra-node
            ];
            env = {
              MOCK_RACE_SIMULATOR = "1";
            };
          };
        }
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.project.${system}.devShell;
        hydra = self.hydraApp.project.${system}.devShell;
      });

      apps = perSystem (system: {
        # TODO: https://github.com/mlabs-haskell/cardano-racers/issues/27
        # docs = self.offchain.project.${system}.launchSearchablePursDocs { };
        # ctl-docs = cardano-transaction-lib.apps.${system}.docs;

        runtime = (nixpkgsFor system).launchCtlRuntime {};
        script-exporter = {
          # nix run .#script-exporter -- offchain/src
          type = "app";
          program = (onchain.script-exporter system).outPath;
        };
        /* TODO: https://github.com/mlabs-haskell/cardano-racers/issues/28
        format = {
          type = "app";
          program = (formatCheckFor system).format.outPath;
        };
        */
      });
    };
}
