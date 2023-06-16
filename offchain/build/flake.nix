{
  description = "racers-bot";

  inputs = {
    npmlock2nix =
      { flake = false;
        url = "github:nix-community/npmlock2nix";
      };
  };

  outputs = { self, nixpkgs, ctl, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; };
      npmlock2nixFor = pkgs: (import inputs.npmlock2nix { inherit pkgs; }).v2;
      mkNodeEnvFor = pkgs: 
        let
          npmlock2nix = (import inputs.npmlock2nix { inherit pkgs; }).v2;
          mods = npmlock2nix.node_modules {
            src = ./.;
            nodejs = pkgs.nodejs-16_x;
          } + /node_modules;
        in mods;
    in
    {
      # `buildCtlRuntime` will generate a Nix expression that, when built with
      # `pkgs.arion.build`, outputs a JSON file compatible with Arion. This can
      # be run directly with Arion or passed to another derivation. Or you can
      # use `buildCtlRuntime` with `runArion` (from the `hercules-ci-effects`)
      # library
      #
      # Use `nix build .#<PACKAGE>` to build. To run with Arion (i.e. in your
      # shell): `arion --prebuilt-file ./result up`
      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          nodeEnv = mkNodeEnvFor pkgs;
        in
        {
          default = self.packages.${system}.nodeEnv;
          nodeEnv = mkNodeEnvFor pkgs;
        });

      # `launchCtlRuntime` will generate a Nix expression from the provided
      # config, build it into a JSON file, and then run it with Arion
      #
      # Use `nix run .#<APP>` to run the services (e.g. `nix run .#ctl-runtime`)
      # apps = perSystem (system:
      #   let
      #     pkgs = nixpkgsFor system;
      #   in
      #   {
      #     default = self.apps.${system}.ctl-scaffold-runtime;
      #     ctl-scaffold-runtime = pkgs.launchCtlRuntime runtimeConfig;
      #     ctl-scaffold-blockfrost-runtime = pkgs.launchCtlRuntime
      #       (pkgs.lib.recursiveUpdate runtimeConfig { blockfrost = { enable = true; }; });
      #     docs = (psProjectFor pkgs).launchSearchablePursDocs { };
      #   });

      devShells = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nodejs-16_x
              npmlock2nix
              self.packages.${system}.nodeEnv
            ];
          };
        });
    };
}
