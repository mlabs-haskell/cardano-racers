.PHONY: build test format run-a run-b start-race run-simulator gen-keys

ps-sources := $(shell fd --no-ignore-parent -epurs)
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"
keys-dir := data/keys

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell. Run 'nix develop' first." \
				&& false \
		)

build: requires-nix-shell
	spago build --purs-args ${purs-args}

test: requires-nix-shell
	spago run --main Test.CardanoRacers.Hydra.Main

format: requires-nix-shell
	purs-tidy format-in-place ${ps-sources}

run-a: requires-nix-shell
	spago run --main CardanoRacers.Hydra.Main --exec-args 'config-a.json'

run-b: requires-nix-shell
	spago run --main CardanoRacers.Hydra.Main --exec-args 'config-b.json'

start-race: requires-nix-shell
	spago run --main CardanoRacers.Hydra.Demo.StartRace --exec-args 'demo/config.json'

run-simulator: requires-nix-shell
	spago run --main CardanoRacers.Hydra.Demo.RunSimulator --exec-args 'simulator/input.csv'

gen-keys: requires-nix-shell
	@hydra-node gen-hydra-key --output-file ${keys-dir}/hydra-a
	@hydra-node gen-hydra-key --output-file ${keys-dir}/hydra-b
	@cardano-cli address key-gen \
		--signing-key-file ${keys-dir}/cardano-a.sk \
		--verification-key-file ${keys-dir}/cardano-a.vk
	@cardano-cli address build \
		--payment-verification-key-file ${keys-dir}/cardano-a.vk \
		--testnet-magic 1
	@echo
	@cardano-cli address key-gen \
		--signing-key-file ${keys-dir}/cardano-b.sk \
		--verification-key-file ${keys-dir}/cardano-b.vk
	@cardano-cli address build \
		--payment-verification-key-file ${keys-dir}/cardano-b.vk \
		--testnet-magic 1
	@echo
