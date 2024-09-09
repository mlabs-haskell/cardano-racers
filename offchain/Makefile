SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.PHONY: test build bundles

ps-sources := $$(fd -epurs)

ps-entrypoint := CardanoRacers.Main
ps-entrypoint-function := main
# Whether to bundle for the browser ("1") or the node ("")
# NOTE: bundling for the node is not necessary, see https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/using-from-js.md
browser-runtime := 1 # Use "1" for true and "" for false

e2e-entrypoint := CardanoRacers.Test.E2E.Serve
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js
serve-port := 4008
purs-args := "--stash --censor-lib --censor-codes=UserDefinedWarning,ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport"


build:
	@spago build --purs-args ${purs-args}


create-bundle-entrypoint:
	@mkdir -p dist/
	@echo 'import("../output/${ps-entrypoint}/index.js").then(m => m.${ps-entrypoint-function}());' > ./dist/entrypoint.js

create-html-entrypoint:
	@mkdir -p dist/
	@cat << EOF > dist/index.html
	<!DOCTYPE html>
	<html>
	  <body><script type="module" src="./index.js"></script></body>
	</html>
	EOF

delete-bundle-entrypoint:
	@rm -f ./dist/entrypoint.js

esbuild-bundle:build create-bundle-entrypoint
	BROWSER_RUNTIME=${browser-runtime} node --trace-uncaught esbuild/bundle.js ./dist/entrypoint.js dist/index.js
	@make delete-bundle-entrypoint

esbuild-serve: build create-bundle-entrypoint create-html-entrypoint
	BROWSER_RUNTIME=1 node --trace-uncaught esbuild/serve.js ./dist/entrypoint.js dist/index.js dist/ ${serve-port}

webpack-bundle: build create-bundle-entrypoint
	BROWSER_RUNTIME=${browser-runtime} webpack --mode=production \
		-o dist/ --env entry=./dist/entrypoint.js
	@make delete-bundle-entrypoint

webpack-serve: build create-bundle-entrypoint create-html-entrypoint
	BROWSER_RUNTIME=1 webpack-dev-server --progress \
		--port ${serve-port} \
		-o dist/ --env entry=./dist/entrypoint.js

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

e2e-serve:
	spago bundle-module -m ${e2e-entrypoint} --to output.js
	BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources}

format:
	@purs-tidy format-in-place ${ps-sources}

test:
	spago test --main Test.CardanoRacers.Unit
	spago test --main Test.CardanoRacers.Main


bundles: build
	@mkdir -p dist/admin dist/client dist/bot

	@echo '"use strict";'  > ./dist/client/entrypoint.js
	@echo \
		'import("../../output/Lib.CardanoRacers.ClientFFI/index.js").then(m => window.racersClient = m);' \
		 >> ./dist/client/entrypoint.js
	@echo 'console.log("racersClient ready");'  >> ./dist/client/entrypoint.js

	@echo '"use strict";'  > ./dist/admin/entrypoint.js
	@echo \
		'import("../../output/Lib.CardanoRacers.AdminFFI/index.js").then(m => window.racersAdmin = m);' \
		>> ./dist/admin/entrypoint.js
	@echo 'console.log("racersAdmin ready");'  >> ./dist/admin/entrypoint.js

	BROWSER_RUNTIME=${browser-runtime} webpack --mode=production \
		-o dist/client/ --env entry=./dist/client/entrypoint.js
	@cp ./index.html ./dist/client/

	BROWSER_RUNTIME=${browser-runtime} webpack --mode=production \
		-o dist/admin/ --env entry=./dist/admin/entrypoint.js
	@cp ./index.html ./dist/admin/

	@cp -r ./output ./dist/bot/
	@cp ./build/* ./dist/bot/

	@rm ./dist/client/entrypoint.js ./dist/admin/entrypoint.js

	@tar -czf ./dist/admin-browser-bundle.tar.gz -C ./dist/admin .
	@tar -czf ./dist/client-browser-bundle.tar.gz -C ./dist/client .
	@tar -czf ./dist/bot-bundle.tar.gz -C ./dist/bot .
	@zip -j ./dist/bundles.zip ./dist/admin-browser-bundle.tar.gz \
		./dist/bot-bundle.tar.gz \
		./dist/client-browser-bundle.tar.gz

