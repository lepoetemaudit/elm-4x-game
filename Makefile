watch:
	chokidar '**/*.elm' -c 'elm make Main.elm --output elm.compiled.js' --initial
