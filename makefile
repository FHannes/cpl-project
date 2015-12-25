all: build

build:
	elm-make Main.elm --output=js/elm.js

clean:
	rm -f js/elm.js
