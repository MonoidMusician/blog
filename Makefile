TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all
all : init sass prod-ps sources.txt pandoc

.PHONY : watch-all
watch-all :
	./watch-all.sh

.PHONY : init
init : | node_modules rendered $(BUILDIR) cache

node_modules :
	npm install

rendered :
	mkdir -p rendered

cache :
	mkdir -p cache

assets/json :
	mkdir -p assets/json

.PHONY : clean
clean : clean-pandoc clean-ps

.PHONY : clean-pandoc
clean-pandoc :
	rm -rf rendered/*
	rm -rf cache/

.PHONY : clean-ps
clean-ps :
	rm -rf output
	rm -rf output-es
	rm -rf .psci_modules
	rm -rf generated-docs
	rm -rf assets/purs

.PHONY : squeaky-clean
squeaky-clean : clean
	rm -rf node_modules
	rm -rf dist-newstyle
	rm -rf .spago

sass : $(BUILDIR)/styles/bundles.css

.PHONY : watch-sass
watch-sass : | $(BUILDIR)
	rm -f $(BUILDIR)/styles/bundled*.css.gz
	sass -w styles/bundled.sass:$(BUILDIR)/styles/bundled.css styles/bundled_light.sass:$(BUILDIR)/styles/bundled_light.css styles/bundled_sans.sass:$(BUILDIR)/styles/bundled_sans.css styles/bundled_light_sans.sass:$(BUILDIR)/styles/bundled_light_sans.css

$(BUILDIR)/styles/bundles.css : styles/*.sass | $(BUILDIR)
	rm -f $(BUILDIR)/styles/bundled*.css.gz
	sass styles/bundled.sass:$(BUILDIR)/styles/bundled.css styles/bundled_light.sass:$(BUILDIR)/styles/bundled_light.css styles/bundled_sans.sass:$(BUILDIR)/styles/bundled_sans.css styles/bundled_light_sans.sass:$(BUILDIR)/styles/bundled_light_sans.css
	gzip -f9k $(BUILDIR)/styles/bundled*.css

$(BUILDIR) : | rendered
	mkdir -p $(BUILDIR)
	rm -f $(BUILDIR)/assets
	ln -s $$(realpath assets) $(BUILDIR)/assets
	rm -f $(BUILDIR)/rendered
	ln -s $$(realpath rendered) $(BUILDIR)/rendered

pandoc : $(HTMLS)

$(BUILDIR)/%.html : $(TXTDIR)/%.md pandoc/defaults.yaml pandoc/post.html PureScript/src/PureScript/Highlight.purs | cache
	pandoc --defaults=pandoc/defaults.yaml $< -o $@

.PHONY : watch-pandoc
watch-pandoc :
	watchexec -w pandoc -f 'pandoc/**' -w $(TXTDIR) -f '$(TXTDIR)/*.md' -r -- make pandoc

prod-ps : $(BUILDIR) PureScript/src PureScript/directives.txt spago.yaml PureScript/spago.yaml
	rm -f $(BUILDIR)/widgets.js.gz
	spago build
	purs-backend-es bundle-app --directives PureScript/directives.txt --main Main --to $(BUILDIR)/widgets.js
	gzip -f9k $(BUILDIR)/widgets.js
	make assets-ps

.PHONY : assets-ps
assets-ps : assets/json
	time spago run -m Build
	(for f in assets/json/*-parser-states.json; do gzip -f9k "$$f"; done)

ps : $(BUILDIR) PureScript/src spago.yaml PureScript/spago.yaml
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle --bundle-type app --module Main --outfile ../$(BUILDIR)/widgets.js
	make assets-ps

sources.txt : spago.lock spago.yaml PureScript/spago.yaml
	spago sources > sources.txt 2>/dev/null

.PHONY : trypurescript
trypurescript : PureScript/src sources.txt
	watchexec -w $(BUILDIR)/widgets.js -r --shell=bash 'set -o noglob; trypurescript 6565 $$(cat sources.txt)'

.PHONY : watch-ps
watch-ps : $(BUILDIR)
	rm -f $(BUILDIR)/widgets.js.gz
	watchexec -w PureScript/src -f 'PureScript/src/**/*.{purs,js}' -r --shell=bash 'spago bundle --bundle-type app --module Main --outfile ../$(BUILDIR)/widgets.js && make assets-ps >/dev/null'

.PHONY : docs-ps
docs-ps :
	spago docs -f markdown
	spago docs -f html

.PHONY : live
live : live/node_modules
	(cd live; npm run live)

live/node_modules :
	(cd live; npm install)
