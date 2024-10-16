TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all
all : init sass prod-ps pandoc

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

.PHONY : clean
clean :
	rm -rf rendered/*
	rm -rf cache/

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
	watchexec -w pandoc -f 'pandoc/**' -w $(TXTDIR) -f '$(TXTDIR)/*.md' -- make pandoc

prod-ps : $(BUILDIR) PureScript/src PureScript/directives.txt packages.dhall spago.dhall
	rm -f $(BUILDIR)/widgets.js.gz
	spago build --purs-args "-g corefn,js"
	spago run --purs-args "-g corefn,js" -m PreBuild
	purs-backend-es bundle-app --directives PureScript/directives.txt --main Main --to $(BUILDIR)/widgets.js
	gzip -f9k $(BUILDIR)/widgets.js
	make assets-ps

.PHONY : assets-ps
assets-ps :
	cp PureScript/src/Parser/Parserlude.purs output/Parser.Parserlude/index.purs
	cp PureScript/src/Riverdragon/Dragon/Nest.purs output/Riverdragon.Dragon.Nest/index.purs

prebuild : PureScript/src/PreBuild.purs PureScript/src/Parser/Parserlude.purs
	spago run --purs-args "-g corefn,js" -m PreBuild

.PHONY : grammars
grammars :
	time spago run --purs-args "-g corefn,js" -m Build
	(for f in assets/json/*-parser-states.json; do gzip -f9k "$$f"; done)

ps : $(BUILDIR) PureScript/src prebuild packages.dhall spago.dhall
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle-app --purs-args "-g corefn,js" --main Main --to $(BUILDIR)/widgets.js
	make assets-ps

watch-prebuild :
	watchexec -w PureScript/src/PreBuild.purs -w PureScript/src/Parser/Parserlude.purs -r -- make prebuild

.PHONY : trypurescript
trypurescript : PureScript/src
	(set -f; watchexec -w PureScript/src -f 'PureScript/src/**/*.purs' -r -- trypurescript 6565 $$(spago sources))

.PHONY : watch-ps
watch-ps : $(BUILDIR)
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle-app --purs-args "-g corefn,js" -w --then "make assets-ps" --main Main --to $(BUILDIR)/widgets.js

.PHONY : docs-ps
docs-ps :
	spago docs -f markdown --no-search
	spago docs -f html --no-search

.PHONY : live
live : live/node_modules
	(cd live; npm run live)

live/node_modules :
	(cd live; npm install)
