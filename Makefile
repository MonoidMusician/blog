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
init : node_modules rendered static cache

node_modules :
	npm install

static : rendered
	mkdir -p static
	rm -f static/assets
	ln -s $$(realpath assets) static/assets
	rm -f static/rendered
	ln -s $$(realpath rendered) static/rendered

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
watch-sass : $(BUILDIR)
	rm -f $(BUILDIR)/styles/bundled*.css.gz
	sass -w styles/bundled.sass:$(BUILDIR)/styles/bundled.css styles/bundled_light.sass:$(BUILDIR)/styles/bundled_light.css styles/bundled_sans.sass:$(BUILDIR)/styles/bundled_sans.css styles/bundled_light_sans.sass:$(BUILDIR)/styles/bundled_light_sans.css

$(BUILDIR)/styles/bundles.css : styles/*.sass $(BUILDIR)
	rm -f $(BUILDIR)/styles/bundled*.css.gz
	sass styles/bundled.sass:$(BUILDIR)/styles/bundled.css styles/bundled_light.sass:$(BUILDIR)/styles/bundled_light.css styles/bundled_sans.sass:$(BUILDIR)/styles/bundled_sans.css styles/bundled_light_sans.sass:$(BUILDIR)/styles/bundled_light_sans.css
	gzip -f9k $(BUILDIR)/styles/bundled*.css

$(BUILDIR) :
	mkdir -p $(BUILDIR)

pandoc : $(HTMLS) PureScript/src/PureScript/Highlight.purs

$(BUILDIR)/%.html : $(TXTDIR)/%.md pandoc/defaults.yaml pandoc/post.html
	pandoc --defaults=pandoc/defaults.yaml $< -o $@

.PHONY : watch-pandoc
watch-pandoc :
	ls $(TXTDIR)/*.md pandoc/defaults.yaml pandoc/post.html | entr make pandoc

prod-ps : $(BUILDIR) PureScript/src packages.dhall spago.dhall
	rm -f $(BUILDIR)/widgets.js.gz
	spago build --purs-args "-g corefn,js"
	spago run --purs-args "-g corefn,js" -m PreBuild
	purs-backend-es bundle-app --main Main --to $(BUILDIR)/widgets.js
	gzip -f9k $(BUILDIR)/widgets.js

prebuild : PureScript/src/PreBuild.purs PureScript/src/Parser/Parserlude.purs
	spago run --purs-args "-g corefn,js" -m PreBuild

ps : $(BUILDIR) PureScript/src prebuild packages.dhall spago.dhall
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle-app --purs-args "-g corefn,js" --main Main --to $(BUILDIR)/widgets.js

watch-prebuild :
	ls PureScript/src/PreBuild.purs PureScript/src/Parser/Parserlude.purs | entr make prebuild

.PHONY : trypurescript
trypurescript : PureScript/src
	ls PureScript/src/**/*.purs | (set -f; entr -r trypurescript 6565 $$(spago sources))

.PHONY : watch-ps
watch-ps : $(BUILDIR)
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle-app --purs-args "-g corefn,js" -w --main Main --to $(BUILDIR)/widgets.js
