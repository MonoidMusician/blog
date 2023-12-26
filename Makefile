TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all watch-sass watch-all watch-pandoc watch-ps trypurescript

all : sass pandoc prod-ps

watch-all :
	./watch-all.sh

init :
	rm -f static/assets
	ln -s $(realpath assets) static/assets
	mkdir -p rendered
	rm -f static/rendered
	ln -s $(realpath rendered) static/rendered
	mkdir -p cache

clean :
	rm -rf rendered/*
	rm -rf cache/

sass : $(BUILDIR)/styles/bundles.css

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

watch-pandoc :
	ls $(TXTDIR)/*.md pandoc/defaults.yaml pandoc/post.html | entr make pandoc

prod-ps : $(BUILDIR) PureScript/src packages.dhall spago.dhall
	rm -f $(BUILDIR)/widgets.js.gz
	spago build --purs-args "-g corefn,js"
	spago run -m PreBuild
	purs-backend-es bundle-app --main Main --to $(BUILDIR)/widgets.js
	gzip -f9k $(BUILDIR)/widgets.js

prebuild : PureScript/src/PreBuild.purs PureScript/src/Parser/Parserlude.purs
	spago run -m PreBuild

ps : $(BUILDIR) PureScript/src prebuild packages.dhall spago.dhall
	spago build

watch-prebuild :
	ls PureScript/src/PreBuild.purs PureScript/src/Parser/Parserlude.purs | entr make prebuild

trypurescript : PureScript/src
	ls PureScript/src/**/*.purs | (set -f; entr -r trypurescript 6565 $$(spago sources))

watch-ps : $(BUILDIR)
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle-app -w --main Main --to $(BUILDIR)/widgets.js
