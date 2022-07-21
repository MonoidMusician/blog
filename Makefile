TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all

all : $(HTMLS) $(BUILDIR)/styles/bundles.css

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

watch-sass :
	sass -w styles/bundled.sass $(BUILDIR)/styles/bundled.css

$(BUILDIR)/styles/bundles.css : styles/*.sass
	sass styles/bundled.sass $(BUILDIR)/styles/bundled.css

$(BUILDIR) :
	mkdir -p $(BUILDIR)

pandoc : $(BUILDIR)/*.html

$(BUILDIR)/%.html : $(TXTDIR)/%.md $(BUILDIR) pandoc/defaults.yaml pandoc/post.html
	pandoc --defaults=pandoc/defaults.yaml $< -o $@

watch-pandoc :
	ls $(TXTDIR)/*.md pandoc/defaults.yaml pandoc/post.html | entr make pandoc

ps : PureScript/src packages.dhall spago.dhall
	spago bundle-app --main Main --to static/widgets.js

watch-ps :
	spago bundle-app -w --main Main --to static/widgets.js
