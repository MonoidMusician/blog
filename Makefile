TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all watch-sass watch-all watch-pandoc watch-ps

all : sass pandoc ps

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

pandoc : quick-ps $(HTMLS)

$(BUILDIR)/%.html : $(TXTDIR)/%.md $(BUILDIR) pandoc/defaults.yaml pandoc/post.html
	pandoc --defaults=pandoc/defaults.yaml $< -o $@

watch-pandoc :
	ls $(TXTDIR)/*.md pandoc/defaults.yaml pandoc/post.html | entr make pandoc

ps : $(BUILDIR) PureScript/src packages.dhall spago.dhall
	rm -f $(BUILDIR)/widgets.js.gz
	spago build --purs-args "-g corefn"
	purs-backend-es bundle-app --main Main --to $(BUILDIR)/widgets.js
	gzip -f9k $(BUILDIR)/widgets.js

quick-ps : $(BUILDIR) PureScript/src packages.dhall spago.dhall
	spago build

watch-ps : $(BUILDIR)
	rm -f $(BUILDIR)/widgets.js.gz
	spago bundle-app -w --main Main --to $(BUILDIR)/widgets.js
