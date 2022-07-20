TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all

all : $(HTMLS) $(BUILDIR)/styles/bundles.css

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

watch :
	sass -w styles/bundled.sass $(BUILDIR)/styles/bundled.css

$(BUILDIR)/styles/bundles.css : styles/*.sass
	sass styles/bundled.sass $(BUILDIR)/styles/bundled.css

$(BUILDIR) :
	mkdir -p $(BUILDIR)

$(BUILDIR)/%.html : $(TXTDIR)/%.md $(BUILDIR)
	pandoc --defaults=pandoc/defaults.yaml $< -o $@

ps :
	spago bundle-app --main Main --to static/widgets.js
