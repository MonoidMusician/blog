TXTDIR=sources
BUILDIR=static
MDS=$(wildcard $(TXTDIR)/*.md)
HTMLS=$(patsubst $(TXTDIR)/%.md, $(BUILDIR)/%.html, $(MDS))

.PHONY : all

all : $(HTMLS) $(BUILDIR)/styles/bundles.css

watch :
	sass -w styles/bundled.sass $(BUILDIR)/styles/bundled.css

$(BUILDIR)/styles/bundles.css : styles/bundled.sass
	sass styles/bundled.sass $(BUILDIR)/styles/bundled.css

$(BUILDIR) :
	mkdir -p $(BUILDIR)

$(BUILDIR)/%.html : $(TXTDIR)/%.md $(BUILDIR)
	#pandoc --from markdown+tex_math_single_backslash+footnotes+inline_notes --toc --toc-depth=5 -V --lang=en --section-divs --template=pandoc/post --filter pandoc-sidenote --lua-filter=pandoc/lua/anchor-links.lua --filter pandoc-static-katex --css https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/katex.min.css --mathjax=https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js --css=styles/bundled.css --css=https://cdn.jsdelivr.net/npm/katex@0.13.23/dist/contrib/copy-tex.css $< -o $@
	# pandoc --from markdown+tex_math_single_backslash+footnotes+inline_notes --toc --toc-depth=5 -V --lang=en --section-divs --template=pandoc/post --filter pandoc-sidenote --lua-filter=pandoc/lua/anchor-links.lua --katex --css=styles/bundled.css --css=https://cdn.jsdelivr.net/npm/katex@0.13.23/dist/contrib/copy-tex.css $< -o $@
	pandoc --from markdown+tex_math_single_backslash+footnotes+inline_notes --toc --toc-depth=5 -V --lang=en --section-divs --template=pandoc/post --lua-filter=pandoc/lua/sidenotes.lua --lua-filter=pandoc/lua/anchor-links.lua --lua-filter=pandoc/lua/static-katex.lua --lua-filter=pandoc/lua/concat.lua --lua-filter=pandoc/lua/canvas.lua --css https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/katex.min.css --css=https://cdn.jsdelivr.net/npm/katex@0.13.23/dist/contrib/copy-tex.css --css=styles/bundled.css $< -o $@
