function CodeBlock(h)
  if h.attr.classes:includes('purescript') or h.attr.classes:includes('ps') then
    os.execute('mkdir -p cache')
    local fname = 'cache/render-purescript-' .. pandoc.sha1(h.text)
    local output = io.open(fname .. '.html')
    if output == nil then
      local t = pandoc.pipe(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/../../script.sh', {'--no-build', 'highlightPandoc'}, h.text)
      output = io.open(fname .. '.html', 'w')
      output:write(t)
      output:close()
      output = io.open(fname .. '.html')
    end
    -- Trim whitespace, particularly trailing newlines
    local result = output:read("*all"):gsub("^%s*(.-)%s*$", "%1")
    output:close()

    return pandoc.RawInline('html', result)
  end
end

function Code(h)
  if h.attr.classes:includes('purescript') or h.attr.classes:includes('ps') then
    os.execute('mkdir -p cache')
    local fname = 'cache/render-purescript-inline-' .. pandoc.sha1(h.text)
    local output = io.open(fname .. '.html')
    if output == nil then
      local t = pandoc.pipe(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/../../script.sh', {'--no-build', 'highlightPandocInline'}, h.text)
      output = io.open(fname .. '.html', 'w')
      output:write(t)
      output:close()
      output = io.open(fname .. '.html')
    end
    -- Trim whitespace, particularly trailing newlines
    local result = output:read("*all"):gsub("^%s*(.-)%s*$", "%1")
    output:close()

    return pandoc.RawInline('html', result)
  end
end
