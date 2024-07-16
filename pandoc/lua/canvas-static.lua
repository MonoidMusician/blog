function RawBlock(h)
  if h.format == 'html' then
    if re.find(h.text, "'<'[Cc]*[Aa][Nn][Vv][Aa][Ss]") ~= nil then
      os.execute('mkdir -p cache')
      local fname = 'cache/render-canvas2-' .. pandoc.sha1(h.text)
      local output = io.open(fname .. '.html')
      if output == nil then
        os.execute('mkdir -p ./rendered')
        local t = pandoc.pipe('node', {pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/canvas.js', '--replace', '--', './rendered'}, h.text)
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
end
