local bin = false

function Math(h)
  local display = (h.mathtype == 'DisplayMath')
  os.execute('mkdir -p cache')
  local fname = 'cache/render-katex2-' .. pandoc.sha1(h.text .. h.mathtype .. io.open('assets/katex_macros.tex'):read("*all"))
  local output = io.open(fname .. '.html')
  if output == nil then
    if bin == false then
      os.execute('ln -sf $(which katex || npx -p katex which katex) cache/katex')
      bin = true
    end
    local display_flag = ''
    if display then display_flag = '--display-mode' end
    local input = io.open(fname .. '.tex', 'w')
    input:write(h.text)
    input:close()
    os.execute('./cache/katex --no-throw-on-error --trust --macro-file assets/katex_macros.tex -F mathml ' .. display_flag .. ' -i ' .. fname .. '.tex' .. ' -o ' .. fname .. '.html')
    output = io.open(fname .. '.html')
  end
  -- Trim whitespace, particularly trailing newlines
  local result = output:read("*all"):gsub("^%s*(.-)%s*$", "%1")
  output:close()

  return pandoc.RawInline('html', result)
end
