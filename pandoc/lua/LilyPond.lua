function CodeBlock(h)
  if h.attr.classes:includes('lilypond') then

    if not string.find(h.text, "\\version", nil, false) then
      local tfile = io.open(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/../../LilyPond/template.ly')
      h.text = h.text .. tfile:read("*all")
      tfile:close()
    end

    os.execute('mkdir -p cache')
    local fname = 'cache/render-lilypond-' .. pandoc.sha1(h.text)
    local output = io.open(fname .. '.svg')
    if output == nil then
      output = io.open(fname .. '.ly', 'w')
      output:write(h.text)
      output:close()
      local t = pandoc.pipe(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/LilyPond.sh', {fname}, "")
      output = io.open(fname .. '.svg')
    end
    local logs = io.open(fname .. '.out')
    local result_log = ""
    if logs then
      result_log = logs:read("*all")
      logs:close()
    end
    -- Trim whitespace, particularly trailing newlines
    local result_svg = output:read("*all"):gsub("^%s*(.-)%s*$", "%1"):gsub("xlink:href=\"[^\"]*\"", "")
    output:close()

    local cls = "lilypond"

    if result_svg == "" then
      cls = "lilypond error"
    end

    return pandoc.Div({
      pandoc.RawInline('html', result_svg),
      pandoc.CodeBlock(result_log, { class = "log" }),
    }, { class = cls })
  end
end
