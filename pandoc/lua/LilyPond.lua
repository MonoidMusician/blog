function CodeBlock(h)
  if h.attr.classes:includes('lilypond') then

    local full_source = h.text
    if not string.find(h.text, "\\version", nil, false) then
      local tfile = io.open(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/../../LilyPond/template.ly')
      full_source = h.text .. '\n\n' .. tfile:read("*all")
      tfile:close()
    end

    os.execute('mkdir -p cache')
    local fname = 'cache/render-lilypond-' .. pandoc.sha1(full_source)
    local output_wide = io.open(fname .. '-wide.svg')
    local output_narrow = io.open(fname .. '-narrow.svg')
    local output_extra = io.open(fname .. '-extra.svg')
    if output_wide == nil or output_narrow == nil or output_extra == nil then
      local input = io.open(fname .. '.ly', 'w')
      input:write(full_source)
      input:close()
      local t1 = pandoc.pipe(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/LilyPond.sh', {fname, fname .. '-wide', '-dpaper-size="a5"'}, "")
      local t2 = pandoc.pipe(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/LilyPond.sh', {fname, fname .. '-narrow', '-dpaper-size="a6"'}, "")
      local t3 = pandoc.pipe(pandoc.path.directory(PANDOC_SCRIPT_FILE) .. '/LilyPond.sh', {fname, fname .. '-extra', '-dpaper-size="a7"'}, "")
      output_wide = io.open(fname .. '-wide.svg')
      output_narrow = io.open(fname .. '-narrow.svg')
      output_extra = io.open(fname .. '-extra.svg')
    end
    -- local logs = io.open(fname .. '-wide.out')
    -- local result_log = ""
    -- if logs then
    --   result_log = logs:read("*all")
    --   logs:close()
    -- end

    local wide_svg = output_wide:read("*all")
    local narrow_svg = output_narrow:read("*all")
    local extra_svg = output_extra:read("*all")
    output_wide:close()
    output_narrow:close()
    output_extra:close()

    return pandoc.Div({
      pandoc.Div(pandoc.RawInline('html', wide_svg), { class = "wide" }),
      pandoc.Div(pandoc.RawInline('html', narrow_svg), { class = "narrow" }),
      pandoc.Div(pandoc.RawInline('html', extra_svg), { class = "extra-narrow" }),
      -- pandoc.CodeBlock(result_log, { class = "log" }),
      pandoc.RawBlock('html', '<details>'),
      pandoc.RawInline('html', '<summary>LilyPond Source</summary>'),
      h,
      pandoc.RawBlock('html', '</details>'),
    }, { class = "lilypond lilypond-wrapper" })
  end
end
