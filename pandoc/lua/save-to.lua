function CodeBlock(h)
  fname = h.attr.attributes['save-to']
  if fname and not string.find(fname, '../', 1, true) then
    allowed = { 'assets/misc/', 'assets/files/' }
    matched = nil
    for _, path in ipairs(allowed) do
      if string.sub(fname, 1, string.len(path)) == path then
        matched = path
      end
    end
    if matched then
      output = io.open('./' .. fname, 'w')
      output:write(h.text .. "\n")
      output:close()
      h.attr.attributes['save-to'] = nil
      return h
    end
  elseif fname then
    print('thinking of escaping? ' .. pandoc.json.encode(fname))
  end
end
