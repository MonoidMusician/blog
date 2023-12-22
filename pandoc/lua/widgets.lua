-- function Inlines(h)
--   result = {}
--   for k,v in pairs(h) do
--     table.insert(result, v)
--   end
--   return result
-- end

function Span(h)
  f = io.open('/tmp/span', 'w')
  found = false
  f:write(pandoc.utils.type(h.attr.attributes) .. '\n')
  for name, value in pairs(h.attr.attributes) do
    f:write(tostring(name) .. '\n')
    if name == 't' or name == 'data-t' then
      found = true
      f:write('v: ' .. tostring(value) .. '\n')
      break
    end
  end
  f:write('found: ' .. dump(found) .. '\n')
  if not found then return h end
  -- h.attr.attributes:insert("widget", "")
  f:write('attr["t"]: ' .. dump(h.attr.attributes["t"]) .. '\n')
  f:write('attr["data-t"]: ' .. dump(h.attr.attributes["t"]) .. '\n')
  table.insert(h.attr.attributes, {"widget", ""})
  f:write('attr["widget"]: ' .. dump(h.attr.attributes["widget"]) .. '\n')
  f:write(pandoc.write(singleton(h), 'html') .. '\n')
  return h
end

function singleton(h)
  return pandoc.Pandoc(pandoc.Plain(h))
end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

