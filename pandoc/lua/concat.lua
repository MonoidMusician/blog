-- Based on https://github.com/jgm/pandoc/commit/acab2087bb7ad5ebeaec79e451e95c97da505642

function Blocks(h)
  result = {}
  last = nil
  for k,v in pairs(h) do
    if v.tag == 'RawBlock' then
      if last == nil then
        last = v
      elseif last.format == v.format then
        last.text = last.text .. '\n' .. v.text
      else
        table.insert(result, last)
        last = v
      end
    else
      if last ~= nil then
        table.insert(result, last)
        last = nil
      end
      table.insert(result, v)
    end
  end
  if last ~= nil then
    table.insert(result, last)
  end
  return result
end
