-- A port of https://github.com/jez/pandoc-sidenote/blob/master/src/Text/Pandoc/SideNote.hs
-- With the change the margin notes also generate a blank label

i = 0

function Note (h)
  -- local value = pandoc.utils.stringify(h.content)
  -- local prefix = pandoc.text.sub(value, 1, 4)
  --[[
  if prefix == "{-} " then
    type = "marginnote"
  elseif prefix == "{.} " then
    type = "footnote"
  end
  --]]
  local type = "sidenote"
  if #h.content > 0 then
    if (h.content[1].t == "Plain" or h.content[1].t == "Para") and #h.content[1].content > 1 then
      local foci = h.content[1].content
      if foci[1] == pandoc.Str("{-}") and foci[2] == pandoc.Space() then
        type = "marginnote"
        foci:remove(1)
        foci:remove(1)
      elseif foci[1] == pandoc.Str("{.}") and foci[2] == pandoc.Space() then
        type = "footnote"
        foci:remove(1)
        foci:remove(1)
      end
    elseif h.content[1].t == "LineBlock" and #h.content[1].content > 0  and #h.content[1].content[1] > 1 then
      local foci = h.content[1].content[1]
      if foci[1] == pandoc.Str("{-}") and foci[2] == pandoc.Space() then
        type = "marginnote"
        foci:remove(1)
        foci:remove(1)
      elseif foci[1] == pandoc.Str("{.}") and foci[2] == pandoc.Space() then
        type = "footnote"
        foci:remove(1)
        foci:remove(1)
      end
    end
  end
  if type == "footnote" then
    return h
  end
  local text = pandoc.utils.blocks_to_inlines(h.content, { pandoc.LineBreak(), pandoc.LineBreak() })
  i = i + 1
  local label_class = "margin-toggle"
  -- Note: I itentionally leave this blank so it can be styled with CSS
  local label_sym = "" -- "&#8853;"
  if type == "sidenote" then
    label_class = label_class .. " sidenote-number"
    label_sym = ""
  end
  local toggle = pandoc.RawInline('html', "<label class=\"" .. label_class .. "\" for=\"sd-" .. i .. "\">" .. label_sym .. "</label><input type=\"checkbox\" class=\"margin-toggle\" id=\"sd-" .. i .. "\"/>")
  local wrapped = pandoc.Span({toggle, pandoc.Span(text, { class = type })}, { class = "sidenote-wrapper" })
  return wrapped
end
