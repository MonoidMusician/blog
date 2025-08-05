-- Remove width annotations from the table
function Table(h)
  for i,colspec in ipairs(h.colspecs) do
    colspec[2] = nil
  end
  return h
end
