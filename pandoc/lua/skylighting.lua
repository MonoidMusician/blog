function CodeBlock (cb)
  -- Remove code blocks with ```xml{.skylighting}
  if cb.classes[1] == "xml" and cb.classes[2] == "skylighting" then
    return {}
  end
end
