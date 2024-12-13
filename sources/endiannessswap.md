---
title: 2024/12/13
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2024/12/13
---

<label class="input-wrapper text" style="width: 100%">
  <span>Input</span>
  <input id="test-input" style="width: 100%" />
</label>

<label class="input-wrapper text" style="width: 100%">
  <span>Output</span>
  <input id="test-output" style="width: 100%" />
</label>

```python
def endiannessswap(text):
  """*swaps your data's endianness*"""
  data = text.encode('utf-16')
  return (data[1::-1] + data[2::]).decode('utf-16')
print(endiannessswap.__doc__)
print(endiannessswap(endiannessswap.__doc__))
```

```javascript
overChars = f => s => Array.prototype.map.call(s, c => String.fromCharCode(f(c.charCodeAt()))).join('');
byteswap = c => (c & 0xFF) << 8 | (c & 0xFF00) >> 8;
endiannessswap = overChars(byteswap);
```

<script>
overChars = f => s => Array.prototype.map.call(s, c => String.fromCharCode(f(c.charCodeAt()))).join('');
byteswap = c => (c & 0xFF) << 8 | (c & 0xFF00) >> 8;
endiannessswap = overChars(byteswap);

document.getElementById("test-input")
  .addEventListener("input", () => {
    document.getElementById("test-output").value = endiannessswap(document.getElementById("test-input").value);
  });
document.getElementById("test-output")
  .addEventListener("input", () => {
    document.getElementById("test-input").value = endiannessswap(document.getElementById("test-output").value);
  });
</script>
