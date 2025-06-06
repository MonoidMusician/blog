---
title: "Quapteryx Takes Flight"
subtitle: "Quapteryx Part V"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
# date: 2025/05/11
---

<input id="quapteryx_input"/>

<span id="quapteryx_output"></span>

<script src="../assets/js/quapteryx.js"></script>

<script>
{
  let input = document.getElementById("quapteryx_input");
  let output = document.getElementById("quapteryx_output");
  input.onchange = () => {
    if (input.value) {
      var evaluating = quapteryx(input.value);
      if (typeof evaluating === 'string') {
        output.textContent = evaluating;
      } else {
        evaluating.then(
          evaluated => output.textContent = evaluated,
          err => console.error(err),
        );
      }
    }
  };
};
</script>
