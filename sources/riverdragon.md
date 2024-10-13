:::{.centered}
bones resting beneath\
a lake, a stagnant stream.\
of wings and flying we dream,\
as flowing rivers yearn to drag on.\
:::

This input sends `\i -> Left (Tuple i i)`{.purescript}, which youʼll have to take my word for it that it is cool that it deduplicates events and does not send two events `Left (Tuple newValue oldValue), Left (Tuple newValue newValue)`{.purescript}.

<input id="test-input" />

These inputs are joined together `\i j -> Right (Tuple i j)`{.purescript} with `<*>`{.purescript}/`lift2`{.purescript}.
This means it sends an event whenever either one updates.
However, the subscription is set up to ignore the initial value of the input, so the combined event will not activate until both inputs have received an updated value.

<input id="test-input1" />
<input id="test-input2" />

<span data-widget="Riverdragon.Test"></span>

Just some nonsense that it renders for testing.
Itʼs pretty cool that it works, trust me.
<div id="render-target"></div>

<script>
// Clear inputs
for (let x of document.querySelectorAll("input")) {
  x.value = "";
}
// Listen for the widgets
((n) => {
  if (!String(window.location).includes("?live")) return;
  const es = new EventSource('widgets.js?watch');
  es.onmessage = _ => {
    if (n++) {
      es.close();
      location.reload();
    }
  };
})(0);
</script>
