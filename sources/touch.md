Touch events

:::{#controls}
:::

::::{style="display: flex"}

:::{.Bonus #outputs style="width: 50%"}
:::

:::{.Details #allEvents style="width: 50%"}
:::

::::

<script type="module">
import { touch } from "/assets/js/Riverdragon/examples.js";

touch();
</script>

<svg id="renderTouches" style="width: 100vw; height: 100vh; left: 0; top: 0; position: fixed; pointer-events: none;">
    <circle class="touchPoint" r="40" cx="10" cy="20" fill="transparent"/>
</svg>

<style>
body {
    touch-action: none; /* disable touch scrolling */
}
</style>
