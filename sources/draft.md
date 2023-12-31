---
title:
---

<!--
::::{#drafter}

:::{#composer}

<div style="font-variant-numeric: lining-nums tabular-nums" class="sourceCode unicode" data-lang="Markdown"><pre><code><textarea id="post" style="min-height: 40vh">---
title: Draft
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

</textarea></code></pre></div>
<div><button id="post_post">Preview!</button></div>

:::

<hr id="divider" class="full-width"/>

<iframe name="posted_post" src="" class="full-width" style="border: none;"></iframe>

::::
-->

<style>
  #drafter {
    display: flex;
  }
  #composer {
    flex: 0.3;
    flex-basis: 200px;
    padding: 0 calc(2% + 8px);
    height: 100vh;
  }
  #posted_post {
    flex: 0.7;
    height: 100vh;
  }
  #posts > li {
    margin: 0;
  }
</style>

::::{#drafter .full-width}

:::{#composer}

<div style="font-variant-numeric: lining-nums tabular-nums" class="sourceCode unicode" data-lang="Markdown"><pre><code><textarea id="markdown" style="min-height: calc(75vh - 40px - 40px)"></textarea></code></pre></div>

<div>
  <label class="input-wrapper text"><span>Slug</span><input id="slug"/></label>
  <button id="load" class="">Load</button>
  <button id="post" class="add">Post</button>
  <button id="preview" class="big">Preview</button>

  <ul id="posts" style="max-height: calc(10vh + 20px); overflow: auto; border-width: 1px; border-color: #2372ffaa; border-radius: 4px;">
  </ul>
</div>

:::

<iframe id="posted_post" src="" style="border: none;"></iframe>

::::

<script>
  const the = (...arg) => document.getElementById(...arg);
  the("slug").value = new URLSearchParams(window.location.search).get("post");
  async function preview() {
    const r = await fetch('http://localhost:5656', {
      method: 'PUT', body: the("markdown").value,
    });
    the("posted_post").srcdoc = await r.text();
  };
  async function load() {
    let slug = the("slug").value;
    if (!slug) {
      if (!the("markdown").value.trim()) {
        slug = "new";
      } else {
        return;
      }
    }
    const r = await fetch('http://localhost:5656/' + slug, {
      method: 'GET',
    });
    the("markdown").value = await r.text();
    preview();
  }
  async function post() {
    if (!the("slug").value || !the("markdown").value) return;
    const r = await fetch('http://localhost:5656/' + the("slug").value, {
      method: 'POST', body: the("markdown").value,
    });
    the("posted_post").srcdoc = await r.text();
  };
  the("preview").addEventListener("click", () => preview());
  the("load").addEventListener("click", () => load());
  the("post").addEventListener("click", () => post());
  load();
  the("posts").addEventListener("dblclick", e => {
    if (e.target.nodeName === 'LI') {
      the("slug").value = e.target.textContent;
      e.preventDefault();
      load();
      window.getSelection().removeAllRanges();
      return true;
    }
  });
  fetch('http://localhost:5656').then(r => r.json()).then(posts => {
    posts.sort();
    for (let post of posts) {
      the("posts").appendChild(Object.assign(document.createElement("li"), {
        textContent: post,
      }));
    }
  });
</script>
