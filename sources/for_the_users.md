---
title: For the Users!
subtitle: My user scripts and user styles (& othersʼ!)
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

## User styles

I use the Stylus extension for Chromium.
It is decent.
Unlike Stylish, which is crap.
(I lost all my user styles on Stylish.)

### Dark mode

I use my computer in the dark a lot.
Way too much.

This is my unreasonably effective dark mode that I apply to more and more websites as I find them:

```css
html {
    background: #191919 !important;
}
body {
    background: white !important;
    filter: invert(0.9) hue-rotate(180deg);
    box-shadow: none;
}
img, video {
    filter: invert(1) contrast(111%) hue-rotate(180deg);
}
```

How this works:

- It inverts brightness, and softens the contrast by 10% in the process.
  (I find too stark of contrast on dark themes to be unpleasant.)
- It also flips the colors back around, so it mostly matches the original!
- Images have the reverse transformations applied, including bumping contrast back up.
  (This is lossy but shouldnʼt matter much. Also, `1.1` is a little less than `1/0.9`.)
- Finally we fix up the backgrounds: `html`{.css} gets a black background for overscroll, while `body`{.css} gets a white background and the filter.
  (The filter on `body`{.css} will not apply to overscroll.)

Most bright websites look surprisingly good with this filter!
A lot actually look better!

Unfortunately thereʼs no good one-click way to add it to a new site.
You have to open up the stylesheet in Stylus and add a new domain rule manually.

#### Wikipedia

Uhhh I use [Dark Wikipedia by @nodaguti – User Styles Archive](https://uso.kkx.one/style/42313).
Itʼs really old but it mostly works??
Thereʼs some rough edges.
But a lot of other stylesheets just donʼt work.


<details class="Details">

<summary>Additional rules</summary>

I add these rules to smooth out the rough edges:

```css
@-moz-document domain("wikipedia.org") {
    figure[typeof~='mw:File/Thumb'] > figcaption, figure[typeof~='mw:File/Frame'] > figcaption {
        border: 1px solid #87898c;
        border-top: 0;
        background-color: #2c2f31;
        font-size: 88.4%;
        color: #dddddd;
    }
    figure[typeof~='mw:File/Thumb'], figure[typeof~='mw:File/Frame'] {
        border: 1px solid #87898c;
        border-bottom: 0;
        background-color: #2c2f31;
    }
    .mwe-math-fallback-image-inline img, .mwe-math-element img, img[src*="LaTeX"] {
        filter: invert(100%);
    }
    .mwe-math-fallback-image-inline img::selection, .mwe-math-element img::selection, img[src*="LaTeX"] ::selection {
        background-color: rgba(91, 142, 118, 1.0);
    }

    @media screen and (min-width: 1000px) {
        .vector-feature-zebra-design-disabled #vector-toc-pinned-container .vector-toc::after {
            background: linear-gradient(rgba(255,255,255,0),#333333);
        }
    }

    .mwe-popups .mwe-popups-extract[dir='ltr']::after {
        right: 0;
        background-image: linear-gradient(to right,rgba(255,255,255,0),#333333 50%);
    }

    .mw-logo-container {
        filter: invert(1);
    }

    .mw-notification-area-overlay {
        display: none !important;
    }
}
```

</details>

#### Haskell Discourse

To make the logo fit in:

```css
@-moz-document domain("discourse.haskell.org") {
    .d-header #site-logo {
        filter: invert(1) contrast(87%) hue-rotate(180deg);
    }
}
```

## User scripts

I use TamperMonkey extension on Chromium.

### Dark mode

I made a script to apply the above generic dark mode to any page that has no styles.
This is particularly common for 404 pages and other things.

It will mostly still flash on page load though&nbsp;…

<details class="Details">

<summary>Script</summary>

```js
// ==UserScript==
// @name         Emergency dark mode
// @namespace    http://tampermonkey.net/
// @version      0.1
// @author       MonoidMusician
// @match        *://*
// @match        *://*/*
// @sandbox      DOM
// @require      https://raw.githubusercontent.com/emn178/js-md5/master/src/md5.js#md5=e7875ebe9e8341e772e830c48a732f02
// ==/UserScript==

(function() {
    'use strict';

    const knownMD5s = {
        "e30da815843cee743f9881389d76c1cb": true, // validation_data.html
        "d524ff95b8cd14c17def47962cb3a734": true, // lshw -html
        "b4aebef39771fb9d57f8641b14ed4aef": true, // wasmtime explore (old)
        "66cf6596c2f88bdad3c3ea77ea45bcdf": true, // wasmtime explore (new)
    };
    const knownLinks = {
        "linuwial.css": true,
    };
    const custom = [
        e => /^https:\/\/hackage.haskell.org\/package\/.*?\/docs\/src\/style\.css/.test(e.href) || undefined,
    ];

    function should() {
        if (document.querySelector("body > div.debugger + div.pin-prompt")) {
            // Flask error pages
            return true;
        }

        for (let link of document.querySelectorAll("link[rel='stylesheet']")) {
            const href = link.getAttribute("href");
            if (href in knownLinks) {
                console.debug("Dark mode trigger", knownLinks[href], href, link);
                return knownLinks[href];
            }
            for (const f of custom) {
                const r = f(link);
                if (r != null) return r;
            }
        }

        for (let style of document.querySelectorAll("style")) {
            let thisMD5 = window.md5(style.textContent);
            if (thisMD5 in knownMD5s) {
                console.debug("Dark mode trigger", knownMD5s[thisMD5], thisMD5, style);
                return knownMD5s[thisMD5];
            } else {
                //console.debug("Nope", thisMD5, style);
            }
            for (const f of custom) {
                const r = f(style);
                if (r != null) return r;
            }
        }

        try {
            // <iframe>s
            if (window.self !== window.top) return console.debug("No dark mode", document.body);
        } catch (e) {
            console.log("Error in window");
            return;
        }

        var href = window.location.href;
        if (href.endsWith(".pdf") || href.endsWith(".ps") || href.endsWith(".m3u8")) {
            return console.debug("No dark mode", ".pdf/.ps/.m3u8");
        }
        if (document.querySelector("body > embed")) {
            return console.debug("No dark mode", "body > embed");
        }
        if (href.includes("/bitstream")) {
            return console.debug("No dark mode", "/bitstream");
        }
        if (document.querySelector("link[rel='stylesheet'], style, link[href$='.css']")) {
            return console.debug("No dark mode", "link[rel='stylesheet'], style, link[href$='.css']");
        }
        if (document.querySelector(":root[style], body[style], body > :first-child:last-child[style]")) {
            return console.debug("No dark mode", ":root[style], body[style], body > :first-child:last-child[style]");
        }
        return true;
    }

    const directive = should();

    if (!directive) return;

    var style = document.createElement("style");
    style.textContent = typeof directive === 'string' ? directive : `
            html {
                background: #191919 !important;
            }
            body {
                background: white !important;
                filter: invert(0.9) hue-rotate(180deg);
                box-shadow: none;
            }
            img, video {
                filter: invert(1) contrast(111%) hue-rotate(180deg);
            }
    `;
    document.head.appendChild(style);
})();
```

</details>

### Local<=>Remote

I made a little script that helps me keep track of local versions of remote sites.

You just give it a mapping of local and remote.

Note that it requires permissions on all pages, just so that updating the JavaScript code makes it work.
But you can of course scope it to just the websites you want!

<details class="Details">

<summary>Script</summary>

```js
// ==UserScript==
// @name         Local<=>Remote
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Add helpful link to remote version of website
// @author       MonoidMusician
// @match        *://*
// @match        *://*/*
// @sandbox      DOM
// ==/UserScript==

(async function() {
    'use strict';

    var matches = Object.entries({
        'https://blog.veritates.love': 'http://localhost:7933',
        'https://monoidmusician.github.io': 'http://localhost',
        // ^ unfortunately this captures all of localhost, even different ports ...
    });

    var here = window.location.href;
    var there = null;
    for (let match of matches) {
        for (let [src, dst] of [match, match]) {
            // Flip src and dst for the next time around
            match.reverse();

            // Test if the URL starts with the src
            if (here.startsWith(src)) {
                // If so, replace it with the dst
                there = dst + here.substring(src.length);
                break;
            }
        }
        if (there) break;
    }

    if (here === there || !there) return;

    // Create a link that floats in the top left of the page
    var link = document.createElement('a');
    Object.assign(link.style, {
        'position': 'fixed',
        'top': '0',
        'left': '0',
        'display': 'block',
        'padding': '4px',
        'line-height': '1',
        'width': '1em',
        'height': '1em',
        'text-align': 'center',
    });
    link.href = there;
    // Give the link the text `@`
    link.textContent = "@";

    document.body.appendChild(link);
})();
```

</details>
