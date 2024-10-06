#!/usr/bin/env node

// watchexec -c -w pandoc/lua/masto.mjs --shell=fish -- "node pandoc/lua/masto.mjs (cat thread_url) ~/Downloads/archive-*.zip | sponge sources/infodump.md"

import fs from "node:fs/promises";
import { spawn } from "node:child_process";

async function piped(proc, str) {
  return new Promise((resolve, reject) => {
    let result = "";
    proc.stdout.on('data', (chunk) => { result += chunk.toString() });
    proc.stderr.pipe(process.stderr);
    if (str) {
      proc.stdin.write(str);
      proc.stdin.end();
    }
    proc.on('close', () => {
      if (proc.exitCode === 0) {
        resolve(result);
      } else {
        reject(new Error("Exit code " + proc.exitCode));
      }
    });
  });
}

function mkDateRange(...rawDates) {
  // ugh i just hate writing this code, there are much better abstractions we could be using
  const dates = Array.from(new Set(rawDates.map(raw => {
    const d = new Date(raw);
    return `${
      String(d.getFullYear()).padStart(4, "0")
    }/${
      String(d.getMonth()+1).padStart(2, "0")
    }/${
      String(d.getDate()).padStart(2, "0")
    }`;
  })));
  dates.sort();
  if (dates.length === 0) return "";
  if (dates.length === 1) return dates[0];
  return `${dates.at(0)} – ${dates.at(-1)}`;
}

const template = ({markdown, link, date}) => `
---
title: Thread
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: ${date}
---

:::{.centered}
*Originally posted at <${link}>*
:::

${markdown}
`.trim();

async function main() {
  // The first argument is link(s), the rest are filenames of archives (the latest is taken)
  // If the first argument is empty, it will pull out all of the threads
  let [ links, ...archives ] = process.argv.slice(2);
  links = links ? links.split(/\s+/g).map(s => s.trim()) : [];
  const defaults_json = `cat pandoc/defaults.yaml | yaml2cbor.rb | cbor2json.rb`;
  const defaults = JSON.parse(await piped(spawn(defaults_json, {
    shell: 'bash',
  })));
  const format = defaults['from'];//.replace('markdown', 'markdown_github');
  // console.log(format);

  let latest = ""; let archive = "";
  for (let file of archives) {
    let timestamp = /archive-(\d{14})/.exec(file)[1];
    if (timestamp > latest) {
      latest = timestamp;
      archive = file;
    }
  }

  const outbox = JSON.parse(await piped(spawn('unzip', ['-p', archive, 'outbox.json'])));
  const posts = outbox.orderedItems.filter(p=>p.type==='Create').map(p=>p.object).filter(b=>!!b);

  function downstream(link) {
    let start;
    function matches(...urls) {
      if (urls.includes(link)) return true;
      for (let url of urls) {
        var r = /\d+/.exec(url);
        if (r && r[0] === link) return true;
      }
    }
    for (let post of posts) {
      if (matches(post.url, post.id)) start = post;
    }
    if (!start) {
      console.error(new Error("Post not found: "+link));
      return;
    }
    const conversation = start.conversation;
    // console.log(conversation);
    const sameThread = Array.from(posts.filter(p=>p.conversation===conversation));
    // console.log(sameThread.length);
    let leafs = [start.id];
    const thread = [start];
    while (leafs.length) {
      const parents = leafs;
      leafs = [];
      for (let post of sameThread) {
        if (parents.includes(post.inReplyTo)) {
          thread.push(post);
          leafs.push(post.id);
        }
      }
    }
    // console.log(thread.length);
    return thread;
  }

  let threads;
  if (!links.length || links.length === 1 && Number(links[0]) < 100) {
    const search_len = Number(links[0]) || 8;
    const freqs = {};
    for (let post of posts) {
      if (!freqs[post.conversation]) freqs[post.conversation] = [];
      freqs[post.conversation].push(post);
    }
    const filtered = {};
    for (const conv in freqs) {
      if (freqs[conv].length < search_len || !conv.startsWith("tag:tech.lgbt,")) continue;
      const thread = downstream(freqs[conv][0].id);
      if (thread.length < search_len) continue;
      filtered[conv] = thread;
    }
    // console.log(Object.fromEntries(Object.entries(filtered).map(([x,y])=>[x,y.length])));
    threads = Object.values(filtered);
  } else {
    threads = links.map(downstream);
  }

  for (let thread of threads) {
    const linkfmts =
      [
        [ '', /target="_blank" rel="nofollow noopener noreferrer"/g ],
        [ '<a href="$1">$2$3$4</a>', /<a href="([^"]*)"[^<>]*><span[^<>]*>([^<>]+)<\/span><span[^<>]*>([^<>]*)<\/span><span[^<>]*>([^<>]*)<\/span><\/a>/g ],
        [ '<a href="$1">@$2</a>', /<span[^<>]*><a href="([^"]*)"[^<>]*>@<span[^<>]*>([^<>]*)<\/span><\/a><\/span>/g ],
      ];

    let html = thread.map(post => {
      let content = post.content;
      if (post.summary?.trim()) {
        content = `<details class="Warning">\n<summary>${post.summary}</summary>\n${content}\n</details>`;
      }
      return content;
    }).join("\n<hr/>\n");
    for (let [rpl,fmt] of linkfmts) html = html.replace(fmt,rpl);

    let markdown = await piped(spawn('pandoc', [
      '-r', 'html+raw_html', '-w', format+'+raw_html-raw_attribute', '--lua-filter=./pandoc/lua/to-code-block.lua'
    ]), html);
    markdown = markdown.replace(/<summary>\n\n([^\n]+)\n\n<\/summary>/g, '<summary>$1</summary>');
    const date = mkDateRange(...thread.map(post => post.published));
    if (threads.length === 1) {
      console.log(template({ markdown, link: thread[0].url, date }));
    } else {
      console.log(`# ${thread[0].url} (${thread.length})\n  <p class="dated">${date}</p>\n${('\n'+markdown+'\n').replace(/\n(?=[^\n])/g,"\n  ")}`);
    }
  }
}

main();
