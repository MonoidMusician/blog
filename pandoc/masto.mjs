#!/usr/bin/env node

// watchexec -c -w pandoc/masto.mjs --shell=fish -- "node pandoc/masto.mjs (cat thread_url) ~/Downloads/archive-*.zip | sponge sources/infodump.md"

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

async function main() {
  const [ link, ...archives ] = process.argv.slice(2);
  const defaults_json = `cat pandoc/defaults.yaml | yaml2cbor.rb | cbor2json.rb`;
  const defaults = JSON.parse(await piped(spawn(defaults_json, {
    shell: 'bash',
  })));
  const format = defaults['from'];//.replace('markdown', 'markdown_github');
  // console.log(format);

  let mtime = 0; let archive = "";
  for (let file of archives) {
    let stat = await fs.stat(file);
    if (stat.mtime > mtime) {
      mtime = stat.mtime;
      archive = file;
    }
  }
  // console.log(archive);

  const outbox = JSON.parse(await piped(spawn('unzip', ['-p', archive, 'outbox.json'])));
  const posts = outbox.orderedItems.filter(p=>p.type==='Create').map(p=>p.object).filter(b=>!!b);

  let start;
  for (let post of posts) {
    if (post.url === link) start = post;
  }
  if (!start) throw new Error("Post not found: "+link);
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

  const linkfmts =
    [
      [ '', /target="_blank" rel="nofollow noopener noreferrer"/g ],
      [ '<a href="$1">$2$3$4</a>', /<a href="([^"]*)"[^<>]*><span[^<>]*>([^<>]+)<\/span><span[^<>]*>([^<>]*)<\/span><span[^<>]*>([^<>]*)<\/span><\/a>/g ],
    ];

  let html = thread.map(post => post.content).join("\n<hr/>\n")
  for (let [rpl,fmt] of linkfmts) html = html.replace(fmt,rpl);

  const markdown = await piped(spawn('pandoc', [
    '-r', 'html', '-w', format, '--lua-filter=./pandoc/lua/to-code-block.lua'
  ]), html);
  console.log(`
---
title: Thread
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

:::{.centered}
*Originally posted at <${link}>*
:::

${markdown}
`.trim());
}

main();
