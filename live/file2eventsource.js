const express = require("express");
const bodyParser = require('body-parser');
const fs = require("node:fs/promises");
const watch = require("node-watch");

let app = express();

app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());

async function resolve(path) {
  for (let resolved of [path, __dirname + '/' + path]) {
    try {
      if (await fs.stat(resolved)) return resolved;
    } catch {}
  }
};

app.get('*', async function(req, res, next) {
  if (!('live' in req.query)) return next();
  const path = await resolve(req.path.substring(1));
  res.send(await fs.readFile(path, 'utf-8') + `<script src="/selfwatch.js"></script>`);
});

app.get('*', async function(req, res, next) {
  if (!('watch' in req.query)) return next();
  res.writeHead(200, {
    'Connection': 'keep-alive',
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache'
  });
  const path = await resolve(req.path.substring(1));
  let last = null;
  const push = msg => {
    const ev = JSON.stringify(msg);
    if (ev === last) return;
    last = ev;
    res.write('data: ' + ev + '\n\n');
  };
  push(await fs.readFile(path, 'utf-8'));
  const watching = watch(path, async function(evt, name) {
    if (evt === 'remove') {
      res.end();
    } else if (evt === 'update') {
      push(await fs.readFile(path, 'utf-8'));
      console.log(name);
      if (name !== path) throw new Error(name + " instead of " + path);
    } else {
      throw new Error("Unknown event " + evt);
    }
  });
  res.on('close', () => watching.close());
});

app.use(express.static('./'));
app.use(express.static(__dirname + '/'));

var port = 5678;
app.listen(port, function() {
  console.log("Edit ./live/scribe.svg (e.g. in Inkscape)");
  console.log("and view the result live at http://localhost:"+port+"/scribe.html?live");
});
