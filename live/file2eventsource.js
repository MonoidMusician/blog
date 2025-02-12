const express = require("express");
const bodyParser = require('body-parser');
const fs = require("node:fs/promises");
const watch = require("node-watch");
const https = require("node:https");

let app = express();

app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());

async function resolve(path) {
  for (let resolved of ['static/' + path, __dirname + '/' + path]) {
    try {
      let stat;
      if (stat = await fs.stat(resolved)) {
        if (stat.isDirectory()) {
          resolved = resolved+'/index.html';
          if (stat = await fs.stat(resolved))
            return resolved;
        }
        return resolved;
      }
    } catch {
      console.log("Not", resolved);
    }
  }
};

app.get('*', async function(req, res, next) {
  if (!('live' in req.query)) return next();
  const path = await resolve(req.path.substring(1));
  if (!path) return res.sendStatus(404);
  res.send(await fs.readFile(path, 'utf-8') + `<script src="/selfwatch.js"></script>`);
});

app.get('*', async function(req, res, next) {
  if (!('watch' in req.query)) return next();
  const path = await resolve(req.path.substring(1));
  if (!path) return res.sendStatus(404);
  res.writeHead(200, {
    'Connection': 'keep-alive',
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache'
  });
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

app.use(express.static('./static/'));
app.use(express.static('./'));
app.use(express.static(__dirname + '/'));

var port = 5678;
fs.stat('./cert/localhost.key').then(v => !!v, () => false).then(async hasCert => {
  let server = app;
  if (hasCert) {
    server = https.createServer({
      key: await fs.readFile('./cert/localhost.key'),
      cert: await fs.readFile('./cert/localhost.crt'),
    }, app);
    server.on('tlsClientError', (tlsError, tlsSocket) => {
      if (tlsError.reason === 'http request') {
        tlsSocket._parent.write('HTTP/1.1 302 Found\n' +
                                `Location: https://localhost:${port}`);
      }
    });
  }
  server.listen(port, function() {
    console.log("Edit ./live/scribe.svg (e.g. in Inkscape)");
    console.log("and view the result live at http://localhost:"+port+"/scribe.html?live");
  });
});
