const fs = require("fs");
const parse5 = require('parse5');
const { createCanvas, registerFont } = require('canvas');
const canvas = require('../../assets/js/canvas.js');
const process2 = require("process");
const crypto = require("crypto");

var output_dir = process2.argv[2];
if (output_dir === '--') {
  output_dir = process2.argv[3];
}

registerFont('assets/fonts/KaTeX/KaTeX_Math-Italic.ttf', { family: "KaTeX Math", style: "italic" });
registerFont('assets/fonts/KaTeX/KaTeX_Math-BoldItalic.ttf', { family: "KaTeX Math", style: "italic", weight: "bold" });

const stdin = fs.readFileSync(0, "utf-8");
const document = parse5.parseFragment(stdin);

function visit(node) {
  if (node.tagName === 'canvas') {
    process(node);
  } else if (node.childNodes) {
    for (let child of node.childNodes) {
      visit(child);
    }
  }
}

function attrs(node) {
  var r = {};
  for (let { name, value } of node.attrs) {
    r[name] = value;
  }
  return r;
}

function process(node) {
  var a = attrs(node);
  if (!a['width'] || !a['height']) return;
  if (node.childNodes.length > 1) return;
  if (node.childNodes.length === 1) {
    if (node.childNodes[0].nodeName === '#text' && node.childNodes[0].value.trim() !== '') {
      return;
    }
  }
  if (a['data-graph']) {
    var image = "";
    if (output_dir) {
      var data = a['data-graph']+(+a['width'])+(+a['height']);
      var hash = crypto.createHash("sha1").update(data).digest("hex");
      var filename = output_dir + "/" + hash + ".png";
      var image = filename;
    }
    if (!output_dir || !fs.existsSync(filename)) {
      var simulated = createCanvas(+a.width, +a.height);
      canvas.justDrawPixelGraph(simulated, '#000', canvas.graphsFromAttr(a['data-graph']));
      if (output_dir) {
        fs.writeFileSync(filename, simulated.toBuffer());
        var image = filename;
      } else {
        var image = simulated.toDataURL();
      }
    }
    var noscript = {
      nodeName: 'noscript',
      tagName: 'noscript',
      namespaceURI: node.namespaceURI,
      attrs: [],
      parentNode: node,
      childNodes: [],
    };
    var img = {
      nodeName: 'img',
      tagName: 'img',
      namespaceURI: node.namespaceURI,
      attrs: [
        { name: 'src', value: image },
        { name: 'style', value: "width: 100%" },
      ],
      childNodes: [],
      parentNode: noscript,
    };
    noscript.childNodes.push(img);
    node.childNodes = [noscript];
  }
}

visit(document);

fs.writeFileSync(1, parse5.serialize(document), "utf-8")
