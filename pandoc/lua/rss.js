const fs = require("fs");
const parse5 = require('parse5');
const process2 = require("process");
const crypto = require("crypto");

var base = process2.argv[2];

const stdin = fs.readFileSync(0, "utf-8");

function replaceCDATAs(input) {
  var r = [];
  var last = -1;
  var i = 0;
  var j = 0;
  while ((i = input.indexOf("<![CDATA[", last+1)) !== -1) {
    r.push(input.substring(last, i));
    j = input.indexOf("]]>", i);
    var part = input.substring(i + "<![CDATA[".length, j);
    r.push("<![CDATA[" + replacer(part) + "]]>");
    last = j + "]]>".length;
  }
  r.push(input.substring(last, input.length));
  return r.join("");
}

function replacer(input) {
  const document = parse5.parseFragment(input);

  function visit(node) {
    process(node);
    if (node.childNodes) {
      for (let child of node.childNodes) {
        visit(child);
      }
    }
  }

  function fix(link) {
    if (!link.includes("://") && link[0] !== "#") {
      return base + "/" + link;
    }
    return link;
  }

  function process(node) {
    if (node.tagName === 'a') {
      for (let attr of node.attrs) {
        if (attr.name === 'href') {
          attr.value = fix(attr.value);
        }
      }
    }
    if (node.tagName === 'img') {
      for (let attr of node.attrs) {
        if (attr.name === 'src') {
          attr.value = fix(attr.value);
        }
      }
    }
  }

  visit(document);

  return parse5.serialize(document);
}

fs.writeFileSync(1, replaceCDATAs(stdin), "utf-8");

