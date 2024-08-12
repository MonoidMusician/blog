function hex(text) {
    var result = "";
    var tsplit = text.split('');
    for (var pos in tsplit) {
        result += tsplit[pos].charCodeAt(0).toString(16);
    }
    return result;
}

function unhex(text) {
    last = text;
    text = text.replace(/[^0-9a-fA-F]/, '', 'gm');
    while (text = text.replace(/[^0-9a-fA-F]/, '', 'gm'))
    {
        if (last != text) {
            last = text;
        } else {
            break;
        }
    }
    if (text.length % 2 != 0) {
        return "";
    } else {
        var result = "";
        var stext = text.split('');
        for (var i = 0; i < text.length; i += 2) {
            result += String.fromCharCode(parseInt("0x" + stext[i] + '' + stext[i+1]));
        }
        return result;
    }
}

var getTextNodesIn = function(el) {
    //console.log($(el));
    return $(el).find(":not(iframe)").addBack().contents().filter(function() {
        //console.log(this);
        return this.nodeType == 3;
    });
};
function getTextNodesIn_(node, includeWhitespaceNodes, includeHidden) {
    var textNodes = [], nonWhitespaceMatcher = /\S/;

    function getTextNodes(node) {
        if (node.nodeType === 3) {
            if (includeWhitespaceNodes || nonWhitespaceMatcher.test(node.nodeValue)) {
                textNodes.push(node);
            }
        } else {
            if (node.nodeName === "SCRIPT") return;
            if (!includeHidden && !$(node).is(':visible')) return;
            for (var i = 0, len = node.childNodes.length; i < len; ++i) {
                getTextNodes(node.childNodes[i]);
            }
        }
    }

    getTextNodes(node);
    return textNodes;
};
function getTextValue(node) {
    return $.map(getTextNodesIn_($(node)[0], true, true), function(a){return a.textContent}).join("").trim().replace(/\s+/g, " ");
};
var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
};

function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
    });
}
