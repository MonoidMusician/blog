"use strict";
window.OP = function(sb, s, sa) {
	this.space_before = sb;
	this.space_after  = sa;
	this.text = s;
};
OP.prototype.toString = function() {return this.text;};
window.ISOP = function(o) {
	return o instanceof OP;
}
window.OP_post = function(s) {return new OP(false, s, true );};
window.OP_pre  = function(s) {return new OP(true,  s, false);};
window.OP_none = function(s) {return new OP(false, s, false);};
window.OP_both = function(s) {return new OP(true,  s, true );};

OP.PERIOD = OP_post(".");
OP.COMMA  = OP_post(",");
OP.COLON  = OP_post(":");
OP.APOS   = OP_post("’");
OP.LPAREN = OP_pre ("(");
OP.RPAREN = OP_post(")");
OP.LQUOTE = OP_pre ("“");
OP.RQUOTE = OP_post("”");
OP.DASH   = OP_none("—"); // m-dash
OP.MDASH  = OP_none("—"); // m-dash
OP.NDASH  = OP_both("–"); // n-dash
OP.QUEST  = OP_post("?");
OP.EXCL   = OP_post("!");
OP.PARAGRAPH = OP_none("<br>");
OP.MATCHING_CHOICES = OP_none("[matching-choices]");
OP.MULTIPLE_CHOICE  = OP_none("[multiple-choice]");
OP.MATCHING   = OP_none("[matching]");
OP.USER_INPUT = OP_none("[user-input]");
OP.USER_PARAGRAPH = OP_none("[user-paragraph]");

function lexify_punctuation(str) {
	var OPS = [
		"PERIOD","COMMA","COLON","APOS",
		"QUEST","EXCL","MDASH","NDASH",
		"LPAREN","RPAREN","LQUOTE","RQUOTE",
	].map(function(a) {return OP[a];});
	var result = [""], i = 0, lens = str.length;
	while (i < lens) {
		let match;
		for (let op of OPS) {
			if (str.startsWith(op.text,i))
			{ match = op; break; }
		}
		if (match) {
			result.push(match);
			i += match.text.length;
		} else {
			if (typeof result[result.length-1] !== "string")
				result.push("");
			result[result.length-1] += str[i];
			i += 1;
		}
	}
	for (let i in result) {
		if (typeof result[i] === "string")
			result[i] = result[i].trim();
	}
	result = result.filter(function(a) {return a;});
	return result;
}

// N.B. Assumes it receives a practically complete sentence
function simplify_punctuation(ops) {
	if (!ops) return ops;
	// Remove punctuation that doesn't belong at start of a sentence
	while ([
		OP.COMMA, OP.PERIOD, OP.COLON, OP.APOS,
		OP.QUEST, OP.EXCL
	].includes(ops[0])) ops.shift();
	var precedence = [
		[OP.QUEST,OP.EXCL,OP.MDASH,OP.NDASH],
		[OP.PERIOD],
		[OP.COMMA]
	];
	var get_precedence = function(o) {
		var prec;
		for (let p in precedence) {
			for (let op of precedence[p]) {
				if (o === op) {
					prec=1+precedence.length-p; break;
				}
			}
			if (prec) break;
		}
		return prec;
	};
	// Question marks, etc., can override periods, which can override commas.
	// Also remove straight duplicates, keeping the first one.
	var i = 0;
	while (i < ops.length-1) {
		let op = ops[i], prec = get_precedence(op);
		if (!prec)
		{ i+=1;continue; }
		let j = i+1;
		while (j < ops.length) {
			if (!ISOP(ops[j])) break;
			let o = ops[j];
			let p = get_precedence(o);
			if (!p) {
				j+=1;
			} else if (p < prec) {
				ops.splice(j, 1);
			} else if (p > prec || op === o) {
				ops.splice(i, 1);
				i = j-1;
			} else j += 1;
		}
		i = j;
	}
	// Follow American quotation rules for commas and periods
	i = 0;
	while (i < ops.length-1) {
		let op = ops[i];
		if (![OP.LQUOTE,OP.RQUOTE].includes(op))
		{ i+=1;continue; }
		let j = i+1, o = ops[j];
		if ([OP.PERIOD,OP.COMMA].includes(o)) {
			ops[i] = o;
			ops[j] = op;
			i = j+1;
		} else i += 1;
	}
	// Obliterate close quotes followed by open quotes
	i = 0;
	while (i < ops.length-1) {
		let op = ops[i];
		if (op !== OP.RQUOTE)
		{ i+=1;continue; }
		j = i+1;
		while (j < ops.length) {
			o = ops[j];
			if (!ISOP(o)) break;
			if (o !== OP.LQUOTE)
			{ j+=1; continue; }
			ops.splice(i, j-i+1);
			break;
		}
		i = j;
	}
	return ops;
}
function serialize_sentence_part(vector,allow_space) {
	var r = "";
	for (let w of vector) {
		let n = ISOP(w) ? w : OP_both(w); // OP_both is most permissive ...
		if (n.space_before && allow_space) r += " ";
		allow_space = n.space_after;
		r += n.text.trim().replace(/\s+/g, ' ');
	}
	return r;
}
function normalize_punctuation(str) {
	var ops = lexify_punctuation(str);
	ops = simplify_punctuation(ops);
	if (ops && ops[ops.length-1] === OP.COMMA)
		ops[ops.length-1] = OP.PERIOD;
	return serialize_sentence_part(ops);
}

