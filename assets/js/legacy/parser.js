"use strict";
window.swap_escaping = function(s) {
	s = s.split('\\\\');
	for (let r of ['{','}','(',')','[',']','|'])
		s = s.map(function(a) {
			var l = '\\'+r;
			return a.split(l).map(function(b){return b.split(r).join(l)}).join(r);
		});
	return s.join('\\\\');
};
window.parser = (function() {
	// Ugly hack around no lookbehind support: capture a single non backslash character
	var meta = /(^|[^\\])(\\\\)*(\\)/.source;
	var brace = new RegExp(meta+"\\{", 'g'),
		ecarb = new RegExp(meta+"\\}", 'g'),
		paren = new RegExp(meta+"\\(", 'g'),
		nerap = new RegExp(meta+"\\)", 'g'),
		brckt = new RegExp(meta+"\\[", 'g'),
		tkcrb = new RegExp(meta+"\\]", 'g');
	var regexes = [brace,paren,brckt];

	function search2(s, regex, inclusive) {
		if (regex.lastIndex > 0) regex.lastIndex -= 1; // lookbehind hack
		var r = regex.exec(s);
		if (r === null)
			return s.length;
		var off = (r[1]?r[1].length:0); // continue lookbehind hack
		if (inclusive)
			return r.index + r[0].length;
		return r.index + off + (r[2]?r[2].length:0);
	};
	function getfirst(s, regexes, startAt) {
		var t = null, i = s.length;
		for (let type in regexes) {
			let r = regexes[type];
			r.lastIndex = startAt;
			var j = search2(s, r, false);
			if (j < i) {
				t = type; i = j;
			}
		}
		if (t == null)
			return null;
		return [t,i];
	}
	function level_content(s, regex, xeger, startAt) {
		if (startAt === undefined) startAt = 0;
		var L, l, R, r,
			next = function(p) {
				if (typeof p === 'number') regex.lastIndex = p;
				var r = search2(s, regex, true);
				xeger.lastIndex = regex.lastIndex;
				return r;
			},
			txen = function(p) {
				if (typeof p === 'number') xeger.lastIndex = p;
				var r = search2(s, xeger, false);
				regex.lastIndex = xeger.lastIndex;
				return r;
			},
			// like txen, but advances the position past the end of the current match first
			TXEN = function(p) {
				if (typeof p === 'number') xeger.lastIndex = p;
				var r = search2(s, xeger, true);
				regex.lastIndex = xeger.lastIndex;
				if (r == s.length) return r;
				r = search2(s, xeger, false);
				regex.lastIndex = xeger.lastIndex;
				return r;
			};
		L = next(startAt);
		l = next(L);
		R = txen(L);
		while (l <= R && R < s.length) {
			R = TXEN(R);
			l = next(l);
		}
		return s.substr(L, R-L);
	}
	function* split1(s) {
		var l = 0, r = '', m = false;
		for (let c of s.split('')) {
			if (c === '\\' && !m) {
				m = true;
				continue;
			} else
			if (m) {
				m = false;
				if (c == '|' && l == 0) {
					yield r;
					r = '';
					continue;
				} else
				if (c != '\\') {
					switch(c) {
						case '{':
						case '(':
						case '[':
							l += 1;
							break;
						case '}':
						case ')':
						case ']':
							l -= 1;
							break;
					}
				}
				c = '\\'+c;
			}
			r += c;
		}
		yield r;
	}
	function parser(s) {
		this.s = s;
		this.udata = {};
	}
	parser.prototype.inner_part = function(s) {
		this.PUSH();
		var pos = 0;
		var m = getfirst(s, regexes, pos), c;
		while (m != null) {
			var type = m[0]; var jump = m[1] - pos;
			if (jump) this.ADD(this.TEXT(s.substr(pos, jump)));
			if (type == 0) {
				c = level_content(s, brace, ecarb, pos);
				this.ADD(this.BRACE(this.inner(c)));
			} else if (type == 1) {
				c = level_content(s, paren, nerap, pos);
				this.ADD(this.PAREN(this.inner(c)));
			} else if (type == 2) {
				c = level_content(s, brckt, tkcrb, pos);
				this.ADD(this.BRCKT(this.inner(c)));
			} else c = " ";
			pos += jump + c.length + 4; // 4 = "\\{\\}".length;
			m = getfirst(s, regexes, pos);
		}
		if (pos < s.length) this.ADD(this.TEXT(s.substr(pos)));
		return this.POP();
	}
	parser.prototype.inner = function(s) {
		var my = this;
		return this.CHOICES([...(function*() {
			for (let S of split1(s)) {
				yield my.inner_part(S);
			}
		})()]);
	}
	parser.prototype.parse = function() {
		return this.inner(this.s);
	}
	return parser;
})();

function reconstruct(s) {
	var p = new parser(s);
	p.udata.result = [''];
	p.PUSH = function(){this.udata.result.push('')};
	p.POP = function(){return this.udata.result.pop()};
	p.ADD = function(a){this.udata.result[this.udata.result.length-1] += a};
	p.TEXT = function(a){return a};
	p.BRACE = function(a){return '\\{'+a+'\\}'};
	p.PAREN = function(a){return '\\('+a+'\\)'};
	p.BRCKT = function(a){return '\\['+a+'\\]'};
	p.CHOICES = function(a){return a.join('\\|');};
	return p.parse();
}

function tree(s) {
	var p = new parser(s);
	var simplify = function(a){return (!Array.isArray(a) || a.length > 1) ? a : a[0]};
	p.udata.result = [];
	p.PUSH = function(){this.udata.result.push([])};
	p.POP = function(){return simplify(this.udata.result.pop())};
	p.ADD = function(a){this.udata.result[this.udata.result.length-1].push(a)};
	p.TEXT = function(a){return a};
	p.BRACE = function(a){return {type:'brace',data:simplify(a)}};
	p.PAREN = function(a){return {type:'paren',data:simplify(a)}};
	p.BRCKT = function(a){return {type:'brckt',data:simplify(a)}};
	p.CHOICES = function(a){return a.length>1?{type:'choices',data:a}:a};
	return p.parse();
}

function permute(s) {
	var t = tree(s);
	if (typeof t === 'string') return [t];
	var capital = function(d) {
		if (!d) return false;
		if (typeof d === "string") return d[0] === "*";
		return capital(d[0]);
	}, decapital = function(d) {
		if (typeof d === "string") return d.slice(1);
		if (Array.isArray(d)) d[0] = decapital(d[0]);
		return d;
	};
	var permutate = function(t) {
		if (typeof t === 'string') return t;
		if (!Array.isArray(t)) {
			switch (t.type) {
				case 'brckt':
					if (typeof t.data === 'string')
						return new Permutator.Combo([t.data, '']);
					return new Permutator.Combo([permutate(t.data),'']);
				case 'paren':
				case 'brace':
					return permutate(t.data);
				case 'choices':
					return new (Permutator.Combo.bind.apply(Permutator.Combo, [null].concat([t.data.map(permutate)])))();
			}
			console.log(t);
		}
		var M = new Permutator.Mixed();
		var _permutate = function(r) {
			var n = 0;
			for (let m of r) {
				if (typeof m !== 'object') continue;
				if (m.type === 'brace') n += 1;
				if (n > 1) break;
			}
			for (let m of r) {
				if (typeof m === 'string')
					M.post(m);
				else if (Array.isArray(m))
					_permutate(m);
				else if (typeof m !== 'object') continue;
				else if (m.type === 'brace' && n > 1)
					if (capital(m.data))
						M.post_Permute(permutate(decapital(m.data)));
					else M.post_permute(permutate(m.data));
				else
					M.post(permutate(m));
			}
		}; _permutate(t);
		M = M.simplify();
		return M;
	};
	return permutate(t);
}

function match(s, m, d, simplify) {
	var D = d+1, M = null;
	if (simplify) m = simplify(m);
	for (let match of permute(s)) {
		let distance = damerau_levenshtein(simplify?simplify(match):match, m, Infinity).steps;
		if (distance < D) {
			M = match;
			D = distance;
		}
	}
	return M;
}
