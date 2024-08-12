var la_ipa = (function () {
	var my = {};
	my.constructor = arguments.callee;

	my.mix = function () {
		var arg = arguments;
		return function(r) {
			for (i in arg) {
				r = arg[i](r);
			}
			return r;
		};
	};
	my.replasor = function (from,to) {
		return function(r) {return r.split(from).join(to)}
	};
	my.upper = function (r) {
		return r.toUpperCase();
	};
	my.lower = function (r) {
		return r.toLowerCase();
	};
	my.nspace = function (r) {
		return r.replace(/^\s*|\s(?=\s)|\s*$/g, "");
	};
	my.ASCIIize = function (r) {
		return my.ae_oe(my.sonusmedius(r)).split("").map(function(a) {return a.match(/[^\u0000-\u007f]/)?"":a}).join("");
	};
	my.sonusmedius = function (r) {
		return r.split("ⱶ").join("i");
	};
	my.ae_oe = function(r) {
		return (
			r.split("\u00E6").join("ae")
			 .split("\u0153").join("oe")
			 .split("\u00C6").join("Ae")
			 .split("\u0152").join("Oe")
		);
	};
	my.deASCIIize = function (r) {
		return (
			r.split("ß").join("ss")
			 .replace(/ae(?![e\u0308\u0304])/g, "æ")
			 .replace(/oe(?![e\u0308\u0304])/g, "œ")
			 .replace(/æ([\u0308\u0304e])/g, "ae$1")
			 .replace(/œ([\u0308\u0304e])/g, "oe$1")
			 .replace(/([aeiouy])\1(?!\u0304)/g, "$1\u0304")
		);
	};
	my.double_vowels = function (r) {
		return r.split("\u0304").map(function(a,i,l) {return i<l.length-1?a+a[a.length-1]:a;}).join("");
	};
	my.undouble_vowels = function (r) {
		return r.replace(/([aeiouy])\1(?!\u0304)/g, "$1\u0304");
	};
	my.LA = function (r) {
		return r.split("i\u0304").map(my.ASCIIize).join("\uA7FE").split("U").join("V").split("J").join("I");
	};
	my.LA_apex = function (r) {
		var map = function(r){return r.split("\u0304").map(my.ASCIIize).join("\u0301")};
		return r.split("i\u0304").map(map).join("\uA7FE").split("U").join("V").split("J").join("I");
	};
	my.old_latin = function (r) {
		return r.replace(/[uv](?=[sm])/gi,"o").replace(/([ao])e/gi,"$1i");
	};
	my.short_vowels = function (r) {
		return r.replace(/([aeiouy])\u0301?(?!\u0304)/g, "$1\u0306").replace(/([qg]u)\u0306|([aeiou])\u0306([eiu])\u0306|([aeiou])\u0306(m)(?![\w])/g,"$1$2$3$4$5");
	};
	my.apex = function (r) {
		return r.split("\u0304").join("\u0301");
	};
	my.orthography_j = function (r) {
		return r.replace(/jj?i/g, "i").replace(/jj/g, "j").replace(/JJ?I/g, "I").replace(/JJ/g, "J");
	};
	my.no_j = function (r) {
		return r.replace(/j/g, "i").replace(/ii+/g, "i").replace(/J/g, "I").replace(/I[Ii]+/g, "I");
	};
	my.nasal = function (r) {
		return r.replace(/([aeiouy])m(?![a-zA-Z])/g,"$1\u0303");
	};
	my.eszett = function (r) {
		return r.split("ss").join("ß");
	};
	my.silicus = function (r) {
		return r.replace(/([bcdfghj-np-tvwxz])\1/g,"$1\u0357");
	};
	function dbl_vwls_hlpr(r) {
		var l = r.length;
		var ret = r[0];
		for (var i=l; i>1; i-=2) {
			ret += "\u02D0";
		}
		if (i) ret += "\u02D1";
		return ret;
	};
	my.IPA_longa = function (r) {
		return r.split("\u0304").join("\u02D0").replace(/([bcdfghj-np-tvwxz])\1+/g,dbl_vwls_hlpr).split("\u0306").join("");
	};
	my.IPA_accent = function (r) {
		return r.replace(/([aeiouy])\u0301/g, "\u02C8$1");
	};
	my.IPA_transcr = function (r) {
		// Note: assumes orthography uses i/j and u/v correctly
		return (
			r
			// Normalize
			.split("x").join("ks")
			.split("c").join("k")
			.split("v").join("w")
			// Assimilations
			.split("bs").join("ps")
			// Convert sequences to IPA
				// er allophone
				/*.replace(/e(?=[.]?r)/g,"\u00E6")
				.replace(/([ao])\u00E6/g, "$1e")/**/
				// Rhotics
				.replace(/r(?![.]?[r\u02D0])/g,"\u027E")
				.replace(/r[.]\u027E/g, "r.r")
				.replace(/([tdbpfvnm][\u02D0]?)\u027E/g, "$1r")
				// Double articulations
				.replace(/(?=mn|pt|pk|bd|bg)(.)(.)/g,"$1\u0361$2")
				// Digraphs/dipthongs
				.split("qu").join("k\u02B7")
				.split("ngu").join("ng\u02B7")
				.replace(/([ao]e|[aeo]u|[eu]i)(?![aeiou]?[\u02D0\u032F])/g,"$1\u032F")
				// Aspiration
				.replace(/([tkp])h/g, "$1\u02B0")
				// Velar nasal
				.replace(/g([.]?n)|n([.]?[gk])/g,"\u014B$1$2")
				// Nasalized vowels
				.replace(/([aeiou])(n(?=[szfv])|m((?![.]|(?=[.][.])|k\u02B7e|ve)|(?![\w.])))/g,"$1\u0303\u02D0")
				//.replace(/([aeiou])(n(?=[szfv])|m((?![.]|(?=[.][.])|k\u02B7e|ve)|(?![\w.])))/g,"$1\u0303\u02D0$2\u0325")
				//.replace(/\u02B7(?=[ei\u025B\u026A])/g,"\u1DA3")
				// Short vowels
				.replace(/\u2C76(?=[mbpf])/g, "\u0289") // sonus medius rounded/labialized
				.replace(/\u2C76/g, "\u0268") // sonus medius
				.replace(/i(?![aeiou]|[\u02D0\u032F\u02D0])/g, "\u026A") // note: all vowels here, not just dipthongs
				.replace(/e(?![eiu]\u032F|[\u0303\u032F\u02D0])/g, "\u025B")
				.replace(/o(?![eiu]\u032F|[\u0303\u032F\u02D0])/g, "\u0254")
				.replace(/u(?![eiu]\u032F|[\u0303\u032F\u02D0])/g, "\u028A")
				// Double period means ignore the adjancency, but has no IPA value
				.replace(/[.][.]+/g, ".")
		);
	};
	my.tengwar_map = (function() {
		var c = String.fromCodePoint || String.fromCharCode, i = 0xE000-1;
		return {
			tinco: c(i+=1),
			parma: c(i+=1),
			calma: c(i+=1),
			quesse: c(i+=1),
			ando: c(i+=1),
			umbar: c(i+=1),
			anga: c(i+=1),
			ungwe: c(i+=1),
			thuule: c(i+=1),
			formen: c(i+=1),
			harma: c(i+=1),
			hwesta: c(i+=1),
			anto: c(i+=1),
			ampa: c(i+=1),
			anca: c(i+=1),
			unque: c(i+=1),
			nuumen: c(i+=1),
			malta: c(i+=1),
			ngoldo: c(i+=1),
			nwalme: c(i+=1),
			oore: c(i+=1),
			vala: c(i+=1),
			anna: c(i+=1),
			vilya: c(i+=1),
			Tinco: c(i+=1),
			Parma: c(i+=1),
			Calma: c(i+=1),
			Quesse: c(i+=1),
			Ando: c(i+=1),
			Umbar: c(i+=1),
			Anga: c(i+=1),
			Ungue: c(i+=1),
			roomen: c(i+=1),
			arda: c(i+=1),
			lambe: c(i+=1),
			alda: c(i+=1),
			silme: c(i+=1),
			Silme: c(i+=1),
			aare: c(i+=1),
			Aare: c(i+=1),
			hyarmen: c(i+=1),
			Hwesta: c(i+=1),
			yanta: c(i+=1),
			uure: c(i+=1),
			__: c(i+=1),
			halla: c(i+=1),
			_: c(i+=1),
			a: "\uE040",
			A: "\uE055",
			a_: "\uE032",
			e: "\uE046",
			E: "\uE046\uE046",
			e_: "\uE02A",
			i: "\uE044",
			I: "\uE042",
			i_: "\uE02E",
			o: "\uE04A",
			O: "\uE04A\uE04A",
			o_: "\uE016",
			u: "\uE04C",
			U: "\uE04C\uE04C",
			u_: "\uE02B",
			j: "\uE02A\uE045",
			nasalize: "\uE050",
			geminate: "\uE051",
			andaith: "\u0301", // XXX: \u0301 ?
			",": " \u2E31",
			".": " :",
			":": " \u205D",
			"!": " \uE065",
			"?": " \uE066",
			"\u2023": "\uE068",
			"(": "\uE067",
			")": "\uE067",
		};
	})();
	my.tengwar = function(r) {
		if (!$('#tengwar-font').length)
			$('head').append('<style id="tengwar-font">.format-word-la { font-family: Tengwar !important; font-weight: normal !important; }</style>');
		var t = my.tengwar_map;
		return (
			r.toLowerCase()
			.replace(/m\b/g, t.vala) // final m is weaker, hence vala for malta
			.replace(/n\b/g, t.oore) // final n is weaker, hence oore for nuumen
			.split("j").join(t.j)
			.replace(/([ao]e|[ae]u|[e]i)(?![\u0304\u0308])/g, function(a) {
				return t[a[0]+"_"]+t[a[1]];
			})
			.replace(/([aeiou])([aeiou])\u0304/g, function(_,a,b) {
				return a + t.__ + t[b];
			})
			.replace(/([aeiou])([aeiou])/g, function(_,a,b) {
				return a + t._ + t[b];
			})
			.replace(/\b([aeiou])\u0304/g, function(a,b) {
				return t.__ + t[b];
			})
			.replace(/\b([aeiou])/g, function(a,b) {
				return t._ + t[b];
			})
			.replace(/h([aeiou])\u0304/g, function(a,b) {
				return t.halla + t.__ + t[b];
			})
			.replace(/h([aeiou])/g, function(a,b) {
				return t.halla + t._ + t[b];
			})
			.replace(/([aeiou])\u0304/g, function(a,b) {
				return t[b.toUpperCase()];
			})
			.replace(/([aeiou])/g, function(a,b) {
				return t[b];
			})
			.replace(/[mn]([pbtdckg])/g, "$1"+t.nasalize)
			.split(t.j+t._).join(t.j)
			.split("q"+t.u+t.__).join(t.quesse+t.__)
			.split("g"+t.u+t.__).join(t.ungwe+t.__)
			.split("q"+t.nasalize+t.u+t.__).join(t.quesse+t.nasalize+t.__)
			.split("g"+t.nasalize+t.u+t.__).join(t.ungwe+t.nasalize+t.__)
			.split("q"+t.u+t._).join(t.quesse)
			.split("g"+t.u+t._).join(t.ungwe)
			.split("q"+t.nasalize+t.u+t._).join(t.quesse+t.nasalize)
			.split("g"+t.nasalize+t.u+t._).join(t.ungwe+t.nasalize)
			.split("q"+t.u).join(t.quesse)
			.split("g"+t.u).join(t.ungwe)
			.split("q"+t.nasalize+t.u).join(t.quesse+t.nasalize)
			.split("g"+t.nasalize+t.u).join(t.ungwe+t.nasalize)

			.replace(/([tdpbqgckfnmlr])\1/g, "$1"+t.geminate)
			.replace(/([tdpbqgck])r/g, "$1"+t.roomen)
			.replace(/([tdpbqgck])l/g, "$1"+t.lambe)

			.split("x").join("ks")
			.split("t").join(t.tinco)
			.split("d").join(t.ando)
			.split("p").join(t.parma)
			.split("b").join(t.umbar)
			.split("gn").join(t.ngoldo+t.nuumen)
			.split("c").join(t.calma)
			.split("k").join(t.calma)
			.split("g").join(t.anga)
			.split("f").join(t.formen)
			.split("ph").join(t.formen)
			.split("v").join(t.vilya)
			.split("n").join(t.nuumen)
			.split("m").join(t.malta)
			.split("l").join(t.alda)
			.split("r").join(t.arda)
			.split("ss").join(t.aare)
			.replace(new RegExp(t.aare+"(?="+[t.a,t.A,t.e,t.E,t.i,t.I,t.o,t.O,t.u,t.U].join("|")+")","g"), t.Aare)
			.split("s").join(t.silme)
			.replace(new RegExp(t.silme+"(?="+[t.a,t.A,t.e,t.E,t.i,t.I,t.o,t.O,t.u,t.U].join("|")+")","g"), t.Silme)

			.split("\u0308").join("")
			.split("\u2023").join(t["\u2023"])
			.split(":").join(t[":"])
			.split(",").join(t[","])
			.split(".").join(t["."])
			.split("!").join(t["!"])
			.split("?").join(t["?"])
			.split("(").join(t["("])
			.split(")").join(t[")"])
		);
	};
	my.tengwar_carriers = function(r) {
		var t = my.tengwar_map;
		return (
			r
			.split(t.A).join(t.__+t.a)
			.split(t.E).join(t.__+t.e)
			.split(t.I).join(t.__+t.i)
			.split(t.O).join(t.__+t.o)
			.split(t.U).join(t.__+t.u)
			.split(t.Aare+t.__).join(t.aare+t.__)
			.split(t.Silme+t.__).join(t.silme+t.__)
		);
	};
	my.tengwar_beleriand = function(r) {
		var t = my.tengwar_map;
		return (
			r
			.split(t._).join("")
			.split(t.Aare).join(t.aare)
			.split(t.Silme).join(t.silme)
			.split(t.A).join(t.a_+t.andaith)
			.split(t.__+t.a).join(t.a_+t.andaith)
			.split(t.E).join(t.e_+t.andaith)
			.split(t.__+t.e).join(t.e_+t.andaith)
			.split(t.I).join(t.i_+t.andaith)
			.split(t.__+t.i).join(t.i_+t.andaith)
			.split(t.O).join(t.o_+t.andaith)
			.split(t.__+t.o).join(t.o_+t.andaith)
			.split(t.U).join(t.u_+t.andaith)
			.split(t.__+t.u).join(t.u_+t.andaith)
			.split(t.a).join(t.a_)
			.split(t.e).join(t.e_)
			.split(t.i).join(t.i_)
			.split(t.o).join(t.o_)
			.split(t.u).join(t.u_)
			.replace(new RegExp("([^ ])"+t.geminate,"g"), "$1$1")
			.split(t.j).join(t.__)
			.split(t.andaith).join(t.e) // HACK
		);
	};
	my.IPA_eccl = function (r) {
		return (
			r
			// Normalization
			.split("y").join("i")
			.split("x").join("ks")
			// Rhotics
			.replace(/r(?![.]?[r\u02D0])/g,"\u027E")
			.replace(/r[.]\u027E/g, "r.r")
			.replace(/([tdbpfvnm])\u027E/g, "$1r")
			// Double articulations
			.replace(/(?=mn|pt|pk|bd|bg)(.)(.)/g,"$1\u0361$2")
			// Digraphs/dipthongs
			.split("qu").join("k\u02B7")
			.split("ngu").join("ng\u02B7")
			.replace(/([ao]e|[aeo]u|[eu]i)(?![aeiou]?[\u02D0\u032F])/g,"$1\u032F")
			.replace(/[ao]e\u032F/g, "e")
			// Velar nasal
			.replace(/g([.])?n/g,"$1\u0272")
			// Italian rules
			.replace(/(^|[^stx])ti(?=[aeou])/g, "$1t\u0361si")
			.replace(/z/g, "d\u0361z")
			.replace(/sc(?=[ei])/g, "\u0283")
			.replace(/g(?=[ei])/g, "d\u0361\u0292")
			.replace(/c(?:h(?=[ei])|(?![ei]))/g, "k")
			.replace(/c(?:i(?=[auo]))?/g, "t\u0361\u0283")
			.replace(/([aeiou])s(?=[aeiou])/g, "$1z")
			.split("gh").join("g")
			.split("w").join("v")
			// Double period means ignore the adjancency, but has no IPA value
			.replace(/[.][.]+/g, ".")
			.replace(/[.](?=\u02C8)/g, "")
		);
	};
	my.qv = function (r) {
		return r.split("qu").join("qv").split("Qu").join("Qv").split("QU").join("QV");
	};
	my.Llath = function (r) {
		return (
			r.split("i\u0304").join("j")
			 .split("I\u0304").join("J")
			 .split("u\u0304").join("w")
			 .split("U\u0304").join("W")
			 .split("v").join("w")
			 .split("V").join("W")
			 .split("ⱶ").join("ı") //sonus medius -> ih (i-dot)
			 .split("gn").join("ñ") //enya
			 .split("qu").join("ẇ") //wh (w-dot)
			 .split("Qu").join("Ẇ") //wh (w-dot)
			 .split("QU").join("Ẇ") //wh (w-dot)
			 .split("ph").join("ṗ") //ph (p-dot)
			 .split("Ph").join("Ṗ") //ph (p-dot)
			 .split("PH").join("Ṗ") //ph (p-dot)
			 .split("th").join("þ") //th (thorn)
			 .split("Th").join("Þ") //th (thorn)
			 .split("TH").join("Þ") //th (thorn)
			 .split("ts").join("ŝ") //s-hat
			 .split("TS").join("Ŝ") //s-hat
			 .split("Ts").join("Ŝ") //s-hat
			 .split("dz").join("ẑ") //z-hat
			 .split("Dz").join("Ẑ") //z-hat
			 .split("DZ").join("Ẑ") //z-hat
		);
	};
	my.Greek = function (r) {
		return (
			r.replace(/us(?=[^w])/,"os")
			 .replace(/um(?=[^w])/,"on")
			 .split("\u00E6").join("ai")
			 .split("\u0153").join("oi")
			 .split("\u00C6").join("Ai")
			 .split("\u0152").join("Oi")
			 .split("Qu").join("Κυ")
			 .split("QU").join("ΚΥ")
			 .split("ph").join("f")
			 .split("th").join("θ")
			 .split("ch").join("χ")
			 .split("ps").join("ψ")
			 .split("x").join("ξ")
			 .split("a").join("α")
			 .split("b").join("β")
			 .split("c").join("k")
			 .split("d").join("δ")
			 .split("e\u0304").join("η")
			 .split("e").join("ε")
			 .split("f").join("φ")
			 .split("g").join("γ")
			 .split("i").join("j")
			 .split("j").join("ι")
			 .split("k").join("κ")
			 .split("l").join("λ")
			 .split("m").join("μ")
			 .split("n").join("ν")
			 .split("o\u0304").join("ω")
			 .split("o").join("ο")
			 .split("p").join("π")
			 .split("qu").join("κυ")
			 .split("r").join("ρ")
			 .split("s").join("σ")
			 .split("t").join("τ")
			 .split("u").join("υ")
			 .split("v").join("υ")
			 .split("y").join("υ")
			 .split("z").join("ζ")
			 .split("PH").join("Ph")
			 .split("TH").join("Th")
			 .split("CH").join("Ch")
			 .split("PS").join("Ps")
			 .split("Ph").join("F")
			 .split("Th").join("Θ")
			 .split("Ch").join("Χ")
			 .split("Ps").join("Ψ")
			 .split("X").join("Ξ")
			 .split("A").join("Α")
			 .split("B").join("Β")
			 .split("C").join("Κ")
			 .split("D").join("Δ")
			 .split("E\u0304").join("Η")
			 .split("E").join("Ε")
			 .split("F").join("Φ")
			 .split("G").join("Γ")
			 .split("I").join("J")
			 .split("J").join("Ι")
			 .split("K").join("Κ")
			 .split("L").join("Λ")
			 .split("M").join("Μ")
			 .split("N").join("Ν")
			 .split("O\u0304").join("Ω")
			 .split("O").join("Ο")
			 .split("P").join("Π")
			 .split("R").join("Ρ")
			 .split("S").join("Σ")
			 .split("T").join("Τ")
			 .split("U").join("Υ")
			 .split("V").join("Υ")
			 .split("Y").join("Υ")
			 .split("Z").join("Ζ")
			 .split("\u0304").join("")
			 .replace(/(^|[^Α-Ωα-ω])h([αειουωη])/gi, "$1$2\u0314")
			 .replace(/(^|[^Α-Ωα-ω])([αειουωη])(?![\u0314])/gi, "$1$2\u0313")
			 .replace(/σ(?![Α-Ωα-ω])/g, "ς")
		);
	};
	// Adapted from: https://sim0n.wordpress.com/2009/03/28/javascript-char-code-to-unicode-fullwidth-latin/
	my.fullwidth = function (r) {
		var ret = "";
		for(i=0; i<r.length; i++) {
			if(r.charCodeAt(i) >= 33 && r.charCodeAt(i) <= 270) {
				ret += String.fromCharCode(r.charCodeAt(i) + 65248);
			} else if(r.charCodeAt(i) == 32) {
				ret += String.fromCharCode(12288);
			}
		}
		return ret;
	}
	my.transforms = {
		"Original": function(a){return a},
		"No diacritics": my.mix(my.sonusmedius, my.ASCIIize),
		"Default orthography": my.mix(my.ae_oe, my.sonusmedius, my.no_j),
		"Mark all vowels": my.mix(
			my.undouble_vowels,
			my.lower,
			my.short_vowels,
			my.orthography_j
		),
		"Silicus": my.mix(my.silicus, my.sonusmedius, my.orthography_j),
		"Nasal": my.mix(my.nasal, my.sonusmedius, my.orthography_j),
		"Silicus+Nasal": my.mix(my.nasal, my.silicus, my.sonusmedius, my.orthography_j),
		"Silicus+Eszett+Nasal": my.mix(my.nasal, my.eszett, my.silicus, my.orthography_j),
		"qv": my.qv,
		"fullwidth": my.mix(
			my.ASCIIize,
			my.fullwidth
		),
		"Greek": my.Greek,
		"Ļaþ": my.Llath,
		"Tengwar": my.mix(my.sonusmedius, my.ae_oe, my.orthography_j, my.tengwar, my.tengwar_carriers),
		"Tengwar (Alt.)": my.mix(my.sonusmedius, my.ae_oe, my.orthography_j, my.tengwar),
		"Tengwar Beleriand": my.mix(my.sonusmedius, my.ae_oe, my.orthography_j, my.tengwar, my.tengwar_beleriand),
		"Finnish": my.mix(
			my.double_vowels,
			my.replasor("c","k"),
			my.ASCIIize
		),
		"Latin inscription (upper-case)": my.mix(
			my.LA,
			my.upper,
			my.no_j
		),
		"Old Latin inscription (upper-case)": my.mix(
			my.LA,
			my.old_latin,
			my.upper,
			my.no_j
		),
		"Old Latin inscription (upper-case, apex)": my.mix(
			my.LA_apex,
			my.old_latin,
			my.upper,
			my.no_j
		),
		"IPA transcription": my.mix(
			my.undouble_vowels,
			my.lower,
			my.IPA_longa,
			my.IPA_accent,
			my.IPA_transcr
		),
	};
	my.transform_key = my.old_transform = null;
	my.select_transformer = function(nomen) {
		$('#tengwar-font').remove();
		my.transform_key = nomen;
		my.transform = my.transforms[nomen];
		return my;
	}
	my.set_selector = function(selector) {
		my.selector = selector;
		return my;
	}
	my.set_lang = function(lang) {
		my.selector = '.format-word-'+lang;
		return my;
	}
	my.select_transformer("Default orthography");
	//my.select_transformer("IPA transcription");
	//my.transform = my.transforms["Silicus+Eszett+Nasal"];
	//my.select_transformer("Greek");
	my.unformatted = {};
	my.selector = '.format-word-la';
	my.format = function (space) {
		if (space === undefined) space = $(my.selector);
		else space = $(space).find(my.selector);
		getTextNodesIn(space).each(function() {
			var t = $(this), r, original;
			var p = $(this.parentElement);
			var i = getTextNodesIn(p).index(this);
			var attr = 'data-original-word'+i;
			if (!p.attr(attr)) p.attr(attr, r=t.text());
			else r = p.attr(attr);
			r = r.normalize('NFKD');
			r = my.transforms[my.transform_key](r);
			// XXX: this is really ugly
			// because we can't do this:
			// t.text(r);
			// (t is not a valid jQuery object??)
			this.textContent = r;
		});
	}
	$(function() {
		my.format();
	});

	return my;
}());

if (!æ) var æ = la_ipa;
