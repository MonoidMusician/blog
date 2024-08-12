---
title: Roman Numerals & Time
---

<style>
  .select2 {
    text-shadow: none;
  }
  .select2-results {
    color: black;
  }
  /* Wat?? */
  table.cal {
    width: fit-content;
  }
</style>

<script src="assets/js/legacy/jquery.js"></script>
<script src="assets/js/legacy/select2.min.js"></script>
<script src="assets/js/legacy/d3.min.js"></script>
<script src="assets/js/legacy/d3.chart.js"></script>
<script src="assets/js/legacy/d3.compose-all.js"></script>
<script src="assets/js/legacy/calendar.js"></script>
<script src="assets/js/legacy/util.js"></script>
<script src="assets/js/legacy/la_ipa.js"></script>
<script src="assets/js/legacy/suncalc.js"></script>
<link rel="stylesheet" href="assets/js/legacy/CSS/select2.min.css" />
<!-- <link rel="stylesheet" href="assets/js/legacy/CSS/interface.css" /> -->
<link rel="stylesheet" href="assets/js/legacy/CSS/calendar.css" />

<style>
input:not([type="checkbox"]):not([type="radio"]), button {
    border: 1px solid #aaaaaa;
    height: 1.5em;
    margin: 1px;
    border-radius: 2px;

    padding-left: 5px;
    padding-right: 5px;

    font-size: 105%;
    font-family: inherit;

    box-sizing: border-box;
    -moz-box-sizing: border-box;
}

input:not([type="checkbox"]):not([type="radio"]) {
    width: 300px;
}
.select2 input {
    width: auto;
}

input:not([type="checkbox"]):not([type="radio"]).autosizeable {
    min-width: 6em;
    width: 0px;
    max-width: 90%;
}

textarea {
    width: 300px;
    height: 100px;
    font: inherit;
}
</style>

<style>
/*!
 * d3.compose - Compose complex, data-driven visualizations from reusable charts and components with d3
 * v0.15.11 - https://github.com/CSNW/d3.compose - license: MIT
 */
.chart-compose {
  font-size: 14px;
  fill: currentColor;
}

.chart-index-0 {stroke: #1f77b4; fill: #1f77b4;}
.chart-index-1 {stroke: #ff7f0e; fill: #ff7f0e;}
.chart-index-2 {stroke: #2ca02c; fill: #2ca02c;}
.chart-index-3 {stroke: #d62728; fill: #d62728;}
.chart-index-4 {stroke: #9467bd; fill: #9467bd;}
.chart-index-5 {stroke: #8c564b; fill: #8c564b;}
.chart-index-6 {stroke: #e377c2; fill: #e377c2;}
.chart-index-7 {stroke: #7f7f7f; fill: #7f7f7f;}

.chart-line {
  stroke-width: 1.5px;
  fill: none;
}

.chart-bar {
  stroke: none;
  shape-rendering: crispEdges;
}

.chart-label-text {
  font-size: 0.85em;
  stroke: none;
  fill: black;
}
.chart-label-bg {
  opacity: 0.2;
  fill: white;
  stroke: none;
}

.chart-title {
  font-size: 1.2em;
  font-weight: bold;
}
.chart-axis-title {
  font-size: 1.1em;
  font-weight: bold;
}

.chart-axis path {
  fill: none;
  stroke-width: 2px;
  stroke: currentColor;
}
.chart-axis .tick {
  font-size: 0.9em;
}
.chart-axis .tick line, .chart-axis line.tick {
  stroke: currentColor;
}

.chart-legend-label {
  font-size: 0.9em;
}

.chart-gridline {
  stroke-width: 0.5px;
  stroke: #c9c9c9;
  fill: none;
}
</style>

<h2>Roman numerals</h2>
<br>
<input id="arabic-number" placeholder="Arabic Number" type="number" min="0" max="499999" style="width:100px">
= <input id="roman-number" placeholder="Roman Numeral" style="width:230px">
<span id="output-uc"></span>
<span id="output-lc"></span>

<br>
<select id="gender" style="width: 150px;">
<option>feminine
<option>masculine
<option>neuter
</select>
<select id="number" style="width: 100px;">
<option>singular
<option>plural
</select>
<select id="case" style="width: 200px;">
<option>nominative
<option>accusative
<option>ablative
<option>dative
<option>genitive
<option>vocative
</select>
<br>
Cardinal: <span id="cardinal" class="format-word-la"></span>
<br>
Ordinal: <span id="ordinal" class="format-word-la"></span>
<br>
Distributive: <span id="distributive" class="format-word-la"></span>
<br>
Adverbial: <span id="adverbial" class="format-word-la"></span>

<select id="variants" style="width: 200px">
</select>

<script>
let transform_list = document.getElementById("variants");
for (let name in la_ipa.transforms) {
	let e = document.createElement("option");
	e.textContent = name;
	transform_list.appendChild(e);
}
transform_list.value = "Default orthography";
$(transform_list).on("change", (e) => {
	console.log(e, transform_list.value);
	la_ipa.select_transformer(transform_list.value);
	la_ipa.format();
});

// From http://blog.stevenlevithan.com/archives/javascript-roman-numeral-converter
function romanize (num) {
	if (!+num)
		return false;
	var	digits = String(+num).split(""),
		key = ["","C","CC","CCC","CD","D","DC","DCC","DCCC","CM",
		       "","X","XX","XXX","XL","L","LX","LXX","LXXX","XC",
		       "","I","II","III","IV","V","VI","VII","VIII","IX"],
		roman = "",
		i = 3;
	while (i--)
		roman = (key[+digits.pop() + (i * 10)] || "") + roman;
	return Array(+digits.join("") + 1).join("M") + roman;
}

function deromanize (str) {
	var	str = str.toUpperCase(),
		validator = /^M*(?:D?C{0,3}|C[MD])(?:L?X{0,3}|X[CL])(?:V?I{0,3}|I[XV])$/,
		token = /[MDLV]|C[MD]?|X[CL]?|I[XV]?/g,
		key = {M:1000,CM:900,D:500,CD:400,C:100,XC:90,L:50,XL:40,X:10,IX:9,V:5,IV:4,I:1},
		num = 0, m;
	if (!(str && validator.test(str)))
		return false;
	while (m = token.exec(str))
		num += key[m[0]];
	return num;
}

// unicode!
function reromanize(str) {
	// also thanks to http://ingram-braun.net/public/programming/web/roman-numeral-unicode-form/
	$.each([
		['MMMMM','ↁ'],
		['MMMM','Ⅿↁ'],
		['ↁↁ',    'ↂ'],
		['ↂↂↂↂↂ',    'ↇ'],
		['ↂↂↂↂ',    'ↂↇ'],
		['ↇↇ',    'ↈ'],
		['ↇↂↇ',    'ↂↈ'],
		['ↁⅯↁ',    'Ⅿↂ'],
		['ↂↁⅯↁ',    'ↂⅯↂ'],
		['M',   'Ⅿ'],
		['D',   'Ⅾ'],
		['C',   'Ⅽ'],
		['L',   'Ⅼ'],
		['VIII','Ⅷ'],
		['III', 'Ⅲ'],
		['VII', 'Ⅶ'],
		['XII', 'Ⅻ'],
		['II',  'Ⅱ'],
		['VI',  'Ⅵ'],
		['IX',  'Ⅸ'],
		['XI',  'Ⅺ'],
		['X',   'Ⅹ'],
		['IV',  'Ⅳ'],
		['V',   'Ⅴ'],
		['I',   'Ⅰ'],
	], function(_,r) {
		str = str.split(r[0]).join(r[1]);
	});
	return str;
}
function halfreromanize(str) {
	$.each([
		['MMMMM','ↁ'],
		['MMMM','Ⅿↁ'],
		['ↁↁ',    'ↂ'],
		['ↂↂↂↂↂ',    'ↇ'],
		['ↂↂↂↂ',    'ↂↇ'],
		['ↇↇ',    'ↈ'],
		['ↇↂↇ',    'ↂↈ'],
		['ↁⅯↁ',    'Ⅿↂ'],
		['ↂↁⅯↁ',    'ↂⅯↂ'],
	], function(_,r) {
		str = str.split(r[0]).join(r[1]);
	});
	return str;
}

var change = [function() {
	var val = $(this).val();
	if (val >= +$(this).attr('max')) val = '';
	var result = val && (romanize(val) || "Error"), rr = '= ' + reromanize(result);
	$('#output-uc').text((!result || result == "Error") ? "" : rr);
	$('#output-lc').text((!result || result == "Error") ? "" : rr.toLowerCase());
	if (result === "Error")
	{ $('#roman-number').attr('placeholder', result); result = "" }
	else $('#roman-number').attr('placeholder', "Roman Numeral");
	$('#roman-number') .val(halfreromanize(result));
	verbalize();
}, function() {
	var val = $(this).val(), result = val && (deromanize(val) || "Error");
	$('#output-uc').text(val ? rr = '= '+reromanize(val.toUpperCase()) : '');
	$('#output-lc').text(val ? rr.toLowerCase() : '');
	if (result === "Error")
	{ $('#arabic-number').attr('placeholder', result); result = "" }
	else $('#arabic-number').attr('placeholder', "Arabic Number");
	$('#arabic-number').val(result);
	verbalize();
}]
$('#arabic-number').on('keyup', change[0]).on('keydown', change[0]).on('change', change[0]);
$('#roman-number') .on('keyup', change[1]).on('keydown', change[1]);



$('select').select2({minimumResultsForSearch: -1});
var adj12 = function(b) {
	return {
		'singular': {
			'nominative': {
				'masculine': b+'us',
				'feminine':  b+'a',
				'neuter':    b+'um'
			},
			'accusative': {
				'masculine': b+'um',
				'feminine':  b+'am',
				'neuter':    b+'um'
			},
			'ablative': {
				'masculine': b+'ō',
				'feminine':  b+'ā',
				'neuter':    b+'ō'
			},
			'dative': {
				'masculine': b+'ō',
				'feminine':  b+'æ',
				'neuter':    b+'ō'
			},
			'genitive': {
				'masculine': b+'ī',
				'feminine':  b+'æ',
				'neuter':    b+'ī'
			},
			'vocative': {
				'masculine': b+'e',
				'feminine':  b+'a',
				'neuter':    b+'um'
			}
		},
		'plural': {
			'nominative': {
				'masculine': b+'ī',
				'feminine':  b+'æ',
				'neuter':    b+'a'
			},
			'accusative': {
				'masculine': b+'ōs',
				'feminine':  b+'ās',
				'neuter':    b+'a'
			},
			'ablative': {
				'masculine': b+'īs',
				'feminine':  b+'īs',
				'neuter':    b+'īs'
			},
			'dative': {
				'masculine': b+'īs',
				'feminine':  b+'īs',
				'neuter':    b+'īs'
			},
			'genitive': {
				'masculine': b+'ōrum',
				'feminine':  b+'ārum',
				'neuter':    b+'ōrum'
			},
			'vocative': {
				'masculine': b+'ī',
				'feminine':  b+'æ',
				'neuter':    b+'a'
			}
		}
	}
};
var adj3 = function(b) {
	return {
		'singular': {
			'nominative': {
				'masculine': b+'is',
				'feminine':  b+'is',
				'neuter':    b+'e'
			},
			'accusative': {
				'masculine': b+'em',
				'feminine':  b+'em',
				'neuter':    b+'e'
			},
			'ablative': {
				'masculine': b+'ī',
				'feminine':  b+'ī',
				'neuter':    b+'ī'
			},
			'dative': {
				'masculine': b+'ī',
				'feminine':  b+'ī',
				'neuter':    b+'ī'
			},
			'genitive': {
				'masculine': b+'isī',
				'feminine':  b+'is',
				'neuter':    b+'is'
			},
			'vocative': {
				'masculine': b+'is',
				'feminine':  b+'is',
				'neuter':    b+'e'
			}
		},
		'plural': {
			'nominative': {
				'masculine': b+'ēs',
				'feminine':  b+'ēs',
				'neuter':    b+'ēia'
			},
			'accusative': {
				'masculine': b+'ēs',
				'feminine':  b+'ēs',
				'neuter':    b+'ia'
			},
			'ablative': {
				'masculine': b+'ibus',
				'feminine':  b+'ibus',
				'neuter':    b+'ibus'
			},
			'dative': {
				'masculine': b+'ibus',
				'feminine':  b+'ibus',
				'neuter':    b+'ibus'
			},
			'genitive': {
				'masculine': b+'iu',
				'feminine':  b+'ium',
				'neuter':    b+'ium'
			},
			'vocative': {
				'masculine': b+'ēs',
				'feminine':  b+'ēs',
				'neuter':    b+'ia'
			}
		}
	}
};
var modify = function(forms, list) {
	$.each(list, function(_,l) {
		var f = forms, v = l.pop(), i;
		while (l.length > 1)
			f = f[l.pop()];
		i = l.pop();
		if (typeof f[i] !== 'object')
			f[i] = v;
		else {
			$.each(f[i], function(k, _) {
				f[i][k] = v;
			});
		}
	});
	return forms;
};
var milia = {
	'nominative': 'mīlia',
	'accusative': 'mīlia',
	'vocative': 'mīlia',
	'dative': 'mīlibus',
	'ablative': 'mīlibus',
	'genitive': 'mīlium',
};
var cardinals = {
	'': 'quot',
	0: adj12('nūll'),
	1: modify(adj12('ūn'), [
		['dative','singular','ūnī'],
		['genitive','singular','ūnīus'],
	]),
	2: {
		'nominative': {
			'masculine': 'duo',
			'feminine':  'duæ',
			'neuter':    'duo',
		},
		'accusative': {
			'masculine': 'duōs',
			'feminine':  'duās',
			'neuter':    'duo',
		},
		'ablative': {
			'masculine': 'duōbus',
			'feminine':  'duābus',
			'neuter':    'duōbus',
		},
		'dative': {
			'masculine': 'duōbus',
			'feminine':  'duābus',
			'neuter':    'duōbus',
		},
		'genitive': {
			'masculine': 'duōrum',
			'feminine':  'duārum',
			'neuter':    'duōrum',
		},
		'vocative': {
			'masculine': 'duo',
			'feminine':  'duæ',
			'neuter':    'duo',
		},
	},
	3: {
		'masculine': 'trēs',
		'feminine':  'trēs',
		'neuter':    'tria',
		'ablative': 'tribus',
		'dative':   'tribus',
		'genitive': 'trium',
	},
	4: 'quattuor', 5: 'quīnque', 6: 'sex', 7: 'septem', 8: 'octō', 9: 'novem',
	10: 'decem', 11: 'ūndecim', 12: 'duodecim', 13: 'tredecim', 14: 'quattuordecim',
	15: 'quīndecim', 16: 'sēdecim', 17: 'septendecim',
	20: 'vīgintī', 100: 'centum',
	1000: 'mīlle',
};
var ordinals = {
	'': adj12('quot'),
	1: adj12('prīm'),
	2: adj12('secund'),
	3: adj12('terti'),
	4: adj12('quārt'),
	5: adj12('quīnt'),
	6: adj12('sext'),
	7: adj12('septim'),
	8: adj12('octāv'),
	9: adj12('nōn'),
	10: adj12('decim'),
	11: adj12('ūndecim'),
	12: adj12('duodecim'),
	20: adj12('vīcē(n)sim'),
	30: adj12('trīcē(n)sim'),
	100: adj12('centē(n)sim'),
	1000: adj12('mīllē(n)sim'),
};
var distributives = {
	'': adj12('quotēn'),
	1: adj12('singul'),
	2: adj12('bīn'),
	3: adj12('tern'),
	4: adj12('quatern'),
	5: adj12('quīn'),
	6: adj12('sēn'),
	7: adj12('septēn'),
	8: adj12('octōn'),
	9: adj12('novēn'),
	10: adj12('dēn'),
	11: adj12('ūndēn'),
	12: adj12('duodēn'),
	16: adj12('sēdēn'),
	20: adj12('vīcēn'),
	30: adj12('trīcēn'),
	100: adj12('centēn'),
	1000: adj12('mīllēn'),
};
var adverbials = {
	'': 'quotiē(n)s',
	1: 'semel',
	2: 'bis',
	3: 'ter',
	4: 'quater',
	5: 'quīnquiē(n)s',
	6: 'sexiē(n)s',
	7: 'septiē(n)s',
	8: 'octiē(n)s',
	9: 'noviē(n)s',
	10: 'deciē(n)s',
	11: 'ūndeciē(n)s',
	12: 'duodeciē(n)s',
	13: 'terdeciē(n)s',
	14: 'quaterdeciē(n)s',
	15: 'quīndeciē(n)s',
	16: 'sēdeciē(n)s',
	20: 'vīciē(n)s',
	30: 'trīciē(n)s',
	1000: 'mīlliē(n)s',
};
var tens = [
	'vī', 'trī', 'quadrā', 'quīnquā',
	'sexā', 'septuā', 'octō', 'nōnā'
];
var hundreds = [
	'', 'du', 'tre', 'quadrin',
	'quīn', 'ses', 'septin', 'octin', 'nōn'
];
$.each(tens, function(i, pre) {
	i = (i+2)*10;
	if (!(i in cardinals))
		cardinals    [i] = pre+'gintā';
	if (!(i in ordinals))
		ordinals     [i] = adj12(pre+'gē(n)sim');
	if (!(i in distributives))
		distributives[i] = adj12(pre+'gēn');
	if (!(i in adverbials))
		adverbials   [i] = pre+'giē(n)s';
});
$.each(hundreds, function(i, pre) {
	i = (i+1)*100;
	var c = pre.endsWith('n') ? 'g':'c';
	if (!(i in cardinals))
		cardinals    [i] = adj12(pre+c+'ent')['plural'];
	if (!(i in ordinals))
		ordinals     [i] = adj12(pre+c+'entē(n)sim');
	if (!(i in distributives))
		distributives[i] = adj12(pre+c+'ēn');
	if (!(i in adverbials))
		adverbials   [i] = pre+c+'entiē(n)s';
});
var decs = function(n,pad) {
	var ret = [];
	while (n) {
		ret.push(n % 10);
		n = Math.floor(n/10);
	}
	while (pad && ret.length < pad)
		ret.push(0);
	return ret;
};
var parse = function(verb, number, _case, gender) {
	if (typeof verb === 'object' && number in verb)
		verb = verb[number];
	if (typeof verb === 'object' && _case  in verb)
		verb = verb[_case];
	if (typeof verb === 'object' && gender in verb)
		verb = verb[gender];
	if (typeof verb === 'object')
		verb = null;
	return verb;
};
var milparse = function(_case) {
	return function(verb){return parse(verb, 'plural', _case, 'neuter')};
};
var combine = function() {
	var res = "";
	for(var i = 0; i < arguments.length; i++) {
		var a = arguments[i];
		if (!a && a !== '') return;
		res += a;
	}
	return res;
};
var getter = function(generator) {
	return function(n, number, _case, gender, multiple) {
		var parse = function(verb) {
			if (typeof verb === 'object' && number in verb)
				verb = verb[number];
			if (typeof verb === 'object' && _case  in verb)
				verb = verb[_case];
			if (typeof verb === 'object' && gender in verb)
				verb = verb[gender];
			if (typeof verb === 'object')
				verb = null;
			return verb;
		};
		var mparse = milparse(_case);
		return generator(n, parse, mparse, multiple);
	}
};


var _cardinal = function(n, parse, milparse, multiple) {
	var dec =  decs(n,4), cardinal, c;
	if (n > 0 || n === '') {
		cardinal = parse(cardinals[n]);

		if (n > 10) {
			if (!cardinal && n > 20) {
				if (n < 100)
					cardinal = combine(parse(cardinals[dec[1]*10]), ' ', parse(cardinals[dec[0]]));
				else if (n < 1000)
					cardinal = combine(parse(cardinals[dec[2]*100]), ' et ', _cardinal(n%100, parse, multiple));
				else {
					var et = '';
					if (dec[0] || dec[1] || dec[2])
						et = combine(' et ', dec[2]?parse(cardinals[dec[2]*100]):'', ' ', dec[0]||dec[1]?_cardinal(n%100, parse):'');

					if (n >= 2000)
						cardinal = combine(_cardinal(Math.floor(n/1000), milparse), ' ', parse(milia), et, ' (+GEN)');
					else cardinal = combine(cardinals[1000], et);
				}
			}

			if (n < 100 && (n % 10) == 8 && n != 98)
				c = combine('duodē',parse(cardinals[n+2]));
			else if (n < 100 && (n % 10) == 9)
				c = combine('ūndē',parse(cardinals[n+1]));

			if (c && cardinal != c)
				if (cardinal && multiple) cardinal = cardinal + ' / ' + c;
				else cardinal = c;
		}
	}
	if (cardinal) return cardinal;
};
var _ordinal = function(n, parse, milparse, multiple) {
	var dec =  decs(n), ordinal, o;
	if (n > 0 || n === '') {
		ordinal = parse(ordinals[n]);

		if (n > 10) {
			if (!ordinal)
				if (n < 20)
					// e.g. tertius decimus
					ordinal = combine(parse(ordinals[dec[0]]), ' ', parse(ordinals[dec[1]*10]));
				else if (n < 100)
					// e.g. vicensimus primus
					ordinal = combine(parse(ordinals[dec[1]*10]), ' ', parse(ordinals[dec[0]]));
				else if (n < 1000)
					ordinal = combine(_cardinal(n%100, parse), ' et ', parse(ordinals[dec[2]*100]));
				else {
					var et = '', mult = '';
					if (dec[0] || dec[1] || dec[2])
						et = combine(dec[2]?parse(cardinals[dec[2]*100]):'', ' ', dec[0]||dec[1]?_cardinal(n%100, parse):'', ' et ');

					if (n >= 2000)
						mult = _adverbial(Math.floor(n/1000), milparse);
					ordinal = combine(et, mult, ' ', parse(ordinals[1000]));
				}

			if (n < 100 && (n % 10) == 8 && n != 98)
				o = combine('duodē',parse(ordinals[n+2]));
			else if (n < 100 && (n % 10) == 9)
				o = combine('ūndē',parse(ordinals[n+1]));

			if (o && ordinal != o)
				if (ordinal && multiple) ordinal = ordinal + ' / ' + o;
				else ordinal = o;
		}
	}
	if (ordinal) return ordinal;
};
var _distributive = function(n, parse, milparse, multiple) {
	var dec =  decs(n), distributive, d;
	if (n > 0 || n === '') {
		distributive = parse(distributives[n]);

		if (n > 10) {
			if (!distributive)
				if (n < 20)
					// e.g. terni deni
					distributive = combine(parse(distributives[dec[0]]), ' ', parse(distributives[dec[1]*10]));
				else if (n < 100)
					// e.g. viceni singuli
					distributive = combine(parse(distributives[dec[1]*10]),' ',parse(distributives[dec[0]]));
				else if (n < 1000)
					distributive = combine(parse(distributives[dec[2]*100]), ' et ', _distributive(n%100, parse, multiple));
				else {
					var et = '';
					if (dec[0] || dec[1] || dec[2])
						et = combine(' et ', dec[2]?parse(distributives[dec[2]*100]):'', ' ', dec[0]||dec[1]?_distributive(n%100, parse):'');

					if (n >= 2000)
						distributive = combine(_distributive(Math.floor(n/1000), milparse), ' ', parse(milia), et, ' (+GEN)');
					else distributive = combine(parse(distributives[1000]), et);
				}

			if (n < 100 && (n % 10) == 8 && n != 98)
				d = combine('duodē',parse(distributives[n+2]));
			else if (n < 100 && (n % 10) == 9)
				d = combine('ūndē',parse(distributives[n+1]));

			if (d && distributive != d)
				if (distributive && multiple) distributive = distributive + ' / ' + d;
				else distributive = d;
		}
	}
	if (distributive) return distributive;
};
var _adverbial = function(n, parse, milparse, multiple) {
	var dec =  decs(n), adverbial, a;
	if (n > 0 || n === '') {
		adverbial = parse(adverbials[n]);

		if (n > 10) {
			if (!adverbial)
				if (n < 20)
					adverbial = combine(parse(adverbials[dec[0]]), ' ', parse(adverbials[dec[1]*10]));
				else if (n < 100)
					adverbial = combine(parse(adverbials[dec[1]*10]), ' ', parse(adverbials[dec[0]]));
				else if (n < 1000)
					adverbial = combine(_cardinal(n%100, parse), ' et ', parse(adverbials[dec[2]*100]));
				else {
					var et = '', mult = '';
					if (dec[0] || dec[1] || dec[2])
						et = combine(' et ', dec[2]?parse(adverbials[dec[2]*100]):'', ' ', dec[0]||dec[1]?_adverbial(n%100, parse):'');

					if (n >= 2000)
						mult = _adverbial(Math.floor(n/1000), milparse);
					adverbial = combine(mult, ' ', parse(adverbials[1000]), et);
				}

			if (n < 100 && (n % 10) == 8 && n != 98)
				a = combine('duodē',parse(adverbials[n+2]));
			else if (n < 100 && (n % 10) == 9)
				a = combine('ūndē',parse(adverbials[n+1]));

			if (a && adverbial != a)
				if (adverbial && multiple) adverbial = adverbial + ' / ' + a;
				else adverbial = a;
		}
	}
	if (adverbial) return adverbial;
};

var getcardinal     = getter(_cardinal);
var getordinal      = getter(_ordinal);
var getdistributive = getter(_distributive);
var getadverbial    = getter(_adverbial);

var verbalize = function() {
	var n = $('#arabic-number').val(), cardinal, ordinal, distributive, adverbial;
	if (n > +$('#arabic-number').attr('max'))
		n = '';
	if (n !== '') n = +n;
	var number = $('#number').val();
	var _case =  $('#case').val();
	var gender = $('#gender').val();
	var parse = function(verb) {
		if (typeof verb === 'object' && number in verb)
			verb = verb[number];
		if (typeof verb === 'object' && _case  in verb)
			verb = verb[_case];
		if (typeof verb === 'object' && gender in verb)
			verb = verb[gender];
		if (typeof verb === 'object')
			verb = null;
		return verb;
	};
	var mparse = milparse(_case);
	var m = false;
	ordinal      =      _ordinal(n, parse, mparse, m);
	cardinal     =     _cardinal(n, parse, mparse, m);
	distributive = _distributive(n, parse, mparse, m);
	adverbial    =    _adverbial(n, parse, mparse, m);
	if (!cardinal)
		cardinal = '';
	else
		cardinal = cardinal.split('(n)').join('n');
	if (!ordinal)
		ordinal = '';
	else
		ordinal = ordinal.split('(n)').join('n');
	if (!distributive)
		distributive = '';
	else
		distributive = distributive.split('(n)').join('n');
	if (!adverbial)
		adverbial = '';
	else
		adverbial = adverbial.split('(n)').join('n');
	$('#cardinal').text(cardinal).attr('data-original-word0', '');
	$('#ordinal').text(ordinal).attr('data-original-word0', '');
	$('#distributive').text(distributive).attr('data-original-word0', '');
	$('#adverbial').text(adverbial).attr('data-original-word0', '');
	la_ipa.format();
};
$('#case, #gender, #number').on('change', verbalize);

$('#arabic-number').trigger('keyup');
</script>
</article>



<h2>Roman time</h2>
<p>Roman hours split the solar day (sunrise to sunset) into 12 equal parts. Find sunrise/sunset times for a date and location.

<h3>Date</h3>
<div>
  <span id="romandate" class="format-word-la"></span>
</div>
<div>
  <input id="date" class="medium" type="text">
  <button id="today">Today</button>
  <button id="romefounding">Founding of Rome</button>
</div>
<div id="calendar" style="float: left; padding-top: 20px;"></div>
<div style="clear: both;"></div>

<h3>Location</h3>
<input id="place">

<style>
  #place {
    background-color: #fff;
    font-family: Roboto;
    font-size: 15px;
    font-weight: 300;
    margin-left: 12px;
    padding: 0 11px 0 13px;
    text-overflow: ellipsis;
    width: 300px;
    margin-top: 10px;
    border: 1px solid transparent;
    border-radius: 2px 0 0 2px;
    box-sizing: border-box;
    -moz-box-sizing: border-box;
    height: 32px;
    outline: none;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3);
  }

  input#place:focus:not(.select2-search__field) {
    border-color: #4d90fe;
  }
</style>

<input class="medium" id="latitude" placeholder="Latitude"><input class="medium" id="longitude" placeholder="Longitude">
<button id="to-rome">In Rome</button>
<button id="to-london">In London</button>
<button id="hither">WhereAmI</button>

<div id="map" style="height: 300px;"></div>

<h3>Timezone</h3>
UTC+<input id="timezone" class="small" style="width:60px" value="0" min="-12" max="12" type="number">
<button id="utc">UTC</button>
<button id="localtime">My Timezone</button>
<button id="loctime">By location</button>

<h3>Results</h3>
Sunrise:
  <span id="sunrise"></span><br>
Solar noon:
  <span id="noon"></span><br>
Sunset:
  <span id="sunset"></span><br>
Roman hour: <span id="romanhour"></span> hours / <span id="romanminutes"></span> minutes

<br>

<div class="full-width">
<div id="solarchart" style="width: 800px; max-width: 100%; clear: both; margin: auto"></div>
</div>

<script>
var date = new Date(), lat = 0, lng = 0, timezone = 0;
date.setUTCDate(date.getDate());



// Output graph
var chart = d3.select('#solarchart').chart('Compose', function(data) {
	var scales = {
		x: {data: data.data, key: 'x'},
		y: {domain: data.range}
		//y: {data: data, key: 'y'},
	};

	var charts = [
		d3c.lines('times', {
			data: data.data,
			xScale: scales.x,
			yScale: scales.y
		}),
		d3c.lines('dates', {
			data: data.dates,
			xScale: scales.x,
			yScale: scales.y
		}),
	];

	var xAxis = d3c.axis('xAxis', {scale: scales.x});
	var yAxis = d3c.axis('yAxis', {scale: scales.y});
	var legend = d3c.legend({charts: ['times','dates']});
	var title = d3c.title('Sun times throughout the year');
	var xAxisTitle = d3c.axisTitle('Day');
	var yAxisTitle = d3c.axisTitle('Hour');

	return [
		title,
		[yAxisTitle, yAxis, d3c.layered(charts), legend],
		xAxis,
		xAxisTitle
	];
}).width(800).height(500);


function displaytimes(date, lat, lng) {
	var times = SunCalc.getTimes(date, lat, lng);
	$('#latitude').val(lat); $('#longitude').val(lng);
	var _f = d3.time.format.utc("%0H:%M"), f = function(d) {
		if (isNaN(d.getTime())) return "None";
		d = new Date(d);
		d.setHours(d.getHours() + timezone);
		return _f(d);
	};
	function fix(t, d, h) {
		if (!isNaN(t.getTime())) return t;
		t = new Date(d);
		t.setUTCHours(h);
		return t;
	}
	function getT(t, prev) {
		if (Array.isArray(prev)) {
			if (!prev.length) prev = getT(t);
			else prev = prev[prev.length-1];
		}
		if (typeof prev === 'object')
			prev = prev.y;
		if (isNaN(t.getTime()))
			return prev;
		var t = t.getUTCHours() + t.getUTCMinutes()/60 + t.getUTCSeconds()/60/60;
		if (prev === undefined) return t;
		if (t > prev+6) t -= 24;
		else if (t < prev-6) t += 24;
		return t;
	}
	$('#sunrise').text(f(times.sunrise));
	$('#sunset' ).text(f(times.sunset));
	$('#noon'   ).text(f(times.solarNoon));
	var t = getT(times.sunset) - getT(times.sunrise);
	if (t < 0) t += 24;
	$('#romanhour'   ).text((t/12).toFixed(2));
	$('#romanminutes').text((t/12*60).toFixed(0));


	var data = [
	  {
		// required: values: [...]
		// optional: key, name
		key: 'sunrise',
		name: 'Sunrise (Prima Hora)', // (used in legend later)
		values: []
	  },
	  {
		key: 'hr3',
		name: 'Tertia Hora',
		values: []
	  },
	  {
		key: 'noon',
		name: 'Noon (Sexta Hora)',
		values: []
	  },
	  {
		key: 'hr9',
		name: 'Nona Hora',
		values: []
	  },
	  {
		key: 'sunset',
		name: 'Sunset (Duodecima Hora)',
		values: []
	  },
	];
	var dates = [
	  {
	  	key: 'today',
	  	name: 'Selected Date',
	  	values: []
	  },
	  {
	  	key: 'solstice1',
	  	name: 'Solstice',
	  	values: []
	  },
	  {
	  	key: 'solstice2',
	  	name: 'Solstice',
	  	values: []
	  },
	  {
	  	key: 'equinox1',
	  	name: 'Equinox',
	  	values: []
	  },
	  {
	  	key: 'equinox2',
	  	name: 'Equinox',
	  	values: []
	  },
	];
	var year = date.getFullYear(), today;
	var s = new Date(year, 0, 1), e = new Date(year, 12, 1);
	var i = 0; var avgnoon = 0;
	var _t4 = undefined, _t5 = undefined;
	var days = [];
	for (var d = new Date(s); d <= e; d.setDate(d.getDate() + 1)) {
		i++;
		var times = SunCalc.getTimes(d, lat, lng);
		var t1 = getT(times.sunrise, data[0].values);
		data[0].values.push({x:i,y:t1});
		var t2 = getT(times.sunset, data[4].values);
		if (t2 < t1) t2 += 24;
		data[4].values.push({x:i,y:t2});
		var t3 = getT(times.solarNoon, data[2].values);
		if (t3 < t1) t3 += 24;
		data[2].values.push({x:i,y:t3});
		var t4 = 0.5 * t1 + 0.5 * t3;
		if (t4 < t1) t4 += 24;
		data[1].values.push({x:i,y:t4});
		var t5 = 0.5 * t2 + 0.5 * t3;
		if (t5 < t1) t5 += 24;
		data[3].values.push({x:i,y:t5});
		avgnoon += getT(times.solarNoon);
		if (d.getMonth() == date.getMonth() && d.getDate() == date.getDate()) {
			today = dates[0];
			today.values.push({x:i,y:t1});
			today.values.push({x:i,y:t2});
		}
		_t4 = t4; _t5 = t5;
		days.push([d,t1,t2,t3,t4,t5,t2-t1,Math.abs(t2-t1-12)]);
	}
	var maxlux = 0, minlux = 0, eq1 = 0, eq2 = 0;
	$.each(days, function(i,day) {
		var lux = day[6], equi = day[7];
		if (lux > days[maxlux][6]) maxlux = i;
		else if (maxlux != 0 && lux == days[maxlux][6]) {
			dates[1].values.push({'x':maxlux,'y':days[maxlux][1]});
			dates[1].values.push({'x':maxlux,'y':days[maxlux][2]});
		}
		if (lux < days[minlux][6]) minlux = i;
		else if (maxlux != 0 && lux == days[minlux][6])	{
			dates[2].values.push({'x':minlux,'y':days[minlux][1]});
			dates[2].values.push({'x':minlux,'y':days[minlux][2]});
		}
		if (i <= days.length/2) {
			if (equi < days[eq1][7]) eq1 = i;
			else if (eq1 != 0 && lux == days[eq1][6])	{
				dates[3].values.push({'x':eq1,'y':days[eq1][1]});
				dates[3].values.push({'x':eq1,'y':days[eq1][2]});
			}
		} else {
			if (equi < days[eq2][7]) eq2 = i;
			else if (eq2 != 0 && lux == days[eq2][6])	{
				dates[4].values.push({'x':eq2,'y':days[eq2][1]});
				dates[4].values.push({'x':eq2,'y':days[eq2][2]});
			}
		}
	});
	dates[1].values.push({'x':maxlux,'y':days[maxlux][1]});
	dates[1].values.push({'x':maxlux,'y':days[maxlux][2]});
	dates[2].values.push({'x':minlux,'y':days[minlux][1]});
	dates[2].values.push({'x':minlux,'y':days[minlux][2]});
	dates[3].values.push({'x':eq1,'y':days[eq1][1]});
	dates[3].values.push({'x':eq1,'y':days[eq1][2]});
	dates[4].values.push({'x':eq2,'y':days[eq2][1]});
	dates[4].values.push({'x':eq2,'y':days[eq2][2]});
	avgnoon /= (i-1);
	var tz = Math.round(12 - avgnoon);
	$.each(data, function(i,d) {
		$.each(d.values, function(j,pt) {
			pt.y += timezone;
		});
	});
	$.each(dates, function(i,d) {
		$.each(d.values, function(j,pt) {
			pt.y += timezone;
		});
	});

	chart.draw({data:data, dates:dates, range:[-tz+timezone,-tz+timezone+24]});
}







// Timezone
$('#utc').on('click', function() {
	$('#timezone').val(0).trigger('change');
});
$('#localtime').on('click', function() {
	$('#timezone').val(-(new Date()).getTimezoneOffset()/60).trigger('change');
});
$('#loctime').on('click', function() {
	$.get('https://maps.googleapis.com/maps/api/timezone/json', {
		location: lat+','+lng,
		timestamp: date.getTime()/1000,
		key: 'AIzaSyB2kxU6e0-_Wqgaac-IJ7ZI5X1gEaG6IsE',
	}).done(function(data) {
		console.log(data);
		if (data.status != "OK")
			alert(data.status);
		$('#timezone').val((data.dstOffset + data.rawOffset)/60/60).trigger('change');
	});
});
$('#timezone').on('change', function() {
	timezone = +$(this).val();
	displaytimes(date, lat, lng);
});

// Calendar
var months = [
	adj12('jānuāri'),
	adj12('februāri'),
	adj12('mārti'),
	adj3('aprīl'),
	adj12('māi'),
	adj12('jūni'),
	adj12('jūli'),
	adj12('august'),
	modify(adj3('septembr'), [
		['nominative','singular','september']
	]),
	modify(adj3('octōbr'), [
		['nominative','singular','octōber']
	]),
	modify(adj3('novembr'), [
		['nominative','singular','november']
	]),
	modify(adj3('decembr'), [
		['nominative','singular','december']
	]),
];
var days = {
	'ides': {
		'nominative': 'īdūs',
		'accusative': 'īdūs',
		'ablative':   'īdibus',
		'dative':     'īdibus',
		'genitive':   'īduum',
		'vocative':   'īdūs'
	},
	'kalends': {
		'nominative': 'kalendæ',
		'accusative': 'kalendās',
		'ablative':   'kalendīs',
		'dative':     'kalendīs',
		'genitive':   'kalendārum',
		'vocative':   'kalendæ'
	},
	'nones': {
		'nominative': 'nōnæ',
		'accusative': 'nōnās',
		'ablative':   'nōnīs',
		'dative':     'nōnās',
		'genitive':   'nōnārum',
		'vocative':   'nōnæ'
	}
};
var weekdays = [
	'Sōlis', 'Lūnæ', 'Martis', 'Mercuriī', 'Iovis', 'Veneris', 'Saturnī'
];
var nones = function(month) {
	if (month == 2 || month == 4 || month == 6 || month == 9)
		return 7;
	return 5;
};
var getclass = function(date) {
	var m = date.getUTCMonth(), d = date.getUTCDate();
	if (d == 1) return 'kalends';
	if (d == nones(m)) return 'nones';
	if (d == nones(m)+8) return 'ides';
};
var romancalendar = function(date) {
	var d = new Date(date);
	for (var o = 0; !getclass(d); ++o, d.setDate(d.getDate() + 1)) {}
	var c = getclass(d), m = d.getUTCMonth(), y = d.getUTCFullYear(), year = '', _case = 'accusative', pre = '';
	if (o == 0) _case = 'ablative';
	else if (o == 1) pre = 'prīdiē ';
	else {
		d.setDate(d.getDate() - 1);
		if (m == 2 && o > 5 && d.getUTCDate() == 29)
			o -= 1; // leap years have two VI ante Kal. Feb.
		pre = 'diē ' + getordinal(o+1, 'singular', 'ablative', 'masculine') + ' ante ';
	}
	if (y > 0) {
		year = ' annō dominī ' + romanize(y);
	}
	return weekdays[date.getUTCDay()] + ' ' + pre + Titlecase(days[c][_case]) + ' ' + Titlecase(months[m]['plural'][_case]['feminine']) + year;
};
var Romancalendar = function(date) {
	$('#romandate').text(romancalendar(date)).attr('data-original-word0', '');
	la_ipa.format();
};
function Titlecase(string) {
	return string.charAt(0).toUpperCase() + string.slice(1);
};
var calendar = $('#calendar').calendar({date:date,classes:getclass}).on('click', function() {
	$('input#date').val($(this).data('date'));
	date = new Date($(this).data('date'));
	displaytimes(date, +lat, +lng);
	Romancalendar(date);
});
var update_date = function(d) {
	date = d; var s = d3.time.format.iso(date).split("T")[0];
	$('input#date').val(s);
	calendar.data('date', s).update(date);
	displaytimes(date, +lat, +lng);
	Romancalendar(d);
};
$('#today').on('click', function() {
	var d = new Date();
	d.setUTCDate(d.getDate());
	update_date(d);
});
$('#romefounding').on('click', function() {
	update_date(new Date("-000753-04-21"));
});
$('#date').on('change', function() {
	var d = new Date($(this).val());
	if (d)
		update_date(d);
});

// Location
function update_map(latitude, longitude, searching) {
	lat = +latitude, lng = +longitude;
	$('#latitude').val(lat);
	$('#longitude').val(lng);
	if (typeof map != 'undefined') {
		map.setCenter({lat:lat,lng:lng});
		marker.setPosition({lat:lat,lng:lng});
		if (searching != true) {
			if (!searching) searching = '';
			$(input).val(searching);
		}
	}
	displaytimes(date, lat, lng);
}
$('#latitude, #longitude').on('change', function() {
	lat = $('#latitude').val(), lng = $('#longitude').val();
	if (lat == +lat && lng == +lng)
		update_map(lat, lng);
});
$('#to-rome'  ).on('click', function() {update_map(41.90278349999999,12.496365500000024,"Rome");}).trigger('click');
$('#to-london').on('click', function() {update_map(51.5073509,-0.12775829999998223,"London");});
if ("geolocation" in navigator) {
	$('#hither').on('click', function() {
		navigator.geolocation.getCurrentPosition(function(position) {
			update_map(position.coords.latitude, position.coords.longitude);
		});
	});
} else {
	$('#hither').remove();
}


update_date(date);

var map, marker, input, searchBox;
window.initMap = function() {
	map = new google.maps.Map(document.getElementById('map'), {
		center: {lat: lat, lng: lng},
		zoom: 5
	});
	marker = new google.maps.Marker({
		position: {lat:lat,lng:lng},
		map: map, draggable: true,
		title: "Drag me!",
	});
	google.maps.event.addListener(marker, 'dragend', function(event) {
		update_map(marker.getPosition().lat(),marker.getPosition().lng());
	});

	input = document.getElementById('place');
	searchBox = new google.maps.places.SearchBox(input);
	map.controls[google.maps.ControlPosition.TOP_LEFT].push(input);

	// Bias the SearchBox results towards current map's viewport.
	map.addListener('bounds_changed', function() {
		searchBox.setBounds(map.getBounds());
	});

	var markers = [];
	// Listen for the event fired when the user selects a prediction and retrieve
	// more details for that place.
	searchBox.addListener('places_changed', function() {
		var places = searchBox.getPlaces();

		if (places.length == 0) {
			return;
		}

		// Clear out the old markers.
		markers.forEach(function(marker) {
			marker.setMap(null);
		});
		markers = [];

		if (places.length == 1) {
			var loc = places[0].geometry.location;
	        update_map(loc.lat(),loc.lng(),true);
			return;
		}

		// For each place, get the icon, name and location.
		var bounds = new google.maps.LatLngBounds();
		places.forEach(function(place) {
			var icon = {
				url: place.icon,
				size: new google.maps.Size(71, 71),
				origin: new google.maps.Point(0, 0),
				anchor: new google.maps.Point(17, 34),
				scaledSize: new google.maps.Size(25, 25)
			};

			// Create a marker for each place.
			markers.push(new google.maps.Marker({
				map: map,
				icon: icon,
				title: place.name,
				position: place.geometry.location
			}));

			if (place.geometry.viewport) {
				// Only geocodes have viewport.
				bounds.union(place.geometry.viewport);
			} else {
				bounds.extend(place.geometry.location);
			}
		});
		map.fitBounds(bounds);
	});
}

</script>
<script src="https://maps.googleapis.com/maps/api/js?key=AIzaSyB2kxU6e0-_Wqgaac-IJ7ZI5X1gEaG6IsE&callback=initMap&libraries=places" async defer></script>
