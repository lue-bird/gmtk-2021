(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $ianmackenzie$elm_units$Quantity$Quantity = function (a) {
	return {$: 'Quantity', a: a};
};
var $ianmackenzie$elm_units$Quantity$zero = $ianmackenzie$elm_units$Quantity$Quantity(0);
var $MartinSStewart$elm_audio$Audio$audioDefaultConfig = {loop: $elm$core$Maybe$Nothing, playbackRate: 1, startAt: $ianmackenzie$elm_units$Quantity$zero};
var $MartinSStewart$elm_audio$Audio$BasicAudio = function (a) {
	return {$: 'BasicAudio', a: a};
};
var $MartinSStewart$elm_audio$Audio$audioWithConfig = F3(
	function (audioSettings, source, startTime) {
		return $MartinSStewart$elm_audio$Audio$BasicAudio(
			{settings: audioSettings, source: source, startTime: startTime});
	});
var $MartinSStewart$elm_audio$Audio$audio = F2(
	function (source, startTime) {
		return A3($MartinSStewart$elm_audio$Audio$audioWithConfig, $MartinSStewart$elm_audio$Audio$audioDefaultConfig, source, startTime);
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $MartinSStewart$elm_audio$Audio$Group = function (a) {
	return {$: 'Group', a: a};
};
var $MartinSStewart$elm_audio$Audio$group = function (audios) {
	return $MartinSStewart$elm_audio$Audio$Group(audios);
};
var $MartinSStewart$elm_audio$Audio$silence = $MartinSStewart$elm_audio$Audio$group(_List_Nil);
var $author$project$Main$audio = F2(
	function (_v0, model) {
		var _v1 = model.music;
		if (_v1.$ === 'Just') {
			var music = _v1.a;
			return A2(
				$MartinSStewart$elm_audio$Audio$audio,
				music,
				$elm$time$Time$millisToPosix(model.timePlaying));
		} else {
			return $MartinSStewart$elm_audio$Audio$silence;
		}
	});
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$audioPortFromJS = _Platform_incomingPort('audioPortFromJS', $elm$json$Json$Decode$value);
var $author$project$Main$audioPortToJS = _Platform_outgoingPort('audioPortToJS', $elm$core$Basics$identity);
var $MartinSStewart$elm_audio$Audio$UserMsg = function (a) {
	return {$: 'UserMsg', a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioData = function (a) {
	return {$: 'AudioData', a: a};
};
var $MartinSStewart$elm_audio$Audio$audioData = function (_v0) {
	var model = _v0.a;
	return $MartinSStewart$elm_audio$Audio$AudioData(
		{sourceData: model.sourceData});
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$document = _Browser_document;
var $MartinSStewart$elm_audio$Audio$getUserModel = function (_v0) {
	var model = _v0.a;
	return model.userModel;
};
var $MartinSStewart$elm_audio$Audio$Model = function (a) {
	return {$: 'Model', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $MartinSStewart$elm_audio$Audio$audioSourceBufferId = function (_v0) {
	var audioSource = _v0.a;
	return audioSource.bufferId;
};
var $ianmackenzie$elm_units$Duration$inSeconds = function (_v0) {
	var numSeconds = _v0.a;
	return numSeconds;
};
var $ianmackenzie$elm_units$Duration$inMilliseconds = function (duration) {
	return $ianmackenzie$elm_units$Duration$inSeconds(duration) * 1000;
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$core$Basics$round = _Basics_round;
var $ianmackenzie$elm_units$Duration$addTo = F2(
	function (time, duration) {
		return $elm$time$Time$millisToPosix(
			$elm$time$Time$posixToMillis(time) + $elm$core$Basics$round(
				$ianmackenzie$elm_units$Duration$inMilliseconds(duration)));
	});
var $MartinSStewart$elm_audio$Audio$audioStartTime = function (audio_) {
	return A2($ianmackenzie$elm_units$Duration$addTo, audio_.startTime, audio_.offset);
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $MartinSStewart$elm_audio$Audio$encodeBufferId = function (_v0) {
	var bufferId = _v0.a;
	return $elm$json$Json$Encode$int(bufferId);
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $MartinSStewart$elm_audio$Audio$encodeDuration = A2($elm$core$Basics$composeR, $ianmackenzie$elm_units$Duration$inMilliseconds, $elm$json$Json$Encode$float);
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $MartinSStewart$elm_audio$Audio$encodeLoopConfig = function (maybeLoop) {
	if (maybeLoop.$ === 'Just') {
		var loop = maybeLoop.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'loopStart',
					$MartinSStewart$elm_audio$Audio$encodeDuration(loop.loopStart)),
					_Utils_Tuple2(
					'loopEnd',
					$MartinSStewart$elm_audio$Audio$encodeDuration(loop.loopEnd))
				]));
	} else {
		return $elm$json$Json$Encode$null;
	}
};
var $MartinSStewart$elm_audio$Audio$encodeTime = A2($elm$core$Basics$composeR, $elm$time$Time$posixToMillis, $elm$json$Json$Encode$int);
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $mgold$elm_nonempty_list$List$Nonempty$toList = function (_v0) {
	var x = _v0.a;
	var xs = _v0.b;
	return A2($elm$core$List$cons, x, xs);
};
var $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline = function (volumeTimeline) {
	return A2(
		$elm$json$Json$Encode$list,
		function (_v0) {
			var time = _v0.a;
			var volume = _v0.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'time',
						$MartinSStewart$elm_audio$Audio$encodeTime(time)),
						_Utils_Tuple2(
						'volume',
						$elm$json$Json$Encode$float(volume))
					]));
		},
		$mgold$elm_nonempty_list$List$Nonempty$toList(volumeTimeline));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $mgold$elm_nonempty_list$List$Nonempty$Nonempty = F2(
	function (a, b) {
		return {$: 'Nonempty', a: a, b: b};
	});
var $mgold$elm_nonempty_list$List$Nonempty$map = F2(
	function (f, _v0) {
		var x = _v0.a;
		var xs = _v0.b;
		return A2(
			$mgold$elm_nonempty_list$List$Nonempty$Nonempty,
			f(x),
			A2($elm$core$List$map, f, xs));
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $MartinSStewart$elm_audio$Audio$volumeTimelines = function (audio_) {
	return A2(
		$elm$core$List$map,
		$mgold$elm_nonempty_list$List$Nonempty$map(
			$elm$core$Tuple$mapFirst(
				function (a) {
					return A2($ianmackenzie$elm_units$Duration$addTo, a, audio_.offset);
				})),
		audio_.volumeTimelines);
};
var $MartinSStewart$elm_audio$Audio$encodeStartSound = F2(
	function (nodeGroupId, audio_) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('startSound')),
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'bufferId',
					$MartinSStewart$elm_audio$Audio$encodeBufferId(
						$MartinSStewart$elm_audio$Audio$audioSourceBufferId(audio_.source))),
					_Utils_Tuple2(
					'startTime',
					$MartinSStewart$elm_audio$Audio$encodeTime(
						$MartinSStewart$elm_audio$Audio$audioStartTime(audio_))),
					_Utils_Tuple2(
					'startAt',
					$MartinSStewart$elm_audio$Audio$encodeDuration(audio_.startAt)),
					_Utils_Tuple2(
					'volume',
					$elm$json$Json$Encode$float(audio_.volume)),
					_Utils_Tuple2(
					'volumeTimelines',
					A2(
						$elm$json$Json$Encode$list,
						$MartinSStewart$elm_audio$Audio$encodeVolumeTimeline,
						$MartinSStewart$elm_audio$Audio$volumeTimelines(audio_))),
					_Utils_Tuple2(
					'loop',
					$MartinSStewart$elm_audio$Audio$encodeLoopConfig(audio_.loop)),
					_Utils_Tuple2(
					'playbackRate',
					$elm$json$Json$Encode$float(audio_.playbackRate))
				]));
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $ianmackenzie$elm_units$Quantity$plus = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x + y);
	});
var $MartinSStewart$elm_audio$Audio$flattenAudio = function (audio_) {
	switch (audio_.$) {
		case 'Group':
			var group_ = audio_.a;
			return $elm$core$List$concat(
				A2($elm$core$List$map, $MartinSStewart$elm_audio$Audio$flattenAudio, group_));
		case 'BasicAudio':
			var source = audio_.a.source;
			var startTime = audio_.a.startTime;
			var settings = audio_.a.settings;
			return _List_fromArray(
				[
					{loop: settings.loop, offset: $ianmackenzie$elm_units$Quantity$zero, playbackRate: settings.playbackRate, source: source, startAt: settings.startAt, startTime: startTime, volume: 1, volumeTimelines: _List_Nil}
				]);
		default:
			var effect = audio_.a;
			var _v1 = effect.effectType;
			switch (_v1.$) {
				case 'ScaleVolume':
					var scaleVolume_ = _v1.a;
					return A2(
						$elm$core$List$map,
						function (a) {
							return _Utils_update(
								a,
								{volume: scaleVolume_.scaleBy * a.volume});
						},
						$MartinSStewart$elm_audio$Audio$flattenAudio(effect.audio));
				case 'ScaleVolumeAt':
					var volumeAt = _v1.a.volumeAt;
					return A2(
						$elm$core$List$map,
						function (a) {
							return _Utils_update(
								a,
								{
									volumeTimelines: A2($elm$core$List$cons, volumeAt, a.volumeTimelines)
								});
						},
						$MartinSStewart$elm_audio$Audio$flattenAudio(effect.audio));
				default:
					var duration = _v1.a;
					return A2(
						$elm$core$List$map,
						function (a) {
							return _Utils_update(
								a,
								{
									offset: A2($ianmackenzie$elm_units$Quantity$plus, duration, a.offset)
								});
						},
						$MartinSStewart$elm_audio$Audio$flattenAudio(effect.audio));
			}
	}
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $MartinSStewart$elm_audio$Audio$encodeSetLoopConfig = F2(
	function (nodeGroupId, loop) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setLoopConfig')),
					_Utils_Tuple2(
					'loop',
					$MartinSStewart$elm_audio$Audio$encodeLoopConfig(loop))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetPlaybackRate = F2(
	function (nodeGroupId, playbackRate) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setPlaybackRate')),
					_Utils_Tuple2(
					'playbackRate',
					$elm$json$Json$Encode$float(playbackRate))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetVolume = F2(
	function (nodeGroupId, volume) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setVolume')),
					_Utils_Tuple2(
					'volume',
					$elm$json$Json$Encode$float(volume))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetVolumeAt = F2(
	function (nodeGroupId, volumeTimelines_) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setVolumeAt')),
					_Utils_Tuple2(
					'volumeAt',
					A2($elm$json$Json$Encode$list, $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline, volumeTimelines_))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeStopSound = function (nodeGroupId) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'action',
				$elm$json$Json$Encode$string('stopSound')),
				_Utils_Tuple2(
				'nodeGroupId',
				$elm$json$Json$Encode$int(nodeGroupId))
			]));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $MartinSStewart$elm_audio$Audio$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $MartinSStewart$elm_audio$Audio$removeAt = F2(
	function (index, l) {
		if (index < 0) {
			return l;
		} else {
			var tail = $elm$core$List$tail(
				A2($elm$core$List$drop, index, l));
			var head = A2($elm$core$List$take, index, l);
			if (tail.$ === 'Nothing') {
				return l;
			} else {
				var t = tail.a;
				return A2($elm$core$List$append, head, t);
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$updateAudioState = F2(
	function (_v0, _v1) {
		var nodeGroupId = _v0.a;
		var audioGroup = _v0.b;
		var flattenedAudio = _v1.a;
		var audioState = _v1.b;
		var json = _v1.c;
		var validAudio = A2(
			$elm$core$List$filter,
			function (_v7) {
				var a = _v7.b;
				return _Utils_eq(a.source, audioGroup.source) && (_Utils_eq(
					$MartinSStewart$elm_audio$Audio$audioStartTime(a),
					$MartinSStewart$elm_audio$Audio$audioStartTime(audioGroup)) && _Utils_eq(a.startAt, audioGroup.startAt));
			},
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, flattenedAudio));
		var _v2 = A2(
			$MartinSStewart$elm_audio$Audio$find,
			function (_v3) {
				var a = _v3.b;
				return _Utils_eq(a, audioGroup);
			},
			validAudio);
		if (_v2.$ === 'Just') {
			var _v4 = _v2.a;
			var index = _v4.a;
			return _Utils_Tuple3(
				A2($MartinSStewart$elm_audio$Audio$removeAt, index, flattenedAudio),
				audioState,
				json);
		} else {
			if (validAudio.b) {
				var _v6 = validAudio.a;
				var index = _v6.a;
				var a = _v6.b;
				var encodeValue = F2(
					function (getter, encoder) {
						return _Utils_eq(
							getter(audioGroup),
							getter(a)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
							A2(
								encoder,
								nodeGroupId,
								getter(a)));
					});
				var effects = A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(
							encodeValue,
							function ($) {
								return $.volume;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetVolume),
							A2(
							encodeValue,
							function ($) {
								return $.loop;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetLoopConfig),
							A2(
							encodeValue,
							function ($) {
								return $.playbackRate;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetPlaybackRate),
							A2(encodeValue, $MartinSStewart$elm_audio$Audio$volumeTimelines, $MartinSStewart$elm_audio$Audio$encodeSetVolumeAt)
						]));
				return _Utils_Tuple3(
					A2($MartinSStewart$elm_audio$Audio$removeAt, index, flattenedAudio),
					A3($elm$core$Dict$insert, nodeGroupId, a, audioState),
					_Utils_ap(effects, json));
			} else {
				return _Utils_Tuple3(
					flattenedAudio,
					A2($elm$core$Dict$remove, nodeGroupId, audioState),
					A2(
						$elm$core$List$cons,
						$MartinSStewart$elm_audio$Audio$encodeStopSound(nodeGroupId),
						json));
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$diffAudioState = F3(
	function (nodeGroupIdCounter, audioState, newAudio) {
		var _v0 = A3(
			$elm$core$List$foldl,
			$MartinSStewart$elm_audio$Audio$updateAudioState,
			_Utils_Tuple3(
				$MartinSStewart$elm_audio$Audio$flattenAudio(newAudio),
				audioState,
				_List_Nil),
			$elm$core$Dict$toList(audioState));
		var newAudioLeft = _v0.a;
		var newAudioState = _v0.b;
		var json2 = _v0.c;
		var _v1 = A3(
			$elm$core$List$foldl,
			F2(
				function (audioLeft, _v2) {
					var counter = _v2.a;
					var audioState_ = _v2.b;
					var json_ = _v2.c;
					return _Utils_Tuple3(
						counter + 1,
						A3($elm$core$Dict$insert, counter, audioLeft, audioState_),
						A2(
							$elm$core$List$cons,
							A2($MartinSStewart$elm_audio$Audio$encodeStartSound, counter, audioLeft),
							json_));
				}),
			_Utils_Tuple3(nodeGroupIdCounter, newAudioState, json2),
			newAudioLeft);
		var newNodeGroupIdCounter = _v1.a;
		var newAudioState2 = _v1.b;
		var json3 = _v1.c;
		return _Utils_Tuple3(newAudioState2, newNodeGroupIdCounter, json3);
	});
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $MartinSStewart$elm_audio$Audio$encodeAudioLoadRequest = F2(
	function (index, audioLoad) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audioUrl',
					$elm$json$Json$Encode$string(audioLoad.audioUrl)),
					_Utils_Tuple2(
					'requestId',
					$elm$json$Json$Encode$int(index))
				]));
	});
var $MartinSStewart$elm_audio$Audio$flattenAudioCmd = function (audioCmd) {
	if (audioCmd.$ === 'AudioLoadRequest') {
		var data = audioCmd.a;
		return _List_fromArray(
			[data]);
	} else {
		var list = audioCmd.a;
		return $elm$core$List$concat(
			A2($elm$core$List$map, $MartinSStewart$elm_audio$Audio$flattenAudioCmd, list));
	}
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $MartinSStewart$elm_audio$Audio$encodeAudioCmd = F2(
	function (_v0, audioCmd) {
		var model = _v0.a;
		var flattenedAudioCmd = $MartinSStewart$elm_audio$Audio$flattenAudioCmd(audioCmd);
		var newPendingRequests = A2(
			$elm$core$List$indexedMap,
			F2(
				function (index, request) {
					return _Utils_Tuple2(model.requestCount + index, request);
				}),
			flattenedAudioCmd);
		return _Utils_Tuple2(
			$MartinSStewart$elm_audio$Audio$Model(
				_Utils_update(
					model,
					{
						pendingRequests: A2(
							$elm$core$Dict$union,
							model.pendingRequests,
							$elm$core$Dict$fromList(newPendingRequests)),
						requestCount: model.requestCount + $elm$core$List$length(flattenedAudioCmd)
					})),
			A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				A2(
					$elm$core$List$map,
					function (_v1) {
						var index = _v1.a;
						var value = _v1.b;
						return A2($MartinSStewart$elm_audio$Audio$encodeAudioLoadRequest, index, value);
					},
					newPendingRequests)));
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $MartinSStewart$elm_audio$Audio$initHelper = F3(
	function (audioPort, audioFunc, _v0) {
		var model = _v0.a;
		var cmds = _v0.b;
		var audioCmds = _v0.c;
		var _v1 = A3(
			$MartinSStewart$elm_audio$Audio$diffAudioState,
			0,
			$elm$core$Dict$empty,
			A2(
				audioFunc,
				$MartinSStewart$elm_audio$Audio$AudioData(
					{sourceData: $elm$core$Dict$empty}),
				model));
		var audioState = _v1.a;
		var newNodeGroupIdCounter = _v1.b;
		var json = _v1.c;
		var initialModel = $MartinSStewart$elm_audio$Audio$Model(
			{audioState: audioState, nodeGroupIdCounter: newNodeGroupIdCounter, pendingRequests: $elm$core$Dict$empty, requestCount: 0, samplesPerSecond: $elm$core$Maybe$Nothing, sourceData: $elm$core$Dict$empty, userModel: model});
		var _v2 = A2($MartinSStewart$elm_audio$Audio$encodeAudioCmd, initialModel, audioCmds);
		var initialModel2 = _v2.a;
		var audioRequests = _v2.b;
		var portMessage = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audio',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, json)),
					_Utils_Tuple2('audioCmds', audioRequests)
				]));
		return _Utils_Tuple2(
			initialModel2,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $MartinSStewart$elm_audio$Audio$UserMsg, cmds),
						audioPort(portMessage)
					])));
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $MartinSStewart$elm_audio$Audio$FromJSMsg = function (a) {
	return {$: 'FromJSMsg', a: a};
};
var $MartinSStewart$elm_audio$Audio$JsonParseError = function (a) {
	return {$: 'JsonParseError', a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioLoadFailed = function (a) {
	return {$: 'AudioLoadFailed', a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioLoadSuccess = function (a) {
	return {$: 'AudioLoadSuccess', a: a};
};
var $MartinSStewart$elm_audio$Audio$InitAudioContext = function (a) {
	return {$: 'InitAudioContext', a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $MartinSStewart$elm_audio$Audio$BufferId = function (a) {
	return {$: 'BufferId', a: a};
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $MartinSStewart$elm_audio$Audio$decodeBufferId = A2($elm$json$Json$Decode$map, $MartinSStewart$elm_audio$Audio$BufferId, $elm$json$Json$Decode$int);
var $MartinSStewart$elm_audio$Audio$FailedToDecode = {$: 'FailedToDecode'};
var $MartinSStewart$elm_audio$Audio$NetworkError = {$: 'NetworkError'};
var $MartinSStewart$elm_audio$Audio$UnknownError = {$: 'UnknownError'};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $MartinSStewart$elm_audio$Audio$decodeLoadError = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		switch (value) {
			case 'NetworkError':
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$NetworkError);
			case 'MediaDecodeAudioDataUnknownContentType':
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$FailedToDecode);
			case 'DOMException: The buffer passed to decodeAudioData contains an unknown content type.':
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$FailedToDecode);
			default:
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$UnknownError);
		}
	},
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $ianmackenzie$elm_units$Duration$seconds = function (numSeconds) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numSeconds);
};
var $MartinSStewart$elm_audio$Audio$decodeFromJSMsg = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		switch (value) {
			case 0:
				return A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (requestId, error) {
							return $MartinSStewart$elm_audio$Audio$AudioLoadFailed(
								{error: error, requestId: requestId});
						}),
					A2($elm$json$Json$Decode$field, 'requestId', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'error', $MartinSStewart$elm_audio$Audio$decodeLoadError));
			case 1:
				return A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (requestId, bufferId, duration) {
							return $MartinSStewart$elm_audio$Audio$AudioLoadSuccess(
								{
									bufferId: bufferId,
									duration: $ianmackenzie$elm_units$Duration$seconds(duration),
									requestId: requestId
								});
						}),
					A2($elm$json$Json$Decode$field, 'requestId', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'bufferId', $MartinSStewart$elm_audio$Audio$decodeBufferId),
					A2($elm$json$Json$Decode$field, 'durationInSeconds', $elm$json$Json$Decode$float));
			case 2:
				return A2(
					$elm$json$Json$Decode$map,
					function (samplesPerSecond) {
						return $MartinSStewart$elm_audio$Audio$InitAudioContext(
							{samplesPerSecond: samplesPerSecond});
					},
					A2($elm$json$Json$Decode$field, 'samplesPerSecond', $elm$json$Json$Decode$int));
			default:
				return $elm$json$Json$Decode$succeed(
					$MartinSStewart$elm_audio$Audio$JsonParseError(
						{
							error: 'Type ' + ($elm$core$String$fromInt(value) + ' not handled.')
						}));
		}
	},
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $MartinSStewart$elm_audio$Audio$fromJSPortSub = function (json) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $MartinSStewart$elm_audio$Audio$decodeFromJSMsg, json);
	if (_v0.$ === 'Ok') {
		var value = _v0.a;
		return $MartinSStewart$elm_audio$Audio$FromJSMsg(value);
	} else {
		var error = _v0.a;
		return $MartinSStewart$elm_audio$Audio$FromJSMsg(
			$MartinSStewart$elm_audio$Audio$JsonParseError(
				{
					error: $elm$json$Json$Decode$errorToString(error)
				}));
	}
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $MartinSStewart$elm_audio$Audio$subscriptions = F2(
	function (app, _v0) {
		var model = _v0.a;
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					A2(
					$elm$core$Platform$Sub$map,
					$MartinSStewart$elm_audio$Audio$UserMsg,
					A2(
						app.subscriptions,
						$MartinSStewart$elm_audio$Audio$audioData(
							$MartinSStewart$elm_audio$Audio$Model(model)),
						model.userModel)),
					app.audioPort.fromJS($MartinSStewart$elm_audio$Audio$fromJSPortSub)
				]));
	});
var $MartinSStewart$elm_audio$Audio$File = function (a) {
	return {$: 'File', a: a};
};
var $MartinSStewart$elm_audio$Audio$flip = F3(
	function (func, a, b) {
		return A2(func, b, a);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $mgold$elm_nonempty_list$List$Nonempty$head = function (_v0) {
	var x = _v0.a;
	var xs = _v0.b;
	return x;
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $MartinSStewart$elm_audio$Audio$rawBufferId = function (_v0) {
	var bufferId = _v0.a;
	return bufferId;
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $MartinSStewart$elm_audio$Audio$updateHelper = F4(
	function (audioPort, audioFunc, userUpdate, _v0) {
		var model = _v0.a;
		var audioData_ = $MartinSStewart$elm_audio$Audio$audioData(
			$MartinSStewart$elm_audio$Audio$Model(model));
		var _v1 = A2(userUpdate, audioData_, model.userModel);
		var newUserModel = _v1.a;
		var userCmd = _v1.b;
		var audioCmds = _v1.c;
		var _v2 = A3(
			$MartinSStewart$elm_audio$Audio$diffAudioState,
			model.nodeGroupIdCounter,
			model.audioState,
			A2(audioFunc, audioData_, newUserModel));
		var audioState = _v2.a;
		var newNodeGroupIdCounter = _v2.b;
		var json = _v2.c;
		var newModel = $MartinSStewart$elm_audio$Audio$Model(
			_Utils_update(
				model,
				{audioState: audioState, nodeGroupIdCounter: newNodeGroupIdCounter, userModel: newUserModel}));
		var _v3 = A2($MartinSStewart$elm_audio$Audio$encodeAudioCmd, newModel, audioCmds);
		var newModel2 = _v3.a;
		var audioRequests = _v3.b;
		var portMessage = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audio',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, json)),
					_Utils_Tuple2('audioCmds', audioRequests)
				]));
		return _Utils_Tuple2(
			newModel2,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $MartinSStewart$elm_audio$Audio$UserMsg, userCmd),
						audioPort(portMessage)
					])));
	});
var $MartinSStewart$elm_audio$Audio$update = F3(
	function (app, msg, _v0) {
		var model = _v0.a;
		if (msg.$ === 'UserMsg') {
			var userMsg = msg.a;
			return A4(
				$MartinSStewart$elm_audio$Audio$updateHelper,
				app.audioPort.toJS,
				app.audio,
				A2($MartinSStewart$elm_audio$Audio$flip, app.update, userMsg),
				$MartinSStewart$elm_audio$Audio$Model(model));
		} else {
			var response = msg.a;
			switch (response.$) {
				case 'AudioLoadSuccess':
					var requestId = response.a.requestId;
					var bufferId = response.a.bufferId;
					var duration = response.a.duration;
					var _v3 = A2($elm$core$Dict$get, requestId, model.pendingRequests);
					if (_v3.$ === 'Just') {
						var pendingRequest = _v3.a;
						var sourceData = A3(
							$elm$core$Dict$insert,
							$MartinSStewart$elm_audio$Audio$rawBufferId(bufferId),
							{duration: duration},
							model.sourceData);
						var source = $elm$core$Result$Ok(
							$MartinSStewart$elm_audio$Audio$File(
								{bufferId: bufferId}));
						var maybeUserMsg = A2(
							$MartinSStewart$elm_audio$Audio$find,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(source)),
							$mgold$elm_nonempty_list$List$Nonempty$toList(pendingRequest.userMsg));
						if (maybeUserMsg.$ === 'Just') {
							var _v5 = maybeUserMsg.a;
							var userMsg = _v5.b;
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2($MartinSStewart$elm_audio$Audio$flip, app.update, userMsg),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests),
											sourceData: sourceData
										})));
						} else {
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2(
									$MartinSStewart$elm_audio$Audio$flip,
									app.update,
									$mgold$elm_nonempty_list$List$Nonempty$head(pendingRequest.userMsg).b),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests),
											sourceData: sourceData
										})));
						}
					} else {
						return _Utils_Tuple2(
							$MartinSStewart$elm_audio$Audio$Model(model),
							$elm$core$Platform$Cmd$none);
					}
				case 'AudioLoadFailed':
					var requestId = response.a.requestId;
					var error = response.a.error;
					var _v6 = A2($elm$core$Dict$get, requestId, model.pendingRequests);
					if (_v6.$ === 'Just') {
						var pendingRequest = _v6.a;
						var a = $elm$core$Result$Err(error);
						var b = A2(
							$MartinSStewart$elm_audio$Audio$find,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(a)),
							$mgold$elm_nonempty_list$List$Nonempty$toList(pendingRequest.userMsg));
						if (b.$ === 'Just') {
							var _v8 = b.a;
							var userMsg = _v8.b;
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2($MartinSStewart$elm_audio$Audio$flip, app.update, userMsg),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests)
										})));
						} else {
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2(
									$MartinSStewart$elm_audio$Audio$flip,
									app.update,
									$mgold$elm_nonempty_list$List$Nonempty$head(pendingRequest.userMsg).b),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests)
										})));
						}
					} else {
						return _Utils_Tuple2(
							$MartinSStewart$elm_audio$Audio$Model(model),
							$elm$core$Platform$Cmd$none);
					}
				case 'InitAudioContext':
					var samplesPerSecond = response.a.samplesPerSecond;
					return _Utils_Tuple2(
						$MartinSStewart$elm_audio$Audio$Model(
							_Utils_update(
								model,
								{
									samplesPerSecond: $elm$core$Maybe$Just(samplesPerSecond)
								})),
						$elm$core$Platform$Cmd$none);
				default:
					var error = response.a.error;
					return _Utils_Tuple2(
						$MartinSStewart$elm_audio$Audio$Model(model),
						$elm$core$Platform$Cmd$none);
			}
		}
	});
var $ianmackenzie$elm_units$Duration$milliseconds = function (numMilliseconds) {
	return $ianmackenzie$elm_units$Duration$seconds(0.001 * numMilliseconds);
};
var $MartinSStewart$elm_audio$Audio$Effect = function (a) {
	return {$: 'Effect', a: a};
};
var $MartinSStewart$elm_audio$Audio$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $MartinSStewart$elm_audio$Audio$offsetBy = F2(
	function (offset_, audio_) {
		return $MartinSStewart$elm_audio$Audio$Effect(
			{
				audio: audio_,
				effectType: $MartinSStewart$elm_audio$Audio$Offset(offset_)
			});
	});
var $MartinSStewart$elm_audio$Audio$withAudioOffset = function (app) {
	return _Utils_update(
		app,
		{
			audio: F2(
				function (audioData_, model) {
					return A2(
						$MartinSStewart$elm_audio$Audio$offsetBy,
						$ianmackenzie$elm_units$Duration$milliseconds(50),
						A2(app.audio, audioData_, model));
				})
		});
};
var $MartinSStewart$elm_audio$Audio$documentWithAudio = A2(
	$elm$core$Basics$composeR,
	$MartinSStewart$elm_audio$Audio$withAudioOffset,
	function (app) {
		return $elm$browser$Browser$document(
			{
				init: A2(
					$elm$core$Basics$composeR,
					app.init,
					A2($MartinSStewart$elm_audio$Audio$initHelper, app.audioPort.toJS, app.audio)),
				subscriptions: $MartinSStewart$elm_audio$Audio$subscriptions(app),
				update: $MartinSStewart$elm_audio$Audio$update(app),
				view: function (model) {
					var _v0 = A2(
						app.view,
						$MartinSStewart$elm_audio$Audio$audioData(model),
						$MartinSStewart$elm_audio$Audio$getUserModel(model));
					var title = _v0.title;
					var body = _v0.body;
					return {
						body: A2(
							$elm$core$List$map,
							$elm$html$Html$map($MartinSStewart$elm_audio$Audio$UserMsg),
							body),
						title: title
					};
				}
			});
	});
var $author$project$Main$Join = {$: 'Join'};
var $author$project$Main$PlanetGenerated = function (a) {
	return {$: 'PlanetGenerated', a: a};
};
var $author$project$Main$Playing = function (a) {
	return {$: 'Playing', a: a};
};
var $author$project$Main$Resized = function (a) {
	return {$: 'Resized', a: a};
};
var $author$project$Main$SoundLoadingResult = function (a) {
	return {$: 'SoundLoadingResult', a: a};
};
var $author$project$Main$StarsGenerated = function (a) {
	return {$: 'StarsGenerated', a: a};
};
var $lue_bird$elm_xy$Xy$xy = $elm$core$Tuple$pair;
var $lue_bird$elm_xy$Xy$fromSize = function (record) {
	return A2($lue_bird$elm_xy$Xy$xy, record.width, record.height);
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
			});
	});
var $MartinSStewart$elm_audio$Audio$AudioLoadRequest = function (a) {
	return {$: 'AudioLoadRequest', a: a};
};
var $MartinSStewart$elm_audio$Audio$ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage = {$: 'ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage'};
var $MartinSStewart$elm_audio$Audio$enumeratedResults = A2(
	$mgold$elm_nonempty_list$List$Nonempty$Nonempty,
	$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage),
	_Utils_ap(
		_List_fromArray(
			[
				$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$FailedToDecode),
				$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$NetworkError),
				$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$UnknownError)
			]),
		A2(
			$elm$core$List$map,
			function (bufferId) {
				return $elm$core$Result$Ok(
					$MartinSStewart$elm_audio$Audio$File(
						{
							bufferId: $MartinSStewart$elm_audio$Audio$BufferId(bufferId)
						}));
			},
			A2($elm$core$List$range, 0, 1000))));
var $MartinSStewart$elm_audio$Audio$loadAudio = F2(
	function (userMsg, url) {
		return $MartinSStewart$elm_audio$Audio$AudioLoadRequest(
			{
				audioUrl: url,
				userMsg: A2(
					$mgold$elm_nonempty_list$List$Nonempty$map,
					function (results) {
						return _Utils_Tuple2(
							results,
							userMsg(results));
					},
					$MartinSStewart$elm_audio$Audio$enumeratedResults)
			});
	});
var $author$project$Main$Kill = {$: 'Kill'};
var $author$project$Main$Planet = F7(
	function (position, v, r, color, whenHit, tail, deadTails) {
		return {color: color, deadTails: deadTails, position: position, r: r, tail: tail, v: v, whenHit: whenHit};
	});
var $author$project$Main$Split = {$: 'Split'};
var $elm$random$Random$constant = function (value) {
	return $elm$random$Random$Generator(
		function (seed) {
			return _Utils_Tuple2(value, seed);
		});
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$map2 = F3(
	function (func, _v0, _v1) {
		var genA = _v0.a;
		var genB = _v1.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v2 = genA(seed0);
				var a = _v2.a;
				var seed1 = _v2.b;
				var _v3 = genB(seed1);
				var b = _v3.a;
				var seed2 = _v3.b;
				return _Utils_Tuple2(
					A2(func, a, b),
					seed2);
			});
	});
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $lue_bird$elm_xy$Xy$mapXY = F2(
	function (xMap, yMap) {
		return A2($elm$core$Tuple$mapBoth, xMap, yMap);
	});
var $lue_bird$elm_xy$Xy$x = $elm$core$Tuple$first;
var $lue_bird$elm_xy$Xy$y = $elm$core$Tuple$second;
var $lue_bird$elm_xy$Xy$map2 = F3(
	function (combine, a, b) {
		return A3(
			$lue_bird$elm_xy$Xy$mapXY,
			combine(
				$lue_bird$elm_xy$Xy$x(a)),
			combine(
				$lue_bird$elm_xy$Xy$y(a)),
			b);
	});
var $elm$random$Random$map5 = F6(
	function (func, _v0, _v1, _v2, _v3, _v4) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		var genD = _v3.a;
		var genE = _v4.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v5 = genA(seed0);
				var a = _v5.a;
				var seed1 = _v5.b;
				var _v6 = genB(seed1);
				var b = _v6.a;
				var seed2 = _v6.b;
				var _v7 = genC(seed2);
				var c = _v7.a;
				var seed3 = _v7.b;
				var _v8 = genD(seed3);
				var d = _v8.a;
				var seed4 = _v8.b;
				var _v9 = genE(seed4);
				var e = _v9.a;
				var seed5 = _v9.b;
				return _Utils_Tuple2(
					A5(func, a, b, c, d, e),
					seed5);
			});
	});
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$Main$randomAndMap = function (randomAspect) {
	return A2(
		$elm$random$Random$map2,
		F2(
			function (a, f) {
				return f(a);
			}),
		randomAspect);
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$scaleFrom255 = function (c) {
	return c / 255;
};
var $avh4$elm_color$Color$rgb255 = F3(
	function (r, g, b) {
		return A4(
			$avh4$elm_color$Color$RgbaSpace,
			$avh4$elm_color$Color$scaleFrom255(r),
			$avh4$elm_color$Color$scaleFrom255(g),
			$avh4$elm_color$Color$scaleFrom255(b),
			1.0);
	});
var $author$project$Main$randomColor = function () {
	var component = A2($elm$random$Random$int, 0, 255);
	return A4($elm$random$Random$map3, $avh4$elm_color$Color$rgb255, component, component, component);
}();
var $elm$random$Random$addOne = function (value) {
	return _Utils_Tuple2(1, value);
};
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $elm$random$Random$uniform = F2(
	function (value, valueList) {
		return A2(
			$elm$random$Random$weighted,
			$elm$random$Random$addOne(value),
			A2($elm$core$List$map, $elm$random$Random$addOne, valueList));
	});
var $author$project$Main$randomSign = A2(
	$elm$random$Random$map2,
	F2(
		function (f, a) {
			return f(a);
		}),
	A2(
		$elm$random$Random$uniform,
		$elm$core$Basics$identity,
		_List_fromArray(
			[
				function (x) {
				return -x;
			}
			])));
var $author$project$Main$randomPlanet = function (_v0) {
	var awayFrom = _v0.awayFrom;
	var atLeast = _v0.atLeast;
	return A2(
		$author$project$Main$randomAndMap,
		$elm$random$Random$constant(_List_Nil),
		A2(
			$author$project$Main$randomAndMap,
			$elm$random$Random$constant(_List_Nil),
			A6(
				$elm$random$Random$map5,
				$author$project$Main$Planet,
				A2(
					$elm$random$Random$map,
					A2($lue_bird$elm_xy$Xy$map2, $elm$core$Basics$add, awayFrom),
					A3(
						$elm$random$Random$map2,
						$lue_bird$elm_xy$Xy$xy,
						$author$project$Main$randomSign(
							A2($elm$random$Random$float, atLeast, 200)),
						$author$project$Main$randomSign(
							A2($elm$random$Random$float, atLeast, 200)))),
				A3(
					$elm$random$Random$map2,
					$lue_bird$elm_xy$Xy$xy,
					A2($elm$random$Random$float, -2, 2),
					A2($elm$random$Random$float, -2, 2)),
				A2(
					$elm$random$Random$map,
					function (r) {
						return A2($elm$core$Basics$pow, 2, r);
					},
					A2($elm$random$Random$float, 0.33, 1)),
				$author$project$Main$randomColor,
				A2(
					$elm$random$Random$weighted,
					_Utils_Tuple2(3, $author$project$Main$Join),
					_List_fromArray(
						[
							_Utils_Tuple2(2, $author$project$Main$Split),
							_Utils_Tuple2(1, $author$project$Main$Kill)
						])))));
};
var $author$project$Main$Star = F2(
	function (position, r) {
		return {position: position, r: r};
	});
var $lue_bird$elm_xy$Xy$both = function (valueOfBothCoordinates) {
	return A2($lue_bird$elm_xy$Xy$xy, valueOfBothCoordinates, valueOfBothCoordinates);
};
var $author$project$Main$xyRandom = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return A3($elm$random$Random$map2, $lue_bird$elm_xy$Xy$xy, x, y);
};
var $author$project$Main$randomStar = function (_v0) {
	var awayFrom = _v0.awayFrom;
	var atLeast = _v0.atLeast;
	return A3(
		$elm$random$Random$map2,
		$author$project$Main$Star,
		A2(
			$elm$random$Random$map,
			A2($lue_bird$elm_xy$Xy$map2, $elm$core$Basics$add, awayFrom),
			$author$project$Main$xyRandom(
				$lue_bird$elm_xy$Xy$both(
					$author$project$Main$randomSign(
						A2($elm$random$Random$float, atLeast, 200))))),
		A2($elm$random$Random$float, 0.02, 0.44));
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var $lue_bird$elm_xy$Xy$zero = $lue_bird$elm_xy$Xy$both(0);
var $author$project$Main$init = _Utils_Tuple3(
	{
		explosions: _List_Nil,
		gameStage: $author$project$Main$Playing(
			_Utils_Tuple2(
				{
					color: A3($avh4$elm_color$Color$rgb, 1, 1, 0),
					deadTails: _List_Nil,
					position: $lue_bird$elm_xy$Xy$zero,
					r: 2,
					tail: _List_Nil,
					v: $lue_bird$elm_xy$Xy$zero,
					whenHit: $author$project$Main$Join
				},
				_List_Nil)),
		music: $elm$core$Maybe$Nothing,
		pressedKeys: _List_Nil,
		stars: _List_Nil,
		timePlaying: 0,
		windowSize: $lue_bird$elm_xy$Xy$zero
	},
	$elm$core$Platform$Cmd$batch(
		_Utils_ap(
			A2(
				$elm$core$List$repeat,
				9,
				A2(
					$elm$random$Random$generate,
					$author$project$Main$PlanetGenerated,
					$author$project$Main$randomPlanet(
						{atLeast: 70, awayFrom: $lue_bird$elm_xy$Xy$zero}))),
			_List_fromArray(
				[
					A2(
					$elm$random$Random$generate,
					$author$project$Main$StarsGenerated,
					A2(
						$elm$random$Random$list,
						800,
						$author$project$Main$randomStar(
							{atLeast: 0, awayFrom: $lue_bird$elm_xy$Xy$zero}))),
					A2(
					$elm$core$Task$perform,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.viewport;
						},
						A2($elm$core$Basics$composeR, $lue_bird$elm_xy$Xy$fromSize, $author$project$Main$Resized)),
					$elm$browser$Browser$Dom$getViewport)
				]))),
	A2($MartinSStewart$elm_audio$Audio$loadAudio, $author$project$Main$SoundLoadingResult, 'https://cors-anywhere.herokuapp.com/https://freepd.com/music/Wakka%20Wakka.mp3'));
var $author$project$Main$Frame = function (a) {
	return {$: 'Frame', a: a};
};
var $author$project$Main$KeyMsg = function (a) {
	return {$: 'KeyMsg', a: a};
};
var $lue_bird$elm_xy$Xy$map = function (mapBothXAndY) {
	return A2($lue_bird$elm_xy$Xy$mapXY, mapBothXAndY, mapBothXAndY);
};
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $elm$browser$Browser$Events$Window = {$: 'Window'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		$elm$browser$Browser$Events$Window,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $ohanhi$keyboard$Keyboard$Down = function (a) {
	return {$: 'Down', a: a};
};
var $ohanhi$keyboard$Keyboard$Up = function (a) {
	return {$: 'Up', a: a};
};
var $ohanhi$keyboard$Keyboard$RawKey = function (a) {
	return {$: 'RawKey', a: a};
};
var $ohanhi$keyboard$Keyboard$eventKeyDecoder = A2(
	$elm$json$Json$Decode$field,
	'key',
	A2($elm$json$Json$Decode$map, $ohanhi$keyboard$Keyboard$RawKey, $elm$json$Json$Decode$string));
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $ohanhi$keyboard$Keyboard$downs = function (toMsg) {
	return $elm$browser$Browser$Events$onKeyDown(
		A2($elm$json$Json$Decode$map, toMsg, $ohanhi$keyboard$Keyboard$eventKeyDecoder));
};
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $ohanhi$keyboard$Keyboard$ups = function (toMsg) {
	return $elm$browser$Browser$Events$onKeyUp(
		A2($elm$json$Json$Decode$map, toMsg, $ohanhi$keyboard$Keyboard$eventKeyDecoder));
};
var $ohanhi$keyboard$Keyboard$subscriptions = $elm$core$Platform$Sub$batch(
	_List_fromArray(
		[
			$ohanhi$keyboard$Keyboard$downs($ohanhi$keyboard$Keyboard$Down),
			$ohanhi$keyboard$Keyboard$ups($ohanhi$keyboard$Keyboard$Up)
		]));
var $author$project$Main$subscriptions = F2(
	function (_v0, _v1) {
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					$elm$browser$Browser$Events$onResize(
					F2(
						function (w, h) {
							return $author$project$Main$Resized(
								A2(
									$lue_bird$elm_xy$Xy$map,
									$elm$core$Basics$toFloat,
									_Utils_Tuple2(w, h)));
						})),
					$elm$browser$Browser$Events$onAnimationFrame(
					A2($elm$core$Basics$composeR, $elm$time$Time$posixToMillis, $author$project$Main$Frame)),
					A2($elm$core$Platform$Sub$map, $author$project$Main$KeyMsg, $ohanhi$keyboard$Keyboard$subscriptions)
				]));
	});
var $author$project$Main$GameOver = {$: 'GameOver'};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $ohanhi$keyboard$Keyboard$ArrowDown = {$: 'ArrowDown'};
var $ohanhi$keyboard$Keyboard$ArrowLeft = {$: 'ArrowLeft'};
var $ohanhi$keyboard$Keyboard$ArrowRight = {$: 'ArrowRight'};
var $ohanhi$keyboard$Keyboard$ArrowUp = {$: 'ArrowUp'};
var $ohanhi$keyboard$Keyboard$Arrows$boolToInt = function (bool) {
	return bool ? 1 : 0;
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $ohanhi$keyboard$Keyboard$Arrows$arrows = function (keys) {
	var toInt = function (key) {
		return $ohanhi$keyboard$Keyboard$Arrows$boolToInt(
			A2($elm$core$List$member, key, keys));
	};
	var x = toInt($ohanhi$keyboard$Keyboard$ArrowRight) - toInt($ohanhi$keyboard$Keyboard$ArrowLeft);
	var y = toInt($ohanhi$keyboard$Keyboard$ArrowUp) - toInt($ohanhi$keyboard$Keyboard$ArrowDown);
	return {x: x, y: y};
};
var $MartinSStewart$elm_audio$Audio$AudioCmdGroup = function (a) {
	return {$: 'AudioCmdGroup', a: a};
};
var $MartinSStewart$elm_audio$Audio$cmdNone = $MartinSStewart$elm_audio$Audio$AudioCmdGroup(_List_Nil);
var $author$project$Main$difference = $lue_bird$elm_xy$Xy$map2(
	F2(
		function (aC, bC) {
			return bC - aC;
		}));
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $lue_bird$elm_xy$Xy$direction = function (radians) {
	return A3(
		$lue_bird$elm_xy$Xy$mapXY,
		$elm$core$Basics$cos,
		$elm$core$Basics$sin,
		$lue_bird$elm_xy$Xy$both(radians));
};
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$Main$length = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$Basics$sqrt(
		A2($elm$core$Basics$pow, x, 2) + A2($elm$core$Basics$pow, y, 2));
};
var $author$project$Main$distance = F2(
	function (a, b) {
		return $author$project$Main$length(
			A2($author$project$Main$difference, a.position, b.position));
	});
var $turboMaCk$non_empty_list_alias$List$NonEmpty$fromList = function (xs) {
	if (xs.b) {
		var h = xs.a;
		var t = xs.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(h, t));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $turboMaCk$non_empty_list_alias$List$NonEmpty$toList = function (_v0) {
	var h = _v0.a;
	var t = _v0.b;
	return A2($elm$core$List$cons, h, t);
};
var $turboMaCk$non_empty_list_alias$List$NonEmpty$filter = function (f) {
	return A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			$turboMaCk$non_empty_list_alias$List$NonEmpty$fromList,
			$elm$core$List$filter(f)),
		$turboMaCk$non_empty_list_alias$List$NonEmpty$toList);
};
var $lue_bird$elm_xy$Xy$fromXY = function (record) {
	return A2($lue_bird$elm_xy$Xy$xy, record.x, record.y);
};
var $elm$core$Basics$ge = _Utils_ge;
var $turboMaCk$non_empty_list_alias$List$NonEmpty$head = function (_v0) {
	var h = _v0.a;
	return h;
};
var $turboMaCk$non_empty_list_alias$List$NonEmpty$indexedMap = F2(
	function (f, _v0) {
		var h = _v0.a;
		var t = _v0.b;
		return _Utils_Tuple2(
			A2(f, 0, h),
			A2(
				$elm$core$List$indexedMap,
				function (i) {
					return f(i + 1);
				},
				t));
	});
var $turboMaCk$non_empty_list_alias$List$NonEmpty$length = function (_v0) {
	var t = _v0.b;
	return $elm$core$List$length(t) + 1;
};
var $author$project$Main$lengthAtMost = F2(
	function (maximumLength, xy) {
		var length_ = $author$project$Main$length(xy);
		return (_Utils_cmp(length_, maximumLength) > 0) ? A2(
			$lue_bird$elm_xy$Xy$map,
			$elm$core$Basics$mul(maximumLength / length_),
			xy) : xy;
	});
var $turboMaCk$non_empty_list_alias$List$NonEmpty$map = F2(
	function (f, _v0) {
		var h = _v0.a;
		var t = _v0.b;
		return _Utils_Tuple2(
			f(h),
			A2($elm$core$List$map, f, t));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$mapPosition = F2(
	function (map, r) {
		return _Utils_update(
			r,
			{
				position: map(r.position)
			});
	});
var $author$project$Main$mapV = F2(
	function (map, r) {
		return _Utils_update(
			r,
			{
				v: map(r.v)
			});
	});
var $author$project$Main$mass = function (_v0) {
	var r = _v0.r;
	return A2($elm$core$Basics$pow, r, 2.3);
};
var $author$project$Main$massToR = function (m) {
	return A2($elm$core$Basics$pow, m, 1 / 2.3);
};
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $turboMaCk$non_empty_list_alias$List$NonEmpty$maximum = function (ne) {
	var h = ne.a;
	var _v0 = $elm$core$List$maximum(
		$turboMaCk$non_empty_list_alias$List$NonEmpty$toList(ne));
	if (_v0.$ === 'Just') {
		var x = _v0.a;
		return x;
	} else {
		return h;
	}
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$Basics$not = _Basics_not;
var $elm_community$list_extra$List$Extra$removeAt = F2(
	function (index, l) {
		if (index < 0) {
			return l;
		} else {
			var _v0 = A2($elm$core$List$drop, index, l);
			if (!_v0.b) {
				return l;
			} else {
				var rest = _v0.b;
				return _Utils_ap(
					A2($elm$core$List$take, index, l),
					rest);
			}
		}
	});
var $turboMaCk$non_empty_list_alias$List$NonEmpty$sum = A2($elm$core$Basics$composeL, $elm$core$List$sum, $turboMaCk$non_empty_list_alias$List$NonEmpty$toList);
var $elm$core$Basics$atan2 = _Basics_atan2;
var $lue_bird$elm_xy$Xy$toAngle = function (direction_) {
	return A2(
		$elm$core$Basics$atan2,
		$lue_bird$elm_xy$Xy$y(direction_),
		$lue_bird$elm_xy$Xy$x(direction_));
};
var $elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $avh4$elm_color$Color$toRgba = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	return {alpha: a, blue: b, green: g, red: r};
};
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$turns = function (angleInTurns) {
	return (2 * $elm$core$Basics$pi) * angleInTurns;
};
var $ohanhi$keyboard$Keyboard$Backspace = {$: 'Backspace'};
var $ohanhi$keyboard$Keyboard$Clear = {$: 'Clear'};
var $ohanhi$keyboard$Keyboard$Copy = {$: 'Copy'};
var $ohanhi$keyboard$Keyboard$CrSel = {$: 'CrSel'};
var $ohanhi$keyboard$Keyboard$Cut = {$: 'Cut'};
var $ohanhi$keyboard$Keyboard$Delete = {$: 'Delete'};
var $ohanhi$keyboard$Keyboard$EraseEof = {$: 'EraseEof'};
var $ohanhi$keyboard$Keyboard$ExSel = {$: 'ExSel'};
var $ohanhi$keyboard$Keyboard$Insert = {$: 'Insert'};
var $ohanhi$keyboard$Keyboard$Paste = {$: 'Paste'};
var $ohanhi$keyboard$Keyboard$Redo = {$: 'Redo'};
var $ohanhi$keyboard$Keyboard$Undo = {$: 'Undo'};
var $ohanhi$keyboard$Keyboard$editingKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Backspace':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Backspace);
		case 'Clear':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Clear);
		case 'Copy':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Copy);
		case 'CrSel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CrSel);
		case 'Cut':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Cut);
		case 'Delete':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Delete);
		case 'EraseEof':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$EraseEof);
		case 'ExSel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ExSel);
		case 'Insert':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Insert);
		case 'Paste':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Paste);
		case 'Redo':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Redo);
		case 'Undo':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Undo);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$F1 = {$: 'F1'};
var $ohanhi$keyboard$Keyboard$F10 = {$: 'F10'};
var $ohanhi$keyboard$Keyboard$F11 = {$: 'F11'};
var $ohanhi$keyboard$Keyboard$F12 = {$: 'F12'};
var $ohanhi$keyboard$Keyboard$F13 = {$: 'F13'};
var $ohanhi$keyboard$Keyboard$F14 = {$: 'F14'};
var $ohanhi$keyboard$Keyboard$F15 = {$: 'F15'};
var $ohanhi$keyboard$Keyboard$F16 = {$: 'F16'};
var $ohanhi$keyboard$Keyboard$F17 = {$: 'F17'};
var $ohanhi$keyboard$Keyboard$F18 = {$: 'F18'};
var $ohanhi$keyboard$Keyboard$F19 = {$: 'F19'};
var $ohanhi$keyboard$Keyboard$F2 = {$: 'F2'};
var $ohanhi$keyboard$Keyboard$F20 = {$: 'F20'};
var $ohanhi$keyboard$Keyboard$F3 = {$: 'F3'};
var $ohanhi$keyboard$Keyboard$F4 = {$: 'F4'};
var $ohanhi$keyboard$Keyboard$F5 = {$: 'F5'};
var $ohanhi$keyboard$Keyboard$F6 = {$: 'F6'};
var $ohanhi$keyboard$Keyboard$F7 = {$: 'F7'};
var $ohanhi$keyboard$Keyboard$F8 = {$: 'F8'};
var $ohanhi$keyboard$Keyboard$F9 = {$: 'F9'};
var $ohanhi$keyboard$Keyboard$functionKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'F1':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F1);
		case 'F2':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F2);
		case 'F3':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F3);
		case 'F4':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F4);
		case 'F5':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F5);
		case 'F6':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F6);
		case 'F7':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F7);
		case 'F8':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F8);
		case 'F9':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F9);
		case 'F10':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F10);
		case 'F11':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F11);
		case 'F12':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F12);
		case 'F13':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F13);
		case 'F14':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F14);
		case 'F15':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F15);
		case 'F16':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F16);
		case 'F17':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F17);
		case 'F18':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F18);
		case 'F19':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F19);
		case 'F20':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F20);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$ChannelDown = {$: 'ChannelDown'};
var $ohanhi$keyboard$Keyboard$ChannelUp = {$: 'ChannelUp'};
var $ohanhi$keyboard$Keyboard$MediaFastForward = {$: 'MediaFastForward'};
var $ohanhi$keyboard$Keyboard$MediaPause = {$: 'MediaPause'};
var $ohanhi$keyboard$Keyboard$MediaPlay = {$: 'MediaPlay'};
var $ohanhi$keyboard$Keyboard$MediaPlayPause = {$: 'MediaPlayPause'};
var $ohanhi$keyboard$Keyboard$MediaRecord = {$: 'MediaRecord'};
var $ohanhi$keyboard$Keyboard$MediaRewind = {$: 'MediaRewind'};
var $ohanhi$keyboard$Keyboard$MediaStop = {$: 'MediaStop'};
var $ohanhi$keyboard$Keyboard$MediaTrackNext = {$: 'MediaTrackNext'};
var $ohanhi$keyboard$Keyboard$MediaTrackPrevious = {$: 'MediaTrackPrevious'};
var $ohanhi$keyboard$Keyboard$mediaKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'ChannelDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ChannelDown);
		case 'ChannelUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ChannelUp);
		case 'MediaFastForward':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaFastForward);
		case 'MediaPause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPause);
		case 'MediaPlay':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPlay);
		case 'MediaPlayPause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPlayPause);
		case 'MediaRecord':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaRecord);
		case 'MediaRewind':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaRewind);
		case 'MediaStop':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaStop);
		case 'MediaTrackNext':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaTrackNext);
		case 'MediaTrackPrevious':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaTrackPrevious);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Alt = {$: 'Alt'};
var $ohanhi$keyboard$Keyboard$AltGraph = {$: 'AltGraph'};
var $ohanhi$keyboard$Keyboard$CapsLock = {$: 'CapsLock'};
var $ohanhi$keyboard$Keyboard$Control = {$: 'Control'};
var $ohanhi$keyboard$Keyboard$Fn = {$: 'Fn'};
var $ohanhi$keyboard$Keyboard$FnLock = {$: 'FnLock'};
var $ohanhi$keyboard$Keyboard$Hyper = {$: 'Hyper'};
var $ohanhi$keyboard$Keyboard$Meta = {$: 'Meta'};
var $ohanhi$keyboard$Keyboard$NumLock = {$: 'NumLock'};
var $ohanhi$keyboard$Keyboard$ScrollLock = {$: 'ScrollLock'};
var $ohanhi$keyboard$Keyboard$Shift = {$: 'Shift'};
var $ohanhi$keyboard$Keyboard$Super = {$: 'Super'};
var $ohanhi$keyboard$Keyboard$Symbol = {$: 'Symbol'};
var $ohanhi$keyboard$Keyboard$SymbolLock = {$: 'SymbolLock'};
var $ohanhi$keyboard$Keyboard$modifierKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Alt':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Alt);
		case 'AltGraph':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$AltGraph);
		case 'CapsLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CapsLock);
		case 'Control':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Control);
		case 'Fn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Fn);
		case 'FnLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$FnLock);
		case 'Hyper':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Hyper);
		case 'Meta':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Meta);
		case 'NumLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$NumLock);
		case 'ScrollLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ScrollLock);
		case 'Shift':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Shift);
		case 'Super':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Super);
		case 'OS':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Super);
		case 'Symbol':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Symbol);
		case 'SymbolLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$SymbolLock);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$End = {$: 'End'};
var $ohanhi$keyboard$Keyboard$Home = {$: 'Home'};
var $ohanhi$keyboard$Keyboard$PageDown = {$: 'PageDown'};
var $ohanhi$keyboard$Keyboard$PageUp = {$: 'PageUp'};
var $ohanhi$keyboard$Keyboard$navigationKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'ArrowDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowDown);
		case 'ArrowLeft':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'ArrowRight':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowRight);
		case 'ArrowUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowUp);
		case 'Down':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowDown);
		case 'Left':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'Right':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowRight);
		case 'Up':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowUp);
		case 'End':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$End);
		case 'Home':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Home);
		case 'PageDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$PageDown);
		case 'PageUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$PageUp);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$oneOf = F2(
	function (fns, key) {
		oneOf:
		while (true) {
			if (!fns.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var fn = fns.a;
				var rest = fns.b;
				var _v1 = fn(key);
				if (_v1.$ === 'Just') {
					var a = _v1.a;
					return $elm$core$Maybe$Just(a);
				} else {
					var $temp$fns = rest,
						$temp$key = key;
					fns = $temp$fns;
					key = $temp$key;
					continue oneOf;
				}
			}
		}
	});
var $ohanhi$keyboard$Keyboard$AppSwitch = {$: 'AppSwitch'};
var $ohanhi$keyboard$Keyboard$Call = {$: 'Call'};
var $ohanhi$keyboard$Keyboard$Camera = {$: 'Camera'};
var $ohanhi$keyboard$Keyboard$CameraFocus = {$: 'CameraFocus'};
var $ohanhi$keyboard$Keyboard$EndCall = {$: 'EndCall'};
var $ohanhi$keyboard$Keyboard$GoBack = {$: 'GoBack'};
var $ohanhi$keyboard$Keyboard$GoHome = {$: 'GoHome'};
var $ohanhi$keyboard$Keyboard$HeadsetHook = {$: 'HeadsetHook'};
var $ohanhi$keyboard$Keyboard$LastNumberRedial = {$: 'LastNumberRedial'};
var $ohanhi$keyboard$Keyboard$MannerMode = {$: 'MannerMode'};
var $ohanhi$keyboard$Keyboard$Notification = {$: 'Notification'};
var $ohanhi$keyboard$Keyboard$VoiceDial = {$: 'VoiceDial'};
var $ohanhi$keyboard$Keyboard$phoneKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'AppSwitch':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$AppSwitch);
		case 'Call':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Call);
		case 'Camera':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Camera);
		case 'CameraFocus':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CameraFocus);
		case 'EndCall':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$EndCall);
		case 'GoBack':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$GoBack);
		case 'GoHome':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$GoHome);
		case 'HeadsetHook':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$HeadsetHook);
		case 'LastNumberRedial':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$LastNumberRedial);
		case 'Notification':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Notification);
		case 'MannerMode':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MannerMode);
		case 'VoiceDial':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$VoiceDial);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Again = {$: 'Again'};
var $ohanhi$keyboard$Keyboard$Attn = {$: 'Attn'};
var $ohanhi$keyboard$Keyboard$Cancel = {$: 'Cancel'};
var $ohanhi$keyboard$Keyboard$ContextMenu = {$: 'ContextMenu'};
var $ohanhi$keyboard$Keyboard$Escape = {$: 'Escape'};
var $ohanhi$keyboard$Keyboard$Execute = {$: 'Execute'};
var $ohanhi$keyboard$Keyboard$Find = {$: 'Find'};
var $ohanhi$keyboard$Keyboard$Finish = {$: 'Finish'};
var $ohanhi$keyboard$Keyboard$Help = {$: 'Help'};
var $ohanhi$keyboard$Keyboard$Pause = {$: 'Pause'};
var $ohanhi$keyboard$Keyboard$Play = {$: 'Play'};
var $ohanhi$keyboard$Keyboard$Props = {$: 'Props'};
var $ohanhi$keyboard$Keyboard$Select = {$: 'Select'};
var $ohanhi$keyboard$Keyboard$ZoomIn = {$: 'ZoomIn'};
var $ohanhi$keyboard$Keyboard$ZoomOut = {$: 'ZoomOut'};
var $ohanhi$keyboard$Keyboard$uiKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Again':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Again);
		case 'Attn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Attn);
		case 'Cancel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Cancel);
		case 'ContextMenu':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ContextMenu);
		case 'Escape':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Escape);
		case 'Execute':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Execute);
		case 'Find':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Find);
		case 'Finish':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Finish);
		case 'Help':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Help);
		case 'Pause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Pause);
		case 'Play':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Play);
		case 'Props':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Props);
		case 'Select':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Select);
		case 'ZoomIn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ZoomIn);
		case 'ZoomOut':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ZoomOut);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Enter = {$: 'Enter'};
var $ohanhi$keyboard$Keyboard$Spacebar = {$: 'Spacebar'};
var $ohanhi$keyboard$Keyboard$Tab = {$: 'Tab'};
var $ohanhi$keyboard$Keyboard$whitespaceKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Enter':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Enter);
		case 'Tab':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Tab);
		case 'Spacebar':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Spacebar);
		case ' ':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Spacebar);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$anyKeyWith = function (charParser) {
	return $ohanhi$keyboard$Keyboard$oneOf(
		_List_fromArray(
			[$ohanhi$keyboard$Keyboard$whitespaceKey, charParser, $ohanhi$keyboard$Keyboard$modifierKey, $ohanhi$keyboard$Keyboard$navigationKey, $ohanhi$keyboard$Keyboard$editingKey, $ohanhi$keyboard$Keyboard$functionKey, $ohanhi$keyboard$Keyboard$uiKey, $ohanhi$keyboard$Keyboard$phoneKey, $ohanhi$keyboard$Keyboard$mediaKey]));
};
var $ohanhi$keyboard$Keyboard$Character = function (a) {
	return {$: 'Character', a: a};
};
var $elm$core$String$toUpper = _String_toUpper;
var $ohanhi$keyboard$Keyboard$characterKeyUpper = function (_v0) {
	var value = _v0.a;
	return ($elm$core$String$length(value) === 1) ? $elm$core$Maybe$Just(
		$ohanhi$keyboard$Keyboard$Character(
			$elm$core$String$toUpper(value))) : $elm$core$Maybe$Nothing;
};
var $ohanhi$keyboard$Keyboard$anyKeyUpper = $ohanhi$keyboard$Keyboard$anyKeyWith($ohanhi$keyboard$Keyboard$characterKeyUpper);
var $elm$core$Basics$neq = _Utils_notEqual;
var $ohanhi$keyboard$Keyboard$insert = F3(
	function (keyParser, rawKey, list) {
		var _v0 = keyParser(rawKey);
		if (_v0.$ === 'Just') {
			var key = _v0.a;
			return A2(
				$elm$core$List$cons,
				key,
				A2(
					$elm$core$List$filter,
					$elm$core$Basics$neq(key),
					list));
		} else {
			return list;
		}
	});
var $ohanhi$keyboard$Keyboard$remove = F3(
	function (keyParser, rawKey, list) {
		var _v0 = keyParser(rawKey);
		if (_v0.$ === 'Just') {
			var key = _v0.a;
			return A2(
				$elm$core$List$filter,
				$elm$core$Basics$neq(key),
				list);
		} else {
			return list;
		}
	});
var $ohanhi$keyboard$Keyboard$updateWithParser = F3(
	function (keyParser, msg, state) {
		if (msg.$ === 'Down') {
			var key = msg.a;
			return A3($ohanhi$keyboard$Keyboard$insert, keyParser, key, state);
		} else {
			var key = msg.a;
			return A3($ohanhi$keyboard$Keyboard$remove, keyParser, key, state);
		}
	});
var $ohanhi$keyboard$Keyboard$update = $ohanhi$keyboard$Keyboard$updateWithParser($ohanhi$keyboard$Keyboard$anyKeyUpper);
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$update = F2(
	function (_v0, msg) {
		switch (msg.$) {
			case 'Resized':
				var windowSize = msg.a;
				return function (m) {
					return _Utils_Tuple3(
						_Utils_update(
							m,
							{
								windowSize: A2(
									$lue_bird$elm_xy$Xy$map,
									function (c) {
										return c - 3.5;
									},
									windowSize)
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				};
			case 'Frame':
				var millis = msg.a;
				return function (model) {
					var _v2 = model.gameStage;
					if (_v2.$ === 'Playing') {
						var _v3 = _v2.a;
						var player_ = _v3.a;
						var nonPlayerPlanets = _v3.b;
						var updateTail = $turboMaCk$non_empty_list_alias$List$NonEmpty$map(
							function (planet) {
								return _Utils_update(
									planet,
									{
										tail: A2(
											$elm$core$List$take,
											172,
											A2($elm$core$List$cons, planet.position, planet.tail))
									});
							});
						var updatePlanet = function (planet) {
							var cappedV = A2($author$project$Main$lengthAtMost, 2, planet.v);
							return A2(
								$author$project$Main$mapPosition,
								A2($lue_bird$elm_xy$Xy$map2, $elm$core$Basics$add, cappedV),
								_Utils_update(
									planet,
									{v: cappedV}));
						};
						var planetsWithGravity = function (planets) {
							return A2(
								$turboMaCk$non_empty_list_alias$List$NonEmpty$indexedMap,
								F2(
									function (i, planet) {
										return A3(
											$elm$core$List$foldl,
											function (other) {
												return $author$project$Main$mapV(
													A2(
														$lue_bird$elm_xy$Xy$map2,
														$elm$core$Basics$add,
														A2(
															$author$project$Main$lengthAtMost,
															0.2,
															A2(
																$lue_bird$elm_xy$Xy$map,
																$elm$core$Basics$mul(
																	($author$project$Main$mass(other) * 0.4) / A2(
																		$elm$core$Basics$pow,
																		A2($author$project$Main$distance, planet, other),
																		1)),
																$lue_bird$elm_xy$Xy$direction(
																	$lue_bird$elm_xy$Xy$toAngle(
																		A2($author$project$Main$difference, planet.position, other.position)))))));
											},
											planet,
											A2(
												$elm_community$list_extra$List$Extra$removeAt,
												i,
												$turboMaCk$non_empty_list_alias$List$NonEmpty$toList(planets)));
									}),
								planets);
						};
						var overlap = F2(
							function (planet, other) {
								return _Utils_cmp(
									A2($author$project$Main$distance, planet, other),
									A2($elm$core$Basics$max, planet.r, other.r)) < 0;
							});
						var overlaps = function (_v4) {
							var planet = _v4.a;
							var others = _v4.b;
							return _Utils_ap(
								function () {
									var _v5 = A2(
										$elm$core$List$filter,
										A2(
											$elm$core$Basics$composeL,
											$elm$core$Basics$not,
											overlap(planet)),
										others);
									if (_v5.b) {
										var head = _v5.a;
										var tail = _v5.b;
										return overlaps(
											_Utils_Tuple2(head, tail));
									} else {
										return _List_Nil;
									}
								}(),
								A2(
									$elm$core$List$filter,
									overlap(planet),
									others));
						};
						var collide = function (planets) {
							var planet = $turboMaCk$non_empty_list_alias$List$NonEmpty$head(planets);
							return function (overlapping) {
								return A2(
									$elm$core$Maybe$withDefault,
									overlapping,
									A2(
										$elm$core$Maybe$map,
										A2(
											$elm$core$Basics$composeR,
											collide,
											function (planets_) {
												return _Utils_ap(overlapping, planets_);
											}),
										A2(
											$turboMaCk$non_empty_list_alias$List$NonEmpty$filter,
											A2(
												$elm$core$Basics$composeL,
												$elm$core$Basics$not,
												overlap(planet)),
											planets)));
							}(
								A2(
									$elm$core$Maybe$withDefault,
									_List_fromArray(
										[planet]),
									A2(
										$elm$core$Maybe$map,
										function (overlapping) {
											var sum = function (aspect) {
												return A2(
													$elm$core$Basics$composeR,
													$elm$core$List$map(aspect),
													$elm$core$List$sum);
											};
											var splitters = A2(
												$elm$core$List$filter,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.whenHit;
													},
													$elm$core$Basics$eq($author$project$Main$Split)),
												$turboMaCk$non_empty_list_alias$List$NonEmpty$toList(overlapping));
											var splitCount = $elm$core$List$length(splitters) + 1;
											var killerCount = $elm$core$List$length(
												A2(
													$elm$core$List$filter,
													A2(
														$elm$core$Basics$composeR,
														function ($) {
															return $.whenHit;
														},
														$elm$core$Basics$eq($author$project$Main$Kill)),
													$turboMaCk$non_empty_list_alias$List$NonEmpty$toList(overlapping)));
											var joiners = A2(
												$elm$core$List$filter,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.whenHit;
													},
													$elm$core$Basics$eq($author$project$Main$Join)),
												$turboMaCk$non_empty_list_alias$List$NonEmpty$toList(overlapping));
											var componentAvg = function (component) {
												return $turboMaCk$non_empty_list_alias$List$NonEmpty$sum(
													A2(
														$turboMaCk$non_empty_list_alias$List$NonEmpty$map,
														function (_v7) {
															var r = _v7.r;
															var color = _v7.color;
															return r * component(
																$avh4$elm_color$Color$toRgba(color));
														},
														overlapping)) / A2(
													sum,
													function ($) {
														return $.r;
													},
													joiners);
											};
											var biggestR = $turboMaCk$non_empty_list_alias$List$NonEmpty$maximum(
												A2(
													$turboMaCk$non_empty_list_alias$List$NonEmpty$map,
													function ($) {
														return $.r;
													},
													overlapping));
											var biggest = A2(
												$elm$core$Maybe$withDefault,
												planet,
												A2(
													$elm$core$Maybe$map,
													$turboMaCk$non_empty_list_alias$List$NonEmpty$head,
													A2(
														$turboMaCk$non_empty_list_alias$List$NonEmpty$filter,
														A2(
															$elm$core$Basics$composeR,
															function ($) {
																return $.r;
															},
															$elm$core$Basics$eq(biggestR)),
														overlapping)));
											var splitPlanet = function (_v6) {
												var v = _v6.v;
												var position = _v6.position;
												return {
													color: A3(
														$avh4$elm_color$Color$rgb,
														componentAvg(
															function ($) {
																return $.red;
															}),
														componentAvg(
															function ($) {
																return $.green;
															}),
														componentAvg(
															function ($) {
																return $.blue;
															})),
													deadTails: biggest.deadTails,
													position: position,
													r: $author$project$Main$massToR(
														A2(sum, $author$project$Main$mass, joiners) / splitCount),
													tail: biggest.tail,
													v: v,
													whenHit: $author$project$Main$Join
												};
											};
											var avg = F2(
												function (aspect, list) {
													return A2(sum, aspect, list) / $elm$core$List$length(list);
												});
											return (killerCount > 0) ? _List_Nil : ((splitCount === 1) ? _List_fromArray(
												[planet]) : A2(
												$elm$core$List$map,
												function (i) {
													return splitPlanet(
														{
															position: A3(
																$lue_bird$elm_xy$Xy$map2,
																$elm$core$Basics$add,
																A2(
																	$lue_bird$elm_xy$Xy$map,
																	$elm$core$Basics$mul(
																		A2(
																			sum,
																			function ($) {
																				return $.r;
																			},
																			joiners)),
																	$lue_bird$elm_xy$Xy$direction(
																		$elm$core$Basics$turns((i + 1) / splitCount))),
																biggest.position),
															v: function () {
																var splitAngle = $lue_bird$elm_xy$Xy$toAngle(
																	A2(
																		$lue_bird$elm_xy$Xy$map,
																		function (aspect) {
																			return A2(avg, aspect, splitters);
																		},
																		_Utils_Tuple2(
																			A2(
																				$elm$core$Basics$composeR,
																				function ($) {
																					return $.v;
																				},
																				$lue_bird$elm_xy$Xy$x),
																			A2(
																				$elm$core$Basics$composeR,
																				function ($) {
																					return $.v;
																				},
																				$lue_bird$elm_xy$Xy$y))));
																return A2(
																	$lue_bird$elm_xy$Xy$map,
																	$elm$core$Basics$mul(5.3),
																	$lue_bird$elm_xy$Xy$direction(
																		(splitAngle + $elm$core$Basics$turns(1 / 4)) + $elm$core$Basics$turns((i + 1) / splitCount)));
															}()
														});
												},
												A2($elm$core$List$range, 0, splitCount - 1)));
										},
										A2(
											$elm$core$Maybe$andThen,
											function (overlapping) {
												return ($turboMaCk$non_empty_list_alias$List$NonEmpty$length(overlapping) >= 2) ? $elm$core$Maybe$Just(overlapping) : $elm$core$Maybe$Nothing;
											},
											A2(
												$turboMaCk$non_empty_list_alias$List$NonEmpty$filter,
												overlap(planet),
												planets)))));
						};
						var arrows = A2(
							$lue_bird$elm_xy$Xy$map,
							$elm$core$Basics$toFloat,
							$lue_bird$elm_xy$Xy$fromXY(
								$ohanhi$keyboard$Keyboard$Arrows$arrows(model.pressedKeys)));
						var planetsUpdatedPlayer = _Utils_Tuple2(
							A2(
								$author$project$Main$mapV,
								A2(
									$lue_bird$elm_xy$Xy$map2,
									$elm$core$Basics$add,
									A2(
										$lue_bird$elm_xy$Xy$map,
										$elm$core$Basics$mul(1.2),
										arrows)),
								player_),
							nonPlayerPlanets);
						var planetsMoved = A2(
							$turboMaCk$non_empty_list_alias$List$NonEmpty$map,
							updatePlanet,
							planetsWithGravity(planetsUpdatedPlayer));
						var newExplosions = A2(
							$elm$core$List$map,
							function (_v12) {
								var r = _v12.r;
								var position = _v12.position;
								var color = _v12.color;
								return {color: color, position: position, r: r};
							},
							overlaps(planetsMoved));
						var _v8 = planetsMoved;
						var movedPlayer = _v8.a;
						var movedPlanets = _v8.b;
						var collidedPlanets = collide(
							updateTail(
								_Utils_Tuple2(
									movedPlayer,
									A2(
										$elm$core$List$filter,
										function (planet) {
											return A2($author$project$Main$distance, planet, movedPlayer) < 400;
										},
										movedPlanets))));
						return _Utils_Tuple3(
							_Utils_update(
								model,
								{
									explosions: _Utils_ap(
										newExplosions,
										A2(
											$elm$core$List$filter,
											function (_v9) {
												var r = _v9.r;
												return r < 120;
											},
											A2(
												$elm$core$List$map,
												function (ex) {
													return _Utils_update(
														ex,
														{r: ex.r + 3});
												},
												model.explosions))),
									gameStage: function () {
										if (collidedPlanets.b) {
											var head = collidedPlanets.a;
											var tail = collidedPlanets.b;
											return $author$project$Main$Playing(
												_Utils_Tuple2(head, tail));
										} else {
											return $author$project$Main$GameOver;
										}
									}(),
									timePlaying: model.timePlaying + 1
								}),
							function () {
								if (collidedPlanets.b) {
									var player__ = collidedPlanets.a;
									return (!A2($elm$core$Basics$modBy, 17, millis)) ? A2(
										$elm$random$Random$generate,
										$author$project$Main$PlanetGenerated,
										$author$project$Main$randomPlanet(
											{atLeast: 120, awayFrom: player__.position})) : ((!A2($elm$core$Basics$modBy, 23, millis)) ? A2(
										$elm$random$Random$generate,
										$author$project$Main$StarsGenerated,
										A2(
											$elm$random$Random$list,
											540,
											$author$project$Main$randomStar(
												{atLeast: 120, awayFrom: player__.position}))) : $elm$core$Platform$Cmd$none);
								} else {
									return $elm$core$Platform$Cmd$none;
								}
							}(),
							$MartinSStewart$elm_audio$Audio$cmdNone);
					} else {
						return _Utils_Tuple3(
							_Utils_update(
								model,
								{timePlaying: model.timePlaying + 1}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone);
					}
				};
			case 'KeyMsg':
				var keyMsg = msg.a;
				return function (model) {
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								pressedKeys: A2($ohanhi$keyboard$Keyboard$update, keyMsg, model.pressedKeys)
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				};
			case 'PlanetGenerated':
				var planet = msg.a;
				return function (model) {
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								gameStage: function () {
									var _v13 = model.gameStage;
									if (_v13.$ === 'Playing') {
										var _v14 = _v13.a;
										var player_ = _v14.a;
										var tail = _v14.b;
										return $author$project$Main$Playing(
											_Utils_Tuple2(
												player_,
												A2($elm$core$List$cons, planet, tail)));
									} else {
										return $author$project$Main$GameOver;
									}
								}()
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				};
			case 'StarsGenerated':
				var newStars = msg.a;
				return function (model) {
					return _Utils_Tuple3(
						function () {
							var _v15 = model.gameStage;
							if (_v15.$ === 'Playing') {
								var _v16 = _v15.a;
								var player_ = _v16.a;
								return _Utils_update(
									model,
									{
										stars: _Utils_ap(
											A2(
												$elm$core$List$filter,
												function (star) {
													return A2($author$project$Main$distance, star, player_) < 120;
												},
												model.stars),
											newStars)
									});
							} else {
								return model;
							}
						}(),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				};
			default:
				var result = msg.a;
				return function (m) {
					return _Utils_Tuple3(
						_Utils_update(
							m,
							{
								music: $elm$core$Result$toMaybe(result)
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				};
		}
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $timjs$elm_collage$Collage$shift = F2(
	function (_v0, collage) {
		var dx = _v0.a;
		var dy = _v0.b;
		var _v1 = collage.shift;
		var x = _v1.a;
		var y = _v1.b;
		return _Utils_update(
			collage,
			{
				shift: _Utils_Tuple2(x + dx, y + dy)
			});
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $timjs$elm_collage$Collage$Core$Group = function (a) {
	return {$: 'Group', a: a};
};
var $timjs$elm_collage$Collage$Core$Path = F2(
	function (a, b) {
		return {$: 'Path', a: a, b: b};
	});
var $timjs$elm_collage$Collage$Core$Uniform = function (a) {
	return {$: 'Uniform', a: a};
};
var $timjs$elm_collage$Collage$Render$decodeCap = function (cap) {
	switch (cap.$) {
		case 'Round':
			return 'round';
		case 'Padded':
			return 'square';
		default:
			return 'butt';
	}
};
var $timjs$elm_collage$Collage$Render$decodeDashing = function (ds) {
	var decodeOnOff = function (_v0) {
		var x = _v0.a;
		var y = _v0.b;
		return A2(
			$elm$core$String$join,
			',',
			_List_fromArray(
				[
					$elm$core$String$fromInt(x),
					$elm$core$String$fromInt(y)
				]));
	};
	return A2(
		$elm$core$String$join,
		' ',
		A2($elm$core$List$map, decodeOnOff, ds));
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $timjs$elm_collage$Collage$Render$decodeColor = function (c) {
	var _v0 = $avh4$elm_color$Color$toRgba(c);
	var red = _v0.red;
	var green = _v0.green;
	var blue = _v0.blue;
	return $avh4$elm_color$Color$toCssString(
		A3($avh4$elm_color$Color$rgb, red, green, blue));
};
var $timjs$elm_collage$Collage$Render$decodeFill = function (fs) {
	if (fs.$ === 'Uniform') {
		var c = fs.a;
		return $timjs$elm_collage$Collage$Render$decodeColor(c);
	} else {
		return 'none';
	}
};
var $timjs$elm_collage$Collage$Render$decodeOpacity = function (c) {
	var _v0 = $avh4$elm_color$Color$toRgba(c);
	var alpha = _v0.alpha;
	return $elm$core$String$fromFloat(alpha);
};
var $timjs$elm_collage$Collage$Render$decodeFillOpacity = function (fs) {
	if (fs.$ === 'Uniform') {
		var c = fs.a;
		return $timjs$elm_collage$Collage$Render$decodeOpacity(c);
	} else {
		return '0';
	}
};
var $timjs$elm_collage$Collage$Render$decodeJoin = function (join) {
	switch (join.$) {
		case 'Smooth':
			return 'round';
		case 'Sharp':
			return 'miter';
		default:
			return 'bevel';
	}
};
var $timjs$elm_collage$Collage$Render$decodeTransform = function (collage) {
	var sy = $elm$core$String$fromFloat(collage.scale.b);
	var sx = $elm$core$String$fromFloat(collage.scale.a);
	var r = $elm$core$String$fromFloat((((-collage.rotation) / 2) / $elm$core$Basics$pi) * 360);
	var dy = $elm$core$String$fromFloat(-collage.shift.b);
	var dx = $elm$core$String$fromFloat(collage.shift.a);
	return $elm$core$String$concat(
		_List_fromArray(
			['translate(', dx, ',', dy, ') scale(', sx, ',', sy, ') rotate(', r, ')']));
};
var $elm$svg$Svg$Attributes$dominantBaseline = _VirtualDom_attribute('dominant-baseline');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $elm$svg$Svg$Attributes$fontFamily = _VirtualDom_attribute('font-family');
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var $elm$svg$Svg$Attributes$fontStyle = _VirtualDom_attribute('font-style');
var $elm$svg$Svg$Attributes$fontVariant = _VirtualDom_attribute('font-variant');
var $elm$svg$Svg$Attributes$fontWeight = _VirtualDom_attribute('font-weight');
var $elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $elm$svg$Svg$Attributes$strokeDashoffset = _VirtualDom_attribute('stroke-dashoffset');
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var $elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$Attributes$textDecoration = _VirtualDom_attribute('text-decoration');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $timjs$elm_collage$Collage$Render$attrs = function (collage) {
	var _v0 = collage.basic;
	switch (_v0.$) {
		case 'Path':
			var line = _v0.a;
			return _List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke(
					$timjs$elm_collage$Collage$Render$decodeFill(line.fill)),
					$elm$svg$Svg$Attributes$strokeOpacity(
					$timjs$elm_collage$Collage$Render$decodeFillOpacity(line.fill)),
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(line.thickness)),
					$elm$svg$Svg$Attributes$strokeLinecap(
					$timjs$elm_collage$Collage$Render$decodeCap(line.cap)),
					$elm$svg$Svg$Attributes$strokeLinejoin(
					$timjs$elm_collage$Collage$Render$decodeJoin(line.join)),
					$elm$svg$Svg$Attributes$fill('none'),
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(collage.opacity)),
					$elm$svg$Svg$Attributes$transform(
					$timjs$elm_collage$Collage$Render$decodeTransform(collage)),
					$elm$svg$Svg$Attributes$strokeDashoffset(
					$elm$core$String$fromInt(line.dashPhase)),
					$elm$svg$Svg$Attributes$strokeDasharray(
					$timjs$elm_collage$Collage$Render$decodeDashing(line.dashPattern))
				]);
		case 'Shape':
			var _v1 = _v0.a;
			var fill = _v1.a;
			var line = _v1.b;
			return _List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill(
					$timjs$elm_collage$Collage$Render$decodeFill(fill)),
					$elm$svg$Svg$Attributes$fillOpacity(
					$timjs$elm_collage$Collage$Render$decodeFillOpacity(fill)),
					$elm$svg$Svg$Attributes$stroke(
					$timjs$elm_collage$Collage$Render$decodeFill(line.fill)),
					$elm$svg$Svg$Attributes$strokeOpacity(
					$timjs$elm_collage$Collage$Render$decodeFillOpacity(line.fill)),
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(line.thickness)),
					$elm$svg$Svg$Attributes$strokeLinecap(
					$timjs$elm_collage$Collage$Render$decodeCap(line.cap)),
					$elm$svg$Svg$Attributes$strokeLinejoin(
					$timjs$elm_collage$Collage$Render$decodeJoin(line.join)),
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(collage.opacity)),
					$elm$svg$Svg$Attributes$transform(
					$timjs$elm_collage$Collage$Render$decodeTransform(collage)),
					$elm$svg$Svg$Attributes$strokeDashoffset(
					$elm$core$String$fromInt(line.dashPhase)),
					$elm$svg$Svg$Attributes$strokeDasharray(
					$timjs$elm_collage$Collage$Render$decodeDashing(line.dashPattern))
				]);
		case 'Text':
			var _v2 = _v0.b;
			var style = _v2.a;
			var str = _v2.b;
			return _List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill(
					$timjs$elm_collage$Collage$Render$decodeFill(
						$timjs$elm_collage$Collage$Core$Uniform(style.color))),
					$elm$svg$Svg$Attributes$fontFamily(
					function () {
						var _v3 = style.typeface;
						switch (_v3.$) {
							case 'Serif':
								return 'serif';
							case 'Sansserif':
								return 'sans-serif';
							case 'Monospace':
								return 'monospace';
							default:
								var name = _v3.a;
								return name;
						}
					}()),
					$elm$svg$Svg$Attributes$fontSize(
					$elm$core$String$fromInt(style.size)),
					$elm$svg$Svg$Attributes$fontWeight(
					function () {
						var _v4 = style.weight;
						switch (_v4.$) {
							case 'Thin':
								return '200';
							case 'Light':
								return '300';
							case 'Regular':
								return 'normal';
							case 'Medium':
								return '500';
							case 'SemiBold':
								return '600';
							case 'Bold':
								return 'bold';
							default:
								return '800';
						}
					}()),
					$elm$svg$Svg$Attributes$fontStyle(
					function () {
						var _v5 = style.shape;
						switch (_v5.$) {
							case 'Upright':
								return 'normal';
							case 'SmallCaps':
								return 'normal';
							case 'Slanted':
								return 'oblique';
							default:
								return 'italic';
						}
					}()),
					$elm$svg$Svg$Attributes$fontVariant(
					function () {
						var _v6 = style.shape;
						if (_v6.$ === 'SmallCaps') {
							return 'small-caps';
						} else {
							return 'normal';
						}
					}()),
					$elm$svg$Svg$Attributes$textDecoration(
					function () {
						var _v7 = style.line;
						switch (_v7.$) {
							case 'None':
								return 'none';
							case 'Under':
								return 'underline';
							case 'Over':
								return 'overline';
							default:
								return 'line-through';
						}
					}()),
					$elm$svg$Svg$Attributes$textAnchor('middle'),
					$elm$svg$Svg$Attributes$dominantBaseline('middle'),
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(collage.opacity)),
					$elm$svg$Svg$Attributes$transform(
					$timjs$elm_collage$Collage$Render$decodeTransform(collage))
				]);
		default:
			return _List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(collage.opacity)),
					$elm$svg$Svg$Attributes$transform(
					$timjs$elm_collage$Collage$Render$decodeTransform(collage))
				]);
	}
};
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $timjs$elm_collage$Collage$Render$box = F2(
	function (w, h) {
		return _List_fromArray(
			[
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromFloat(w)),
				$elm$svg$Svg$Attributes$height(
				$elm$core$String$fromFloat(h)),
				$elm$svg$Svg$Attributes$x(
				$elm$core$String$fromFloat((-w) / 2)),
				$elm$svg$Svg$Attributes$y(
				$elm$core$String$fromFloat((-h) / 2))
			]);
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $timjs$elm_collage$Collage$Render$decodePoints = function (ps) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$map,
			function (_v0) {
				var x = _v0.a;
				var y = _v0.b;
				return A2(
					$elm$core$String$join,
					',',
					_List_fromArray(
						[
							$elm$core$String$fromFloat(x),
							$elm$core$String$fromFloat(-y)
						]));
			},
			ps));
};
var $elm$svg$Svg$ellipse = $elm$svg$Svg$trustedNode('ellipse');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$svg$Svg$Events$on = $elm$html$Html$Events$on;
var $elm_community$basics_extra$Basics$Extra$uncurry = F2(
	function (f, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		return A2(f, a, b);
	});
var $timjs$elm_collage$Collage$Render$events = function (handlers) {
	return A2(
		$elm$core$List$map,
		$elm_community$basics_extra$Basics$Extra$uncurry($elm$svg$Svg$Events$on),
		handlers);
};
var $elm$svg$Svg$foreignObject = $elm$svg$Svg$trustedNode('foreignObject');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var $elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $timjs$elm_collage$Collage$Render$render = function (collage) {
	render:
	while (true) {
		var name = A2($elm$core$Maybe$withDefault, '_unnamed_', collage.name);
		var _v0 = collage.basic;
		switch (_v0.$) {
			case 'Path':
				var style = _v0.a;
				var path = _v0.b;
				var ps = path.a;
				return A2(
					$elm$svg$Svg$polyline,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id(name),
								$elm$svg$Svg$Attributes$points(
								$timjs$elm_collage$Collage$Render$decodePoints(ps))
							]),
						_Utils_ap(
							$timjs$elm_collage$Collage$Render$attrs(collage),
							$timjs$elm_collage$Collage$Render$events(collage.handlers))),
					_List_Nil);
			case 'Shape':
				var _v2 = _v0.a;
				var fill = _v2.a;
				var line = _v2.b;
				var shape = _v0.b;
				switch (shape.$) {
					case 'Polygon':
						var ps = shape.a;
						return A2(
							$elm$svg$Svg$polygon,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$id(name),
										$elm$svg$Svg$Attributes$points(
										$timjs$elm_collage$Collage$Render$decodePoints(ps))
									]),
								_Utils_ap(
									$timjs$elm_collage$Collage$Render$attrs(collage),
									$timjs$elm_collage$Collage$Render$events(collage.handlers))),
							_List_Nil);
					case 'Circle':
						var r = shape.a;
						return A2(
							$elm$svg$Svg$circle,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$id(name),
										$elm$svg$Svg$Attributes$r(
										$elm$core$String$fromFloat(r))
									]),
								_Utils_ap(
									$timjs$elm_collage$Collage$Render$attrs(collage),
									$timjs$elm_collage$Collage$Render$events(collage.handlers))),
							_List_Nil);
					case 'Ellipse':
						var rx = shape.a;
						var ry = shape.b;
						return A2(
							$elm$svg$Svg$ellipse,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$id(name),
										$elm$svg$Svg$Attributes$rx(
										$elm$core$String$fromFloat(rx)),
										$elm$svg$Svg$Attributes$ry(
										$elm$core$String$fromFloat(ry))
									]),
								_Utils_ap(
									$timjs$elm_collage$Collage$Render$attrs(collage),
									$timjs$elm_collage$Collage$Render$events(collage.handlers))),
							_List_Nil);
					case 'Rectangle':
						var w = shape.a;
						var h = shape.b;
						var r = shape.c;
						return A2(
							$elm$svg$Svg$rect,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$id(name),
										$elm$svg$Svg$Attributes$rx(
										$elm$core$String$fromFloat(r)),
										$elm$svg$Svg$Attributes$ry(
										$elm$core$String$fromFloat(r))
									]),
								_Utils_ap(
									A2($timjs$elm_collage$Collage$Render$box, w, h),
									_Utils_ap(
										$timjs$elm_collage$Collage$Render$attrs(collage),
										$timjs$elm_collage$Collage$Render$events(collage.handlers)))),
							_List_Nil);
					default:
						var path = shape.a;
						var $temp$collage = _Utils_update(
							collage,
							{
								basic: A2($timjs$elm_collage$Collage$Core$Path, line, path)
							});
						collage = $temp$collage;
						continue render;
				}
			case 'Text':
				var _v4 = _v0.b;
				var style = _v4.a;
				var str = _v4.b;
				return A2(
					$elm$svg$Svg$text_,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id(name)
							]),
						_Utils_ap(
							$timjs$elm_collage$Collage$Render$attrs(collage),
							$timjs$elm_collage$Collage$Render$events(collage.handlers))),
					_List_fromArray(
						[
							$elm$svg$Svg$text(str)
						]));
			case 'Image':
				var _v5 = _v0.a;
				var w = _v5.a;
				var h = _v5.b;
				var url = _v0.b;
				return A2(
					$elm$svg$Svg$image,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id(name),
								$elm$svg$Svg$Attributes$xlinkHref(url)
							]),
						_Utils_ap(
							A2($timjs$elm_collage$Collage$Render$box, w, h),
							_Utils_ap(
								$timjs$elm_collage$Collage$Render$attrs(collage),
								$timjs$elm_collage$Collage$Render$events(collage.handlers)))),
					_List_Nil);
			case 'Html':
				var _v6 = _v0.a;
				var w = _v6.a;
				var h = _v6.b;
				var extraAttrs = _v0.b;
				var html = _v0.c;
				return A2(
					$elm$svg$Svg$foreignObject,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id(name)
							]),
						_Utils_ap(
							A2($timjs$elm_collage$Collage$Render$box, w, h),
							_Utils_ap(
								$timjs$elm_collage$Collage$Render$attrs(collage),
								_Utils_ap(
									$timjs$elm_collage$Collage$Render$events(collage.handlers),
									extraAttrs)))),
					_List_fromArray(
						[html]));
			case 'Group':
				var collages = _v0.a;
				return A2(
					$elm$svg$Svg$g,
					A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$id(name),
						_Utils_ap(
							$timjs$elm_collage$Collage$Render$attrs(collage),
							$timjs$elm_collage$Collage$Render$events(collage.handlers))),
					A3(
						$elm$core$List$foldl,
						F2(
							function (col, res) {
								return A2(
									$elm$core$List$cons,
									$timjs$elm_collage$Collage$Render$render(col),
									res);
							}),
						_List_Nil,
						collages));
			default:
				var fore = _v0.a;
				var back = _v0.b;
				var $temp$collage = _Utils_update(
					collage,
					{
						basic: $timjs$elm_collage$Collage$Core$Group(
							_List_fromArray(
								[fore, back]))
					});
				collage = $temp$collage;
				continue render;
		}
	}
};
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$version = _VirtualDom_attribute('version');
var $timjs$elm_collage$Collage$Render$svgAbsolute = F2(
	function (_v0, collage) {
		var width = _v0.a;
		var height = _v0.b;
		var w = $elm$core$String$fromFloat(width);
		var h = $elm$core$String$fromFloat(height);
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$svg,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$width(w),
							$elm$svg$Svg$Attributes$height(h),
							$elm$svg$Svg$Attributes$version('1.1')
						]),
					_List_fromArray(
						[
							$timjs$elm_collage$Collage$Render$render(collage)
						]))
				]));
	});
var $timjs$elm_collage$Collage$Render$svgBox = F2(
	function (_v0, collage) {
		var width = _v0.a;
		var height = _v0.b;
		return A2(
			$timjs$elm_collage$Collage$Render$svgAbsolute,
			_Utils_Tuple2(width, height),
			A2(
				$timjs$elm_collage$Collage$shift,
				_Utils_Tuple2(width / 2, (-height) / 2),
				collage));
	});
var $timjs$elm_collage$Collage$Round = {$: 'Round'};
var $timjs$elm_collage$Collage$Flat = {$: 'Flat'};
var $timjs$elm_collage$Collage$Sharp = {$: 'Sharp'};
var $avh4$elm_color$Color$black = A4($avh4$elm_color$Color$RgbaSpace, 0 / 255, 0 / 255, 0 / 255, 1.0);
var $timjs$elm_collage$Collage$thin = 2.0;
var $timjs$elm_collage$Collage$uniform = $timjs$elm_collage$Collage$Core$Uniform;
var $timjs$elm_collage$Collage$defaultLineStyle = {
	cap: $timjs$elm_collage$Collage$Flat,
	dashPattern: _List_Nil,
	dashPhase: 0,
	fill: $timjs$elm_collage$Collage$uniform($avh4$elm_color$Color$black),
	join: $timjs$elm_collage$Collage$Sharp,
	thickness: $timjs$elm_collage$Collage$thin
};
var $timjs$elm_collage$Collage$broken = F3(
	function (dashes, thickness, fill) {
		return _Utils_update(
			$timjs$elm_collage$Collage$defaultLineStyle,
			{dashPattern: dashes, fill: fill, thickness: thickness});
	});
var $timjs$elm_collage$Collage$Core$Circle = function (a) {
	return {$: 'Circle', a: a};
};
var $timjs$elm_collage$Collage$circle = $timjs$elm_collage$Collage$Core$Circle;
var $avh4$elm_color$Color$hsla = F4(
	function (hue, sat, light, alpha) {
		var _v0 = _Utils_Tuple3(hue, sat, light);
		var h = _v0.a;
		var s = _v0.b;
		var l = _v0.c;
		var m2 = (l <= 0.5) ? (l * (s + 1)) : ((l + s) - (l * s));
		var m1 = (l * 2) - m2;
		var hueToRgb = function (h__) {
			var h_ = (h__ < 0) ? (h__ + 1) : ((h__ > 1) ? (h__ - 1) : h__);
			return ((h_ * 6) < 1) ? (m1 + (((m2 - m1) * h_) * 6)) : (((h_ * 2) < 1) ? m2 : (((h_ * 3) < 2) ? (m1 + (((m2 - m1) * ((2 / 3) - h_)) * 6)) : m1));
		};
		var b = hueToRgb(h - (1 / 3));
		var g = hueToRgb(h);
		var r = hueToRgb(h + (1 / 3));
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, alpha);
	});
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $noahzgordon$elm_color_extra$Color$Manipulate$limit = A2($elm$core$Basics$clamp, 0, 1);
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $avh4$elm_color$Color$toHsla = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var minColor = A2(
		$elm$core$Basics$min,
		r,
		A2($elm$core$Basics$min, g, b));
	var maxColor = A2(
		$elm$core$Basics$max,
		r,
		A2($elm$core$Basics$max, g, b));
	var l = (minColor + maxColor) / 2;
	var s = _Utils_eq(minColor, maxColor) ? 0 : ((l < 0.5) ? ((maxColor - minColor) / (maxColor + minColor)) : ((maxColor - minColor) / ((2 - maxColor) - minColor)));
	var h1 = _Utils_eq(maxColor, r) ? ((g - b) / (maxColor - minColor)) : (_Utils_eq(maxColor, g) ? (2 + ((b - r) / (maxColor - minColor))) : (4 + ((r - g) / (maxColor - minColor))));
	var h2 = h1 * (1 / 6);
	var h3 = $elm$core$Basics$isNaN(h2) ? 0 : ((h2 < 0) ? (h2 + 1) : h2);
	return {alpha: a, hue: h3, lightness: l, saturation: s};
};
var $noahzgordon$elm_color_extra$Color$Manipulate$darken = F2(
	function (offset, cl) {
		var _v0 = $avh4$elm_color$Color$toHsla(cl);
		var hue = _v0.hue;
		var saturation = _v0.saturation;
		var lightness = _v0.lightness;
		var alpha = _v0.alpha;
		return A4(
			$avh4$elm_color$Color$hsla,
			hue,
			saturation,
			$noahzgordon$elm_color_extra$Color$Manipulate$limit(lightness - offset),
			alpha);
	});
var $noahzgordon$elm_color_extra$Color$Manipulate$fadeIn = F2(
	function (offset, cl) {
		var _v0 = $avh4$elm_color$Color$toHsla(cl);
		var hue = _v0.hue;
		var saturation = _v0.saturation;
		var lightness = _v0.lightness;
		var alpha = _v0.alpha;
		return A4(
			$avh4$elm_color$Color$hsla,
			hue,
			saturation,
			lightness,
			$noahzgordon$elm_color_extra$Color$Manipulate$limit(alpha + offset));
	});
var $noahzgordon$elm_color_extra$Color$Manipulate$fadeOut = F2(
	function (offset, cl) {
		return A2($noahzgordon$elm_color_extra$Color$Manipulate$fadeIn, -offset, cl);
	});
var $timjs$elm_collage$Collage$solid = $timjs$elm_collage$Collage$broken(_List_Nil);
var $timjs$elm_collage$Collage$Core$Transparent = {$: 'Transparent'};
var $timjs$elm_collage$Collage$transparent = $timjs$elm_collage$Collage$Core$Transparent;
var $timjs$elm_collage$Collage$invisible = A2($timjs$elm_collage$Collage$solid, 0, $timjs$elm_collage$Collage$transparent);
var $timjs$elm_collage$Collage$Core$Shape = F2(
	function (a, b) {
		return {$: 'Shape', a: a, b: b};
	});
var $timjs$elm_collage$Collage$Core$collage = function (basic) {
	return {
		basic: basic,
		handlers: _List_Nil,
		name: $elm$core$Maybe$Nothing,
		opacity: 1,
		rotation: 0,
		scale: _Utils_Tuple2(1, 1),
		shift: _Utils_Tuple2(0, 0)
	};
};
var $timjs$elm_collage$Collage$styled = function (style) {
	return A2(
		$elm$core$Basics$composeL,
		$timjs$elm_collage$Collage$Core$collage,
		$timjs$elm_collage$Collage$Core$Shape(style));
};
var $timjs$elm_collage$Collage$filled = function (fill) {
	return $timjs$elm_collage$Collage$styled(
		_Utils_Tuple2(fill, $timjs$elm_collage$Collage$invisible));
};
var $timjs$elm_collage$Collage$group = A2($elm$core$Basics$composeL, $timjs$elm_collage$Collage$Core$collage, $timjs$elm_collage$Collage$Core$Group);
var $timjs$elm_collage$Collage$Core$Polygon = function (a) {
	return {$: 'Polygon', a: a};
};
var $timjs$elm_collage$Collage$polygon = $timjs$elm_collage$Collage$Core$Polygon;
var $timjs$elm_collage$Collage$ngon = F2(
	function (n, r) {
		var m = n;
		var t = (2 * $elm$core$Basics$pi) / m;
		var f = function (i) {
			return _Utils_Tuple2(
				r * $elm$core$Basics$cos((t * i) + ($elm$core$Basics$pi / 2)),
				r * $elm$core$Basics$sin((t * i) + ($elm$core$Basics$pi / 2)));
		};
		return $timjs$elm_collage$Collage$polygon(
			A2(
				$elm$core$List$map,
				f,
				A2($elm$core$List$range, 0, n)));
	});
var $timjs$elm_collage$Collage$outlined = function (linestyle) {
	return $timjs$elm_collage$Collage$styled(
		_Utils_Tuple2($timjs$elm_collage$Collage$transparent, linestyle));
};
var $timjs$elm_collage$Collage$Core$Polyline = function (a) {
	return {$: 'Polyline', a: a};
};
var $timjs$elm_collage$Collage$path = $timjs$elm_collage$Collage$Core$Polyline;
var $timjs$elm_collage$Collage$Core$Rectangle = F3(
	function (a, b, c) {
		return {$: 'Rectangle', a: a, b: b, c: c};
	});
var $timjs$elm_collage$Collage$roundedRectangle = $timjs$elm_collage$Collage$Core$Rectangle;
var $timjs$elm_collage$Collage$rectangle = F2(
	function (w, h) {
		return A3($timjs$elm_collage$Collage$roundedRectangle, w, h, 0);
	});
var $avh4$elm_color$Color$rgba = F4(
	function (r, g, b, a) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, a);
	});
var $timjs$elm_collage$Collage$rotate = F2(
	function (t, collage) {
		return _Utils_update(
			collage,
			{rotation: collage.rotation + t});
	});
var $timjs$elm_collage$Collage$scaleXY = F2(
	function (_v0, collage) {
		var sx = _v0.a;
		var sy = _v0.b;
		var _v1 = collage.scale;
		var sx0 = _v1.a;
		var sy0 = _v1.b;
		return _Utils_update(
			collage,
			{
				scale: _Utils_Tuple2(sx0 * sx, sy0 * sy)
			});
	});
var $timjs$elm_collage$Collage$scale = F2(
	function (s, collage) {
		return A2(
			$timjs$elm_collage$Collage$scaleXY,
			_Utils_Tuple2(s, s),
			collage);
	});
var $timjs$elm_collage$Collage$traced = F2(
	function (linestyle, p) {
		return $timjs$elm_collage$Collage$Core$collage(
			A2($timjs$elm_collage$Collage$Core$Path, linestyle, p));
	});
var $author$project$Main$view = function (_v0) {
	var windowSize = _v0.windowSize;
	var gameStage = _v0.gameStage;
	var stars = _v0.stars;
	var explosions = _v0.explosions;
	if (gameStage.$ === 'Playing') {
		var _v2 = gameStage.a;
		var player_ = _v2.a;
		var planets_ = _v2.b;
		var viewTail = function (_v6) {
			var tail = _v6.tail;
			var color = _v6.color;
			var r = _v6.r;
			return $timjs$elm_collage$Collage$group(
				A2(
					$elm$core$List$map,
					function (traceStyle) {
						return A2(
							$timjs$elm_collage$Collage$traced,
							traceStyle,
							$timjs$elm_collage$Collage$path(tail));
					},
					_List_fromArray(
						[
							A2(
							$timjs$elm_collage$Collage$solid,
							r * 2,
							$timjs$elm_collage$Collage$uniform(
								A2($noahzgordon$elm_color_extra$Color$Manipulate$fadeOut, 0.972, color))),
							A2(
							$timjs$elm_collage$Collage$solid,
							0.07,
							$timjs$elm_collage$Collage$uniform(
								A2($noahzgordon$elm_color_extra$Color$Manipulate$fadeOut, 0.45, color)))
						])));
		};
		var viewStar = function (_v5) {
			var position = _v5.position;
			var r = _v5.r;
			var viewTriangle = A2(
				$timjs$elm_collage$Collage$filled,
				$timjs$elm_collage$Collage$uniform(
					A4($avh4$elm_color$Color$rgba, 1, 1, 1, 0.5)),
				A2($timjs$elm_collage$Collage$ngon, 3, r));
			return A2(
				$timjs$elm_collage$Collage$shift,
				position,
				$timjs$elm_collage$Collage$group(
					_List_fromArray(
						[
							viewTriangle,
							A2(
							$timjs$elm_collage$Collage$rotate,
							$elm$core$Basics$turns(1 / 6),
							viewTriangle)
						])));
		};
		var viewPlanet = function (planet) {
			var orbits = 5;
			return $timjs$elm_collage$Collage$group(
				A2(
					$elm$core$List$cons,
					function () {
						var _v4 = planet.whenHit;
						switch (_v4.$) {
							case 'Join':
								return A2(
									$timjs$elm_collage$Collage$filled,
									$timjs$elm_collage$Collage$uniform(planet.color),
									$timjs$elm_collage$Collage$circle(planet.r));
							case 'Kill':
								return A2(
									$timjs$elm_collage$Collage$outlined,
									A2(
										$timjs$elm_collage$Collage$solid,
										planet.r / 2,
										$timjs$elm_collage$Collage$uniform(
											A2($noahzgordon$elm_color_extra$Color$Manipulate$darken, 0.04, planet.color))),
									A2($timjs$elm_collage$Collage$ngon, 5, planet.r));
							default:
								return A2(
									$timjs$elm_collage$Collage$rotate,
									$lue_bird$elm_xy$Xy$toAngle(planet.v) + $elm$core$Basics$turns(1 / 12),
									A2(
										$timjs$elm_collage$Collage$filled,
										$timjs$elm_collage$Collage$uniform(planet.color),
										A2($timjs$elm_collage$Collage$ngon, 3, planet.r)));
						}
					}(),
					A2(
						$elm$core$List$map,
						function (orbit) {
							return A2(
								$timjs$elm_collage$Collage$outlined,
								function (style) {
									return _Utils_update(
										style,
										{cap: $timjs$elm_collage$Collage$Round});
								}(
									A3(
										$timjs$elm_collage$Collage$broken,
										_List_fromArray(
											[
												_Utils_Tuple2(
												$elm$core$Basics$round(((planet.r * 2) * $elm$core$Basics$pi) - 32),
												32)
											]),
										2,
										$timjs$elm_collage$Collage$uniform(
											A4($avh4$elm_color$Color$rgba, 1, 1, 1, 0.025 - ((0.025 * orbit) / orbits))))),
								$timjs$elm_collage$Collage$circle(
									planet.r + (A2($elm$core$Basics$pow, orbit, 3) * 0.7)));
						},
						A2($elm$core$List$range, 1, orbits))));
		};
		var viewExplosion = function (_v3) {
			var r = _v3.r;
			var color = _v3.color;
			var position = _v3.position;
			return A2(
				$timjs$elm_collage$Collage$shift,
				position,
				$timjs$elm_collage$Collage$group(
					_List_fromArray(
						[
							A2(
							$timjs$elm_collage$Collage$traced,
							A2(
								$timjs$elm_collage$Collage$solid,
								1,
								$timjs$elm_collage$Collage$uniform(
									A2(
										$noahzgordon$elm_color_extra$Color$Manipulate$fadeOut,
										1 - (0.2 / A2($elm$core$Basics$pow, r, 1.2)),
										color))),
							$timjs$elm_collage$Collage$path(
								A2(
									$elm$core$List$map,
									function (i) {
										return A2(
											$lue_bird$elm_xy$Xy$map,
											$elm$core$Basics$mul(
												(!A2($elm$core$Basics$modBy, 2, i)) ? 0 : (r * 1.7)),
											$lue_bird$elm_xy$Xy$direction(
												$elm$core$Basics$turns(i / 34)));
									},
									A2($elm$core$List$range, 0, 32)))),
							A2(
							$timjs$elm_collage$Collage$filled,
							$timjs$elm_collage$Collage$uniform(
								A2(
									$noahzgordon$elm_color_extra$Color$Manipulate$fadeOut,
									1 - (1 / A2($elm$core$Basics$pow, r, 1.2)),
									color)),
							$timjs$elm_collage$Collage$circle(r))
						])));
		};
		return $timjs$elm_collage$Collage$group(
			_List_fromArray(
				[
					A2(
					$timjs$elm_collage$Collage$scale,
					10,
					$timjs$elm_collage$Collage$group(
						_List_fromArray(
							[
								viewPlanet(player_),
								A2(
								$timjs$elm_collage$Collage$shift,
								A2(
									$lue_bird$elm_xy$Xy$map,
									function (c) {
										return -c;
									},
									player_.position),
								$timjs$elm_collage$Collage$group(
									A2(
										$elm$core$List$cons,
										viewTail(player_),
										_Utils_ap(
											$elm$core$List$concat(
												A2(
													$elm$core$List$map,
													function (planet) {
														return _List_fromArray(
															[
																viewTail(planet),
																A2(
																$timjs$elm_collage$Collage$shift,
																planet.position,
																viewPlanet(planet))
															]);
													},
													planets_)),
											_Utils_ap(
												A2($elm$core$List$map, viewStar, stars),
												A2($elm$core$List$map, viewExplosion, explosions))))))
							]))),
					A2(
					$timjs$elm_collage$Collage$filled,
					$timjs$elm_collage$Collage$uniform(
						A3($avh4$elm_color$Color$rgb, 0, 0, 0)),
					A2(
						$timjs$elm_collage$Collage$rectangle,
						$lue_bird$elm_xy$Xy$x(windowSize),
						$lue_bird$elm_xy$Xy$y(windowSize)))
				]));
	} else {
		return $timjs$elm_collage$Collage$group(_List_Nil);
	}
};
var $author$project$Main$viewDocument = F2(
	function (_v0, model) {
		return {
			body: $elm$core$List$singleton(
				A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
							A2($elm$html$Html$Attributes$style, 'background-color', 'black')
						]),
					$elm$core$List$singleton(
						A2(
							$timjs$elm_collage$Collage$Render$svgBox,
							model.windowSize,
							$author$project$Main$view(model))))),
			title: 'time to face gravity.'
		};
	});
var $author$project$Main$main = $MartinSStewart$elm_audio$Audio$documentWithAudio(
	{
		audio: $author$project$Main$audio,
		audioPort: {fromJS: $author$project$Main$audioPortFromJS, toJS: $author$project$Main$audioPortToJS},
		init: function (_v0) {
			return $author$project$Main$init;
		},
		subscriptions: $author$project$Main$subscriptions,
		update: $author$project$Main$update,
		view: $author$project$Main$viewDocument
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));