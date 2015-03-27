// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=0,_1=function(_2,_3){return E(_2)[1]!=E(_3)[1]?true:false;},_4=function(_5,_6){return E(_5)[1]==E(_6)[1];},_7=[0,_4,_1],_8=function(_9){var _a=E(_9)[1];return [0,Math.log(_a+(_a+1)*Math.sqrt((_a-1)/(_a+1)))];},_b=function(_c){var _d=E(_c)[1];return [0,Math.log(_d+Math.sqrt(1+_d*_d))];},_e=function(_f){var _g=E(_f)[1];return [0,0.5*Math.log((1+_g)/(1-_g))];},_h=function(_i,_j){return [0,Math.log(E(_j)[1])/Math.log(E(_i)[1])];},_k=[0,3.141592653589793],_l=[0,0],_m=new T(function(){return [0,0/0];}),_n=new T(function(){return [0,-1/0];}),_o=new T(function(){return [0,1/0];}),_p=function(_q,_r){while(1){var _s=E(_q);if(!_s[0]){_q=[1,I_fromInt(_s[1])];continue;}else{var _t=E(_r);if(!_t[0]){_q=_s;_r=[1,I_fromInt(_t[1])];continue;}else{return new F(function(){return I_fromRat(_s[1],_t[1]);});}}}},_u=function(_v,_w){var _x=E(_v);if(!_x[0]){var _y=_x[1],_z=E(_w);return _z[0]==0?_y==_z[1]:I_compareInt(_z[1],_y)==0?true:false;}else{var _A=_x[1],_B=E(_w);return _B[0]==0?I_compareInt(_A,_B[1])==0?true:false:I_compare(_A,_B[1])==0?true:false;}},_C=function(_D,_E){var _F=E(_D);if(!_F[0]){var _G=_F[1],_H=E(_E);return _H[0]==0?_G<_H[1]:I_compareInt(_H[1],_G)>0;}else{var _I=_F[1],_J=E(_E);return _J[0]==0?I_compareInt(_I,_J[1])<0:I_compare(_I,_J[1])<0;}},_K=function(_L,_M){return !B(_u(_M,_l))?[0,B(_p(_L,_M))]:!B(_u(_L,_l))?!B(_C(_L,_l))?E(_o):E(_n):E(_m);},_N=function(_O){var _P=E(_O);return new F(function(){return _K(_P[1],_P[2]);});},_Q=function(_R){return [0,1/E(_R)[1]];},_S=function(_T){var _U=E(_T),_V=_U[1];return _V<0?[0, -_V]:E(_U);},_W=function(_X){var _Y=E(_X);return _Y[0]==0?_Y[1]:I_toNumber(_Y[1]);},_Z=function(_10){return [0,B(_W(_10))];},_11=[0,0],_12=[0,1],_13=[0,-1],_14=function(_15){var _16=E(E(_15)[1]);return _16==0?E(_11):_16<=0?E(_13):E(_12);},_17=function(_18,_19){return [0,E(_18)[1]-E(_19)[1]];},_1a=function(_1b){return [0, -E(_1b)[1]];},_1c=function(_1d,_1e){return [0,E(_1d)[1]+E(_1e)[1]];},_1f=function(_1g,_1h){return [0,E(_1g)[1]*E(_1h)[1]];},_1i=[0,_1c,_1f,_17,_1a,_S,_14,_Z],_1j=function(_1k,_1l){return [0,E(_1k)[1]/E(_1l)[1]];},_1m=[0,_1i,_1j,_Q,_N],_1n=function(_1o){return [0,Math.acos(E(_1o)[1])];},_1p=function(_1q){return [0,Math.asin(E(_1q)[1])];},_1r=function(_1s){return [0,Math.atan(E(_1s)[1])];},_1t=function(_1u){return [0,Math.cos(E(_1u)[1])];},_1v=function(_1w){return [0,cosh(E(_1w)[1])];},_1x=function(_1y){return [0,Math.exp(E(_1y)[1])];},_1z=function(_1A){return [0,Math.log(E(_1A)[1])];},_1B=function(_1C,_1D){return [0,Math.pow(E(_1C)[1],E(_1D)[1])];},_1E=function(_1F){return [0,Math.sin(E(_1F)[1])];},_1G=function(_1H){return [0,sinh(E(_1H)[1])];},_1I=function(_1J){return [0,Math.sqrt(E(_1J)[1])];},_1K=function(_1L){return [0,Math.tan(E(_1L)[1])];},_1M=function(_1N){return [0,tanh(E(_1N)[1])];},_1O=[0,_1m,_k,_1x,_1I,_1z,_1B,_h,_1E,_1K,_1t,_1p,_1r,_1n,_1G,_1M,_1v,_b,_e,_8],_1P=function(_1Q,_1R){return E(_1Q)[1]<E(_1R)[1];},_1S=function(_1T,_1U){return E(_1T)[1]<=E(_1U)[1];},_1V=function(_1W,_1X){return E(_1W)[1]>E(_1X)[1];},_1Y=function(_1Z,_20){return E(_1Z)[1]>=E(_20)[1];},_21=function(_22,_23){var _24=E(_22)[1],_25=E(_23)[1];return _24>=_25?_24!=_25?2:1:0;},_26=function(_27,_28){var _29=E(_27),_2a=E(_28);return _29[1]>_2a[1]?E(_29):E(_2a);},_2b=function(_2c,_2d){var _2e=E(_2c),_2f=E(_2d);return _2e[1]>_2f[1]?E(_2f):E(_2e);},_2g=[0,_7,_21,_1P,_1Y,_1V,_1S,_26,_2b],_2h=[0,1],_2i=function(_2j){return [0,_2j];},_2k=new T(function(){var _2l=newByteArr(256),_2m=_2l,_=_2m["v"]["i8"][0]=8,_=B((function(_2n,_2o,_2p,_){while(1){if(_2p>=256){if(_2n>=256){return E(_);}else{var _2q=imul(2,_2n)|0,_2r=_2o+1|0,_2s=_2n;_2n=_2q;_2o=_2r;_2p=_2s;continue;}}else{var _=_2m["v"]["i8"][_2p]=_2o,_2s=_2p+_2n|0;_2p=_2s;continue;}}})(2,0,1,_)),_2t=_2m,_2u=_2t;return [0,_2u];}),_2v=function(_2w,_2x){while(1){var _2y=(function(_2z,_2A){var _2B=E(_2k)[1]["v"]["i8"][(255&_2z>>>0)>>>0&4294967295];if(_2A>_2B){if(_2B>=8){var _2C=_2z>>8,_2D=_2A-8|0;_2w=_2C;_2x=_2D;return null;}else{return [0,new T(function(){return B(_2i(_2z>>_2B));}),_2A-_2B|0];}}else{return [0,new T(function(){return B(_2i(_2z>>_2A));}),0];}})(_2w,_2x);if(_2y!=null){return _2y;}}},_2E=function(_2F,_2G){while(1){var _2H=E(_2F);if(!_2H[0]){_2F=[1,I_fromInt(_2H[1])];continue;}else{return [1,I_shiftLeft(_2H[1],_2G)];}}},_2I=function(_2J){var _2K=decodeFloat(_2J),_2L=_2K[1],_2M=_2K[2];if(_2M<0){var _2N=function(_2O){if(!_2O){return [0,B(_2i(_2L)),B(_2E(_2h, -_2M))];}else{var _2P=B(_2v(_2L, -_2M));return [0,E(_2P[1]),B(_2E(_2h,_2P[2]))];}};return (_2L>>>0&1)>>>0==0?B(_2N(1)):B(_2N(0));}else{return [0,B(_2E(B(_2i(_2L)),_2M)),_2h];}},_2Q=function(_2R){var _2S=B(_2I(E(_2R)[1]));return [0,E(_2S[1]),E(_2S[2])];},_2T=[0,_1i,_2g,_2Q],_2U=function(_2V){return E(E(_2V)[1]);},_2W=function(_2X){return E(E(_2X)[1]);},_2Y=function(_2Z,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_3a,_3b,_3c,_3d,_3e,_3f,_3g,_3h,_3i,_3j,_3k,_3l,_3m,_3n,_3o,_3p,_3q,_3r,_3s,_3t,_3u,_3v,_3w){return [0,E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_31,_3h]));}),new T(function(){return B(A(_30,[_32,_3l]));})]));}),new T(function(){return B(A(_30,[_33,_3p]));})]));}),new T(function(){return B(A(_30,[_34,_3t]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_31,_3i]));}),new T(function(){return B(A(_30,[_32,_3m]));})]));}),new T(function(){return B(A(_30,[_33,_3q]));})]));}),new T(function(){return B(A(_30,[_34,_3u]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_31,_3j]));}),new T(function(){return B(A(_30,[_32,_3n]));})]));}),new T(function(){return B(A(_30,[_33,_3r]));})]));}),new T(function(){return B(A(_30,[_34,_3v]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_31,_3k]));}),new T(function(){return B(A(_30,[_32,_3o]));})]));}),new T(function(){return B(A(_30,[_33,_3s]));})]));}),new T(function(){return B(A(_30,[_34,_3w]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_35,_3h]));}),new T(function(){return B(A(_30,[_36,_3l]));})]));}),new T(function(){return B(A(_30,[_37,_3p]));})]));}),new T(function(){return B(A(_30,[_38,_3t]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_35,_3i]));}),new T(function(){return B(A(_30,[_36,_3m]));})]));}),new T(function(){return B(A(_30,[_37,_3q]));})]));}),new T(function(){return B(A(_30,[_38,_3u]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_35,_3j]));}),new T(function(){return B(A(_30,[_36,_3n]));})]));}),new T(function(){return B(A(_30,[_37,_3r]));})]));}),new T(function(){return B(A(_30,[_38,_3v]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_35,_3k]));}),new T(function(){return B(A(_30,[_36,_3o]));})]));}),new T(function(){return B(A(_30,[_37,_3s]));})]));}),new T(function(){return B(A(_30,[_38,_3w]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_39,_3h]));}),new T(function(){return B(A(_30,[_3a,_3l]));})]));}),new T(function(){return B(A(_30,[_3b,_3p]));})]));}),new T(function(){return B(A(_30,[_3c,_3t]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_39,_3i]));}),new T(function(){return B(A(_30,[_3a,_3m]));})]));}),new T(function(){return B(A(_30,[_3b,_3q]));})]));}),new T(function(){return B(A(_30,[_3c,_3u]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_39,_3j]));}),new T(function(){return B(A(_30,[_3a,_3n]));})]));}),new T(function(){return B(A(_30,[_3b,_3r]));})]));}),new T(function(){return B(A(_30,[_3c,_3v]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_39,_3k]));}),new T(function(){return B(A(_30,[_3a,_3o]));})]));}),new T(function(){return B(A(_30,[_3b,_3s]));})]));}),new T(function(){return B(A(_30,[_3c,_3w]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_3d,_3h]));}),new T(function(){return B(A(_30,[_3e,_3l]));})]));}),new T(function(){return B(A(_30,[_3f,_3p]));})]));}),new T(function(){return B(A(_30,[_3g,_3t]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_3d,_3i]));}),new T(function(){return B(A(_30,[_3e,_3m]));})]));}),new T(function(){return B(A(_30,[_3f,_3q]));})]));}),new T(function(){return B(A(_30,[_3g,_3u]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_3d,_3j]));}),new T(function(){return B(A(_30,[_3e,_3n]));})]));}),new T(function(){return B(A(_30,[_3f,_3r]));})]));}),new T(function(){return B(A(_30,[_3g,_3v]));})]))),E(B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_2Z,[new T(function(){return B(A(_30,[_3d,_3k]));}),new T(function(){return B(A(_30,[_3e,_3o]));})]));}),new T(function(){return B(A(_30,[_3f,_3s]));})]));}),new T(function(){return B(A(_30,[_3g,_3w]));})])))];},_3x=[0,1],_3y=[0,0],_3z=[0,1],_3A=[0,0],_3B=function(_3C,_3D,_3E,_3F,_3G,_3H,_3I,_3J,_3K,_3L){return new F(function(){return A(_3C,[new T(function(){return B(A(_3C,[new T(function(){return B(A(_3C,[new T(function(){return B(A(_3D,[_3E,_3I]));}),new T(function(){return B(A(_3D,[_3F,_3J]));})]));}),new T(function(){return B(A(_3D,[_3G,_3K]));})]));}),new T(function(){return B(A(_3D,[_3L,_3H]));})]);});},_3M=function(_3N,_3O,_3P,_3Q,_3R,_3S,_3T,_3U,_3V,_3W){var _3X=E(_3O),_3Y=_3X[1],_3Z=_3X[2],_40=_3X[3],_41=_3X[7],_42=function(_43){var _44=new T(function(){return B(A(_3P,[new T(function(){return B(A(_41,[_3z]));}),new T(function(){return B(A(_3Y,[new T(function(){return B(A(_3S,[new T(function(){return B(_3B(_3Y,_3Z,_3T,_3U,_3V,_3W,_3T,_3U,_3V,_3W));})]));}),_3W]));})]));}),_45=B(A(_41,[_3A]));return [0,E(B(A(_3Y,[_3W,new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3T]));}),_3T]));})]))),E(B(A(_40,[new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3T]));}),_3U]));}),_3V]))),E(B(A(_3Y,[new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3T]));}),_3V]));}),_3U]))),E(_45),E(B(A(_3Y,[new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3T]));}),_3U]));}),_3V]))),E(B(A(_3Y,[_3W,new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3U]));}),_3U]));})]))),E(B(A(_40,[new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3U]));}),_3V]));}),_3T]))),E(_45),E(B(A(_40,[new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3T]));}),_3V]));}),_3U]))),E(B(A(_3Y,[new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3U]));}),_3V]));}),_3T]))),E(B(A(_3Y,[_3W,new T(function(){return B(A(_3Z,[new T(function(){return B(A(_3Z,[_44,_3V]));}),_3V]));})]))),E(_45),E(_45),E(_45),E(_45),E(B(A(_41,[_3z])))];};if(!B(A(_3N,[_3T,new T(function(){return B(A(_41,[_3A]));})]))){return new F(function(){return _42(_);});}else{if(!B(A(_3N,[_3U,new T(function(){return B(A(_41,[_3A]));})]))){return new F(function(){return _42(_);});}else{if(!B(A(_3N,[_3V,new T(function(){return B(A(_41,[_3A]));})]))){return new F(function(){return _42(_);});}else{var _46=B(A(_41,[_3A]));return [0,E(_3W),E(_46),E(_46),E(_46),E(_46),E(_3W),E(_46),E(_46),E(_46),E(_46),E(_3W),E(_46),E(_46),E(_46),E(_46),E(B(A(_41,[_3z])))];}}}},_47=function(_48,_49,_4a,_4b,_4c,_4d,_4e){return [0,new T(function(){var _4f=E(B(_2U(_49))[1])[7],_4g=E(_49),_4h=E(_4g[1]),_4i=E(_4a),_4j=B(_3M(E(_48)[1],_4h[1],_4h[2],_4h[3],_4h[4],_4g[4],_4i[1],_4i[2],_4i[3],_4i[4])),_4k=B(A(_4f,[_3y]));return [0,E(_4j[1]),E(_4j[2]),E(_4j[3]),E(_4b),E(_4j[5]),E(_4j[6]),E(_4j[7]),E(_4c),E(_4j[9]),E(_4j[10]),E(_4j[11]),E(_4d),E(_4k),E(_4k),E(_4k),E(B(A(_4f,[_3x])))];}),_4e];},_4l=function(_4m,_4n,_4o,_4p,_4q){var _4r=new T(function(){var _4s=E(_4q),_4t=E(_4s[2]),_4u=B(_47(_4m,_4n,_4s[1],_4t[1],_4t[2],_4t[3],_4s[3]));return [0,_4u[1],_4u[2]];});return [0,new T(function(){var _4v=B(_2W(B(_2U(_4n)))),_4w=E(_4o),_4x=E(E(_4r)[1]);return B(_2Y(_4v[1],_4v[2],_4w[1],_4w[2],_4w[3],_4w[4],_4w[5],_4w[6],_4w[7],_4w[8],_4w[9],_4w[10],_4w[11],_4w[12],_4w[13],_4w[14],_4w[15],_4w[16],_4x[1],_4x[2],_4x[3],_4x[4],_4x[5],_4x[6],_4x[7],_4x[8],_4x[9],_4x[10],_4x[11],_4x[12],_4x[13],_4x[14],_4x[15],_4x[16]));}),new T(function(){return B(A(_4p,[new T(function(){return E(E(_4r)[2]);})]));})];},_4y=function(_4z,_4A,_4B,_4C){var _4D=E(_4B),_4E=B(_4l(_4z,_4A,_4D[1],_4D[2],_4C));return [0,_4E[1],_4E[2]];},_4F=function(_4G,_4H,_){var _4I=B(A(_4H,[_])),_4J=_4I,_4K=jsSetTimeout(_4G,function(_){return new F(function(){return _4F(_4G,_4H,_);});});return _0;},_4L=function(_4M,_4N,_4O,_4P){var _4Q=new T(function(){return B(A(_4P,[_4O]));});return [0,new T(function(){var _4R=B(_2W(B(_2U(_4M)))),_4S=E(_4N),_4T=E(E(_4Q)[1]);return B(_2Y(_4R[1],_4R[2],_4S[1],_4S[2],_4S[3],_4S[4],_4S[5],_4S[6],_4S[7],_4S[8],_4S[9],_4S[10],_4S[11],_4S[12],_4S[13],_4S[14],_4S[15],_4S[16],_4T[1],_4T[2],_4T[3],_4T[4],_4T[5],_4T[6],_4T[7],_4T[8],_4T[9],_4T[10],_4T[11],_4T[12],_4T[13],_4T[14],_4T[15],_4T[16]));}),new T(function(){return E(E(_4Q)[2]);})];},_4U=function(_4V,_4W,_4X,_4Y,_4Z,_50,_51,_52,_53,_54,_55){return [0,B(A(_4X,[new T(function(){return B(A(_4V,[new T(function(){return B(A(_4V,[new T(function(){return B(A(_4W,[_51,_52]));}),new T(function(){return B(A(_4W,[_4Y,_55]));})]));}),new T(function(){return B(A(_4W,[_4Z,_54]));})]));}),new T(function(){return B(A(_4W,[_50,_53]));})])),B(A(_4V,[new T(function(){return B(A(_4V,[new T(function(){return B(A(_4X,[new T(function(){return B(A(_4W,[_51,_53]));}),new T(function(){return B(A(_4W,[_4Y,_54]));})]));}),new T(function(){return B(A(_4W,[_4Z,_55]));})]));}),new T(function(){return B(A(_4W,[_50,_52]));})])),B(A(_4V,[new T(function(){return B(A(_4X,[new T(function(){return B(A(_4V,[new T(function(){return B(A(_4W,[_51,_54]));}),new T(function(){return B(A(_4W,[_4Y,_53]));})]));}),new T(function(){return B(A(_4W,[_4Z,_52]));})]));}),new T(function(){return B(A(_4W,[_50,_55]));})])),B(A(_4X,[new T(function(){return B(A(_4X,[new T(function(){return B(A(_4X,[new T(function(){return B(A(_4W,[_51,_55]));}),new T(function(){return B(A(_4W,[_4Y,_52]));})]));}),new T(function(){return B(A(_4W,[_4Z,_53]));})]));}),new T(function(){return B(A(_4W,[_50,_54]));})]))];},_56=[0,0],_57=function(_58){return E(E(_58)[2]);},_59=function(_5a){return E(E(_5a)[1]);},_5b=function(_5c){return E(E(_5c)[3]);},_5d=function(_5e){return E(E(_5e)[2]);},_5f=function(_5g){return E(E(_5g)[7]);},_5h=function(_5i){return E(E(_5i)[4]);},_5j=function(_5k,_5l,_5m,_5n,_5o,_5p){var _5q=new T(function(){return B(_2U(_5k));}),_5r=new T(function(){return B(_2W(_5q));}),_5s=function(_5t){var _5u=E(_5m),_5v=_5u[1],_5w=_5u[2],_5x=_5u[3],_5y=_5u[4],_5z=function(_5A){var _5B=new T(function(){return B(A(_5d,[_5q,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_57,[_5r,_5n,_5v]));}),new T(function(){return B(A(_57,[_5r,_5o,_5w]));})]));}),new T(function(){return B(A(_57,[_5r,_5p,_5x]));})]));}),new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_5h,[_5k,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_57,[_5r,_5v,_5v]));}),new T(function(){return B(A(_57,[_5r,_5w,_5w]));})]));}),new T(function(){return B(A(_57,[_5r,_5x,_5x]));})]));}),new T(function(){return B(A(_57,[_5r,_5y,_5y]));})]));})]));}),_5y]));})]));});return [0,B(A(_5b,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_57,[_5r,_5n,_5y]));}),new T(function(){return B(A(_57,[_5r,_5v,_5B]));})]));}),new T(function(){return B(A(_57,[_5r,_5p,_5w]));})]));}),new T(function(){return B(A(_57,[_5r,_5o,_5x]));})])),B(A(_5b,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_57,[_5r,_5o,_5y]));}),new T(function(){return B(A(_57,[_5r,_5w,_5B]));})]));}),new T(function(){return B(A(_57,[_5r,_5n,_5x]));})]));}),new T(function(){return B(A(_57,[_5r,_5p,_5v]));})])),B(A(_5b,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_59,[_5r,new T(function(){return B(A(_57,[_5r,_5p,_5y]));}),new T(function(){return B(A(_57,[_5r,_5x,_5B]));})]));}),new T(function(){return B(A(_57,[_5r,_5o,_5v]));})]));}),new T(function(){return B(A(_57,[_5r,_5n,_5w]));})]))];};if(!B(A(_5l,[_5v,new T(function(){return B(A(_5f,[_5r,_56]));})]))){return new F(function(){return _5z(_);});}else{if(!B(A(_5l,[_5w,new T(function(){return B(A(_5f,[_5r,_56]));})]))){return new F(function(){return _5z(_);});}else{if(!B(A(_5l,[_5x,new T(function(){return B(A(_5f,[_5r,_56]));})]))){return new F(function(){return _5z(_);});}else{var _5C=E(_5r)[2];return [0,B(A(_5C,[_5y,_5n])),B(A(_5C,[_5y,_5o])),B(A(_5C,[_5y,_5p]))];}}}};return !B(A(_5l,[_5n,new T(function(){return B(A(_5f,[_5r,_56]));})]))?B(_5s(_)):!B(A(_5l,[_5o,new T(function(){return B(A(_5f,[_5r,_56]));})]))?B(_5s(_)):!B(A(_5l,[_5p,new T(function(){return B(A(_5f,[_5r,_56]));})]))?B(_5s(_)):[0,_5n,_5o,_5p];},_5D=function(_5E,_5F,_5G,_5H,_5I,_5J){var _5K=new T(function(){return B(A(_5J,[_5I]));});return [0,new T(function(){var _5L=B(_2W(B(_2U(_5E)))),_5M=E(_5G),_5N=E(E(_5K)[1]),_5O=B(_4U(_5L[1],_5L[2],_5L[3],_5M[1],_5M[2],_5M[3],_5M[4],_5N[1],_5N[2],_5N[3],_5N[4]));return [0,E(_5O[1]),E(_5O[2]),E(_5O[3]),E(_5O[4])];}),new T(function(){var _5P=B(_2W(B(_2U(_5E))))[1],_5Q=E(E(_5K)[2]),_5R=B(_5j(_5E,E(_5F)[1],_5G,_5Q[1],_5Q[2],_5Q[3])),_5S=E(_5H);return [0,E(B(A(_5P,[_5R[1],_5S[1]]))),E(B(A(_5P,[_5R[2],_5S[2]]))),E(B(A(_5P,[_5R[3],_5S[3]])))];}),new T(function(){return E(E(_5K)[3]);})];},_5T=[0,0],_5U=function(_5V){return E(E(_5V)[1]);},_5W=[0,1],_5X=function(_5Y,_5Z){while(1){var _60=E(_5Y);if(!_60[0]){var _61=_60[1],_62=E(_5Z);if(!_62[0]){var _63=_62[1],_64=addC(_61,_63);if(!E(_64[2])){return [0,_64[1]];}else{_5Y=[1,I_fromInt(_61)];_5Z=[1,I_fromInt(_63)];continue;}}else{_5Y=[1,I_fromInt(_61)];_5Z=_62;continue;}}else{var _65=E(_5Z);if(!_65[0]){_5Y=_60;_5Z=[1,I_fromInt(_65[1])];continue;}else{return [1,I_add(_60[1],_65[1])];}}}},_66=function(_67,_68){var _69=E(_67);return [0,_69,new T(function(){var _6a=B(_66(B(_5X(_69,_68)),_68));return [1,_6a[1],_6a[2]];})];},_6b=function(_6c){var _6d=B(_66(_6c,_5W));return [1,_6d[1],_6d[2]];},_6e=function(_6f,_6g){while(1){var _6h=E(_6f);if(!_6h[0]){var _6i=_6h[1],_6j=E(_6g);if(!_6j[0]){var _6k=_6j[1],_6l=subC(_6i,_6k);if(!E(_6l[2])){return [0,_6l[1]];}else{_6f=[1,I_fromInt(_6i)];_6g=[1,I_fromInt(_6k)];continue;}}else{_6f=[1,I_fromInt(_6i)];_6g=_6j;continue;}}else{var _6m=E(_6g);if(!_6m[0]){_6f=_6h;_6g=[1,I_fromInt(_6m[1])];continue;}else{return [1,I_sub(_6h[1],_6m[1])];}}}},_6n=function(_6o,_6p){var _6q=B(_66(_6o,new T(function(){return B(_6e(_6p,_6o));})));return [1,_6q[1],_6q[2]];},_6r=[0,0],_6s=function(_6t,_6u){var _6v=E(_6t);if(!_6v[0]){var _6w=_6v[1],_6x=E(_6u);return _6x[0]==0?_6w>=_6x[1]:I_compareInt(_6x[1],_6w)<=0;}else{var _6y=_6v[1],_6z=E(_6u);return _6z[0]==0?I_compareInt(_6y,_6z[1])>=0:I_compare(_6y,_6z[1])>=0;}},_6A=function(_6B,_6C){var _6D=E(_6B);if(!_6D[0]){var _6E=_6D[1],_6F=E(_6C);return _6F[0]==0?_6E>_6F[1]:I_compareInt(_6F[1],_6E)<0;}else{var _6G=_6D[1],_6H=E(_6C);return _6H[0]==0?I_compareInt(_6G,_6H[1])>0:I_compare(_6G,_6H[1])>0;}},_6I=function(_6J,_6K,_6L){if(!B(_6s(_6K,_6r))){var _6M=function(_6N){return !B(_C(_6N,_6L))?[1,_6N,new T(function(){return B(_6M(B(_5X(_6N,_6K))));})]:[0];};return new F(function(){return _6M(_6J);});}else{var _6O=function(_6P){return !B(_6A(_6P,_6L))?[1,_6P,new T(function(){return B(_6O(B(_5X(_6P,_6K))));})]:[0];};return new F(function(){return _6O(_6J);});}},_6Q=function(_6R,_6S,_6T){return new F(function(){return _6I(_6R,B(_6e(_6S,_6R)),_6T);});},_6U=function(_6V,_6W){return new F(function(){return _6I(_6V,_5W,_6W);});},_6X=function(_6Y){var _6Z=E(_6Y);return _6Z[0]==0?E(_6Z[1]):I_toInt(_6Z[1]);},_70=function(_71){return [0,B(_6X(_71))];},_72=function(_73){return new F(function(){return _6e(_73,_5W);});},_74=function(_75){return new F(function(){return _5X(_75,_5W);});},_76=function(_77){return new F(function(){return _2i(E(_77)[1]);});},_78=[0,_74,_72,_76,_70,_6b,_6n,_6U,_6Q],_79=function(_7a,_7b){if(_7a<=0){if(_7a>=0){return new F(function(){return quot(_7a,_7b);});}else{if(_7b<=0){return new F(function(){return quot(_7a,_7b);});}else{return quot(_7a+1|0,_7b)-1|0;}}}else{if(_7b>=0){if(_7a>=0){return new F(function(){return quot(_7a,_7b);});}else{if(_7b<=0){return new F(function(){return quot(_7a,_7b);});}else{return quot(_7a+1|0,_7b)-1|0;}}}else{return quot(_7a-1|0,_7b)-1|0;}}},_7c=function(_7d,_7e){while(1){var _7f=E(_7d);if(!_7f[0]){var _7g=E(_7f[1]);if(_7g==(-2147483648)){_7d=[1,I_fromInt(-2147483648)];continue;}else{var _7h=E(_7e);if(!_7h[0]){return [0,B(_79(_7g,_7h[1]))];}else{_7d=[1,I_fromInt(_7g)];_7e=_7h;continue;}}}else{var _7i=_7f[1],_7j=E(_7e);return _7j[0]==0?[0,I_toInt(I_div(_7i,I_fromInt(_7j[1])))]:[1,I_div(_7i,_7j[1])];}}},_7k=new T(function(){return B(unCStr("ArithException"));}),_7l=new T(function(){return B(unCStr("GHC.Exception"));}),_7m=new T(function(){return B(unCStr("base"));}),_7n=[0],_7o=new T(function(){var _7p=hs_wordToWord64(4194982440),_7q=_7p,_7r=hs_wordToWord64(3110813675),_7s=_7r;return [0,_7q,_7s,[0,_7q,_7s,_7m,_7l,_7k],_7n];}),_7t=function(_7u){return E(_7o);},_7v=function(_7w){return E(E(_7w)[1]);},_7x=function(_7y,_7z,_7A){var _7B=B(A(_7y,[_])),_7C=B(A(_7z,[_])),_7D=hs_eqWord64(_7B[1],_7C[1]),_7E=_7D;if(!E(_7E)){return [0];}else{var _7F=hs_eqWord64(_7B[2],_7C[2]),_7G=_7F;return E(_7G)==0?[0]:[1,_7A];}},_7H=function(_7I){var _7J=E(_7I);return new F(function(){return _7x(B(_7v(_7J[1])),_7t,_7J[2]);});},_7K=new T(function(){return B(unCStr("arithmetic underflow"));}),_7L=new T(function(){return B(unCStr("arithmetic overflow"));}),_7M=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_7N=new T(function(){return B(unCStr("denormal"));}),_7O=new T(function(){return B(unCStr("divide by zero"));}),_7P=new T(function(){return B(unCStr("loss of precision"));}),_7Q=function(_7R){switch(E(_7R)){case 0:return E(_7L);case 1:return E(_7K);case 2:return E(_7P);case 3:return E(_7O);case 4:return E(_7N);default:return E(_7M);}},_7S=function(_7T,_7U){var _7V=E(_7T);return _7V[0]==0?E(_7U):[1,_7V[1],new T(function(){return B(_7S(_7V[2],_7U));})];},_7W=function(_7X){return new F(function(){return _7S(_7K,_7X);});},_7Y=function(_7X){return new F(function(){return _7S(_7L,_7X);});},_7Z=function(_7X){return new F(function(){return _7S(_7M,_7X);});},_80=function(_7X){return new F(function(){return _7S(_7N,_7X);});},_81=function(_7X){return new F(function(){return _7S(_7O,_7X);});},_82=function(_7X){return new F(function(){return _7S(_7P,_7X);});},_83=function(_84){switch(E(_84)){case 0:return E(_7Y);case 1:return E(_7W);case 2:return E(_82);case 3:return E(_81);case 4:return E(_80);default:return E(_7Z);}},_85=[0,44],_86=[0,93],_87=[0,91],_88=function(_89,_8a,_8b){var _8c=E(_8a);return _8c[0]==0?B(unAppCStr("[]",_8b)):[1,_87,new T(function(){return B(A(_89,[_8c[1],new T(function(){var _8d=function(_8e){var _8f=E(_8e);return _8f[0]==0?E([1,_86,_8b]):[1,_85,new T(function(){return B(A(_89,[_8f[1],new T(function(){return B(_8d(_8f[2]));})]));})];};return B(_8d(_8c[2]));})]));})];},_8g=function(_8h,_8i){return new F(function(){return _88(_83,_8h,_8i);});},_8j=function(_8k,_8l){switch(E(_8l)){case 0:return E(_7Y);case 1:return E(_7W);case 2:return E(_82);case 3:return E(_81);case 4:return E(_80);default:return E(_7Z);}},_8m=[0,_8j,_7Q,_8g],_8n=new T(function(){return [0,_7t,_8m,_8o,_7H];}),_8o=function(_7X){return [0,_8n,_7X];},_8p=3,_8q=new T(function(){return B(_8o(_8p));}),_8r=new T(function(){return die(_8q);}),_8s=[0,0],_8t=function(_8u,_8v){return !B(_u(_8v,_8s))?B(_7c(_8u,_8v)):E(_8r);},_8w=function(_8x,_8y){var _8z=_8x%_8y;if(_8x<=0){if(_8x>=0){return E(_8z);}else{if(_8y<=0){return E(_8z);}else{var _8A=E(_8z);return _8A==0?0:_8A+_8y|0;}}}else{if(_8y>=0){if(_8x>=0){return E(_8z);}else{if(_8y<=0){return E(_8z);}else{var _8B=E(_8z);return _8B==0?0:_8B+_8y|0;}}}else{var _8C=E(_8z);return _8C==0?0:_8C+_8y|0;}}},_8D=function(_8E,_8F){while(1){var _8G=E(_8E);if(!_8G[0]){var _8H=E(_8G[1]);if(_8H==(-2147483648)){_8E=[1,I_fromInt(-2147483648)];continue;}else{var _8I=E(_8F);if(!_8I[0]){var _8J=_8I[1];return [0,[0,B(_79(_8H,_8J))],[0,B(_8w(_8H,_8J))]];}else{_8E=[1,I_fromInt(_8H)];_8F=_8I;continue;}}}else{var _8K=E(_8F);if(!_8K[0]){_8E=_8G;_8F=[1,I_fromInt(_8K[1])];continue;}else{var _8L=I_divMod(_8G[1],_8K[1]);return [0,[1,_8L[1]],[1,_8L[2]]];}}}},_8M=function(_8N,_8O){if(!B(_u(_8O,_8s))){var _8P=B(_8D(_8N,_8O));return [0,_8P[1],_8P[2]];}else{return E(_8r);}},_8Q=function(_8R,_8S){while(1){var _8T=E(_8R);if(!_8T[0]){var _8U=E(_8T[1]);if(_8U==(-2147483648)){_8R=[1,I_fromInt(-2147483648)];continue;}else{var _8V=E(_8S);if(!_8V[0]){return [0,B(_8w(_8U,_8V[1]))];}else{_8R=[1,I_fromInt(_8U)];_8S=_8V;continue;}}}else{var _8W=_8T[1],_8X=E(_8S);return _8X[0]==0?[0,I_toInt(I_mod(_8W,I_fromInt(_8X[1])))]:[1,I_mod(_8W,_8X[1])];}}},_8Y=function(_8Z,_90){return !B(_u(_90,_8s))?B(_8Q(_8Z,_90)):E(_8r);},_91=function(_92,_93){while(1){var _94=E(_92);if(!_94[0]){var _95=E(_94[1]);if(_95==(-2147483648)){_92=[1,I_fromInt(-2147483648)];continue;}else{var _96=E(_93);if(!_96[0]){return [0,quot(_95,_96[1])];}else{_92=[1,I_fromInt(_95)];_93=_96;continue;}}}else{var _97=_94[1],_98=E(_93);return _98[0]==0?[0,I_toInt(I_quot(_97,I_fromInt(_98[1])))]:[1,I_quot(_97,_98[1])];}}},_99=function(_9a,_9b){return !B(_u(_9b,_8s))?B(_91(_9a,_9b)):E(_8r);},_9c=function(_9d,_9e){while(1){var _9f=E(_9d);if(!_9f[0]){var _9g=E(_9f[1]);if(_9g==(-2147483648)){_9d=[1,I_fromInt(-2147483648)];continue;}else{var _9h=E(_9e);if(!_9h[0]){var _9i=_9h[1];return [0,[0,quot(_9g,_9i)],[0,_9g%_9i]];}else{_9d=[1,I_fromInt(_9g)];_9e=_9h;continue;}}}else{var _9j=E(_9e);if(!_9j[0]){_9d=_9f;_9e=[1,I_fromInt(_9j[1])];continue;}else{var _9k=I_quotRem(_9f[1],_9j[1]);return [0,[1,_9k[1]],[1,_9k[2]]];}}}},_9l=function(_9m,_9n){if(!B(_u(_9n,_8s))){var _9o=B(_9c(_9m,_9n));return [0,_9o[1],_9o[2]];}else{return E(_8r);}},_9p=function(_9q,_9r){while(1){var _9s=E(_9q);if(!_9s[0]){var _9t=E(_9s[1]);if(_9t==(-2147483648)){_9q=[1,I_fromInt(-2147483648)];continue;}else{var _9u=E(_9r);if(!_9u[0]){return [0,_9t%_9u[1]];}else{_9q=[1,I_fromInt(_9t)];_9r=_9u;continue;}}}else{var _9v=_9s[1],_9w=E(_9r);return _9w[0]==0?[0,I_toInt(I_rem(_9v,I_fromInt(_9w[1])))]:[1,I_rem(_9v,_9w[1])];}}},_9x=function(_9y,_9z){return !B(_u(_9z,_8s))?B(_9p(_9y,_9z)):E(_8r);},_9A=function(_9B){return E(_9B);},_9C=function(_9D){return E(_9D);},_9E=[0,1],_9F=[0,2147483647],_9G=new T(function(){return B(_5X(_9F,_9E));}),_9H=function(_9I){var _9J=E(_9I);if(!_9J[0]){var _9K=E(_9J[1]);return _9K==(-2147483648)?E(_9G):_9K<0?[0, -_9K]:E(_9J);}else{var _9L=_9J[1];return I_compareInt(_9L,0)>=0?E(_9J):[1,I_negate(_9L)];}},_9M=function(_9N){var _9O=E(_9N);if(!_9O[0]){var _9P=E(_9O[1]);return _9P==(-2147483648)?E(_9G):[0, -_9P];}else{return [1,I_negate(_9O[1])];}},_9Q=[0,0],_9R=[0,-1],_9S=function(_9T){var _9U=E(_9T);if(!_9U[0]){var _9V=_9U[1];return _9V>=0?E(_9V)==0?E(_9Q):E(_9E):E(_9R);}else{var _9W=I_compareInt(_9U[1],0);return _9W<=0?E(_9W)==0?E(_9Q):E(_9R):E(_9E);}},_9X=function(_9Y,_9Z){while(1){var _a0=E(_9Y);if(!_a0[0]){var _a1=_a0[1],_a2=E(_9Z);if(!_a2[0]){var _a3=_a2[1];if(!(imul(_a1,_a3)|0)){return [0,imul(_a1,_a3)|0];}else{_9Y=[1,I_fromInt(_a1)];_9Z=[1,I_fromInt(_a3)];continue;}}else{_9Y=[1,I_fromInt(_a1)];_9Z=_a2;continue;}}else{var _a4=E(_9Z);if(!_a4[0]){_9Y=_a0;_9Z=[1,I_fromInt(_a4[1])];continue;}else{return [1,I_mul(_a0[1],_a4[1])];}}}},_a5=[0,_5X,_9X,_6e,_9M,_9H,_9S,_9C],_a6=[0,1],_a7=function(_a8){return [0,E(E(_a8)),E(_a6)];},_a9=function(_aa,_ab){var _ac=E(_aa);if(!_ac[0]){var _ad=_ac[1],_ae=E(_ab);return _ae[0]==0?_ad!=_ae[1]:I_compareInt(_ae[1],_ad)==0?false:true;}else{var _af=_ac[1],_ag=E(_ab);return _ag[0]==0?I_compareInt(_af,_ag[1])==0?false:true:I_compare(_af,_ag[1])==0?false:true;}},_ah=[0,_u,_a9],_ai=function(_aj,_ak){var _al=E(_aj);if(!_al[0]){var _am=_al[1],_an=E(_ak);return _an[0]==0?_am<=_an[1]:I_compareInt(_an[1],_am)>=0;}else{var _ao=_al[1],_ap=E(_ak);return _ap[0]==0?I_compareInt(_ao,_ap[1])<=0:I_compare(_ao,_ap[1])<=0;}},_aq=function(_ar,_as){return !B(_ai(_ar,_as))?E(_ar):E(_as);},_at=function(_au,_av){return !B(_ai(_au,_av))?E(_av):E(_au);},_aw=function(_ax,_ay){var _az=E(_ax);if(!_az[0]){var _aA=_az[1],_aB=E(_ay);if(!_aB[0]){var _aC=_aB[1];return _aA!=_aC?_aA>_aC?2:0:1;}else{var _aD=I_compareInt(_aB[1],_aA);return _aD<=0?_aD>=0?1:2:0;}}else{var _aE=_az[1],_aF=E(_ay);if(!_aF[0]){var _aG=I_compareInt(_aE,_aF[1]);return _aG>=0?_aG<=0?1:2:0;}else{var _aH=I_compare(_aE,_aF[1]);return _aH>=0?_aH<=0?1:2:0;}}},_aI=[0,_ah,_aw,_C,_6s,_6A,_ai,_aq,_at],_aJ=[0,_a5,_aI,_a7],_aK=[0,_aJ,_78,_99,_9x,_8t,_8Y,_9l,_8M,_9A],_aL=[0,1],_aM=function(_aN,_aO){while(1){if(!B(_u(_aO,_8s))){var _aP=_aO,_aQ=B(_9x(_aN,_aO));_aN=_aP;_aO=_aQ;continue;}else{return E(_aN);}}},_aR=5,_aS=new T(function(){return B(_8o(_aR));}),_aT=new T(function(){return die(_aS);}),_aU=function(_aV,_aW){if(!B(_u(_aW,_8s))){var _aX=B(_aM(B(_9H(_aV)),B(_9H(_aW))));return !B(_u(_aX,_8s))?[0,B(_91(_aV,_aX)),B(_91(_aW,_aX))]:E(_8r);}else{return E(_aT);}},_aY=function(_aZ,_b0,_b1,_b2){var _b3=B(_9X(_b0,_b1));return new F(function(){return _aU(B(_9X(B(_9X(_aZ,_b2)),B(_9S(_b3)))),B(_9H(_b3)));});},_b4=function(_b5){return E(E(_b5)[1]);},_b6=function(_b7,_b8,_b9){var _ba=new T(function(){if(!B(_u(_b9,_8s))){var _bb=B(_9c(_b8,_b9)),_bc=[0,_bb[1],_bb[2]];}else{var _bc=E(_8r);}return _bc;});return [0,new T(function(){return B(A(_5f,[B(_5U(B(_b4(_b7)))),new T(function(){return E(E(_ba)[1]);})]));}),new T(function(){return [0,E(E(E(_ba)[2])),E(_b9)];})];},_bd=function(_be,_bf,_bg){var _bh=B(_b6(_be,_bf,_bg)),_bi=_bh[1],_bj=E(_bh[2]);if(!B(_C(B(_9X(_bj[1],_a6)),B(_9X(_8s,_bj[2]))))){return E(_bi);}else{var _bk=E(B(_b4(_be))[1]);return new F(function(){return A(_bk[3],[_bi,new T(function(){return B(A(_bk[7],[_a6]));})]);});}},_bl=function(_bm,_bn,_bo,_bp){var _bq=B(A(_bm,[_bo])),_br=B(A(_bm,[_bp])),_bs=B(_aY(_bq[1],_bq[2],_br[1],_br[2]));return new F(function(){return _bd(_bn,_bs[1],_bs[2]);});},_bt=[0,2],_bu=function(_bv){return E(E(_bv)[4]);},_bw=function(_bx,_by,_bz,_bA,_bB,_bC,_bD,_bE,_bF,_bG,_bH,_bI,_bJ,_bK,_bL,_bM,_bN){var _bO=E(_bD)[1],_bP=new T(function(){return B(A(_5b,[_bC,new T(function(){var _bQ=E(_bC),_bR=_bQ[2],_bS=_bQ[7],_bT=new T(function(){return B(A(_bQ[1],[_bN,_by]));});return B(A(_bQ[3],[_bT,new T(function(){var _bU=new T(function(){return B(A(_bR,[new T(function(){return B(A(_bS,[_bt]));}),_by]));});return B(A(_bR,[new T(function(){return B(A(_bS,[new T(function(){return B(_bl(_bL,_aK,_bT,_bU));})]));}),_bU]));})]));}),_by]));}),_bV=B(A(_bB,[_bP]));if(!B(A(_bO,[_bV,new T(function(){return B(A(_5f,[_bC,_aL]));})]))){if(!B(A(_bO,[_bV,new T(function(){return B(A(_bu,[_bC,new T(function(){return B(A(_5f,[_bC,_aL]));})]));})]))){if(!B(A(_bO,[_bV,new T(function(){return B(A(_5f,[_bC,_56]));})]))){var _bW=E(_bC)[2],_bX=E(_bM),_bY=_bX[1],_bZ=_bX[2],_c0=_bX[3],_c1=new T(function(){return B(A(_5d,[_bx,new T(function(){return B(A(_bA,[_bP]));}),new T(function(){return B(A(_bz,[new T(function(){var _c2=B(_2W(_bx)),_c3=_c2[1],_c4=_c2[2];return B(A(_c3,[new T(function(){return B(A(_c3,[new T(function(){return B(A(_c4,[_bY,_bY]));}),new T(function(){return B(A(_c4,[_bZ,_bZ]));})]));}),new T(function(){return B(A(_c4,[_c0,_c0]));})]));})]));})]));});return [0,B(A(_bW,[_c1,_bY])),B(A(_bW,[_c1,_bZ])),B(A(_bW,[_c1,_c0])),E(_bV)];}else{var _c5=E(_bx),_c6=_c5[2],_c7=E(_bM),_c8=_c7[1],_c9=_c7[2],_ca=_c7[3],_cb=new T(function(){return B(A(_bz,[new T(function(){var _cc=E(_c5[1]),_cd=_cc[1],_ce=_cc[2];return B(A(_cd,[new T(function(){return B(A(_cd,[new T(function(){return B(A(_ce,[_c8,_c8]));}),new T(function(){return B(A(_ce,[_c9,_c9]));})]));}),new T(function(){return B(A(_ce,[_ca,_ca]));})]));})]));});return [0,B(A(_c6,[_c8,_cb])),B(A(_c6,[_c9,_cb])),B(A(_c6,[_ca,_cb])),E(_bV)];}}else{var _cf=B(A(_5f,[_bC,_56]));return [0,_cf,_cf,_cf,E(_bV)];}}else{var _cg=B(A(_5f,[_bC,_56]));return [0,_cg,_cg,_cg,E(_bV)];}},_ch=function(_ci,_cj,_ck){return [0,_ci,_cj,_ck];},_cl=function(_cm,_cn,_co,_cp){return function(_cq){return new F(function(){return _ch(new T(function(){var _cr=E(_cm),_cs=E(_cn),_ct=E(_cs[2]),_cu=B(_bw(_cr[1],_cr[2],_cr[4],_cr[8],_cr[10],_cs[1],_ct[1],_ct[2],_ct[3],_ct[4],_ct[5],_ct[6],_ct[7],_ct[8],_cs[3],_co,_cp));return [0,E(_cu[1]),E(_cu[2]),E(_cu[3]),E(_cu[4])];}),new T(function(){var _cv=B(A(B(_5U(_cn))[7],[_5T]));return [0,E(_cv),E(_cv),E(_cv)];}),_cq);});};},_cw=[0,0],_cx=[0,12],_cy=[0,4],_cz=[0,16],_cA=[0,3],_cB=6,_cC=false,_cD=true,_cE=1,_cF=function(_cG){var _cH=B(A(_cG,[_])),_cI=_cH;return E(_cI);},_cJ=function(_cK){return new F(function(){return _cF(function(_){var _=0;return new F(function(){return eval(_cK);});});});},_cL=new T(function(){return B(_cJ("(function(ctx, target, buffer) {ctx.bindBuffer(target, buffer);})"));}),_cM=function(_cN){switch(E(_cN)){case 0:return E(5120);case 1:return E(5121);case 2:return E(5122);case 3:return E(5123);case 4:return E(5124);case 5:return E(5125);default:return E(5126);}},_cO=function(_){var _=0;return new F(function(){return A(_cJ,["false",_]);});},_cP=new T(function(){return B(_cF(_cO));}),_cQ=function(_){var _=0;return new F(function(){return A(_cJ,["true",_]);});},_cR=new T(function(){return B(_cF(_cQ));}),_cS=new T(function(){return B(_cJ("(function(ctx, index, size, type, normalized, stride, offset) {ctx.vertexAttribPointer(index, size, type, normalized, stride, offset);})"));}),_cT=function(_cU){return function(_cV){return function(_cW){return function(_cX){return function(_cY){return function(_cZ){return function(_d0,_){var _d1=B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(_cS,[E(E(_cU)[1])]));}),[E(E(_cV)[1])]));}),[E(E(_cW)[1])]));}),[B(_cM(_cX))]));}),[!E(_cY)?E(_cP):E(_cR)]));}),[E(E(_cZ)[1])]));}),[E(E(_d0)[1]),_])),_d2=_d1;return _0;};};};};};};},_d3=function(_d4,_d5,_d6,_d7,_){var _d8=[0,_d4],_d9=B(A(_cL,[E(_d8[1]),E(34962),E(_d5),_])),_da=_d9,_db=B(A(_cT,[_d8,_d6,_cA,_cB,_cC,_cz,_cw,_])),_dc=_db;return new F(function(){return A(_cT,[_d8,_d7,_cy,_cE,_cD,_cz,_cx,_]);});},_dd=1,_de=6,_df=new T(function(){return B(_cJ("(function(ctx, mode, first, count) {ctx.drawArrays(mode, first, count);})"));}),_dg=function(_dh){switch(E(_dh)){case 0:return E(0);case 1:return E(3);case 2:return E(2);case 3:return E(1);case 4:return E(5);case 5:return E(6);default:return E(4);}},_di=new T(function(){return B(_cJ("(function(ctx, mode, count, type, offset) {ctx.drawElements(mode, count, type, offset);})"));}),_dj=function(_dk){return function(_dl){return function(_dm){var _dn=new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(_di,[E(E(_dk)[1])]));}),[B(_dg(_dl))]));}),[E(E(_dm)[1])]));});return function(_do){return function(_dp,_){var _dq=B(A(new T(function(){if(!E(_do)){var _dr=E(5121);}else{var _dr=E(5123);}var _ds=_dr,_dt=B(A(_dn,[_ds]));return _dt;}),[E(E(_dp)[1]),_])),_du=_dq;return _0;};};};};},_dv=function(_dw,_dx,_){var _dy=[0,_dw],_dz=_dy[1],_dA=E(_dx);if(!_dA[0]){var _dB=E(_dA[6]),_dC=B(_d3(_dw,E(_dA[4])[1],_dB[1],_dB[2],_)),_dD=_dC,_dE=B(A(_cL,[E(_dz),E(34963),E(E(_dA[5])[1]),_])),_dF=_dE;return new F(function(){return A(_dj,[_dy,_de,_dA[1],_dd,_cw,_]);});}else{var _dG=E(_dA[4]),_dH=B(_d3(_dw,E(_dA[3])[1],_dG[1],_dG[2],_)),_dI=_dH,_dJ=B(A(_df,[E(_dz),E(4),E(0),E(E(_dA[1])[1]),_])),_dK=_dJ;return _0;}},_dL=function(_dM,_dN,_dO,_){var _dP=E(_dO)[1],_=writeOffAddr("f32",4,_dP,0,E(_dM)[1]);return new F(function(){return A(_dN,[[0,plusAddr(_dP,4)],_]);});},_dQ=function(_dR,_dS,_dT,_dU,_dV,_dW,_dX,_dY,_dZ,_e0,_e1,_e2,_e3,_e4,_e5,_e6,_e7,_e8){return new F(function(){return A(_dR,[_dT,new T(function(){return B(A(_dR,[_dX,new T(function(){return B(A(_dR,[_e1,new T(function(){return B(A(_dR,[_e5,new T(function(){return B(A(_dR,[_dU,new T(function(){return B(A(_dR,[_dY,new T(function(){return B(A(_dR,[_e2,new T(function(){return B(A(_dR,[_e6,new T(function(){return B(A(_dR,[_dV,new T(function(){return B(A(_dR,[_dZ,new T(function(){return B(A(_dR,[_e3,new T(function(){return B(A(_dR,[_e7,new T(function(){return B(A(_dR,[_dW,new T(function(){return B(A(_dR,[_e0,new T(function(){return B(A(_dR,[_e4,new T(function(){return B(A(_dR,[_e8,_dS]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]);});},_e9=function(_ea){return E(_ea);},_eb=function(_ec,_){return _ec;},_ed=new T(function(){return B(_cJ("(function(ctx, uniform, arr) {ctx.uniformMatrix4fv(uniform, ctx.FALSE, arr[\'v\'][\'f32\']);})"));}),_ee=function(_ef,_eg,_eh,_ei,_ej,_ek,_){var _el=B(A(_ef,[_7,_1O,[0,_eh,_e9],_ek])),_em=E(_el[1]),_en=B(A(_dQ,[_dL,_eb,_em[1],_em[2],_em[3],_em[4],_em[5],_em[6],_em[7],_em[8],_em[9],_em[10],_em[11],_em[12],_em[13],_em[14],_em[15],_em[16],new T(function(){return [0,E(_ej)[1]];}),_])),_eo=_en,_ep=B(A(_ed,[E(E(_eg)[1]),E(E(_ei)[1]),E(E(_ej)[1]),_])),_eq=_ep;return _el[2];},_er=function(_es,_et){while(1){var _eu=E(_es);if(!_eu[0]){return E(_et);}else{_es=_eu[2];var _ev=_et+1|0;_et=_ev;continue;}}},_ew=new T(function(){return [0,"(function(ctx, target, data, usage) {ctx.bufferData(target, data[\'b\'], usage);})"];}),_ex=new T(function(){return B(_cJ("(function(ctx) {return ctx.createBuffer();})"));}),_ey=function(_ez,_eA,_eB,_){while(1){var _eC=E(_eA);if(!_eC[0]){return _0;}else{var _eD=E(_ez);if(!_eD[0]){return _0;}else{var _eE=E(_eD[1]),_eF=E(_eC[1]),_=writeOffAddr("f32",4,_eB,0,E(_eE[1])[1]),_=writeOffAddr("f32",4,plusAddr(_eB,4),0,E(_eE[2])[1]),_=writeOffAddr("f32",4,plusAddr(_eB,8),0,E(_eE[3])[1]),_=writeOffAddr("w8",1,plusAddr(_eB,12),0,E(_eF[1])[1]),_=writeOffAddr("w8",1,plusAddr(_eB,13),0,E(_eF[2])[1]),_=writeOffAddr("w8",1,plusAddr(_eB,14),0,E(_eF[3])[1]),_=writeOffAddr("w8",1,plusAddr(_eB,15),0,255);_ez=_eD[2];_eA=_eC[2];var _eG=plusAddr(_eB,16);_eB=_eG;continue;}}}},_eH=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_eI=new T(function(){return B(unCStr("base"));}),_eJ=new T(function(){return B(unCStr("IOException"));}),_eK=new T(function(){var _eL=hs_wordToWord64(4053623282),_eM=_eL,_eN=hs_wordToWord64(3693590983),_eO=_eN;return [0,_eM,_eO,[0,_eM,_eO,_eI,_eH,_eJ],_7n];}),_eP=function(_eQ){return E(_eK);},_eR=function(_eS){var _eT=E(_eS);return new F(function(){return _7x(B(_7v(_eT[1])),_eP,_eT[2]);});},_eU=new T(function(){return B(unCStr(": "));}),_eV=[0,41],_eW=new T(function(){return B(unCStr(" ("));}),_eX=new T(function(){return B(unCStr("already exists"));}),_eY=new T(function(){return B(unCStr("does not exist"));}),_eZ=new T(function(){return B(unCStr("protocol error"));}),_f0=new T(function(){return B(unCStr("failed"));}),_f1=new T(function(){return B(unCStr("invalid argument"));}),_f2=new T(function(){return B(unCStr("inappropriate type"));}),_f3=new T(function(){return B(unCStr("hardware fault"));}),_f4=new T(function(){return B(unCStr("unsupported operation"));}),_f5=new T(function(){return B(unCStr("timeout"));}),_f6=new T(function(){return B(unCStr("resource vanished"));}),_f7=new T(function(){return B(unCStr("interrupted"));}),_f8=new T(function(){return B(unCStr("resource busy"));}),_f9=new T(function(){return B(unCStr("resource exhausted"));}),_fa=new T(function(){return B(unCStr("end of file"));}),_fb=new T(function(){return B(unCStr("illegal operation"));}),_fc=new T(function(){return B(unCStr("permission denied"));}),_fd=new T(function(){return B(unCStr("user error"));}),_fe=new T(function(){return B(unCStr("unsatisified constraints"));}),_ff=new T(function(){return B(unCStr("system error"));}),_fg=function(_fh,_fi){switch(E(_fh)){case 0:return new F(function(){return _7S(_eX,_fi);});break;case 1:return new F(function(){return _7S(_eY,_fi);});break;case 2:return new F(function(){return _7S(_f8,_fi);});break;case 3:return new F(function(){return _7S(_f9,_fi);});break;case 4:return new F(function(){return _7S(_fa,_fi);});break;case 5:return new F(function(){return _7S(_fb,_fi);});break;case 6:return new F(function(){return _7S(_fc,_fi);});break;case 7:return new F(function(){return _7S(_fd,_fi);});break;case 8:return new F(function(){return _7S(_fe,_fi);});break;case 9:return new F(function(){return _7S(_ff,_fi);});break;case 10:return new F(function(){return _7S(_eZ,_fi);});break;case 11:return new F(function(){return _7S(_f0,_fi);});break;case 12:return new F(function(){return _7S(_f1,_fi);});break;case 13:return new F(function(){return _7S(_f2,_fi);});break;case 14:return new F(function(){return _7S(_f3,_fi);});break;case 15:return new F(function(){return _7S(_f4,_fi);});break;case 16:return new F(function(){return _7S(_f5,_fi);});break;case 17:return new F(function(){return _7S(_f6,_fi);});break;default:return new F(function(){return _7S(_f7,_fi);});}},_fj=[0,125],_fk=new T(function(){return B(unCStr("{handle: "));}),_fl=function(_fm,_fn,_fo,_fp,_fq,_fr){var _fs=new T(function(){var _ft=new T(function(){return B(_fg(_fn,new T(function(){var _fu=E(_fp);return _fu[0]==0?E(_fr):B(_7S(_eW,new T(function(){return B(_7S(_fu,[1,_eV,_fr]));},1)));},1)));},1),_fv=E(_fo);return _fv[0]==0?E(_ft):B(_7S(_fv,new T(function(){return B(_7S(_eU,_ft));},1)));},1),_fw=E(_fq);if(!_fw[0]){var _fx=E(_fm);if(!_fx[0]){return E(_fs);}else{var _fy=E(_fx[1]);return _fy[0]==0?B(_7S(_fk,new T(function(){return B(_7S(_fy[1],[1,_fj,new T(function(){return B(_7S(_eU,_fs));})]));},1))):B(_7S(_fk,new T(function(){return B(_7S(_fy[1],[1,_fj,new T(function(){return B(_7S(_eU,_fs));})]));},1)));}}else{return new F(function(){return _7S(_fw[1],new T(function(){return B(_7S(_eU,_fs));},1));});}},_fz=function(_fA){var _fB=E(_fA);return new F(function(){return _fl(_fB[1],_fB[2],_fB[3],_fB[4],_fB[6],_7n);});},_fC=function(_fD,_fE){var _fF=E(_fD);return new F(function(){return _fl(_fF[1],_fF[2],_fF[3],_fF[4],_fF[6],_fE);});},_fG=function(_fH,_fI){return new F(function(){return _88(_fC,_fH,_fI);});},_fJ=function(_fK,_fL,_fM){var _fN=E(_fL);return new F(function(){return _fl(_fN[1],_fN[2],_fN[3],_fN[4],_fN[6],_fM);});},_fO=[0,_fJ,_fz,_fG],_fP=new T(function(){return [0,_eP,_fO,_fQ,_eR];}),_fQ=function(_fR){return [0,_fP,_fR];},_fS=function(_fT,_){return new F(function(){return die(new T(function(){return B(_fQ(_fT));}));});},_fU=function(_fV,_){return new F(function(){return _fS(_fV,_);});},_fW=[0],_fX=3,_fY=new T(function(){return B(unCStr("out of memory"));}),_fZ=new T(function(){return B(unCStr("malloc"));}),_g0=[0,_fW,_fX,_fZ,_fY,_fW,_fW],_g1=function(_g2,_g3,_g4,_){var _g5=B(_er(_g3,0)),_g6=malloc((imul(_g5,16)|0)>>>0),_g7=_g6;if(!addrEq(_g7,0)){var _g8=B(_ey(_g3,_g4,_g7,_)),_g9=_g8,_ga=E(E(_g2)[1]),_gb=B(A(_ex,[_ga,_])),_gc=_gb,_gd=E(_gc),_ge=E(34962),_gf=B(A(_cL,[_ga,_ge,_gd,_])),_gg=_gf,_gh=B(A(_cJ,[E(_ew)[1],_ga,_ge,E(_g7),E(35044),_])),_gi=_gh;return [0,[0,_g5],[0,_g7],[0,_gd]];}else{return new F(function(){return _fU(_g0,_);});}},_gj=[0,2],_gk=function(_gl){return E(_gj);},_gm=function(_gn){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_gn));}))));});},_go=new T(function(){return B(_gm("ww_s7jz4{v} [lid] base:GHC.Ptr.Ptr{tc 33A} a{tv a7j62} [tv]\n                  -> a{tv a7j62} [tv] -> ghc-prim:GHC.Types.IO{tc 32I} ()"));}),_gp=new T(function(){return B(_gm("ww_s7jz3{v} [lid] base:GHC.Ptr.Ptr{tc 33A} a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.IO{tc 32I} a{tv a7j62} [tv]"));}),_gq=new T(function(){return B(_gm("ww_s7jz2{v} [lid] forall b{tv i3yg7} [tv].\n                  base:GHC.Ptr.Ptr{tc 33A} b{tv i3yg7} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}\n                  -> a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.IO{tc 32I} ()"));}),_gr=new T(function(){return B(_gm("ww_s7jz1{v} [lid] forall b{tv i3yg6} [tv].\n                  base:GHC.Ptr.Ptr{tc 33A} b{tv i3yg6} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}\n                  -> ghc-prim:GHC.Types.IO{tc 32I} a{tv a7j62} [tv]"));}),_gs=new T(function(){return B(_gm("ww_s7jyZ{v} [lid] base:GHC.Ptr.Ptr{tc 33A} a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}\n                  -> ghc-prim:GHC.Types.IO{tc 32I} a{tv a7j62} [tv]"));}),_gt=new T(function(){return B(_gm("ww_s7jyY{v} [lid] a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}"));}),_gu=function(_gv,_gw,_gx,_){var _=writeOffAddr("w16",2,E(_gv)[1],E(_gw)[1],E(_gx)[1]);return _0;},_gy=[0,_gk,_gt,_gs,_gu,_gr,_gq,_gp,_go],_gz=function(_gA){return [0,B(_er(_gA,0))];},_gB=function(_gC){return E(E(_gC)[4]);},_gD=function(_gE,_gF,_gG,_){return new F(function(){return (function(_gH,_gI,_){while(1){var _gJ=E(_gH);if(!_gJ[0]){return _0;}else{var _gK=B(A(new T(function(){return B(_gB(_gE));}),[_gF,[0,_gI],_gJ[1],_])),_gL=_gK;_gH=_gJ[2];var _gM=_gI+1|0;_gI=_gM;continue;}}})(_gG,0,_);});},_gN=function(_gO,_gP,_gQ,_gR,_gS,_){var _gT=B(_g1(_gO,_gQ,_gR,_)),_gU=_gT,_gV=E(_gU),_gW=malloc((imul(B(_er(_gS,0)),2)|0)>>>0),_gX=_gW;if(!addrEq(_gX,0)){var _gY=[0,_gX],_gZ=B(_gD(_gy,_gY,_gS,_)),_h0=_gZ,_h1=E(E(_gO)[1]),_h2=B(A(_ex,[_h1,_])),_h3=_h2,_h4=E(_h3),_h5=E(34963),_h6=B(A(_cL,[_h1,_h5,_h4,_])),_h7=_h6,_h8=B(A(_cJ,[E(_ew)[1],_h1,_h5,E(_gX),E(35044),_])),_h9=_h8;return [0,new T(function(){return B(_gz(_gS));}),_gV[2],_gY,_gV[3],[0,_h4],_gP];}else{return new F(function(){return _fU(_g0,_);});}},_ha=function(_hb,_hc){while(1){var _hd=(function(_he,_hf){if(_he<=1680){return [0,[0,_he],_hf];}else{var _hg=quot(_he,2);_hc=new T(function(){return [0,quot(E(_hf)[1],2)];});_hb=_hg;return null;}})(_hb,_hc);if(_hd!=null){return _hd;}}},_hh=function(_hi,_hj,_hk,_hl,_hm,_hn,_ho,_hp){return [0,B(A(_hj,[new T(function(){return B(A(_hi,[_hl,_hp]));}),new T(function(){return B(A(_hi,[_hm,_ho]));})])),B(A(_hj,[new T(function(){return B(A(_hi,[_hm,_hn]));}),new T(function(){return B(A(_hi,[_hk,_hp]));})])),B(A(_hj,[new T(function(){return B(A(_hi,[_hk,_ho]));}),new T(function(){return B(A(_hi,[_hl,_hn]));})]))];},_hq=function(_hr,_hs,_ht,_hu,_hv,_hw,_hx,_hy,_hz,_hA,_hB,_hC,_hD,_hE){var _hF=E(_hr),_hG=_hF[1],_hH=_hF[2],_hI=_hF[3],_hJ=_hF[4],_hK=_hF[7],_hL=B(A(_hI,[_hz,_hC])),_hM=B(A(_hI,[_hA,_hD])),_hN=B(A(_hI,[_hB,_hE])),_hO=new T(function(){return B(A(_hv,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hH,[_hL,_hL]));}),new T(function(){return B(A(_hH,[_hM,_hM]));})]));}),new T(function(){return B(A(_hH,[_hN,_hN]));})]));})]));}),_hP=B(A(_hs,[_hL,_hO])),_hQ=B(A(_hs,[_hM,_hO])),_hR=B(A(_hs,[_hN,_hO])),_hS=B(_hh(_hH,_hI,_hw,_hx,_hy,_hP,_hQ,_hR)),_hT=_hS[1],_hU=_hS[2],_hV=_hS[3],_hW=new T(function(){return B(A(_hv,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hH,[_hT,_hT]));}),new T(function(){return B(A(_hH,[_hU,_hU]));})]));}),new T(function(){return B(A(_hH,[_hV,_hV]));})]));})]));}),_hX=B(A(_hs,[_hT,_hW])),_hY=B(A(_hs,[_hU,_hW])),_hZ=B(A(_hs,[_hV,_hW])),_i0=B(_hh(_hH,_hI,_hP,_hQ,_hR,_hX,_hY,_hZ)),_i1=_i0[1],_i2=_i0[2],_i3=_i0[3],_i4=B(A(_hK,[_3A]));return [0,E(_hX),E(_hY),E(_hZ),E(B(A(_hJ,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hH,[_hX,_hz]));}),new T(function(){return B(A(_hH,[_hY,_hA]));})]));}),new T(function(){return B(A(_hH,[_hZ,_hB]));})]));})]))),E(_i1),E(_i2),E(_i3),E(B(A(_hJ,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hH,[_i1,_hz]));}),new T(function(){return B(A(_hH,[_i2,_hA]));})]));}),new T(function(){return B(A(_hH,[_i3,_hB]));})]));})]))),E(_hP),E(_hQ),E(_hR),E(B(A(_hJ,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hG,[new T(function(){return B(A(_hH,[_hP,_hz]));}),new T(function(){return B(A(_hH,[_hQ,_hA]));})]));}),new T(function(){return B(A(_hH,[_hR,_hB]));})]));})]))),E(_i4),E(_i4),E(_i4),E(B(A(_hK,[_3z])))];},_i5=function(_i6,_i7){var _i8=jsShowI(_i6),_i9=_i8;return new F(function(){return _7S(fromJSStr(_i9),_i7);});},_ia=[0,41],_ib=[0,40],_ic=function(_id,_ie,_if){if(_ie>=0){return new F(function(){return _i5(_ie,_if);});}else{return _id<=6?B(_i5(_ie,_if)):[1,_ib,new T(function(){var _ig=jsShowI(_ie),_ih=_ig;return B(_7S(fromJSStr(_ih),[1,_ia,_if]));})];}},_ii=new T(function(){return B(_cJ("(function(ctx, program, shader) {ctx.attachShader(program, shader);})"));}),_ij=new T(function(){return B(_cJ("(function(ctx, r, g, b, a) {ctx.clearColor(r, g, b, a);})"));}),_ik=function(_il){return function(_im){return function(_in){return function(_io){return function(_ip,_){var _iq=B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(_ij,[E(E(_il)[1])]));}),[E(E(_im)[1])]));}),[E(E(_in)[1])]));}),[E(E(_io)[1])]));}),[E(E(_ip)[1]),_])),_ir=_iq;return _0;};};};};},_is=new T(function(){return B(_cJ("(function(ctx, mask) {ctx.clear(mask);})"));}),_it=new T(function(){return B(_cJ("(function(ctx, shader) {ctx.compileShader(shader);})"));}),_iu=new T(function(){return B(_cJ("(function(ctx) {return ctx.createProgram();})"));}),_iv=new T(function(){return B(_cJ("(function(ctx, type) {return ctx.createShader(type);})"));}),_iw=new T(function(){return B(_cJ("(function(ctx, index) {ctx.enableVertexAttribArray(index);})"));}),_ix=new T(function(){return B(_cJ("(function(ctx, cap) {ctx.enable(cap);})"));}),_iy=7,_iz=function(_iA){return [0,_fW,_iy,_7n,_iA,_fW,_fW];},_iB=function(_iC,_){return new F(function(){return die(new T(function(){return B(_fQ(new T(function(){return B(_iz(_iC));})));}));});},_iD=function(_iE,_){return new F(function(){return _iB(_iE,_);});},_iF=new T(function(){return B(_cJ("(function(ctx, program, name) {return ctx.getAttribLocation(program, name);})"));}),_iG=function(_iH){return function(_iI){return function(_iJ,_){var _iK=B(A(new T(function(){return B(A(new T(function(){return B(A(_iF,[E(E(_iH)[1])]));}),[E(E(_iI)[1])]));}),[E(toJSStr(E(_iJ))),_])),_iL=_iK;return [0,_iL];};};},_iM=new T(function(){return B(_cJ("(function(elt, name) {return elt.getContext(name);})"));}),_iN=new T(function(){return B(_cJ("(function(ctx, program, name) {return ctx.getUniformLocation(program, name);})"));}),_iO=function(_iP){return function(_iQ){return function(_iR,_){var _iS=B(A(new T(function(){return B(A(new T(function(){return B(A(_iN,[E(E(_iP)[1])]));}),[E(E(_iQ)[1])]));}),[E(toJSStr(E(_iR))),_])),_iT=_iS;return [0,_iT];};};},_iU=[0,2],_iV=function(_iW,_iX,_iY,_iZ,_j0,_j1,_j2,_j3,_j4){var _j5=E(_iW),_j6=_j5[2],_j7=_j5[3],_j8=_j5[7],_j9=new T(function(){return B(A(_j6,[_j1,new T(function(){return B(A(_j0,[new T(function(){return B(A(_iX,[_j3,new T(function(){return B(A(_j8,[_iU]));})]));})]));})]));}),_ja=B(A(_j8,[_3A]));return [0,E(B(A(_iX,[_j1,new T(function(){return B(A(_j6,[_j4,_j9]));})]))),E(_ja),E(_ja),E(_ja),E(_ja),E(B(A(_iX,[_j1,_j9]))),E(_ja),E(_ja),E(_ja),E(_ja),E(B(A(_iX,[new T(function(){return B(A(_j5[1],[_j1,_j2]));}),new T(function(){return B(A(_j7,[_j1,_j2]));})]))),E(B(A(_iX,[new T(function(){return B(A(_j6,[new T(function(){return B(A(_j6,[new T(function(){return B(A(_j8,[_iU]));}),_j1]));}),_j2]));}),new T(function(){return B(A(_j7,[_j1,_j2]));})]))),E(_ja),E(_ja),E(B(A(_j5[4],[new T(function(){return B(A(_j8,[_3z]));})]))),E(_ja)];},_jb=[0,0.7853981852531433],_jc=[0,100],_jd=[0,0.1],_je=function(_jf,_jg,_){var _jh=malloc(64),_ji=_jh;return !addrEq(_ji,0)?[0,new T(function(){return B(_iV(_1i,_1j,_Q,_N,_1K,_jd,_jc,_jb,_jf));}),[0,_jg,[0,_ji]]]:B(_fU(_g0,_));},_jj=new T(function(){return B(_cJ("(function(ctx, program) {ctx.linkProgram(program);})"));}),_jk=new T(function(){return B(unCStr("Pattern match failure in do expression at lwgl4.hs:113:5-15"));}),_jl=new T(function(){return B(unCStr("Control.Exception.Base"));}),_jm=new T(function(){return B(unCStr("base"));}),_jn=new T(function(){return B(unCStr("PatternMatchFail"));}),_jo=new T(function(){var _jp=hs_wordToWord64(18445595),_jq=_jp,_jr=hs_wordToWord64(52003073),_js=_jr;return [0,_jq,_js,[0,_jq,_js,_jm,_jl,_jn],_7n];}),_jt=function(_ju){return E(_jo);},_jv=function(_jw){var _jx=E(_jw);return new F(function(){return _7x(B(_7v(_jx[1])),_jt,_jx[2]);});},_jy=function(_jz){return E(E(_jz)[1]);},_jA=function(_jB,_jC){return new F(function(){return _7S(E(_jB)[1],_jC);});},_jD=function(_jE,_jF){return new F(function(){return _88(_jA,_jE,_jF);});},_jG=function(_jH,_jI,_jJ){return new F(function(){return _7S(E(_jI)[1],_jJ);});},_jK=[0,_jG,_jy,_jD],_jL=new T(function(){return [0,_jt,_jK,_jM,_jv];}),_jM=function(_jN){return [0,_jL,_jN];},_jO=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_jP=function(_jQ,_jR){return new F(function(){return die(new T(function(){return B(A(_jR,[_jQ]));}));});},_jS=function(_jT,_jU){var _jV=E(_jU);if(!_jV[0]){return [0,_7n,_7n];}else{var _jW=_jV[1];if(!B(A(_jT,[_jW]))){return [0,_7n,_jV];}else{var _jX=new T(function(){var _jY=B(_jS(_jT,_jV[2]));return [0,_jY[1],_jY[2]];});return [0,[1,_jW,new T(function(){return E(E(_jX)[1]);})],new T(function(){return E(E(_jX)[2]);})];}}},_jZ=[0,32],_k0=[0,10],_k1=[1,_k0,_7n],_k2=function(_k3){return E(E(_k3)[1])==124?false:true;},_k4=function(_k5,_k6){var _k7=B(_jS(_k2,B(unCStr(_k5)))),_k8=_k7[1],_k9=function(_ka,_kb){return new F(function(){return _7S(_ka,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_7S(_k6,new T(function(){return B(_7S(_kb,_k1));},1)));})));},1));});},_kc=E(_k7[2]);if(!_kc[0]){return new F(function(){return _k9(_k8,_7n);});}else{return E(E(_kc[1])[1])==124?B(_k9(_k8,[1,_jZ,_kc[2]])):B(_k9(_k8,_7n));}},_kd=function(_ke){return new F(function(){return _jP([0,new T(function(){return B(_k4(_ke,_jO));})],_jM);});},_kf=new T(function(){return B(_kd("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus"));}),_kg=function(_kh,_ki){while(1){var _kj=(function(_kk,_kl){var _km=E(_kk);switch(_km[0]){case 0:var _kn=E(_kl);if(!_kn[0]){return [0];}else{_kh=B(A(_km[1],[_kn[1]]));_ki=_kn[2];return null;}break;case 1:var _ko=B(A(_km[1],[_kl])),_kp=_kl;_kh=_ko;_ki=_kp;return null;case 2:return [0];case 3:return [1,[0,_km[1],_kl],new T(function(){return B(_kg(_km[2],_kl));})];default:return E(_km[1]);}})(_kh,_ki);if(_kj!=null){return _kj;}}},_kq=function(_kr,_ks){var _kt=function(_ku){var _kv=E(_ks);if(_kv[0]==3){return [3,_kv[1],new T(function(){return B(_kq(_kr,_kv[2]));})];}else{var _kw=E(_kr);if(_kw[0]==2){return E(_kv);}else{var _kx=E(_kv);if(_kx[0]==2){return E(_kw);}else{var _ky=function(_kz){var _kA=E(_kx);if(_kA[0]==4){return [1,function(_kB){return [4,new T(function(){return B(_7S(B(_kg(_kw,_kB)),_kA[1]));})];}];}else{var _kC=E(_kw);if(_kC[0]==1){var _kD=_kC[1],_kE=E(_kA);return _kE[0]==0?[1,function(_kF){return new F(function(){return _kq(B(A(_kD,[_kF])),_kE);});}]:[1,function(_kG){return new F(function(){return _kq(B(A(_kD,[_kG])),new T(function(){return B(A(_kE[1],[_kG]));}));});}];}else{var _kH=E(_kA);return _kH[0]==0?E(_kf):[1,function(_kI){return new F(function(){return _kq(_kC,new T(function(){return B(A(_kH[1],[_kI]));}));});}];}}},_kJ=E(_kw);switch(_kJ[0]){case 1:var _kK=E(_kx);if(_kK[0]==4){return [1,function(_kL){return [4,new T(function(){return B(_7S(B(_kg(B(A(_kJ[1],[_kL])),_kL)),_kK[1]));})];}];}else{return new F(function(){return _ky(_);});}break;case 4:var _kM=_kJ[1],_kN=E(_kx);switch(_kN[0]){case 0:return [1,function(_kO){return [4,new T(function(){return B(_7S(_kM,new T(function(){return B(_kg(_kN,_kO));},1)));})];}];case 1:return [1,function(_kP){return [4,new T(function(){return B(_7S(_kM,new T(function(){return B(_kg(B(A(_kN[1],[_kP])),_kP));},1)));})];}];default:return [4,new T(function(){return B(_7S(_kM,_kN[1]));})];}break;default:return new F(function(){return _ky(_);});}}}}},_kQ=E(_kr);switch(_kQ[0]){case 0:var _kR=E(_ks);if(!_kR[0]){return [0,function(_kS){return new F(function(){return _kq(B(A(_kQ[1],[_kS])),new T(function(){return B(A(_kR[1],[_kS]));}));});}];}else{return new F(function(){return _kt(_);});}break;case 3:return [3,_kQ[1],new T(function(){return B(_kq(_kQ[2],_ks));})];default:return new F(function(){return _kt(_);});}},_kT=[0,41],_kU=[1,_kT,_7n],_kV=[0,40],_kW=[1,_kV,_7n],_kX=function(_kY,_kZ){while(1){var _l0=E(_kY);if(!_l0[0]){return E(_kZ)[0]==0?true:false;}else{var _l1=E(_kZ);if(!_l1[0]){return false;}else{if(E(_l0[1])[1]!=E(_l1[1])[1]){return false;}else{_kY=_l0[2];_kZ=_l1[2];continue;}}}}},_l2=function(_l3,_l4){return E(_l3)[1]!=E(_l4)[1];},_l5=function(_l6,_l7){return E(_l6)[1]==E(_l7)[1];},_l8=[0,_l5,_l2],_l9=function(_la,_lb){while(1){var _lc=E(_la);if(!_lc[0]){return E(_lb)[0]==0?true:false;}else{var _ld=E(_lb);if(!_ld[0]){return false;}else{if(E(_lc[1])[1]!=E(_ld[1])[1]){return false;}else{_la=_lc[2];_lb=_ld[2];continue;}}}}},_le=function(_lf,_lg){return !B(_l9(_lf,_lg))?true:false;},_lh=[0,_l9,_le],_li=function(_lj,_lk){var _ll=E(_lj);switch(_ll[0]){case 0:return [0,function(_lm){return new F(function(){return _li(B(A(_ll[1],[_lm])),_lk);});}];case 1:return [1,function(_ln){return new F(function(){return _li(B(A(_ll[1],[_ln])),_lk);});}];case 2:return [2];case 3:return new F(function(){return _kq(B(A(_lk,[_ll[1]])),new T(function(){return B(_li(_ll[2],_lk));}));});break;default:var _lo=function(_lp){var _lq=E(_lp);if(!_lq[0]){return [0];}else{var _lr=E(_lq[1]);return new F(function(){return _7S(B(_kg(B(A(_lk,[_lr[1]])),_lr[2])),new T(function(){return B(_lo(_lq[2]));},1));});}},_ls=B(_lo(_ll[1]));return _ls[0]==0?[2]:[4,_ls];}},_lt=[2],_lu=function(_lv){return [3,_lv,_lt];},_lw=function(_lx,_ly){var _lz=E(_lx);if(!_lz){return new F(function(){return A(_ly,[_0]);});}else{return [0,function(_lA){return E(new T(function(){return B(_lw(_lz-1|0,_ly));}));}];}},_lB=function(_lC,_lD,_lE){return function(_lF){return new F(function(){return A(function(_lG,_lH,_lI){while(1){var _lJ=(function(_lK,_lL,_lM){var _lN=E(_lK);switch(_lN[0]){case 0:var _lO=E(_lL);if(!_lO[0]){return E(_lD);}else{_lG=B(A(_lN[1],[_lO[1]]));_lH=_lO[2];var _lP=_lM+1|0;_lI=_lP;return null;}break;case 1:var _lQ=B(A(_lN[1],[_lL])),_lR=_lL,_lP=_lM;_lG=_lQ;_lH=_lR;_lI=_lP;return null;case 2:return E(_lD);case 3:return function(_lS){return new F(function(){return _lw(_lM,function(_lT){return E(new T(function(){return B(_li(_lN,_lS));}));});});};default:return function(_cq){return new F(function(){return _li(_lN,_cq);});};}})(_lG,_lH,_lI);if(_lJ!=null){return _lJ;}}},[new T(function(){return B(A(_lC,[_lu]));}),_lF,0,_lE]);});};},_lU=function(_lV){return new F(function(){return A(_lV,[_7n]);});},_lW=function(_lX,_lY){var _lZ=function(_m0){var _m1=E(_m0);if(!_m1[0]){return E(_lU);}else{var _m2=_m1[1];return !B(A(_lX,[_m2]))?E(_lU):function(_m3){return [0,function(_m4){return E(new T(function(){return B(A(new T(function(){return B(_lZ(_m1[2]));}),[function(_m5){return new F(function(){return A(_m3,[[1,_m2,_m5]]);});}]));}));}];};}};return function(_m6){return new F(function(){return A(_lZ,[_m6,_lY]);});};},_m7=[6],_m8=new T(function(){return B(unCStr("valDig: Bad base"));}),_m9=new T(function(){return B(err(_m8));}),_ma=function(_mb,_mc){var _md=function(_me,_mf){var _mg=E(_me);if(!_mg[0]){return function(_mh){return new F(function(){return A(_mh,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{var _mi=E(_mg[1])[1],_mj=function(_mk){return function(_ml){return [0,function(_mm){return E(new T(function(){return B(A(new T(function(){return B(_md(_mg[2],function(_mn){return new F(function(){return A(_mf,[[1,_mk,_mn]]);});}));}),[_ml]));}));}];};};switch(E(E(_mb)[1])){case 8:if(48>_mi){return function(_mo){return new F(function(){return A(_mo,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{if(_mi>55){return function(_mp){return new F(function(){return A(_mp,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{return new F(function(){return _mj([0,_mi-48|0]);});}}break;case 10:if(48>_mi){return function(_mq){return new F(function(){return A(_mq,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{if(_mi>57){return function(_mr){return new F(function(){return A(_mr,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{return new F(function(){return _mj([0,_mi-48|0]);});}}break;case 16:if(48>_mi){if(97>_mi){if(65>_mi){return function(_ms){return new F(function(){return A(_ms,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{if(_mi>70){return function(_mt){return new F(function(){return A(_mt,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{return new F(function(){return _mj([0,(_mi-65|0)+10|0]);});}}}else{if(_mi>102){if(65>_mi){return function(_mu){return new F(function(){return A(_mu,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{if(_mi>70){return function(_mv){return new F(function(){return A(_mv,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{return new F(function(){return _mj([0,(_mi-65|0)+10|0]);});}}}else{return new F(function(){return _mj([0,(_mi-97|0)+10|0]);});}}}else{if(_mi>57){if(97>_mi){if(65>_mi){return function(_mw){return new F(function(){return A(_mw,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{if(_mi>70){return function(_mx){return new F(function(){return A(_mx,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{return new F(function(){return _mj([0,(_mi-65|0)+10|0]);});}}}else{if(_mi>102){if(65>_mi){return function(_my){return new F(function(){return A(_my,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{if(_mi>70){return function(_mz){return new F(function(){return A(_mz,[new T(function(){return B(A(_mf,[_7n]));})]);});};}else{return new F(function(){return _mj([0,(_mi-65|0)+10|0]);});}}}else{return new F(function(){return _mj([0,(_mi-97|0)+10|0]);});}}}else{return new F(function(){return _mj([0,_mi-48|0]);});}}break;default:return E(_m9);}}};return function(_mA){return new F(function(){return A(_md,[_mA,_e9,function(_mB){var _mC=E(_mB);return _mC[0]==0?[2]:B(A(_mc,[_mC]));}]);});};},_mD=[0,10],_mE=[0,10],_mF=[0,0],_mG=function(_mH,_mI,_mJ){while(1){var _mK=E(_mJ);if(!_mK[0]){return E(_mI);}else{var _mL=B(_5X(B(_9X(_mI,_mH)),B(_2i(E(_mK[1])[1]))));_mJ=_mK[2];_mI=_mL;continue;}}},_mM=function(_mN){var _mO=new T(function(){return B(_kq(B(_kq([0,function(_mP){return E(E(_mP)[1])==45?[1,B(_ma(_mD,function(_mQ){return new F(function(){return A(_mN,[[1,new T(function(){return B(_9M(B(_mG(_mE,_mF,_mQ))));})]]);});}))]:[2];}],[0,function(_mR){return E(E(_mR)[1])==43?[1,B(_ma(_mD,function(_mS){return new F(function(){return A(_mN,[[1,new T(function(){return B(_mG(_mE,_mF,_mS));})]]);});}))]:[2];}])),new T(function(){return [1,B(_ma(_mD,function(_mT){return new F(function(){return A(_mN,[[1,new T(function(){return B(_mG(_mE,_mF,_mT));})]]);});}))];})));});return new F(function(){return _kq([0,function(_mU){return E(E(_mU)[1])==101?E(_mO):[2];}],[0,function(_mV){return E(E(_mV)[1])==69?E(_mO):[2];}]);});},_mW=function(_mX){return new F(function(){return A(_mX,[_fW]);});},_mY=function(_mZ){return new F(function(){return A(_mZ,[_fW]);});},_n0=function(_n1){return function(_n2){return E(E(_n2)[1])==46?[1,B(_ma(_mD,function(_n3){return new F(function(){return A(_n1,[[1,_n3]]);});}))]:[2];};},_n4=function(_n5){return [0,B(_n0(_n5))];},_n6=function(_n7){return new F(function(){return _ma(_mD,function(_n8){return [1,B(_lB(_n4,_mW,function(_n9){return [1,B(_lB(_mM,_mY,function(_na){return new F(function(){return A(_n7,[[5,[1,_n8,_n9,_na]]]);});}))];}))];});});},_nb=function(_nc){return [1,B(_n6(_nc))];},_nd=function(_ne){return E(E(_ne)[1]);},_nf=function(_ng,_nh,_ni){while(1){var _nj=E(_ni);if(!_nj[0]){return false;}else{if(!B(A(_nd,[_ng,_nh,_nj[1]]))){_ni=_nj[2];continue;}else{return true;}}}},_nk=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_nl=function(_nm){return new F(function(){return _nf(_l8,_nm,_nk);});},_nn=[0,8],_no=[0,16],_np=function(_nq){var _nr=function(_ns){return new F(function(){return A(_nq,[[5,[0,_nn,_ns]]]);});},_nt=function(_nu){return new F(function(){return A(_nq,[[5,[0,_no,_nu]]]);});};return function(_nv){return E(E(_nv)[1])==48?E([0,function(_nw){switch(E(E(_nw)[1])){case 79:return [1,B(_ma(_nn,_nr))];case 88:return [1,B(_ma(_no,_nt))];case 111:return [1,B(_ma(_nn,_nr))];case 120:return [1,B(_ma(_no,_nt))];default:return [2];}}]):[2];};},_nx=function(_ny){return [0,B(_np(_ny))];},_nz=function(_nA){var _nB=new T(function(){return B(A(_nA,[_nn]));}),_nC=new T(function(){return B(A(_nA,[_no]));});return function(_nD){switch(E(E(_nD)[1])){case 79:return E(_nB);case 88:return E(_nC);case 111:return E(_nB);case 120:return E(_nC);default:return [2];}};},_nE=function(_nF){return [0,B(_nz(_nF))];},_nG=[0,92],_nH=function(_nI){return new F(function(){return A(_nI,[_mD]);});},_nJ=function(_nK){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_ic(9,_nK,_7n));}))));});},_nL=function(_nM){return [2];},_nN=function(_nO){var _nP=E(_nO);if(!_nP[0]){return E(_nL);}else{var _nQ=_nP[1],_nR=E(_nP[2]);return _nR[0]==0?E(_nQ):function(_nS){return new F(function(){return _kq(B(A(_nQ,[_nS])),new T(function(){return B(A(new T(function(){return B(_nN(_nR));}),[_nS]));}));});};}},_nT=function(_nU){return [2];},_nV=function(_nW,_nX){var _nY=function(_nZ,_o0){var _o1=E(_nZ);if(!_o1[0]){return function(_o2){return new F(function(){return A(_o2,[_nW]);});};}else{var _o3=E(_o0);return _o3[0]==0?E(_nT):E(_o1[1])[1]!=E(_o3[1])[1]?E(_nT):function(_o4){return [0,function(_o5){return E(new T(function(){return B(A(new T(function(){return B(_nY(_o1[2],_o3[2]));}),[_o4]));}));}];};}};return function(_o6){return new F(function(){return A(_nY,[_nW,_o6,_nX]);});};},_o7=new T(function(){return B(unCStr("SOH"));}),_o8=[0,1],_o9=function(_oa){return [1,B(_nV(_o7,function(_ob){return E(new T(function(){return B(A(_oa,[_o8]));}));}))];},_oc=new T(function(){return B(unCStr("SO"));}),_od=[0,14],_oe=function(_of){return [1,B(_nV(_oc,function(_og){return E(new T(function(){return B(A(_of,[_od]));}));}))];},_oh=function(_oi){return [1,B(_lB(_o9,_oe,_oi))];},_oj=new T(function(){return B(unCStr("NUL"));}),_ok=[0,0],_ol=function(_om){return [1,B(_nV(_oj,function(_on){return E(new T(function(){return B(A(_om,[_ok]));}));}))];},_oo=new T(function(){return B(unCStr("STX"));}),_op=[0,2],_oq=function(_or){return [1,B(_nV(_oo,function(_os){return E(new T(function(){return B(A(_or,[_op]));}));}))];},_ot=new T(function(){return B(unCStr("ETX"));}),_ou=[0,3],_ov=function(_ow){return [1,B(_nV(_ot,function(_ox){return E(new T(function(){return B(A(_ow,[_ou]));}));}))];},_oy=new T(function(){return B(unCStr("EOT"));}),_oz=[0,4],_oA=function(_oB){return [1,B(_nV(_oy,function(_oC){return E(new T(function(){return B(A(_oB,[_oz]));}));}))];},_oD=new T(function(){return B(unCStr("ENQ"));}),_oE=[0,5],_oF=function(_oG){return [1,B(_nV(_oD,function(_oH){return E(new T(function(){return B(A(_oG,[_oE]));}));}))];},_oI=new T(function(){return B(unCStr("ACK"));}),_oJ=[0,6],_oK=function(_oL){return [1,B(_nV(_oI,function(_oM){return E(new T(function(){return B(A(_oL,[_oJ]));}));}))];},_oN=new T(function(){return B(unCStr("BEL"));}),_oO=[0,7],_oP=function(_oQ){return [1,B(_nV(_oN,function(_oR){return E(new T(function(){return B(A(_oQ,[_oO]));}));}))];},_oS=new T(function(){return B(unCStr("BS"));}),_oT=[0,8],_oU=function(_oV){return [1,B(_nV(_oS,function(_oW){return E(new T(function(){return B(A(_oV,[_oT]));}));}))];},_oX=new T(function(){return B(unCStr("HT"));}),_oY=[0,9],_oZ=function(_p0){return [1,B(_nV(_oX,function(_p1){return E(new T(function(){return B(A(_p0,[_oY]));}));}))];},_p2=new T(function(){return B(unCStr("LF"));}),_p3=[0,10],_p4=function(_p5){return [1,B(_nV(_p2,function(_p6){return E(new T(function(){return B(A(_p5,[_p3]));}));}))];},_p7=new T(function(){return B(unCStr("VT"));}),_p8=[0,11],_p9=function(_pa){return [1,B(_nV(_p7,function(_pb){return E(new T(function(){return B(A(_pa,[_p8]));}));}))];},_pc=new T(function(){return B(unCStr("FF"));}),_pd=[0,12],_pe=function(_pf){return [1,B(_nV(_pc,function(_pg){return E(new T(function(){return B(A(_pf,[_pd]));}));}))];},_ph=new T(function(){return B(unCStr("CR"));}),_pi=[0,13],_pj=function(_pk){return [1,B(_nV(_ph,function(_pl){return E(new T(function(){return B(A(_pk,[_pi]));}));}))];},_pm=new T(function(){return B(unCStr("SI"));}),_pn=[0,15],_po=function(_pp){return [1,B(_nV(_pm,function(_pq){return E(new T(function(){return B(A(_pp,[_pn]));}));}))];},_pr=new T(function(){return B(unCStr("DLE"));}),_ps=[0,16],_pt=function(_pu){return [1,B(_nV(_pr,function(_pv){return E(new T(function(){return B(A(_pu,[_ps]));}));}))];},_pw=new T(function(){return B(unCStr("DC1"));}),_px=[0,17],_py=function(_pz){return [1,B(_nV(_pw,function(_pA){return E(new T(function(){return B(A(_pz,[_px]));}));}))];},_pB=new T(function(){return B(unCStr("DC2"));}),_pC=[0,18],_pD=function(_pE){return [1,B(_nV(_pB,function(_pF){return E(new T(function(){return B(A(_pE,[_pC]));}));}))];},_pG=new T(function(){return B(unCStr("DC3"));}),_pH=[0,19],_pI=function(_pJ){return [1,B(_nV(_pG,function(_pK){return E(new T(function(){return B(A(_pJ,[_pH]));}));}))];},_pL=new T(function(){return B(unCStr("DC4"));}),_pM=[0,20],_pN=function(_pO){return [1,B(_nV(_pL,function(_pP){return E(new T(function(){return B(A(_pO,[_pM]));}));}))];},_pQ=new T(function(){return B(unCStr("NAK"));}),_pR=[0,21],_pS=function(_pT){return [1,B(_nV(_pQ,function(_pU){return E(new T(function(){return B(A(_pT,[_pR]));}));}))];},_pV=new T(function(){return B(unCStr("SYN"));}),_pW=[0,22],_pX=function(_pY){return [1,B(_nV(_pV,function(_pZ){return E(new T(function(){return B(A(_pY,[_pW]));}));}))];},_q0=new T(function(){return B(unCStr("ETB"));}),_q1=[0,23],_q2=function(_q3){return [1,B(_nV(_q0,function(_q4){return E(new T(function(){return B(A(_q3,[_q1]));}));}))];},_q5=new T(function(){return B(unCStr("CAN"));}),_q6=[0,24],_q7=function(_q8){return [1,B(_nV(_q5,function(_q9){return E(new T(function(){return B(A(_q8,[_q6]));}));}))];},_qa=new T(function(){return B(unCStr("EM"));}),_qb=[0,25],_qc=function(_qd){return [1,B(_nV(_qa,function(_qe){return E(new T(function(){return B(A(_qd,[_qb]));}));}))];},_qf=new T(function(){return B(unCStr("SUB"));}),_qg=[0,26],_qh=function(_qi){return [1,B(_nV(_qf,function(_qj){return E(new T(function(){return B(A(_qi,[_qg]));}));}))];},_qk=new T(function(){return B(unCStr("ESC"));}),_ql=[0,27],_qm=function(_qn){return [1,B(_nV(_qk,function(_qo){return E(new T(function(){return B(A(_qn,[_ql]));}));}))];},_qp=new T(function(){return B(unCStr("FS"));}),_qq=[0,28],_qr=function(_qs){return [1,B(_nV(_qp,function(_qt){return E(new T(function(){return B(A(_qs,[_qq]));}));}))];},_qu=new T(function(){return B(unCStr("GS"));}),_qv=[0,29],_qw=function(_qx){return [1,B(_nV(_qu,function(_qy){return E(new T(function(){return B(A(_qx,[_qv]));}));}))];},_qz=new T(function(){return B(unCStr("RS"));}),_qA=[0,30],_qB=function(_qC){return [1,B(_nV(_qz,function(_qD){return E(new T(function(){return B(A(_qC,[_qA]));}));}))];},_qE=new T(function(){return B(unCStr("US"));}),_qF=[0,31],_qG=function(_qH){return [1,B(_nV(_qE,function(_qI){return E(new T(function(){return B(A(_qH,[_qF]));}));}))];},_qJ=new T(function(){return B(unCStr("SP"));}),_qK=[0,32],_qL=function(_qM){return [1,B(_nV(_qJ,function(_qN){return E(new T(function(){return B(A(_qM,[_qK]));}));}))];},_qO=new T(function(){return B(unCStr("DEL"));}),_qP=[0,127],_qQ=function(_qR){return [1,B(_nV(_qO,function(_qS){return E(new T(function(){return B(A(_qR,[_qP]));}));}))];},_qT=[1,_qQ,_7n],_qU=[1,_qL,_qT],_qV=[1,_qG,_qU],_qW=[1,_qB,_qV],_qX=[1,_qw,_qW],_qY=[1,_qr,_qX],_qZ=[1,_qm,_qY],_r0=[1,_qh,_qZ],_r1=[1,_qc,_r0],_r2=[1,_q7,_r1],_r3=[1,_q2,_r2],_r4=[1,_pX,_r3],_r5=[1,_pS,_r4],_r6=[1,_pN,_r5],_r7=[1,_pI,_r6],_r8=[1,_pD,_r7],_r9=[1,_py,_r8],_ra=[1,_pt,_r9],_rb=[1,_po,_ra],_rc=[1,_pj,_rb],_rd=[1,_pe,_rc],_re=[1,_p9,_rd],_rf=[1,_p4,_re],_rg=[1,_oZ,_rf],_rh=[1,_oU,_rg],_ri=[1,_oP,_rh],_rj=[1,_oK,_ri],_rk=[1,_oF,_rj],_rl=[1,_oA,_rk],_rm=[1,_ov,_rl],_rn=[1,_oq,_rm],_ro=[1,_ol,_rn],_rp=[1,_oh,_ro],_rq=new T(function(){return B(_nN(_rp));}),_rr=[0,1114111],_rs=[0,34],_rt=[0,39],_ru=function(_rv){var _rw=new T(function(){return B(A(_rv,[_oO]));}),_rx=new T(function(){return B(A(_rv,[_oT]));}),_ry=new T(function(){return B(A(_rv,[_oY]));}),_rz=new T(function(){return B(A(_rv,[_p3]));}),_rA=new T(function(){return B(A(_rv,[_p8]));}),_rB=new T(function(){return B(A(_rv,[_pd]));}),_rC=new T(function(){return B(A(_rv,[_pi]));});return new F(function(){return _kq([0,function(_rD){switch(E(E(_rD)[1])){case 34:return E(new T(function(){return B(A(_rv,[_rs]));}));case 39:return E(new T(function(){return B(A(_rv,[_rt]));}));case 92:return E(new T(function(){return B(A(_rv,[_nG]));}));case 97:return E(_rw);case 98:return E(_rx);case 102:return E(_rB);case 110:return E(_rz);case 114:return E(_rC);case 116:return E(_ry);case 118:return E(_rA);default:return [2];}}],new T(function(){return B(_kq([1,B(_lB(_nE,_nH,function(_rE){return [1,B(_ma(_rE,function(_rF){var _rG=B(_mG(new T(function(){return B(_2i(E(_rE)[1]));}),_mF,_rF));return !B(_ai(_rG,_rr))?[2]:B(A(_rv,[new T(function(){var _rH=B(_6X(_rG));if(_rH>>>0>1114111){var _rI=B(_nJ(_rH));}else{var _rI=[0,_rH];}var _rJ=_rI,_rK=_rJ,_rL=_rK;return _rL;})]));}))];}))],new T(function(){return B(_kq([0,function(_rM){return E(E(_rM)[1])==94?E([0,function(_rN){switch(E(E(_rN)[1])){case 64:return E(new T(function(){return B(A(_rv,[_ok]));}));case 65:return E(new T(function(){return B(A(_rv,[_o8]));}));case 66:return E(new T(function(){return B(A(_rv,[_op]));}));case 67:return E(new T(function(){return B(A(_rv,[_ou]));}));case 68:return E(new T(function(){return B(A(_rv,[_oz]));}));case 69:return E(new T(function(){return B(A(_rv,[_oE]));}));case 70:return E(new T(function(){return B(A(_rv,[_oJ]));}));case 71:return E(_rw);case 72:return E(_rx);case 73:return E(_ry);case 74:return E(_rz);case 75:return E(_rA);case 76:return E(_rB);case 77:return E(_rC);case 78:return E(new T(function(){return B(A(_rv,[_od]));}));case 79:return E(new T(function(){return B(A(_rv,[_pn]));}));case 80:return E(new T(function(){return B(A(_rv,[_ps]));}));case 81:return E(new T(function(){return B(A(_rv,[_px]));}));case 82:return E(new T(function(){return B(A(_rv,[_pC]));}));case 83:return E(new T(function(){return B(A(_rv,[_pH]));}));case 84:return E(new T(function(){return B(A(_rv,[_pM]));}));case 85:return E(new T(function(){return B(A(_rv,[_pR]));}));case 86:return E(new T(function(){return B(A(_rv,[_pW]));}));case 87:return E(new T(function(){return B(A(_rv,[_q1]));}));case 88:return E(new T(function(){return B(A(_rv,[_q6]));}));case 89:return E(new T(function(){return B(A(_rv,[_qb]));}));case 90:return E(new T(function(){return B(A(_rv,[_qg]));}));case 91:return E(new T(function(){return B(A(_rv,[_ql]));}));case 92:return E(new T(function(){return B(A(_rv,[_qq]));}));case 93:return E(new T(function(){return B(A(_rv,[_qv]));}));case 94:return E(new T(function(){return B(A(_rv,[_qA]));}));case 95:return E(new T(function(){return B(A(_rv,[_qF]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_rq,[_rv]));})));})));}));});},_rO=function(_rP){return new F(function(){return A(_rP,[_0]);});},_rQ=function(_rR){var _rS=E(_rR);if(!_rS[0]){return E(_rO);}else{var _rT=_rS[2],_rU=E(E(_rS[1])[1]);switch(_rU){case 9:return function(_rV){return [0,function(_rW){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_rV]));}));}];};case 10:return function(_rX){return [0,function(_rY){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_rX]));}));}];};case 11:return function(_rZ){return [0,function(_s0){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_rZ]));}));}];};case 12:return function(_s1){return [0,function(_s2){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_s1]));}));}];};case 13:return function(_s3){return [0,function(_s4){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_s3]));}));}];};case 32:return function(_s5){return [0,function(_s6){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_s5]));}));}];};case 160:return function(_s7){return [0,function(_s8){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_s7]));}));}];};default:var _s9=u_iswspace(_rU),_sa=_s9;return E(_sa)==0?E(_rO):function(_sb){return [0,function(_sc){return E(new T(function(){return B(A(new T(function(){return B(_rQ(_rT));}),[_sb]));}));}];};}}},_sd=function(_se){var _sf=new T(function(){return B(_sd(_se));}),_sg=[1,function(_sh){return new F(function(){return A(_rQ,[_sh,function(_si){return E([0,function(_sj){return E(E(_sj)[1])==92?E(_sf):[2];}]);}]);});}];return new F(function(){return _kq([0,function(_sk){return E(E(_sk)[1])==92?E([0,function(_sl){var _sm=E(E(_sl)[1]);switch(_sm){case 9:return E(_sg);case 10:return E(_sg);case 11:return E(_sg);case 12:return E(_sg);case 13:return E(_sg);case 32:return E(_sg);case 38:return E(_sf);case 160:return E(_sg);default:var _sn=u_iswspace(_sm),_so=_sn;return E(_so)==0?[2]:E(_sg);}}]):[2];}],[0,function(_sp){var _sq=E(_sp);return E(_sq[1])==92?E(new T(function(){return B(_ru(function(_sr){return new F(function(){return A(_se,[[0,_sr,_cD]]);});}));})):B(A(_se,[[0,_sq,_cC]]));}]);});},_ss=function(_st,_su){return new F(function(){return _sd(function(_sv){var _sw=E(_sv),_sx=E(_sw[1]);if(E(_sx[1])==34){if(!E(_sw[2])){return E(new T(function(){return B(A(_su,[[1,new T(function(){return B(A(_st,[_7n]));})]]));}));}else{return new F(function(){return _ss(function(_sy){return new F(function(){return A(_st,[[1,_sx,_sy]]);});},_su);});}}else{return new F(function(){return _ss(function(_sz){return new F(function(){return A(_st,[[1,_sx,_sz]]);});},_su);});}});});},_sA=new T(function(){return B(unCStr("_\'"));}),_sB=function(_sC){var _sD=u_iswalnum(_sC),_sE=_sD;return E(_sE)==0?B(_nf(_l8,[0,_sC],_sA)):true;},_sF=function(_sG){return new F(function(){return _sB(E(_sG)[1]);});},_sH=new T(function(){return B(unCStr(",;()[]{}`"));}),_sI=new T(function(){return B(unCStr(".."));}),_sJ=new T(function(){return B(unCStr("::"));}),_sK=new T(function(){return B(unCStr("->"));}),_sL=[0,64],_sM=[1,_sL,_7n],_sN=[0,126],_sO=[1,_sN,_7n],_sP=new T(function(){return B(unCStr("=>"));}),_sQ=[1,_sP,_7n],_sR=[1,_sO,_sQ],_sS=[1,_sM,_sR],_sT=[1,_sK,_sS],_sU=new T(function(){return B(unCStr("<-"));}),_sV=[1,_sU,_sT],_sW=[0,124],_sX=[1,_sW,_7n],_sY=[1,_sX,_sV],_sZ=[1,_nG,_7n],_t0=[1,_sZ,_sY],_t1=[0,61],_t2=[1,_t1,_7n],_t3=[1,_t2,_t0],_t4=[1,_sJ,_t3],_t5=[1,_sI,_t4],_t6=function(_t7){return new F(function(){return _kq([1,function(_t8){return E(_t8)[0]==0?E(new T(function(){return B(A(_t7,[_m7]));})):[2];}],new T(function(){return B(_kq([0,function(_t9){return E(E(_t9)[1])==39?E([0,function(_ta){var _tb=E(_ta);switch(E(_tb[1])){case 39:return [2];case 92:return E(new T(function(){return B(_ru(function(_tc){return [0,function(_td){return E(E(_td)[1])==39?E(new T(function(){return B(A(_t7,[[0,_tc]]));})):[2];}];}));}));default:return [0,function(_te){return E(E(_te)[1])==39?E(new T(function(){return B(A(_t7,[[0,_tb]]));})):[2];}];}}]):[2];}],new T(function(){return B(_kq([0,function(_tf){return E(E(_tf)[1])==34?E(new T(function(){return B(_ss(_e9,_t7));})):[2];}],new T(function(){return B(_kq([0,function(_tg){return !B(_nf(_l8,_tg,_sH))?[2]:B(A(_t7,[[2,[1,_tg,_7n]]]));}],new T(function(){return B(_kq([0,function(_th){return !B(_nf(_l8,_th,_nk))?[2]:[1,B(_lW(_nl,function(_ti){var _tj=[1,_th,_ti];return !B(_nf(_lh,_tj,_t5))?B(A(_t7,[[4,_tj]])):B(A(_t7,[[2,_tj]]));}))];}],new T(function(){return B(_kq([0,function(_tk){var _tl=E(_tk),_tm=_tl[1],_tn=u_iswalpha(_tm),_to=_tn;return E(_to)==0?E(_tm)==95?[1,B(_lW(_sF,function(_tp){return new F(function(){return A(_t7,[[3,[1,_tl,_tp]]]);});}))]:[2]:[1,B(_lW(_sF,function(_tq){return new F(function(){return A(_t7,[[3,[1,_tl,_tq]]]);});}))];}],new T(function(){return [1,B(_lB(_nx,_nb,_t7))];})));})));})));})));})));}));});},_tr=[0,0],_ts=function(_tt,_tu){return function(_tv){return new F(function(){return A(_rQ,[_tv,function(_tw){return E(new T(function(){return B(_t6(function(_tx){var _ty=E(_tx);return _ty[0]==2?!B(_kX(_ty[1],_kW))?[2]:E(new T(function(){return B(A(_tt,[_tr,function(_tz){return [1,function(_tA){return new F(function(){return A(_rQ,[_tA,function(_tB){return E(new T(function(){return B(_t6(function(_tC){var _tD=E(_tC);return _tD[0]==2?!B(_kX(_tD[1],_kU))?[2]:E(new T(function(){return B(A(_tu,[_tz]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_tE=function(_tF,_tG,_tH){var _tI=function(_tJ,_tK){return new F(function(){return _kq([1,function(_tL){return new F(function(){return A(_rQ,[_tL,function(_tM){return E(new T(function(){return B(_t6(function(_tN){var _tO=E(_tN);if(_tO[0]==4){var _tP=E(_tO[1]);if(!_tP[0]){return new F(function(){return A(_tF,[_tO,_tJ,_tK]);});}else{return E(E(_tP[1])[1])==45?E(_tP[2])[0]==0?E([1,function(_tQ){return new F(function(){return A(_rQ,[_tQ,function(_tR){return E(new T(function(){return B(_t6(function(_tS){return new F(function(){return A(_tF,[_tS,_tJ,function(_tT){return new F(function(){return A(_tK,[new T(function(){return [0, -E(_tT)[1]];})]);});}]);});}));}));}]);});}]):B(A(_tF,[_tO,_tJ,_tK])):B(A(_tF,[_tO,_tJ,_tK]));}}else{return new F(function(){return A(_tF,[_tO,_tJ,_tK]);});}}));}));}]);});}],new T(function(){return [1,B(_ts(_tI,_tK))];}));});};return new F(function(){return _tI(_tG,_tH);});},_tU=function(_tV,_tW){return [2];},_tX=function(_tY){var _tZ=E(_tY);return _tZ[0]==0?[1,new T(function(){return B(_mG(new T(function(){return B(_2i(E(_tZ[1])[1]));}),_mF,_tZ[2]));})]:E(_tZ[2])[0]==0?E(_tZ[3])[0]==0?[1,new T(function(){return B(_mG(_mE,_mF,_tZ[1]));})]:[0]:[0];},_u0=function(_u1){var _u2=E(_u1);if(_u2[0]==5){var _u3=B(_tX(_u2[1]));return _u3[0]==0?E(_tU):function(_u4,_u5){return new F(function(){return A(_u5,[new T(function(){return [0,B(_6X(_u3[1]))];})]);});};}else{return E(_tU);}},_u6=function(_u7){return [1,function(_u8){return new F(function(){return A(_rQ,[_u8,function(_u9){return E([3,_u7,_lt]);}]);});}];},_ua=new T(function(){return B(_tE(_u0,_tr,_u6));}),_ub=new T(function(){var _uc=1/Math.sqrt(3);return [0,E([0,_uc]),E([0,_uc]),E([0,_uc])];}),_ud=[0,1.5],_ue=[0,0],_uf=[0,E(_ud),E(_ue),E(_ue)],_ug=[0,1],_uh=[0,E(_ue),E(_ue),E(_ue),E(_ug)],_ui=function(_uj){return [0,_uh,_uf,_uj];},_uk=[0,13],_ul=[0,14],_um=[0,15],_un=[0,16],_uo=[0,17],_up=[0,18],_uq=[0,19],_ur=[0,20],_us=[0,21],_ut=[0,22],_uu=[0,23],_uv=[1,_uu,_7n],_uw=[1,_ut,_uv],_ux=[1,_ur,_uw],_uy=[1,_ut,_ux],_uz=[1,_us,_uy],_uA=[1,_ur,_uz],_uB=[1,_uq,_uA],_uC=[1,_up,_uB],_uD=[1,_un,_uC],_uE=[1,_up,_uD],_uF=[1,_uo,_uE],_uG=[1,_un,_uF],_uH=[1,_um,_uG],_uI=[1,_ul,_uH],_uJ=[0,12],_uK=[1,_uJ,_uI],_uL=[1,_ul,_uK],_uM=[1,_uk,_uL],_uN=[1,_uJ,_uM],_uO=[0,11],_uP=[1,_uO,_uN],_uQ=[0,10],_uR=[1,_uQ,_uP],_uS=[0,8],_uT=[1,_uS,_uR],_uU=[1,_uQ,_uT],_uV=[0,9],_uW=[1,_uV,_uU],_uX=[1,_uS,_uW],_uY=[0,7],_uZ=[1,_uY,_uX],_v0=[0,6],_v1=[1,_v0,_uZ],_v2=[0,4],_v3=[1,_v2,_v1],_v4=[1,_v0,_v3],_v5=[0,5],_v6=[1,_v5,_v4],_v7=[1,_v2,_v6],_v8=[0,3],_v9=[1,_v8,_v7],_va=[0,2],_vb=[1,_va,_v9],_vc=[0,0],_vd=[1,_vc,_vb],_ve=[1,_va,_vd],_vf=[0,1],_vg=[1,_vf,_ve],_vh=[1,_vc,_vg],_vi=[0,0],_vj=new T(function(){return B(unCStr("uMVMatrix"));}),_vk=new T(function(){return B(unCStr("aVertexColor"));}),_vl=new T(function(){return B(unCStr("aVertexPosition"));}),_vm=new T(function(){return B(unCStr("attribute vec3 aVertexPosition;"));}),_vn=new T(function(){return B(unCStr("attribute vec4 aVertexColor;"));}),_vo=new T(function(){return B(unCStr("uniform mat4 uMVMatrix;"));}),_vp=new T(function(){return B(unCStr("varying vec4 vColor;"));}),_vq=new T(function(){return B(unCStr("void main(void) {"));}),_vr=new T(function(){return B(unCStr("  gl_Position = uMVMatrix * vec4(aVertexPosition, 1.0);"));}),_vs=new T(function(){return B(unCStr("  vColor = aVertexColor;"));}),_vt=[0,125],_vu=[1,_vt,_7n],_vv=[1,_vu,_7n],_vw=[1,_vs,_vv],_vx=[1,_vr,_vw],_vy=[1,_vq,_vx],_vz=[1,_vp,_vy],_vA=[1,_vo,_vz],_vB=[1,_vn,_vA],_vC=[1,_vm,_vB],_vD=new T(function(){return B(unCStr("precision mediump float;"));}),_vE=new T(function(){return B(unCStr("    gl_FragColor = vColor;"));}),_vF=[1,_vE,_vv],_vG=[1,_vq,_vF],_vH=[1,_vp,_vG],_vI=[1,_vD,_vH],_vJ=[0,E(_ue),E(_ug),E(_ue)],_vK=[0,-1],_vL=[0,E(_vK),E(_vK),E(_ug)],_vM=[0,E(_ug),E(_vK),E(_ug)],_vN=[0,E(_ug),E(_vK),E(_vK)],_vO=[0,E(_vK),E(_vK),E(_vK)],_vP=[1,_vL,_7n],_vQ=[1,_vO,_vP],_vR=[1,_vJ,_vQ],_vS=[1,_vO,_vR],_vT=[1,_vN,_vS],_vU=[1,_vJ,_vT],_vV=[1,_vN,_vU],_vW=[1,_vM,_vV],_vX=[1,_vJ,_vW],_vY=[1,_vM,_vX],_vZ=[1,_vL,_vY],_w0=[1,_vJ,_vZ],_w1=[0,5],_w2=[0,255],_w3=[0,0],_w4=[0,E(_w2),E(_w3),E(_w3)],_w5=[0,E(_w3),E(_w2),E(_w3)],_w6=[0,E(_w3),E(_w3),E(_w2)],_w7=[1,_w5,_7n],_w8=[1,_w6,_w7],_w9=[1,_w4,_w8],_wa=[1,_w6,_w9],_wb=[1,_w5,_wa],_wc=[1,_w4,_wb],_wd=[1,_w5,_wc],_we=[1,_w6,_wd],_wf=[1,_w4,_we],_wg=[1,_w6,_wf],_wh=[1,_w5,_wg],_wi=[1,_w4,_wh],_wj=[0,-3.5],_wk=[0,E(_ue),E(_ue),E(_ue)],_wl=[0,E(_ug),E(_ug),E(_ug)],_wm=[0,E(_vK),E(_ug),E(_ug)],_wn=[0,E(_vK),E(_ug),E(_vK)],_wo=[0,E(_ug),E(_ug),E(_vK)],_wp=[1,_wn,_7n],_wq=[1,_wm,_wp],_wr=[1,_vL,_wq],_ws=[1,_vO,_wr],_wt=[1,_vM,_ws],_wu=[1,_wl,_wt],_wv=[1,_wo,_wu],_ww=[1,_vN,_wv],_wx=[1,_vL,_ww],_wy=[1,_vM,_wx],_wz=[1,_vN,_wy],_wA=[1,_vO,_wz],_wB=[1,_wo,_wA],_wC=[1,_wl,_wB],_wD=[1,_wm,_wC],_wE=[1,_wn,_wD],_wF=[1,_vN,_wE],_wG=[1,_wo,_wF],_wH=[1,_wn,_wG],_wI=[1,_vO,_wH],_wJ=[1,_wm,_wI],_wK=[1,_wl,_wJ],_wL=[1,_vM,_wK],_wM=[1,_vL,_wL],_wN=function(_wO){while(1){var _wP=(function(_wQ){var _wR=E(_wQ);if(!_wR[0]){return [0];}else{var _wS=_wR[1],_wT=_wR[2],_wU=function(_wV){return _wV>1?[1,_wS,new T(function(){return B(_wU(_wV-1|0));})]:E([1,_wS,_7n]);},_wW=B(_wU(4));if(!_wW[0]){_wO=_wT;return null;}else{return [1,_wW[1],new T(function(){return B(_7S(_wW[2],new T(function(){return B(_wN(_wT));},1)));})];}}})(_wO);if(_wP!=null){return _wP;}}},_wX=function(_wY,_wZ){var _x0=function(_x1){return _x1>1?[1,_wY,new T(function(){return B(_x0(_x1-1|0));})]:E([1,_wY,_7n]);},_x2=B(_x0(4));return _x2[0]==0?B(_wN(_wZ)):[1,_x2[1],new T(function(){return B(_7S(_x2[2],new T(function(){return B(_wN(_wZ));},1)));})];},_x3=[0,E(_w2),E(_w2),E(_w3)],_x4=[0,130],_x5=[0,E(_w3),E(_x4),E(_x4)],_x6=[0,E(_w2),E(_w3),E(_w2)],_x7=[1,_w6,_7n],_x8=[1,_x6,_x7],_x9=[1,_x5,_x8],_xa=[1,_w5,_x9],_xb=[1,_x3,_xa],_xc=new T(function(){return B(_wX(_w4,_xb));}),_xd=[0,E(_wj),E(_ue),E(_ue)],_xe=function(_uj){return [0,_uh,_xd,_uj];},_xf=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_xg=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_xh=function(_xi){while(1){var _xj=(function(_xk){var _xl=E(_xk);if(!_xl[0]){return [0];}else{var _xm=_xl[2],_xn=E(_xl[1]);if(!E(_xn[2])[0]){return [1,_xn[1],new T(function(){return B(_xh(_xm));})];}else{_xi=_xm;return null;}}})(_xi);if(_xj!=null){return _xj;}}},_xo=new T(function(){return B(_cJ("(function(ctx, shader, source) {ctx.shaderSource(shader, source);})"));}),_xp=[0,10],_xq=function(_xr){var _xs=E(_xr);if(!_xs[0]){return [0];}else{return new F(function(){return _7S(_xs[1],[1,_xp,new T(function(){return B(_xq(_xs[2]));})]);});}},_xt=new T(function(){return B(_cJ("(function(ctx, program) {ctx.useProgram(program);})"));}),_xu=new T(function(){return B(_cJ("(function(ctx, x, y, width, height) {ctx.viewport(x, y, width, height);})"));}),_xv=function(_){var _xw=jsFind("glcanvas"),_xx=_xw,_xy=E(_xx);if(!_xy[0]){return new F(function(){return _iD(_jk,_);});}else{var _xz=E(_xy[1])[1],_xA=jsGet(_xz,"offsetWidth"),_xB=_xA,_xC=jsGet(_xz,"offsetHeight"),_xD=_xC,_xE=B(_xh(B(_kg(_ua,new T(function(){return fromJSStr(_xB);})))));if(!_xE[0]){return new F(function(){return err(_xg);});}else{if(!E(_xE[2])[0]){var _xF=E(_xE[1])[1],_xG=new T(function(){var _xH=B(_xh(B(_kg(_ua,new T(function(){return fromJSStr(_xD);})))));return _xH[0]==0?B(err(_xg)):E(_xH[2])[0]==0?E(_xH[1]):B(err(_xf));}),_xI=B(_ha(_xF,_xG)),_xJ=E(_xI[1])[1],_xK=jsSetAttr(_xz,"width",toJSStr(B(_ic(0,_xJ,_7n)))),_xL=E(_xI[2])[1],_xM=jsSetAttr(_xz,"height",toJSStr(B(_ic(0,_xL,_7n)))),_xN=B(A(_iM,[E(_xz),E("webgl"),_])),_xO=_xN,_xP=[0,_xO],_xQ=E(_xO),_xR=B(A(_iv,[_xQ,E(35632),_])),_xS=_xR,_xT=E(_xS),_xU=B(A(_xo,[_xQ,_xT,E(toJSStr(B(_xq(_vI)))),_])),_xV=_xU,_xW=B(A(_it,[_xQ,_xT,_])),_xX=_xW,_xY=B(A(_iv,[_xQ,E(35633),_])),_xZ=_xY,_y0=E(_xZ),_y1=B(A(_xo,[_xQ,_y0,E(toJSStr(B(_xq(_vC)))),_])),_y2=_y1,_y3=B(A(_it,[_xQ,_y0,_])),_y4=_y3,_y5=B(A(_iu,[_xQ,_])),_y6=_y5,_y7=E(_y6),_y8=B(A(_ii,[_xQ,_y7,_y0,_])),_y9=_y8,_ya=B(A(_ii,[_xQ,_y7,_xT,_])),_yb=_ya,_yc=B(A(_jj,[_xQ,_y7,_])),_yd=_yc,_ye=B(A(_xt,[_xQ,_y7,_])),_yf=_ye,_yg=[0,_y7],_yh=B(A(_iG,[_xP,_yg,_vl,_])),_yi=_yh,_yj=E(_yi),_yk=B(A(_iw,[_xQ,E(_yj[1]),_])),_yl=_yk,_ym=B(A(_iG,[_xP,_yg,_vk,_])),_yn=_ym,_yo=E(_yn),_yp=B(A(_iw,[_xQ,E(_yo[1]),_])),_yq=_yp,_yr=B(A(_iO,[_xP,_yg,_vj,_])),_ys=_yr,_yt=B(A(_ix,[_xQ,E(2929),_])),_yu=_yt,_yv=E(0),_yw=B(A(_xu,[_xQ,_yv,_yv,E(_xJ),E(_xL),_])),_yx=_yw,_yy=B(A(_ik,[_xP,_vi,_vi,_vi,_vi,_])),_yz=_yy,_yA=B(_je(new T(function(){return [0,_xF/E(_xG)[1]];}),_ys,_)),_yB=_yA,_yC=[0,_yj,_yo],_yD=B(_gN(_xP,_yC,_wM,_xc,_vh,_)),_yE=_yD,_yF=B(_g1(_xP,_w0,_wi,_)),_yG=_yF,_yH=E(_yG),_yI=B(_4F(8,function(_){var _yJ=B(A(_cJ,["(function() {return (new Date().getTime());})",_])),_yK=_yJ,_yL=B(A(_is,[_xQ,E((16384>>>0|256>>>0)>>>0&4294967295),_])),_yM=_yL,_yN=new T(function(){var _yO=jsTrunc(_yK),_yP=_yO;return [0,_yP];}),_yQ=function(_yR,_){var _yS=E(new T(function(){var _yT=E(_yB),_yU=B(_4L(_1O,_yT[1],_yT[2],function(_yV){return [0,new T(function(){var _yW=E(_yN)[1]/5000;return B(_hq(_1i,_1j,_Q,_N,_1I,_ue,_ug,_ue,[0,-3.5+13*Math.cos(_yW)],_w1,[0,13*Math.sin(_yW)],_wj,_ue,_ue));}),_yV];}));return [0,_yU[1],_yU[2]];})),_yX=E(_yS[2]),_yY=B(_ee(_4y,_xP,_yS[1],_yX[1],_yX[2],_yR,_)),_yZ=_yY;return new F(function(){return _dv(_xQ,_yZ,_);});},_z0=B(_yQ(new T(function(){var _z1=B(_5D(_1O,_7,_uh,_wk,_yE,_ui)),_z2=B(_5D(_1O,_7,_z1[1],_z1[2],_z1[3],new T(function(){return B(_cl(_1O,_2T,_ub,new T(function(){return [0,E(_yN)[1]/1000];})));},1)));return [0,_z2[1],_z2[2],_z2[3]];}),_)),_z3=_z0,_z4=B(_yQ(new T(function(){var _z5=B(_5D(_1O,_7,_uh,_wk,[1,_yH[1],_yH[2],_yH[3],_yC],_xe)),_z6=B(_5D(_1O,_7,_z5[1],_z5[2],_z5[3],function(_uj){return [0,new T(function(){var _z7= -(E(_yN)[1]/2000);return [0,E(_ue),E([0,Math.sin(_z7)]),E(_ue),E([0,Math.cos(_z7)])];}),_wk,_uj];}));return [0,_z6[1],_z6[2],_z6[3]];}),_)),_z8=_z4;return _0;},_)),_z9=_yI;return _0;}else{return new F(function(){return err(_xf);});}}}},_za=function(_){var _zb=jsSetTimeout(100,_xv);return _0;},_zc=function(_){return new F(function(){return _za(_);});};
var hasteMain = function() {B(A(_zc, [0]));};window.onload = hasteMain;