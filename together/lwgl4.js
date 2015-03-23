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

var _0=0,_1=function(_2){var _3=E(_2)[1];return [0,Math.log(_3+(_3+1)*Math.sqrt((_3-1)/(_3+1)))];},_4=function(_5){var _6=E(_5)[1];return [0,Math.log(_6+Math.sqrt(1+_6*_6))];},_7=function(_8){var _9=E(_8)[1];return [0,0.5*Math.log((1+_9)/(1-_9))];},_a=function(_b,_c){return [0,Math.log(E(_c)[1])/Math.log(E(_b)[1])];},_d=[0,3.141592653589793],_e=[0,0],_f=new T(function(){return [0,0/0];}),_g=new T(function(){return [0,-1/0];}),_h=new T(function(){return [0,1/0];}),_i=function(_j,_k){while(1){var _l=E(_j);if(!_l[0]){_j=[1,I_fromInt(_l[1])];continue;}else{var _m=E(_k);if(!_m[0]){_j=_l;_k=[1,I_fromInt(_m[1])];continue;}else{return new F(function(){return I_fromRat(_l[1],_m[1]);});}}}},_n=function(_o,_p){var _q=E(_o);if(!_q[0]){var _r=_q[1],_s=E(_p);return _s[0]==0?_r==_s[1]:I_compareInt(_s[1],_r)==0?true:false;}else{var _t=_q[1],_u=E(_p);return _u[0]==0?I_compareInt(_t,_u[1])==0?true:false:I_compare(_t,_u[1])==0?true:false;}},_v=function(_w,_x){var _y=E(_w);if(!_y[0]){var _z=_y[1],_A=E(_x);return _A[0]==0?_z<_A[1]:I_compareInt(_A[1],_z)>0;}else{var _B=_y[1],_C=E(_x);return _C[0]==0?I_compareInt(_B,_C[1])<0:I_compare(_B,_C[1])<0;}},_D=function(_E,_F){return !B(_n(_F,_e))?[0,B(_i(_E,_F))]:!B(_n(_E,_e))?!B(_v(_E,_e))?E(_h):E(_g):E(_f);},_G=function(_H){var _I=E(_H);return new F(function(){return _D(_I[1],_I[2]);});},_J=function(_K){return [0,1/E(_K)[1]];},_L=function(_M){var _N=E(_M),_O=_N[1];return _O<0?[0, -_O]:E(_N);},_P=function(_Q){var _R=E(_Q);return _R[0]==0?_R[1]:I_toNumber(_R[1]);},_S=function(_T){return [0,B(_P(_T))];},_U=[0,0],_V=[0,1],_W=[0,-1],_X=function(_Y){var _Z=E(E(_Y)[1]);return _Z==0?E(_U):_Z<=0?E(_W):E(_V);},_10=function(_11,_12){return [0,E(_11)[1]-E(_12)[1]];},_13=function(_14){return [0, -E(_14)[1]];},_15=function(_16,_17){return [0,E(_16)[1]+E(_17)[1]];},_18=function(_19,_1a){return [0,E(_19)[1]*E(_1a)[1]];},_1b=[0,_15,_18,_10,_13,_L,_X,_S],_1c=function(_1d,_1e){return [0,E(_1d)[1]/E(_1e)[1]];},_1f=[0,_1b,_1c,_J,_G],_1g=function(_1h){return [0,Math.acos(E(_1h)[1])];},_1i=function(_1j){return [0,Math.asin(E(_1j)[1])];},_1k=function(_1l){return [0,Math.atan(E(_1l)[1])];},_1m=function(_1n){return [0,Math.cos(E(_1n)[1])];},_1o=function(_1p){return [0,cosh(E(_1p)[1])];},_1q=function(_1r){return [0,Math.exp(E(_1r)[1])];},_1s=function(_1t){return [0,Math.log(E(_1t)[1])];},_1u=function(_1v,_1w){return [0,Math.pow(E(_1v)[1],E(_1w)[1])];},_1x=function(_1y){return [0,Math.sin(E(_1y)[1])];},_1z=function(_1A){return [0,sinh(E(_1A)[1])];},_1B=function(_1C){return [0,Math.sqrt(E(_1C)[1])];},_1D=function(_1E){return [0,Math.tan(E(_1E)[1])];},_1F=function(_1G){return [0,tanh(E(_1G)[1])];},_1H=[0,_1f,_d,_1q,_1B,_1s,_1u,_a,_1x,_1D,_1m,_1i,_1k,_1g,_1z,_1F,_1o,_4,_7,_1],_1I=function(_1J,_1K,_){var _1L=B(A(_1K,[_])),_1M=_1L,_1N=jsSetTimeout(_1J,function(_){return new F(function(){return _1I(_1J,_1K,_);});});return _0;},_1O=function(_1P){return E(E(_1P)[1]);},_1Q=function(_1R){return E(E(_1R)[1]);},_1S=function(_1T,_1U,_1V,_1W,_1X,_1Y,_1Z,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_2a,_2b,_2c,_2d,_2e,_2f,_2g,_2h,_2i,_2j,_2k,_2l,_2m,_2n,_2o,_2p,_2q){return [0,E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1V,_2b]));}),new T(function(){return B(A(_1U,[_1W,_2f]));})]));}),new T(function(){return B(A(_1U,[_1X,_2j]));})]));}),new T(function(){return B(A(_1U,[_1Y,_2n]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1V,_2c]));}),new T(function(){return B(A(_1U,[_1W,_2g]));})]));}),new T(function(){return B(A(_1U,[_1X,_2k]));})]));}),new T(function(){return B(A(_1U,[_1Y,_2o]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1V,_2d]));}),new T(function(){return B(A(_1U,[_1W,_2h]));})]));}),new T(function(){return B(A(_1U,[_1X,_2l]));})]));}),new T(function(){return B(A(_1U,[_1Y,_2p]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1V,_2e]));}),new T(function(){return B(A(_1U,[_1W,_2i]));})]));}),new T(function(){return B(A(_1U,[_1X,_2m]));})]));}),new T(function(){return B(A(_1U,[_1Y,_2q]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1Z,_2b]));}),new T(function(){return B(A(_1U,[_20,_2f]));})]));}),new T(function(){return B(A(_1U,[_21,_2j]));})]));}),new T(function(){return B(A(_1U,[_22,_2n]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1Z,_2c]));}),new T(function(){return B(A(_1U,[_20,_2g]));})]));}),new T(function(){return B(A(_1U,[_21,_2k]));})]));}),new T(function(){return B(A(_1U,[_22,_2o]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1Z,_2d]));}),new T(function(){return B(A(_1U,[_20,_2h]));})]));}),new T(function(){return B(A(_1U,[_21,_2l]));})]));}),new T(function(){return B(A(_1U,[_22,_2p]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_1Z,_2e]));}),new T(function(){return B(A(_1U,[_20,_2i]));})]));}),new T(function(){return B(A(_1U,[_21,_2m]));})]));}),new T(function(){return B(A(_1U,[_22,_2q]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_23,_2b]));}),new T(function(){return B(A(_1U,[_24,_2f]));})]));}),new T(function(){return B(A(_1U,[_25,_2j]));})]));}),new T(function(){return B(A(_1U,[_26,_2n]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_23,_2c]));}),new T(function(){return B(A(_1U,[_24,_2g]));})]));}),new T(function(){return B(A(_1U,[_25,_2k]));})]));}),new T(function(){return B(A(_1U,[_26,_2o]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_23,_2d]));}),new T(function(){return B(A(_1U,[_24,_2h]));})]));}),new T(function(){return B(A(_1U,[_25,_2l]));})]));}),new T(function(){return B(A(_1U,[_26,_2p]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_23,_2e]));}),new T(function(){return B(A(_1U,[_24,_2i]));})]));}),new T(function(){return B(A(_1U,[_25,_2m]));})]));}),new T(function(){return B(A(_1U,[_26,_2q]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_27,_2b]));}),new T(function(){return B(A(_1U,[_28,_2f]));})]));}),new T(function(){return B(A(_1U,[_29,_2j]));})]));}),new T(function(){return B(A(_1U,[_2a,_2n]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_27,_2c]));}),new T(function(){return B(A(_1U,[_28,_2g]));})]));}),new T(function(){return B(A(_1U,[_29,_2k]));})]));}),new T(function(){return B(A(_1U,[_2a,_2o]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_27,_2d]));}),new T(function(){return B(A(_1U,[_28,_2h]));})]));}),new T(function(){return B(A(_1U,[_29,_2l]));})]));}),new T(function(){return B(A(_1U,[_2a,_2p]));})]))),E(B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1T,[new T(function(){return B(A(_1U,[_27,_2e]));}),new T(function(){return B(A(_1U,[_28,_2i]));})]));}),new T(function(){return B(A(_1U,[_29,_2m]));})]));}),new T(function(){return B(A(_1U,[_2a,_2q]));})])))];},_2r=function(_2s,_2t,_2u,_2v){var _2w=new T(function(){return B(A(_2v,[_2u]));});return [0,new T(function(){var _2x=B(_1Q(B(_1O(_2s)))),_2y=E(_2t),_2z=E(E(_2w)[1]);return B(_1S(_2x[1],_2x[2],_2y[1],_2y[2],_2y[3],_2y[4],_2y[5],_2y[6],_2y[7],_2y[8],_2y[9],_2y[10],_2y[11],_2y[12],_2y[13],_2y[14],_2y[15],_2y[16],_2z[1],_2z[2],_2z[3],_2z[4],_2z[5],_2z[6],_2z[7],_2z[8],_2z[9],_2z[10],_2z[11],_2z[12],_2z[13],_2z[14],_2z[15],_2z[16]));}),new T(function(){return E(E(_2w)[2]);})];},_2A=function(_2B,_2C,_2D,_2E,_2F,_2G,_2H,_2I,_2J,_2K,_2L,_2M,_2N,_2O,_2P,_2Q,_2R,_2S){return new F(function(){return A(_2B,[_2D,new T(function(){return B(A(_2B,[_2H,new T(function(){return B(A(_2B,[_2L,new T(function(){return B(A(_2B,[_2P,new T(function(){return B(A(_2B,[_2E,new T(function(){return B(A(_2B,[_2I,new T(function(){return B(A(_2B,[_2M,new T(function(){return B(A(_2B,[_2Q,new T(function(){return B(A(_2B,[_2F,new T(function(){return B(A(_2B,[_2J,new T(function(){return B(A(_2B,[_2N,new T(function(){return B(A(_2B,[_2R,new T(function(){return B(A(_2B,[_2G,new T(function(){return B(A(_2B,[_2K,new T(function(){return B(A(_2B,[_2O,new T(function(){return B(A(_2B,[_2S,_2C]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]));})]);});},_2T=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_2U=new T(function(){return B(unCStr("base"));}),_2V=new T(function(){return B(unCStr("IOException"));}),_2W=[0],_2X=new T(function(){var _2Y=hs_wordToWord64(4053623282),_2Z=_2Y,_30=hs_wordToWord64(3693590983),_31=_30;return [0,_2Z,_31,[0,_2Z,_31,_2U,_2T,_2V],_2W];}),_32=function(_33){return E(_2X);},_34=function(_35){return E(E(_35)[1]);},_36=function(_37,_38,_39){var _3a=B(A(_37,[_])),_3b=B(A(_38,[_])),_3c=hs_eqWord64(_3a[1],_3b[1]),_3d=_3c;if(!E(_3d)){return [0];}else{var _3e=hs_eqWord64(_3a[2],_3b[2]),_3f=_3e;return E(_3f)==0?[0]:[1,_39];}},_3g=function(_3h){var _3i=E(_3h);return new F(function(){return _36(B(_34(_3i[1])),_32,_3i[2]);});},_3j=new T(function(){return B(unCStr(": "));}),_3k=[0,41],_3l=new T(function(){return B(unCStr(" ("));}),_3m=function(_3n,_3o){var _3p=E(_3n);return _3p[0]==0?E(_3o):[1,_3p[1],new T(function(){return B(_3m(_3p[2],_3o));})];},_3q=new T(function(){return B(unCStr("already exists"));}),_3r=new T(function(){return B(unCStr("does not exist"));}),_3s=new T(function(){return B(unCStr("protocol error"));}),_3t=new T(function(){return B(unCStr("failed"));}),_3u=new T(function(){return B(unCStr("invalid argument"));}),_3v=new T(function(){return B(unCStr("inappropriate type"));}),_3w=new T(function(){return B(unCStr("hardware fault"));}),_3x=new T(function(){return B(unCStr("unsupported operation"));}),_3y=new T(function(){return B(unCStr("timeout"));}),_3z=new T(function(){return B(unCStr("resource vanished"));}),_3A=new T(function(){return B(unCStr("interrupted"));}),_3B=new T(function(){return B(unCStr("resource busy"));}),_3C=new T(function(){return B(unCStr("resource exhausted"));}),_3D=new T(function(){return B(unCStr("end of file"));}),_3E=new T(function(){return B(unCStr("illegal operation"));}),_3F=new T(function(){return B(unCStr("permission denied"));}),_3G=new T(function(){return B(unCStr("user error"));}),_3H=new T(function(){return B(unCStr("unsatisified constraints"));}),_3I=new T(function(){return B(unCStr("system error"));}),_3J=function(_3K,_3L){switch(E(_3K)){case 0:return new F(function(){return _3m(_3q,_3L);});break;case 1:return new F(function(){return _3m(_3r,_3L);});break;case 2:return new F(function(){return _3m(_3B,_3L);});break;case 3:return new F(function(){return _3m(_3C,_3L);});break;case 4:return new F(function(){return _3m(_3D,_3L);});break;case 5:return new F(function(){return _3m(_3E,_3L);});break;case 6:return new F(function(){return _3m(_3F,_3L);});break;case 7:return new F(function(){return _3m(_3G,_3L);});break;case 8:return new F(function(){return _3m(_3H,_3L);});break;case 9:return new F(function(){return _3m(_3I,_3L);});break;case 10:return new F(function(){return _3m(_3s,_3L);});break;case 11:return new F(function(){return _3m(_3t,_3L);});break;case 12:return new F(function(){return _3m(_3u,_3L);});break;case 13:return new F(function(){return _3m(_3v,_3L);});break;case 14:return new F(function(){return _3m(_3w,_3L);});break;case 15:return new F(function(){return _3m(_3x,_3L);});break;case 16:return new F(function(){return _3m(_3y,_3L);});break;case 17:return new F(function(){return _3m(_3z,_3L);});break;default:return new F(function(){return _3m(_3A,_3L);});}},_3M=[0,125],_3N=new T(function(){return B(unCStr("{handle: "));}),_3O=function(_3P,_3Q,_3R,_3S,_3T,_3U){var _3V=new T(function(){var _3W=new T(function(){return B(_3J(_3Q,new T(function(){var _3X=E(_3S);return _3X[0]==0?E(_3U):B(_3m(_3l,new T(function(){return B(_3m(_3X,[1,_3k,_3U]));},1)));},1)));},1),_3Y=E(_3R);return _3Y[0]==0?E(_3W):B(_3m(_3Y,new T(function(){return B(_3m(_3j,_3W));},1)));},1),_3Z=E(_3T);if(!_3Z[0]){var _40=E(_3P);if(!_40[0]){return E(_3V);}else{var _41=E(_40[1]);return _41[0]==0?B(_3m(_3N,new T(function(){return B(_3m(_41[1],[1,_3M,new T(function(){return B(_3m(_3j,_3V));})]));},1))):B(_3m(_3N,new T(function(){return B(_3m(_41[1],[1,_3M,new T(function(){return B(_3m(_3j,_3V));})]));},1)));}}else{return new F(function(){return _3m(_3Z[1],new T(function(){return B(_3m(_3j,_3V));},1));});}},_42=function(_43){var _44=E(_43);return new F(function(){return _3O(_44[1],_44[2],_44[3],_44[4],_44[6],_2W);});},_45=function(_46,_47){var _48=E(_46);return new F(function(){return _3O(_48[1],_48[2],_48[3],_48[4],_48[6],_47);});},_49=[0,44],_4a=[0,93],_4b=[0,91],_4c=function(_4d,_4e,_4f){var _4g=E(_4e);return _4g[0]==0?B(unAppCStr("[]",_4f)):[1,_4b,new T(function(){return B(A(_4d,[_4g[1],new T(function(){var _4h=function(_4i){var _4j=E(_4i);return _4j[0]==0?E([1,_4a,_4f]):[1,_49,new T(function(){return B(A(_4d,[_4j[1],new T(function(){return B(_4h(_4j[2]));})]));})];};return B(_4h(_4g[2]));})]));})];},_4k=function(_4l,_4m){return new F(function(){return _4c(_45,_4l,_4m);});},_4n=function(_4o,_4p,_4q){var _4r=E(_4p);return new F(function(){return _3O(_4r[1],_4r[2],_4r[3],_4r[4],_4r[6],_4q);});},_4s=[0,_4n,_42,_4k],_4t=new T(function(){return [0,_32,_4s,_4u,_3g];}),_4u=function(_4v){return [0,_4t,_4v];},_4w=function(_4x,_){return new F(function(){return die(new T(function(){return B(_4u(_4x));}));});},_4y=function(_4z,_){return new F(function(){return _4w(_4z,_);});},_4A=function(_4B,_4C,_4D,_){var _4E=E(_4D)[1],_=writeOffAddr("f32",4,_4E,0,E(_4B)[1]);return new F(function(){return A(_4C,[[0,plusAddr(_4E,4)],_]);});},_4F=[0],_4G=3,_4H=new T(function(){return B(unCStr("out of memory"));}),_4I=new T(function(){return B(unCStr("malloc"));}),_4J=[0,_4F,_4G,_4I,_4H,_4F,_4F],_4K=function(_4L,_){return _4L;},_4M=function(_4N){var _4O=B(A(_4N,[_])),_4P=_4O;return E(_4P);},_4Q=function(_4R){return new F(function(){return _4M(function(_){var _=0;return new F(function(){return eval(_4R);});});});},_4S=new T(function(){return B(_4Q("(function(ctx, uniform, arr) {ctx.uniformMatrix4fv(uniform, ctx.FALSE, arr[\'v\'][\'f32\']);})"));}),_4T=function(_4U,_4V,_4W,_4X,_4Y,_){var _4Z=malloc(64),_50=_4Z;if(!addrEq(_50,0)){var _51=E(_4X),_52=B(A(_2A,[_4A,_4K,_51[1],_51[2],_51[3],_51[4],_51[5],_51[6],_51[7],_51[8],_51[9],_51[10],_51[11],_51[12],_51[13],_51[14],_51[15],_51[16],[0,_50],_])),_53=_52,_54=E(E(_4U)[1]),_55=B(A(_4S,[_54,E(E(_4V)[1]),E(_50),_])),_56=_55,_57=malloc(64),_58=_57;if(!addrEq(_58,0)){var _59=E(_4Y),_5a=B(A(_2A,[_4A,_4K,_59[1],_59[2],_59[3],_59[4],_59[5],_59[6],_59[7],_59[8],_59[9],_59[10],_59[11],_59[12],_59[13],_59[14],_59[15],_59[16],[0,_58],_])),_5b=_5a,_5c=B(A(_4S,[_54,E(E(_4W)[1]),E(_58),_])),_5d=_5c;return _0;}else{return new F(function(){return _4y(_4J,_);});}}else{return new F(function(){return _4y(_4J,_);});}},_5e=function(_5f,_5g){while(1){var _5h=E(_5f);if(!_5h[0]){return E(_5g);}else{_5f=_5h[2];var _5i=_5g+1|0;_5g=_5i;continue;}}},_5j=[0,1],_5k=[0,0],_5l=function(_5m,_5n,_5o,_5p,_5q,_5r,_5s,_5t){return [0,B(A(_5n,[new T(function(){return B(A(_5m,[_5p,_5t]));}),new T(function(){return B(A(_5m,[_5q,_5s]));})])),B(A(_5n,[new T(function(){return B(A(_5m,[_5q,_5r]));}),new T(function(){return B(A(_5m,[_5o,_5t]));})])),B(A(_5n,[new T(function(){return B(A(_5m,[_5o,_5s]));}),new T(function(){return B(A(_5m,[_5p,_5r]));})]))];},_5u=function(_5v,_5w,_5x,_5y,_5z,_5A,_5B,_5C,_5D,_5E,_5F,_5G,_5H,_5I){var _5J=E(_5v),_5K=_5J[1],_5L=_5J[2],_5M=_5J[3],_5N=_5J[4],_5O=_5J[7],_5P=B(A(_5M,[_5D,_5G])),_5Q=B(A(_5M,[_5E,_5H])),_5R=B(A(_5M,[_5F,_5I])),_5S=new T(function(){return B(A(_5z,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5L,[_5P,_5P]));}),new T(function(){return B(A(_5L,[_5Q,_5Q]));})]));}),new T(function(){return B(A(_5L,[_5R,_5R]));})]));})]));}),_5T=B(A(_5w,[_5P,_5S])),_5U=B(A(_5w,[_5Q,_5S])),_5V=B(A(_5w,[_5R,_5S])),_5W=B(_5l(_5L,_5M,_5A,_5B,_5C,_5T,_5U,_5V)),_5X=_5W[1],_5Y=_5W[2],_5Z=_5W[3],_60=new T(function(){return B(A(_5z,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5L,[_5X,_5X]));}),new T(function(){return B(A(_5L,[_5Y,_5Y]));})]));}),new T(function(){return B(A(_5L,[_5Z,_5Z]));})]));})]));}),_61=B(A(_5w,[_5X,_60])),_62=B(A(_5w,[_5Y,_60])),_63=B(A(_5w,[_5Z,_60])),_64=B(_5l(_5L,_5M,_5T,_5U,_5V,_61,_62,_63)),_65=_64[1],_66=_64[2],_67=_64[3],_68=B(A(_5O,[_5k]));return [0,E(_61),E(_62),E(_63),E(B(A(_5N,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5L,[_61,_5D]));}),new T(function(){return B(A(_5L,[_62,_5E]));})]));}),new T(function(){return B(A(_5L,[_63,_5F]));})]));})]))),E(_65),E(_66),E(_67),E(B(A(_5N,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5L,[_65,_5D]));}),new T(function(){return B(A(_5L,[_66,_5E]));})]));}),new T(function(){return B(A(_5L,[_67,_5F]));})]));})]))),E(_5T),E(_5U),E(_5V),E(B(A(_5N,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5K,[new T(function(){return B(A(_5L,[_5T,_5D]));}),new T(function(){return B(A(_5L,[_5U,_5E]));})]));}),new T(function(){return B(A(_5L,[_5V,_5F]));})]));})]))),E(_68),E(_68),E(_68),E(B(A(_5O,[_5j])))];},_69=[0,2],_6a=function(_6b,_6c,_6d,_6e,_6f,_6g,_6h,_6i,_6j){var _6k=E(_6b),_6l=_6k[2],_6m=_6k[3],_6n=_6k[7],_6o=new T(function(){return B(A(_6l,[_6g,new T(function(){return B(A(_6f,[new T(function(){return B(A(_6c,[_6i,new T(function(){return B(A(_6n,[_69]));})]));})]));})]));}),_6p=B(A(_6n,[_5k]));return [0,E(B(A(_6c,[_6g,new T(function(){return B(A(_6l,[_6j,_6o]));})]))),E(_6p),E(_6p),E(_6p),E(_6p),E(B(A(_6c,[_6g,_6o]))),E(_6p),E(_6p),E(_6p),E(_6p),E(B(A(_6c,[new T(function(){return B(A(_6k[1],[_6g,_6h]));}),new T(function(){return B(A(_6m,[_6g,_6h]));})]))),E(B(A(_6c,[new T(function(){return B(A(_6l,[new T(function(){return B(A(_6l,[new T(function(){return B(A(_6n,[_69]));}),_6g]));}),_6h]));}),new T(function(){return B(A(_6m,[_6g,_6h]));})]))),E(_6p),E(_6p),E(B(A(_6k[4],[new T(function(){return B(A(_6n,[_5j]));})]))),E(_6p)];},_6q=function(_6r,_6s,_6t,_6u,_6v,_6w,_6x,_6y,_6z,_6A){var _6B=E(_6r),_6C=_6B[1],_6D=_6B[2],_6E=_6B[3],_6F=_6B[7],_6G=new T(function(){return B(A(_6w,[_6A]));}),_6H=new T(function(){return B(A(_6E,[new T(function(){return B(A(_6F,[_5j]));}),_6G]));}),_6I=new T(function(){return B(A(_6v,[_6A]));}),_6J=B(A(_6F,[_5k]));return [0,E(B(A(_6C,[_6G,new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6x]));}),_6x]));})]))),E(B(A(_6E,[new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6x]));}),_6y]));}),new T(function(){return B(A(_6D,[_6I,_6z]));})]))),E(B(A(_6C,[new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6x]));}),_6z]));}),new T(function(){return B(A(_6D,[_6I,_6y]));})]))),E(_6J),E(B(A(_6C,[new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6x]));}),_6y]));}),new T(function(){return B(A(_6D,[_6I,_6z]));})]))),E(B(A(_6C,[_6G,new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6y]));}),_6y]));})]))),E(B(A(_6E,[new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6y]));}),_6z]));}),new T(function(){return B(A(_6D,[_6I,_6x]));})]))),E(_6J),E(B(A(_6E,[new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6x]));}),_6z]));}),new T(function(){return B(A(_6D,[_6I,_6y]));})]))),E(B(A(_6C,[new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6y]));}),_6z]));}),new T(function(){return B(A(_6D,[_6I,_6x]));})]))),E(B(A(_6C,[_6G,new T(function(){return B(A(_6D,[new T(function(){return B(A(_6D,[_6H,_6z]));}),_6z]));})]))),E(_6J),E(_6J),E(_6J),E(_6J),E(B(A(_6F,[_5j])))];},_6K=function(_6L,_6M){var _6N=jsShowI(_6L),_6O=_6N;return new F(function(){return _3m(fromJSStr(_6O),_6M);});},_6P=[0,41],_6Q=[0,40],_6R=function(_6S,_6T,_6U){if(_6T>=0){return new F(function(){return _6K(_6T,_6U);});}else{return _6S<=6?B(_6K(_6T,_6U)):[1,_6Q,new T(function(){var _6V=jsShowI(_6T),_6W=_6V;return B(_3m(fromJSStr(_6W),[1,_6P,_6U]));})];}},_6X=1,_6Y=false,_6Z=4,_70=6,_71=[0,0],_72=[0,1],_73=new T(function(){return B(_4Q("(function(ctx, program, shader) {ctx.attachShader(program, shader);})"));}),_74=new T(function(){return B(_4Q("(function(ctx, target, buffer) {ctx.bindBuffer(target, buffer);})"));}),_75=new T(function(){return [0,"(function(ctx, target, data, usage) {ctx.bufferData(target, data[\'b\'], usage);})"];}),_76=new T(function(){return B(_4Q("(function(ctx, r, g, b, a) {ctx.clearColor(r, g, b, a);})"));}),_77=function(_78){return function(_79){return function(_7a){return function(_7b){return function(_7c,_){var _7d=B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(_76,[E(E(_78)[1])]));}),[E(E(_79)[1])]));}),[E(E(_7a)[1])]));}),[E(E(_7b)[1])]));}),[E(E(_7c)[1]),_])),_7e=_7d;return _0;};};};};},_7f=new T(function(){return B(_4Q("(function(ctx, mask) {ctx.clear(mask);})"));}),_7g=new T(function(){return B(_4Q("(function(ctx, shader) {ctx.compileShader(shader);})"));}),_7h=new T(function(){return B(_4Q("(function(ctx) {return ctx.createBuffer();})"));}),_7i=new T(function(){return B(_4Q("(function(ctx) {return ctx.createProgram();})"));}),_7j=new T(function(){return B(_4Q("(function(ctx, type) {return ctx.createShader(type);})"));}),_7k=function(_){var _7l=B(A(_4Q,["document.body",_])),_7m=_7l;return [0,_7m];},_7n=function(_){return new F(function(){return _7k(_);});},_7o=function(_){var _=0;return new F(function(){return _7n(_);});},_7p=new T(function(){return B(_4M(_7o));}),_7q=new T(function(){return B(_4Q("(function(ctx, mode, first, count) {ctx.drawArrays(mode, first, count);})"));}),_7r=function(_7s){switch(E(_7s)){case 0:return E(0);case 1:return E(3);case 2:return E(2);case 3:return E(1);case 4:return E(5);case 5:return E(6);default:return E(4);}},_7t=new T(function(){return B(_4Q("(function(ctx, mode, count, type, offset) {ctx.drawElements(mode, count, type, offset);})"));}),_7u=function(_7v){return function(_7w){return function(_7x){var _7y=new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(_7t,[E(E(_7v)[1])]));}),[B(_7r(_7w))]));}),[E(E(_7x)[1])]));});return function(_7z){return function(_7A,_){var _7B=B(A(new T(function(){if(!E(_7z)){var _7C=E(5121);}else{var _7C=E(5123);}var _7D=_7C,_7E=B(A(_7y,[_7D]));return _7E;}),[E(E(_7A)[1]),_])),_7F=_7B;return _0;};};};};},_7G=new T(function(){return B(_4Q("(function(ctx, index) {ctx.enableVertexAttribArray(index);})"));}),_7H=new T(function(){return B(_4Q("(function(ctx, cap) {ctx.enable(cap);})"));}),_7I=new T(function(){return B(_4Q("(function(ctx, program, name) {return ctx.getAttribLocation(program, name);})"));}),_7J=function(_7K){return function(_7L){return function(_7M,_){var _7N=B(A(new T(function(){return B(A(new T(function(){return B(A(_7I,[E(E(_7K)[1])]));}),[E(E(_7L)[1])]));}),[E(toJSStr(E(_7M))),_])),_7O=_7N;return [0,_7O];};};},_7P=new T(function(){return B(_4Q("(function(elt, name) {return elt.getContext(name);})"));}),_7Q=new T(function(){return B(_4Q("(function(ctx, program, name) {return ctx.getUniformLocation(program, name);})"));}),_7R=function(_7S){return function(_7T){return function(_7U,_){var _7V=B(A(new T(function(){return B(A(new T(function(){return B(A(_7Q,[E(E(_7S)[1])]));}),[E(E(_7T)[1])]));}),[E(toJSStr(E(_7U))),_])),_7W=_7V;return [0,_7W];};};},_7X=new T(function(){return B(_4Q("(function(ctx, program) {ctx.linkProgram(program);})"));}),_7Y=[0,0.1],_7Z=[0,100],_80=[0,0.7853981852531433],_81=[0,5],_82=[0,-3.5],_83=[0,36],_84=[0,0],_85=[0,1.5],_86=[0,E(_72),E(_71),E(_71),E(_85),E(_71),E(_72),E(_71),E(_71),E(_71),E(_71),E(_72),E(_71),E(_71),E(_71),E(_71),E(_72)],_87=[0,4],_88=[0,3],_89=[0,E(_72),E(_71),E(_71),E(_82),E(_71),E(_72),E(_71),E(_71),E(_71),E(_71),E(_72),E(_71),E(_71),E(_71),E(_71),E(_72)],_8a=[0,0],_8b=[0,2],_8c=function(_8d){return E(_8b);},_8e=function(_8f){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_8f));}))));});},_8g=new T(function(){return B(_8e("ww_s7jz4{v} [lid] base:GHC.Ptr.Ptr{tc 33A} a{tv a7j62} [tv]\n                  -> a{tv a7j62} [tv] -> ghc-prim:GHC.Types.IO{tc 32I} ()"));}),_8h=new T(function(){return B(_8e("ww_s7jz3{v} [lid] base:GHC.Ptr.Ptr{tc 33A} a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.IO{tc 32I} a{tv a7j62} [tv]"));}),_8i=new T(function(){return B(_8e("ww_s7jz2{v} [lid] forall b{tv i3yg7} [tv].\n                  base:GHC.Ptr.Ptr{tc 33A} b{tv i3yg7} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}\n                  -> a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.IO{tc 32I} ()"));}),_8j=new T(function(){return B(_8e("ww_s7jz1{v} [lid] forall b{tv i3yg6} [tv].\n                  base:GHC.Ptr.Ptr{tc 33A} b{tv i3yg6} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}\n                  -> ghc-prim:GHC.Types.IO{tc 32I} a{tv a7j62} [tv]"));}),_8k=new T(function(){return B(_8e("ww_s7jyZ{v} [lid] base:GHC.Ptr.Ptr{tc 33A} a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}\n                  -> ghc-prim:GHC.Types.IO{tc 32I} a{tv a7j62} [tv]"));}),_8l=new T(function(){return B(_8e("ww_s7jyY{v} [lid] a{tv a7j62} [tv]\n                  -> ghc-prim:GHC.Types.Int{(w) tc 3J}"));}),_8m=function(_8n,_8o,_8p,_){var _=writeOffAddr("w16",2,E(_8n)[1],E(_8o)[1],E(_8p)[1]);return _0;},_8q=[0,_8c,_8l,_8k,_8m,_8j,_8i,_8h,_8g],_8r=[0,4],_8s=function(_8t){return E(_8r);},_8u=function(_8v,_8w,_8x,_){var _=writeOffAddr("f32",4,E(_8v)[1],E(_8w)[1],E(_8x)[1]);return _0;},_8y=[0,_8s,_8l,_8k,_8u,_8j,_8i,_8h,_8g],_8z=new T(function(){return B(unCStr("uMVMatrix"));}),_8A=new T(function(){return B(unCStr("uPMatrix"));}),_8B=new T(function(){return B(unCStr("aVertexColor"));}),_8C=new T(function(){return B(unCStr("aVertexPosition"));}),_8D=new T(function(){return B(unCStr("attribute vec3 aVertexPosition;"));}),_8E=new T(function(){return B(unCStr("attribute vec4 aVertexColor;"));}),_8F=new T(function(){return B(unCStr("uniform mat4 uMVMatrix;"));}),_8G=new T(function(){return B(unCStr("uniform mat4 uPMatrix;"));}),_8H=new T(function(){return B(unCStr("varying vec4 vColor;"));}),_8I=new T(function(){return B(unCStr("void main(void) {"));}),_8J=new T(function(){return B(unCStr("  gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);"));}),_8K=new T(function(){return B(unCStr("  vColor = aVertexColor;"));}),_8L=[0,125],_8M=[1,_8L,_2W],_8N=[1,_8M,_2W],_8O=[1,_8K,_8N],_8P=[1,_8J,_8O],_8Q=[1,_8I,_8P],_8R=[1,_8H,_8Q],_8S=[1,_8G,_8R],_8T=[1,_8F,_8S],_8U=[1,_8E,_8T],_8V=[1,_8D,_8U],_8W=new T(function(){return B(unCStr("precision mediump float;"));}),_8X=new T(function(){return B(unCStr("    gl_FragColor = vColor;"));}),_8Y=[1,_8X,_8N],_8Z=[1,_8I,_8Y],_90=[1,_8H,_8Z],_91=[1,_8W,_90],_92=function(_93){return E(E(_93)[4]);},_94=function(_95,_96,_97,_){return new F(function(){return (function(_98,_99,_){while(1){var _9a=E(_98);if(!_9a[0]){return _0;}else{var _9b=B(A(new T(function(){return B(_92(_95));}),[_96,[0,_99],_9a[1],_])),_9c=_9b;_98=_9a[2];var _9d=_99+1|0;_99=_9d;continue;}}})(_97,0,_);});},_9e=new T(function(){return B(_4Q("(function(ctx, shader, source) {ctx.shaderSource(shader, source);})"));}),_9f=[0,10],_9g=function(_9h){var _9i=E(_9h);if(!_9i[0]){return [0];}else{return new F(function(){return _3m(_9i[1],[1,_9f,new T(function(){return B(_9g(_9i[2]));})]);});}},_9j=new T(function(){return B(_4Q("(function(ctx, program) {ctx.useProgram(program);})"));}),_9k=function(_9l){switch(E(_9l)){case 0:return E(5120);case 1:return E(5122);case 2:return E(5121);case 3:return E(5123);default:return E(5126);}},_9m=function(_){var _=0;return new F(function(){return A(_4Q,["false",_]);});},_9n=new T(function(){return B(_4M(_9m));}),_9o=function(_){var _=0;return new F(function(){return A(_4Q,["true",_]);});},_9p=new T(function(){return B(_4M(_9o));}),_9q=new T(function(){return B(_4Q("(function(ctx, index, size, type, normalized, stride, offset) {ctx.vertexAttribPointer(index, size, type, normalized, stride, offset);})"));}),_9r=function(_9s){return function(_9t){return function(_9u){return function(_9v){return function(_9w){return function(_9x){return function(_9y,_){var _9z=B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(new T(function(){return B(A(_9q,[E(E(_9s)[1])]));}),[E(E(_9t)[1])]));}),[E(E(_9u)[1])]));}),[B(_9k(_9v))]));}),[!E(_9w)?E(_9n):E(_9p)]));}),[E(E(_9x)[1])]));}),[E(E(_9y)[1]),_])),_9A=_9z;return _0;};};};};};};},_9B=new T(function(){return B(_4Q("(function(ctx, x, y, width, height) {ctx.viewport(x, y, width, height);})"));}),_9C=[0,-1],_9D=[1,_72,_2W],_9E=[1,_9C,_9D],_9F=[1,_9C,_9E],_9G=[1,_9C,_9F],_9H=[1,_9C,_9G],_9I=[1,_9C,_9H],_9J=[1,_71,_9I],_9K=[1,_72,_9J],_9L=[1,_71,_9K],_9M=[1,_9C,_9L],_9N=[1,_9C,_9M],_9O=[1,_9C,_9N],_9P=[1,_9C,_9O],_9Q=[1,_9C,_9P],_9R=[1,_72,_9Q],_9S=[1,_71,_9R],_9T=[1,_72,_9S],_9U=[1,_71,_9T],_9V=[1,_9C,_9U],_9W=[1,_9C,_9V],_9X=[1,_72,_9W],_9Y=[1,_72,_9X],_9Z=[1,_9C,_9Y],_a0=[1,_72,_9Z],_a1=[1,_71,_a0],_a2=[1,_72,_a1],_a3=[1,_71,_a2],_a4=[1,_72,_a3],_a5=[1,_9C,_a4],_a6=[1,_72,_a5],_a7=[1,_72,_a6],_a8=[1,_9C,_a7],_a9=[1,_9C,_a8],_aa=[1,_71,_a9],_ab=[1,_72,_aa],_ac=[1,_71,_ab],_ad=[1,_71,_9D],_ae=[1,_72,_ad],_af=[1,_71,_ae],_ag=[1,_72,_af],_ah=[1,_72,_ag],_ai=[1,_71,_ah],_aj=[1,_71,_ai],_ak=[1,_72,_aj],_al=[1,_71,_ak],_am=[1,_71,_al],_an=[1,_72,_am],_ao=[1,_72,_an],_ap=[1,_72,_ao],_aq=[1,_71,_ap],_ar=[1,_71,_aq],_as=[1,_72,_ar],_at=[1,_71,_as],_au=[1,_72,_at],_av=[1,_71,_au],_aw=[1,_72,_av],_ax=[1,_71,_aw],_ay=[1,_71,_ax],_az=[1,_72,_ay],_aA=[1,_72,_az],_aB=[1,_71,_aA],_aC=[1,_72,_aB],_aD=[1,_71,_aC],_aE=[1,_72,_aD],_aF=[1,_72,_aE],_aG=[1,_71,_aF],_aH=[1,_71,_aG],_aI=[1,_72,_aH],_aJ=[1,_71,_aI],_aK=[1,_71,_aJ],_aL=[1,_72,_aK],_aM=[1,_72,_aL],_aN=[1,_72,_aM],_aO=[1,_71,_aN],_aP=[1,_71,_aO],_aQ=[1,_72,_aP],_aR=[1,_71,_aQ],_aS=[1,_72,_aR],_aT=[1,_71,_aS],_aU=[1,_72,_aT],_aV=[1,_71,_aU],_aW=[1,_71,_aV],_aX=[1,_72,_aW],_aY=[1,_9C,_2W],_aZ=[1,_72,_aY],_b0=[1,_9C,_aZ],_b1=[1,_72,_b0],_b2=[1,_72,_b1],_b3=[1,_9C,_b2],_b4=[1,_72,_b3],_b5=[1,_9C,_b4],_b6=[1,_9C,_b5],_b7=[1,_9C,_b6],_b8=[1,_9C,_b7],_b9=[1,_9C,_b8],_ba=[1,_72,_b9],_bb=[1,_9C,_ba],_bc=[1,_72,_bb],_bd=[1,_72,_bc],_be=[1,_72,_bd],_bf=[1,_72,_be],_bg=[1,_9C,_bf],_bh=[1,_72,_bg],_bi=[1,_72,_bh],_bj=[1,_9C,_bi],_bk=[1,_9C,_bj],_bl=[1,_72,_bk],_bm=[1,_72,_bl],_bn=[1,_9C,_bm],_bo=[1,_9C,_bn],_bp=[1,_72,_bo],_bq=[1,_9C,_bp],_br=[1,_72,_bq],_bs=[1,_9C,_br],_bt=[1,_9C,_bs],_bu=[1,_72,_bt],_bv=[1,_9C,_bu],_bw=[1,_9C,_bv],_bx=[1,_9C,_bw],_by=[1,_9C,_bx],_bz=[1,_72,_by],_bA=[1,_72,_bz],_bB=[1,_72,_bA],_bC=[1,_72,_bB],_bD=[1,_72,_bC],_bE=[1,_72,_bD],_bF=[1,_72,_bE],_bG=[1,_9C,_bF],_bH=[1,_9C,_bG],_bI=[1,_72,_bH],_bJ=[1,_9C,_bI],_bK=[1,_9C,_bJ],_bL=[1,_9C,_bK],_bM=[1,_72,_bL],_bN=[1,_9C,_bM],_bO=[1,_72,_bN],_bP=[1,_72,_bO],_bQ=[1,_9C,_bP],_bR=[1,_72,_bQ],_bS=[1,_9C,_bR],_bT=[1,_9C,_bS],_bU=[1,_9C,_bT],_bV=[1,_9C,_bU],_bW=[1,_72,_bV],_bX=[1,_72,_bW],_bY=[1,_9C,_bX],_bZ=[1,_72,_bY],_c0=[1,_72,_bZ],_c1=[1,_72,_c0],_c2=[1,_72,_c1],_c3=[1,_9C,_c2],_c4=[1,_72,_c3],_c5=[1,_72,_c4],_c6=[1,_9C,_c5],_c7=[1,_9C,_c6],_c8=function(_c9){var _ca=E(_c9);if(!_ca[0]){return [0];}else{return new F(function(){return _3m(_ca[1],new T(function(){return B(_c8(_ca[2]));},1));});}},_cb=function(_cc){var _cd=E(_cc);if(!_cd[0]){return [0];}else{var _ce=_cd[1],_cf=function(_cg){return _cg>1?[1,_ce,new T(function(){return B(_cf(_cg-1|0));})]:E([1,_ce,new T(function(){return B(_cb(_cd[2]));})]);};return new F(function(){return _cf(4);});}},_ch=function(_ci,_cj){var _ck=function(_cl){return _cl>1?[1,_ci,new T(function(){return B(_ck(_cl-1|0));})]:E([1,_ci,new T(function(){return B(_cb(_cj));})]);};return new F(function(){return _ck(4);});},_cm=[1,_72,_9D],_cn=[1,_71,_cm],_co=[1,_72,_cn],_cp=[1,_71,_cn],_cq=[1,_cp,_2W],_cr=[1,_co,_cq],_cs=[0,0.5],_ct=[1,_cs,_9D],_cu=[1,_cs,_ct],_cv=[1,_72,_cu],_cw=[1,_cv,_cr],_cx=[1,_af,_cw],_cy=[1,_72,_ae],_cz=[1,_cy,_cx],_cA=[1,_71,_ad],_cB=[1,_72,_cA],_cC=new T(function(){return B(_ch(_cB,_cz));}),_cD=new T(function(){return B(_c8(_cC));}),_cE=[0,0],_cF=[0,1],_cG=[0,2],_cH=[0,3],_cI=[0,4],_cJ=[0,5],_cK=[0,6],_cL=[0,7],_cM=[0,8],_cN=[0,9],_cO=[0,10],_cP=[0,11],_cQ=[0,12],_cR=[0,13],_cS=[0,14],_cT=[0,15],_cU=[0,16],_cV=[0,17],_cW=[0,18],_cX=[0,19],_cY=[0,20],_cZ=[0,21],_d0=[0,22],_d1=[0,23],_d2=[1,_d1,_2W],_d3=[1,_d0,_d2],_d4=[1,_cY,_d3],_d5=[1,_d0,_d4],_d6=[1,_cZ,_d5],_d7=[1,_cY,_d6],_d8=[1,_cX,_d7],_d9=[1,_cW,_d8],_da=[1,_cU,_d9],_db=[1,_cW,_da],_dc=[1,_cV,_db],_dd=[1,_cU,_dc],_de=[1,_cT,_dd],_df=[1,_cS,_de],_dg=[1,_cQ,_df],_dh=[1,_cS,_dg],_di=[1,_cR,_dh],_dj=[1,_cQ,_di],_dk=[1,_cP,_dj],_dl=[1,_cO,_dk],_dm=[1,_cM,_dl],_dn=[1,_cO,_dm],_do=[1,_cN,_dn],_dp=[1,_cM,_do],_dq=[1,_cL,_dp],_dr=[1,_cK,_dq],_ds=[1,_cI,_dr],_dt=[1,_cK,_ds],_du=[1,_cJ,_dt],_dv=[1,_cI,_du],_dw=[1,_cH,_dv],_dx=[1,_cG,_dw],_dy=[1,_cE,_dx],_dz=[1,_cG,_dy],_dA=[1,_cF,_dz],_dB=[1,_cE,_dA],_dC=function(_){var _dD=jsCreateElem("canvas"),_dE=_dD,_dF=B(A(_4Q,["window.innerWidth",_])),_dG=_dF,_dH=B(A(_4Q,["window.innerHeight",_])),_dI=_dH,_dJ=jsTrunc(_dG),_dK=_dJ,_dL=_dK,_dM=jsSetAttr(_dE,"width",toJSStr(B(_6R(0,_dK,_2W)))),_dN=jsTrunc(_dI),_dO=_dN,_dP=_dO,_dQ=jsSetAttr(_dE,"height",toJSStr(B(_6R(0,_dO,_2W)))),_dR=jsAppendChild(_dE,E(_7p)[1]),_dS=B(A(_7P,[E(_dE),E("webgl"),_])),_dT=_dS,_dU=E(_dT),_dV=B(A(_7j,[_dU,E(35632),_])),_dW=_dV,_dX=E(_dW),_dY=B(A(_9e,[_dU,_dX,E(toJSStr(B(_9g(_91)))),_])),_dZ=_dY,_e0=B(A(_7g,[_dU,_dX,_])),_e1=_e0,_e2=B(A(_7j,[_dU,E(35633),_])),_e3=_e2,_e4=E(_e3),_e5=B(A(_9e,[_dU,_e4,E(toJSStr(B(_9g(_8V)))),_])),_e6=_e5,_e7=B(A(_7g,[_dU,_e4,_])),_e8=_e7,_e9=B(A(_7i,[_dU,_])),_ea=_e9,_eb=E(_ea),_ec=B(A(_73,[_dU,_eb,_e4,_])),_ed=_ec,_ee=B(A(_73,[_dU,_eb,_dX,_])),_ef=_ee,_eg=B(A(_7X,[_dU,_eb,_])),_eh=_eg,_ei=B(A(_9j,[_dU,_eb,_])),_ej=_ei,_ek=[0,_eb],_el=[0,_dU],_em=B(A(_7J,[_el,_ek,_8C,_])),_en=_em,_eo=E(_en),_ep=B(A(_7G,[_dU,E(_eo[1]),_])),_eq=_ep,_er=B(A(_7J,[_el,_ek,_8B,_])),_es=_er,_et=E(_es),_eu=B(A(_7G,[_dU,E(_et[1]),_])),_ev=_eu,_ew=B(A(_7R,[_el,_ek,_8A,_])),_ex=_ew,_ey=B(A(_7R,[_el,_ek,_8z,_])),_ez=_ey,_eA=B(A(_7h,[_dU,_])),_eB=_eA,_eC=E(_eB),_eD=E(34962),_eE=B(A(_74,[_dU,_eD,_eC,_])),_eF=_eE,_eG=malloc((imul(B(_5e(_ac,0)),4)|0)>>>0),_eH=_eG;if(!addrEq(_eH,0)){var _eI=B(_94(_8y,[0,_eH],_ac,_)),_eJ=_eI,_eK=E(35044),_eL=E(_75)[1],_eM=B(A(_4Q,[_eL,_dU,_eD,E(_eH),_eK,_])),_eN=_eM,_eO=B(A(_7h,[_dU,_])),_eP=_eO,_eQ=E(_eP),_eR=B(A(_74,[_dU,_eD,_eQ,_])),_eS=_eR,_eT=malloc((imul(B(_5e(_aX,0)),4)|0)>>>0),_eU=_eT;if(!addrEq(_eU,0)){var _eV=B(_94(_8y,[0,_eU],_aX,_)),_eW=_eV,_eX=B(A(_4Q,[_eL,_dU,_eD,E(_eU),_eK,_])),_eY=_eX,_eZ=B(A(_7h,[_dU,_])),_f0=_eZ,_f1=E(_f0),_f2=B(A(_74,[_dU,_eD,_f1,_])),_f3=_f2,_f4=malloc((imul(B(_5e(_c7,0)),4)|0)>>>0),_f5=_f4;if(!addrEq(_f5,0)){var _f6=B(_94(_8y,[0,_f5],_c7,_)),_f7=_f6,_f8=B(A(_4Q,[_eL,_dU,_eD,E(_f5),_eK,_])),_f9=_f8,_fa=B(A(_7h,[_dU,_])),_fb=_fa,_fc=E(_fb),_fd=B(A(_74,[_dU,_eD,_fc,_])),_fe=_fd,_ff=malloc((imul(B(_5e(_cD,0)),4)|0)>>>0),_fg=_ff;if(!addrEq(_fg,0)){var _fh=B(_94(_8y,[0,_fg],_cD,_)),_fi=_fh,_fj=B(A(_4Q,[_eL,_dU,_eD,E(_fg),_eK,_])),_fk=_fj,_fl=B(A(_7h,[_dU,_])),_fm=_fl,_fn=E(_fm),_fo=E(34963),_fp=B(A(_74,[_dU,_fo,_fn,_])),_fq=_fp,_fr=malloc((imul(B(_5e(_dB,0)),2)|0)>>>0),_fs=_fr;if(!addrEq(_fs,0)){var _ft=B(_94(_8q,[0,_fs],_dB,_)),_fu=_ft,_fv=B(A(_4Q,[_eL,_dU,_fo,E(_fs),_eK,_])),_fw=_fv,_fx=B(A(_7H,[_dU,E(2929),_])),_fy=_fx,_fz=malloc(80),_fA=_fz;if(!addrEq(_fA,0)){var _=writeOffAddr("i32",4,_fA,0,3),_=writeOffAddr("i32",4,_fA,1,2),_fB=readOffAddr("i32",4,_fA,0),_fC=_fB,_fD=B(_1I(16,function(_){var _fE=B(A(_77,[_el,_8a,_8a,_8a,_8a,_])),_fF=_fE,_fG=B(A(_4Q,["(function() {return (new Date().getTime());})",_])),_fH=_fG,_fI=E(0),_fJ=B(A(_9B,[_dU,_fI,_fI,E(_dL),E(_dP),_])),_fK=_fJ,_fL=B(A(_7f,[_dU,E((16384>>>0|256>>>0)>>>0&4294967295),_])),_fM=_fL,_fN=B(A(_74,[_dU,_eD,_eC,_])),_fO=_fN,_fP=B(A(_9r,[_el,_eo,_88,_6Z,_6Y,_84,_84,_])),_fQ=_fP,_fR=B(A(_74,[_dU,_eD,_eQ,_])),_fS=_fR,_fT=B(A(_9r,[_el,_et,_87,_6Z,_6Y,_84,_84,_])),_fU=_fT,_fV=new T(function(){var _fW=jsTrunc(_fH),_fX=_fW;return [0,_fX+_fC|0];}),_fY=new T(function(){var _fZ=E(_fV)[1]/5000;return B(_5u(_1b,_1c,_J,_G,_1B,_71,_72,_71,[0,-3.5+13*Math.cos(_fZ)],_81,[0,13*Math.sin(_fZ)],_82,_71,_71));}),_g0=new T(function(){return B(_6a(_1b,_1c,_J,_G,_1D,_7Y,_7Z,_80,new T(function(){return [0,_dL/_dP];})));}),_g1=B(_4T(_el,_ex,_ez,_g0,new T(function(){var _g2=E(_fY),_g3=E(B(_2r(_1H,_89,_0,function(_g4){return [0,new T(function(){var _g5= -(E(_fV)[1]/2000),_g6=[0,Math.cos(_g5)],_g7=Math.sin(_g5);return [0,E(_g6),E(_71),E([0,_g7]),E(_71),E(_71),E(_72),E(_71),E(_71),E([0, -_g7]),E(_71),E(_g6),E(_71),E(_71),E(_71),E(_71),E(_72)];}),_g4];}))[1]);return B(_1S(_15,_18,_g2[1],_g2[2],_g2[3],_g2[4],_g2[5],_g2[6],_g2[7],_g2[8],_g2[9],_g2[10],_g2[11],_g2[12],_g2[13],_g2[14],_g2[15],_g2[16],_g3[1],_g3[2],_g3[3],_g3[4],_g3[5],_g3[6],_g3[7],_g3[8],_g3[9],_g3[10],_g3[11],_g3[12],_g3[13],_g3[14],_g3[15],_g3[16]));},1),_)),_g8=_g1,_g9=B(A(_7q,[_dU,E(4),_fI,E(12),_])),_ga=_g9,_gb=B(A(_74,[_dU,_eD,_f1,_])),_gc=_gb,_gd=B(A(_9r,[_el,_eo,_88,_6Z,_6Y,_84,_84,_])),_ge=_gd,_gf=B(A(_74,[_dU,_eD,_fc,_])),_gg=_gf,_gh=B(A(_9r,[_el,_et,_87,_6Z,_6Y,_84,_84,_])),_gi=_gh,_gj=B(A(_74,[_dU,_fo,_fn,_])),_gk=_gj,_gl=B(_4T(_el,_ex,_ez,_g0,new T(function(){var _gm=E(_fY),_gn=E(B(_2r(_1H,_86,_0,function(_g4){return [0,new T(function(){var _go=1/Math.sqrt(3);return B(_6q(_1b,_1c,_J,_G,_1x,_1m,[0,_go],[0,_go],[0,_go],new T(function(){return [0,E(_fV)[1]/1000];})));}),_g4];}))[1]);return B(_1S(_15,_18,_gm[1],_gm[2],_gm[3],_gm[4],_gm[5],_gm[6],_gm[7],_gm[8],_gm[9],_gm[10],_gm[11],_gm[12],_gm[13],_gm[14],_gm[15],_gm[16],_gn[1],_gn[2],_gn[3],_gn[4],_gn[5],_gn[6],_gn[7],_gn[8],_gn[9],_gn[10],_gn[11],_gn[12],_gn[13],_gn[14],_gn[15],_gn[16]));},1),_)),_gp=_gl;return new F(function(){return A(_7u,[_el,_70,_83,_6X,_84,_]);});},_)),_gq=_fD;return _0;}else{return new F(function(){return _4y(_4J,_);});}}else{return new F(function(){return _4y(_4J,_);});}}else{return new F(function(){return _4y(_4J,_);});}}else{return new F(function(){return _4y(_4J,_);});}}else{return new F(function(){return _4y(_4J,_);});}}else{return new F(function(){return _4y(_4J,_);});}},_gr=function(_){var _gs=jsSetTimeout(100,_dC);return _0;},_gt=function(_){return new F(function(){return _gr(_);});};
var hasteMain = function() {B(A(_gt, [0]));};window.onload = hasteMain;