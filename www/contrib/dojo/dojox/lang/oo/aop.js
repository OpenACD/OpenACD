/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.lang.oo.aop"]){
dojo._hasResource["dojox.lang.oo.aop"]=true;
dojo.provide("dojox.lang.oo.aop");
dojo.require("dojox.lang.oo.Decorator");
dojo.require("dojox.lang.oo.chain");
dojo.require("dojox.lang.oo.general");
(function(){
var oo=dojox.lang.oo,md=oo.makeDecorator,_1=oo.aop;
_1.before=oo.chain.before;
_1.around=oo.general.wrap;
_1.afterReturning=md(function(_2,_3,_4){
return dojo.isFunction(_4)?function(){
var _5=_4.apply(this,arguments);
_3.call(this,_5);
return _5;
}:function(){
_3.call(this);
};
});
_1.afterThrowing=md(function(_6,_7,_8){
return dojo.isFunction(_8)?function(){
var _9;
try{
_9=_8.apply(this,arguments);
}
catch(e){
_7.call(this,e);
throw e;
}
return _9;
}:_8;
});
_1.after=md(function(_a,_b,_c){
return dojo.isFunction(_c)?function(){
var _d;
try{
_d=_c.apply(this,arguments);
}
finally{
_b.call(this);
}
return _d;
}:function(){
_b.call(this);
};
});
})();
}
