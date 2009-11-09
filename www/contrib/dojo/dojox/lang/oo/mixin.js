/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.lang.oo.mixin"]){
dojo._hasResource["dojox.lang.oo.mixin"]=true;
dojo.provide("dojox.lang.oo.mixin");
dojo.experimental("dojox.lang.oo.mixin");
dojo.require("dojox.lang.oo.Filter");
dojo.require("dojox.lang.oo.Decorator");
(function(){
var oo=dojox.lang.oo,_1=oo.Filter,_2=oo.Decorator,_3={},_4=function(_5){
return _5;
},_6=function(_7,_8,_9){
return _8;
},_a=function(_b,_c,_d,_e){
_b[_c]=_d;
},_f={},_10=oo.applyDecorator=function(_11,_12,_13,_14){
if(_13 instanceof _2){
var d=_13.decorator;
_13=_10(_11,_12,_13.value,_14);
return d(_12,_13,_14);
}
return _11(_12,_13,_14);
};
oo.__mixin=function(_15,_16,_17,_18,_19){
var _1a,_1b,_1c,_1d,_1e;
for(_1a in _16){
if(!(_1a in _3)){
_1c=_16[_1a];
_1b=_18(_1a,_15,_16,_1c);
if(_1b){
_1e=_15[_1b];
_1d=_10(_17,_1b,_1c,_1e);
if(_1e!==_1d){
_19(_15,_1b,_1d,_1e);
}
}
}
}
return _15;
};
oo.mixin=function(_1f,_20){
var _21,_22,i=1,l=arguments.length;
for(;i<l;++i){
_20=arguments[i];
if(_20 instanceof _1){
_22=_20.filter;
_20=_20.bag;
}else{
_22=_4;
}
if(_20 instanceof _2){
_21=_20.decorator;
_20=_20.value;
}else{
_21=_6;
}
oo.__mixin(_1f,_20,_21,_22,_a);
}
return _1f;
};
})();
}
