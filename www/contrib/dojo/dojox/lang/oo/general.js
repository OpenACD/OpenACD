/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.lang.oo.general"]){
dojo._hasResource["dojox.lang.oo.general"]=true;
dojo.provide("dojox.lang.oo.general");
dojo.require("dojox.lang.oo.Decorator");
(function(){
var oo=dojox.lang.oo,md=oo.makeDecorator,_1=oo.general;
_1.augment=md(function(_2,_3,_4){
return typeof _4=="undefined"?_3:_4;
});
_1.override=md(function(_5,_6,_7){
return typeof _7!="undefined"?_6:_7;
});
_1.shuffle=md(function(_8,_9,_a){
return dojo.isFunction(_a)?function(){
return _a.apply(this,_9.apply(this,arguments));
}:_a;
});
_1.wrap=md(function(_b,_c,_d){
return function(){
return _c.call(this,_d,arguments);
};
});
_1.tap=md(function(_e,_f,_10){
return function(){
_f.apply(this,arguments);
return this;
};
});
_1.before=md(function(_11,_12,_13){
return dojo.isFunction(_13)?function(){
_12.apply(this,arguments);
return _13.apply(this,arguments);
}:_12;
});
_1.after=md(function(_14,_15,_16){
return dojo.isFunction(_16)?function(){
_16.apply(this,arguments);
return _15.apply(this,arguments);
}:_15;
});
})();
}
