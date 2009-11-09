/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojo._base.declare"]){
dojo._hasResource["dojo._base.declare"]=true;
dojo.provide("dojo._base.declare");
dojo.require("dojo._base.lang");
dojo.require("dojo._base.array");
(function(){
var d=dojo,op=Object.prototype,_1=d.isFunction,_2=d._mixin,_3=new Function,_4=0;
function _5(_6){
throw new Error("declare: "+_6);
};
function _7(_8){
var _9=[],_a=[{cls:0,refs:[]}],_b={},_c=1,l=_8.length,i=0,j,_d,_e,_f,_10,rec,_11,_12;
for(;i<l;++i){
_e=_8[i];
if(!_e){
_5("mixin #"+i+" is null");
}
_d=_e._meta?_e._meta.bases:[_e];
_f=0;
for(j=_d.length-1;j>=0;--j){
_10=_d[j].prototype;
if(!_10.hasOwnProperty("declaredClass")){
_10.declaredClass="uniqName_"+(_4++);
}
_11=_10.declaredClass;
if(!_b.hasOwnProperty(_11)){
_b[_11]={count:0,refs:[],cls:_d[j]};
++_c;
}
rec=_b[_11];
if(_f&&_f!==rec){
rec.refs.push(_f);
++_f.count;
}
_f=rec;
}
++_f.count;
_a[0].refs.push(_f);
}
while(_a.length){
_f=_a.pop();
_9.push(_f.cls);
--_c;
while(_12=_f.refs,_12.length==1){
_f=_12[0];
if(!_f||--_f.count){
_f=0;
break;
}
_9.push(_f.cls);
--_c;
}
if(_f){
for(i=0,l=_12.length;i<l;++i){
_f=_12[i];
if(!--_f.count){
_a.push(_f);
}
}
}
}
if(_c){
_5("can't build consistent linearization");
}
_e=_8[0];
_9[0]=_e?_e._meta&&_e===_9[_9.length-_e._meta.bases.length]?_e._meta.bases.length:1:0;
return _9;
};
function _13(_14,_15,_16){
var _17=_14.constructor._meta,_18=_17.bases,l=_18.length,i,f,opf,_19,_1a,_1b;
_16=_16||_15.nom;
if(!_16){
_5("can't deduce a name to call inherited()");
}
if(_16=="constructor"?_17.chains.constructor!=="manual":_17.chains.hasOwnProperty(_16)){
_5("calling chained method as inherited: "+_16);
}
_19=_14._inherited;
_1a=_18[_19.pos];
_17=_1a&&_1a._meta;
_1b=_1a&&_1a.prototype;
if(!_1a||_19.name!=_16||!(_17?(_17.hidden[_16]===_15||_1b.hasOwnProperty(_16)&&_1b[_16]===_15):(_1b[_16]===_15))){
for(i=0;i<l;++i){
_1a=_18[i];
_17=_1a._meta;
_1b=_1a.prototype;
if(_17?(_17.hidden[_16]===_15||_1b.hasOwnProperty(_16)&&_1b[_16]===_15):(_1b[_16]===_15)){
break;
}
}
_19.name=_16;
_19.pos=i<l?i:-1;
}
i=_19.pos;
opf=op[_16];
while(++i<l){
_1a=_18[i];
_1b=_1a.prototype;
if(_1a._meta){
if(_1b.hasOwnProperty(_16)){
f=_1b[_16];
break;
}
}else{
f=_1b[_16];
if(f&&f!==opf){
break;
}
}
}
_19.pos=i;
return i<l&&f||_16!="constructor"&&opf;
};
function _1c(_1d,a){
var _1e;
if(typeof _1d=="string"){
_1e=_1d;
_1d=a;
}
return _13(this,_1d.callee,_1e);
};
function _1f(_20,a,f){
var _21;
if(typeof _20=="string"){
_21=_20;
_20=a;
a=f;
}
f=_13(this,_20.callee,_21);
return f?f.apply(this,a||_20):undefined;
};
function _22(cls){
var _23=this.constructor._meta.bases;
for(var i=0,l=_23.length;i<l;++i){
if(_23[i]===cls){
return true;
}
}
return this instanceof cls;
};
function _24(_25,_26){
var _27,t,i=0,l=d._extraNames.length;
for(_27 in _26){
t=_26[_27];
if((t!==op[_27]||!(_27 in op))&&_27!="constructor"){
if(_1(t)){
t.nom=_27;
}
_25[_27]=t;
}
}
for(;i<l;++i){
_27=d._extraNames[i];
t=_26[_27];
if((t!==op[_27]||!(_27 in op))&&_27!="constructor"){
if(_1(t)){
t.nom=_27;
}
_25[_27]=t;
}
}
};
function _28(_29){
_24(this.prototype,_29);
};
function _2a(_2b,_2c){
return function(){
var a=arguments,_2d=a,a0=a[0],f,i,m,h,l=_2b.length,_2e;
this._inherited={};
if(_2c&&(a0&&a0.preamble||this.preamble)){
_2e=new Array(_2b.length);
_2e[0]=a;
for(i=0;;){
a0=a[0];
if(a0){
f=a0.preamble;
if(f){
a=f.apply(this,a)||a;
}
}
f=_2b[i].prototype;
f=f.hasOwnProperty("preamble")&&f.preamble;
if(f){
a=f.apply(this,a)||a;
}
if(++i==l){
break;
}
_2e[i]=a;
}
}
for(i=l-1;i>=0;--i){
f=_2b[i];
m=f._meta;
if(m){
h=m.hidden;
f=h.hasOwnProperty("constructor")&&h.constructor;
}
if(f){
f.apply(this,_2e?_2e[i]:a);
}
}
f=this.postscript;
if(f){
f.apply(this,_2d);
}
};
};
function _2f(_30){
return function(){
var a=arguments,f,i=0,l=_30.length;
this._inherited={};
for(;i<l;++i){
f=_30[i];
m=f._meta;
if(m){
h=m.hidden;
f=h.hasOwnProperty("constructor")&&h.constructor;
}
if(f){
f.apply(this,a);
break;
}
}
f=this.postscript;
if(f){
f.apply(this,a);
}
};
};
function _31(_32,_33,_34){
return function(){
var b,m,h,f,i=0,l=_33.length,_35=1;
if(_34){
i=l-1;
_35=l=-1;
}
for(;i!=l;i+=_35){
f=0;
b=_33[i];
m=b._meta;
if(m){
h=m.hidden;
f=h.hasOwnProperty(_32)&&h[_32];
}else{
f=b.prototype[_32];
}
if(f){
f.apply(this,arguments);
}
}
};
};
d.declare=function(_36,_37,_38){
var _39,i,t,_3a,_3b,_3c,_3d=1,_3e={};
if(typeof _36!="string"){
_38=_37;
_37=_36;
_36="";
}
_38=_38||{};
if(d.isArray(_37)){
_3c=_7(_37);
t=_3c[0];
_3d=_3c.length-t;
_37=_3c[_3d];
}else{
_3c=[0];
if(_37){
t=_37._meta;
_3c=_3c.concat(t?t.bases:_37);
}
}
if(_37){
if(_37._meta){
_3.prototype=_37._meta.chains;
_3e=new _3;
}
for(i=_3d-1;;--i){
_3.prototype=_37.prototype;
_39=new _3;
if(!i){
break;
}
t=_3c[i];
if(t._meta){
_2(_3e,t._meta.chains);
_2(_39,t._meta.hidden);
}else{
_2(_39,t.prototype);
}
_3a=new Function;
_3a.superclass=_37;
_3a.prototype=_39;
_37=_39.constructor=_3a;
}
}else{
_39={};
}
_24(_39,_38);
t=_38.constructor;
if(t!==op.constructor){
t.nom="constructor";
_39.constructor=t;
}
_3.prototype=0;
if(_39.hasOwnProperty("-chains-")){
_2(_3e,_39["-chains-"]);
}
_3c[0]=_3a=_3e.constructor==="manual"?_2f(_3c):_2a(_3c,!_3e.hasOwnProperty("constructor"));
_3a._meta={bases:_3c,hidden:_38,chains:_3e};
_3a.superclass=_37&&_37.prototype;
_3a.extend=_28;
_3a.prototype=_39;
_39.constructor=_3a;
_39.getInherited=_1c;
_39.inherited=_1f;
_39.isInstanceOf=_22;
if(_36){
_39.declaredClass=_36;
d.setObject(_36,_3a);
}
for(_3b in _3e){
if(_39[_3b]&&typeof _3e[_3b]=="string"&&_3b!="constructor"){
t=_39[_3b]=_31(_3b,_3c,_3e[_3b]==="after");
t.nom=_3b;
}
}
return _3a;
};
d.safeMixin=_24;
})();
}
