/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.lang.oo.declare"]){
dojo._hasResource["dojox.lang.oo.declare"]=true;
dojo.provide("dojox.lang.oo.declare");
dojo.experimental("dojox.lang.oo.declare");
(function(){
var d=dojo,oo=dojox.lang.oo,op=Object.prototype,_1=d.isFunction,_2=d.forEach,_3=function(){
},_4=0;
function _5(_6){
throw new Error("declare: "+_6);
};
function _7(_8){
var _9=[0],l=_8.length,_a=new Array(l),i=0,j,m,m2,c,_b,_c,_d,_e,t;
for(;i<l;++i){
c=_8[i];
if(!c){
_5("mixin #"+i+" is null");
}
_c=c._meta&&c._meta.bases||[c];
m={};
for(j=0,m2=_c.length;j<m2;++j){
_b=_c[j];
_d=_b.prototype;
_e=_d.hasOwnProperty("declaredClass")&&_d.declaredClass;
if(!_e){
_e=_d.declaredClass="dojoUniqClassName_"+(_4++);
}
m[_e]=_b;
}
_a[i]={idx:0,map:m,lin:d.map(_c,function(c){
return c.prototype.declaredClass;
})};
}
while(l){
if(l==1){
c=_a[0];
m=c.map;
return _9.concat(d.map(c.lin.slice(c.idx),function(c){
return m[c];
}));
}
for(i=l;i--;){
m=_a[i];
c=m.lin[m.idx];
if(c){
t=1;
for(j=l;j--;){
m2=_a[j];
if(i!=j&&(c in m2.map)){
if(c==m2.lin[m2.idx]){
++t;
}else{
break;
}
}
}
if(j<0){
_9.push(m.map[c]);
for(j=l;j--;){
m=_a[j];
if(c==m.lin[m.idx]){
++m.idx;
if(!--t){
break;
}
}
}
break;
}
}else{
_a.splice(i,1);
--l;
}
}
if(i<0&&l>0){
_5("can't build consistent linearization");
}
}
return _9;
};
oo.makeDeclare=function(_f,_10){
_10=_10||{};
function _11(_12,_13){
var _14=[],i=0,l=_12.length,h,b;
for(;i<l;++i){
b=_12[i];
h=b._meta;
if(h){
h=h.hidden;
if(h.hasOwnProperty(_13)){
_14.push(h[_13]);
}
}else{
if(_13=="constructor"){
_14.push(b);
}else{
h=b.prototype[_13];
if(h&&h!==op[_13]){
_14.push(h);
}
}
}
}
if(_13!="constructor"){
h=op[_13];
if(h){
_14.push(h);
}
}
return _10[_13]==="after"?_14.reverse():_14;
};
function _15(_16,a,b){
var c=this.constructor,m=c._meta,_17=c._cache,_18,i,l,f,n,ch,x,_19;
if(typeof _16=="string"){
_19=_16;
_16=a;
a=b;
}
_18=_15.caller;
n=_18.nom;
if(n&&_19&&n!==_19){
_5("calling inherited() with a different name: "+_19);
}
_19=_19||n;
ch=_17.hasOwnProperty(_19)&&_17[_19];
if(!ch){
if(!_19){
_5("can't deduce a name to call inherited()");
}
if(_19=="constructor"&&_f){
_5("calling constructor as inherited");
}
if(_10.hasOwnProperty(_19)){
_5("calling chained method as inherited: "+_19);
}
ch=_17[_19]=_11(m.bases,_19);
}
x=this._inherited;
if(x.name!==_19||ch[x.pos]!==_18){
for(i=0,l=ch.length;i<l&&ch[i]!==_18;++i){
}
if(i==l){
if(this[_19]===_18){
i=-1;
}else{
_5("can't find the caller for inherited()");
}
}
this._inherited=x={name:_19,pos:i};
}
f=ch[++x.pos];
return f?f.apply(this,a||_16):undefined;
};
return function(_1a,_1b,_1c){
var _1d,_1e,i,l,t,_1f,_20,_21,_22;
if(typeof _1a!="string"){
_1c=_1b;
_1b=_1a;
_1a="";
}
_1c=_1c||{};
t=0;
if(d.isArray(_1b)){
if(_1b.length>1){
_22=_7(_1b);
l=_22.length-1;
_1b=_22[l];
for(i=l-1;;){
t=_22[i--];
_3.prototype=_1b.prototype;
_1e=new _3;
if(!t){
break;
}
d._mixin(_1e,t.prototype);
_1f=function(){
};
_1f.superclass=_1b;
_1f.prototype=_1e;
_1b=_1e.constructor=_1f;
}
t=1;
}else{
_1b=_1b[0];
}
}
if(!t){
_22=[0];
if(_1b){
t=_1b._meta;
if(t){
_22=_22.concat(t.bases);
}else{
_22.push(_1b);
}
_3.prototype=_1b.prototype;
_1e=new _3;
}else{
_1e={};
}
}
_3.prototype=0;
for(_21 in _1c){
t=_1c[_21];
if(t!==op[_21]&&_1(t)){
t.nom=_21;
}
}
_2(d._extraNames,function(_23,t){
t=_1c[_23];
if(t!==op[_23]&&_1(t)){
t.nom=_23;
}
});
d._mixin(_1e,_1c);
if(_f){
_1f=function(){
var a=arguments,_24=a,a0=a[0],f,i,l,h,_25;
this._inherited={};
if(a0&&a0.preamble||this.preamble){
_25=new Array(_22.length);
_25[0]=a;
for(i=0,l=_22.length;;){
a0=a[0];
if(a0){
f=a0.preamble;
if(f){
a=f.apply(this,a)||a;
}
}
f=_22[i]._meta.hidden.preamble;
if(f){
a=f.apply(this,a)||a;
}
if(++i==l){
break;
}
_25[i]=a;
}
for(--i;i>=0;--i){
h=_22[i]._meta.hidden;
if(h.hasOwnProperty("constructor")){
h.constructor.apply(this,_25[i]);
}
}
}else{
for(i=_20.length-1;i>=0;--i){
_20[i].apply(this,a);
}
}
f=this.postscript;
if(f){
f.apply(this,_24);
}
};
}else{
if(_10.hasOwnProperty("constructor")){
_1f=function(){
var a=arguments,f,i,l;
this._inherited={};
for(i=0,l=_20.length;i<l;++i){
_20[i].apply(this,a);
}
f=this.postscript;
if(f){
f.apply(this,args);
}
};
}else{
_1f=function(){
var a=arguments,f;
this._inherited={};
f=_20[0];
if(f){
f.apply(this,a);
}
f=this.postscript;
if(f){
f.apply(this,args);
}
};
}
}
_22[0]=_1f;
_1f._meta={bases:_22,hidden:_1c};
_1f._cache={};
_1f.superclass=_1b&&_1b.prototype;
_1e.constructor=_1f;
_1f.prototype=_1e;
_1e.inherited=_15;
_1e.isInstanceOf=function(cls){
for(var i=0,l=_22.length;i<l;++i){
if(_22[i]===cls){
return true;
}
}
return this instanceof cls;
};
if(_1a){
_1e.declaredClass=_1a;
d.setObject(_1a,_1f);
}
function _26(_27){
if(typeof _10[_27]=="string"){
var f=_1e[_27]=function(){
var t=_11(_22,_27),l=t.length,f=function(){
for(var i=0;i<l;++i){
t[i].apply(this,arguments);
}
};
f.nom=_27;
_1f.prototype[_27]=f;
f.apply(this,arguments);
};
}
f.nom=_27;
};
for(_21 in _10){
_26(_21);
}
_20=_11(_22,"constructor");
if(!_f&&!_10.hasOwnProperty(_21)){
_1f._cache.constructor=_20;
}
return _1f;
};
};
oo.declare=oo.makeDeclare(true);
})();
}
