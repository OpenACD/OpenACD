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
var d=dojo,oo=dojox.lang.oo,op=Object.prototype,_1="constructor",_2="hasOwnProperty",_3="declaredClass",_4=d.isFunction,_5=d.forEach,_6=function(){
},_7=0;
function _8(_9){
throw new Error("declare: "+_9);
};
function _a(_b){
var _c=[0],l=_b.length,_d=new Array(l),i=0,j,m,m2,c,_e,_f,_10,_11,t;
for(;i<l;++i){
c=_b[i];
if(!c){
_8("mixin #"+i+" is null");
}
_f=c._meta&&c._meta.bases||[c];
m={};
for(j=0,m2=_f.length;j<m2;++j){
_10=(_e=_f[j]).prototype;
_11=_10[_2](_3)&&_10[_3];
if(!_11){
_11=_10[_3]="dojoUniqClassName_"+(_7++);
}
m[_11]=_e;
}
_d[i]={idx:0,map:m,lin:d.map(_f,function(c){
return c.prototype[_3];
})};
}
while(l){
if(l==1){
c=_d[0];
m=c.map;
return _c.concat(d.map(c.lin.slice(c.idx),function(c){
return m[c];
}));
}
for(i=l;i--;){
m=_d[i];
c=m.lin[m.idx];
if(c){
t=1;
for(j=l;j--;){
m2=_d[j];
if(i!=j&&(c in m2.map)){
if(c==m2.lin[m2.idx]){
++t;
}else{
break;
}
}
}
if(j<0){
_c.push(m.map[c]);
for(j=l;j--;){
m=_d[j];
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
_d.splice(i,1);
--l;
}
}
if(i<0&&l>0){
_8("can't build consistent linearization");
}
}
return _c;
};
oo.makeDeclare=function(_12,_13){
_13=_13||{};
function _14(_15,_16){
var _17=[],i=0,l=_15.length,h,b;
for(;i<l;++i){
b=_15[i];
if(h=b._meta){
(h=h.hidden)[_2](_16)&&_17.push(h[_16]);
}else{
(_16==_1)&&_17.push(b)||(h=b.prototype[_16])&&h!==op[_16]&&_17.push(h);
}
}
_16!=_1&&(h=op[_16])&&_17.push(h);
return _13[_16]==="after"?_17.reverse():_17;
};
function _18(_19,a,b){
var c=this.constructor,m=c._meta,_1a=c._cache,_1b,i,l,f,n,ch,x,_1c;
if(typeof _19=="string"){
_1c=_19;
_19=a;
a=b;
}
_1b=_18.caller;
n=_1b.nom;
if(n&&_1c&&n!==_1c){
_8("calling inherited() with a different name: "+_1c);
}
_1c=_1c||n;
ch=_1a[_2](_1c)&&_1a[_1c];
if(!ch){
if(!_1c){
_8("can't deduce a name to call inherited()");
}
if(_1c==_1&&_12){
_8("calling constructor as inherited");
}
if(_13[_2](_1c)){
_8("calling chained method as inherited: "+_1c);
}
ch=_1a[_1c]=_14(m.bases,_1c);
}
if((x=this._inherited).name!==_1c||ch[x.pos]!==_1b){
for(i=0,l=ch.length;i<l&&ch[i]!==_1b;++i){
}
if(i==l){
this[_1c]===_1b&&(i=-1)||_8("can't find the caller for inherited()");
}
this._inherited=x={name:_1c,pos:i};
}
f=ch[++x.pos];
return f?f.apply(this,a||_19):undefined;
};
return function(_1d,_1e,_1f){
var _20,_21,i,l,t,_22,_23,_24,_25;
if(typeof _1d!="string"){
_1f=_1e;
_1e=_1d;
_1d="";
}
_1f=_1f||{};
t=0;
if(d.isArray(_1e)){
if(_1e.length>1){
_25=_a(_1e);
_1e=_25[l=_25.length-1];
for(i=l-1;;){
t=_25[i--];
_6.prototype=_1e.prototype;
_21=new _6;
if(!t){
break;
}
d._mixin(_21,t.prototype);
(_22=function(){
}).superclass=_1e;
_22.prototype=_21;
_1e=_21.constructor=_22;
}
t=1;
}else{
_1e=_1e[0];
}
}
if(!t){
_25=[0];
if(_1e){
(t=_1e._meta)&&(_25=_25.concat(t.bases))||_25.push(_1e);
_6.prototype=_1e.prototype;
_21=new _6;
}else{
_21={};
}
}
_6.prototype=0;
for(_24 in _1f){
(t=_1f[_24])!==op[_24]&&_4(t)&&(t.nom=_24);
}
_5(d._extraNames,function(_26,t){
(t=_1f[_26])!==op[_26]&&_4(t)&&(t.nom=_26);
});
d._mixin(_21,_1f);
if(_12){
_22=function(){
var a=arguments,_27=a,a0,f,i,l,h,_28;
this._inherited={};
if((a0=a[0])&&a0.preamble||this.preamble){
_28=new Array(_25.length);
_28[0]=a;
for(i=0,l=_25.length;;){
a=(a0=a[0])&&(f=a0.preamble)&&f.apply(this,a)||a;
a=(f=_25[i]._meta.hidden.preamble)&&f.apply(this,a)||a;
if(++i==l){
break;
}
_28[i]=a;
}
for(--i;i>=0;--i){
h=_25[i]._meta.hidden;
h[_2](_1)&&h.constructor.apply(this,_28[i]);
}
}else{
for(i=_23.length-1;i>=0;_23[i--].apply(this,a)){
}
}
(f=this.postscript)&&f.apply(this,_27);
};
}else{
if(_13[_2](_1)){
_22=function(){
var a=arguments,f,i,l;
this._inherited={};
for(i=0,l=_23.length;i<l;_23[i++].apply(this,a)){
}
(f=this.postscript)&&f.apply(this,a);
};
}else{
_22=function(){
var a=arguments,f;
this._inherited={};
(f=_23[0])&&f.apply(this,a);
(f=this.postscript)&&f.apply(this,a);
};
}
}
_25[0]=_22;
_22._meta={bases:_25,hidden:_1f};
_22._cache={};
_22.superclass=_1e&&_1e.prototype;
_21.constructor=_22;
_22.prototype=_21;
_21.inherited=_18;
_21.isInstanceOf=function(cls){
for(var i=0,l=_25.length;i<l;++i){
if(_25[i]===cls){
return true;
}
}
return this instanceof cls;
};
_1d&&d.setObject(_21[_3]=_1d,_22);
function _29(_2a){
if(typeof _13[_2a]=="string"){
(_21[_2a]=function(){
var t=_14(_25,_2a),l=t.length,f=function(){
for(var i=0;i<l;++i){
t[i].apply(this,arguments);
}
};
f.nom=_2a;
(_22.prototype[_2a]=f).apply(this,arguments);
}).nom=_2a;
}
};
for(_24 in _13){
_29(_24);
}
_23=_14(_25,_1);
return _22;
};
};
oo.declare=oo.makeDeclare(true);
})();
}
