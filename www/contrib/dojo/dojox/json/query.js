/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.json.query"]){
dojo._hasResource["dojox.json.query"]=true;
dojo.provide("dojox.json.query");
(function(){
function _1(_2,_3,_4,_5){
var _6=_2.length,_7=[];
_4=_4||_6;
_3=(_3<0)?Math.max(0,_3+_6):Math.min(_6,_3);
_4=(_4<0)?Math.max(0,_4+_6):Math.min(_6,_4);
for(var i=_3;i<_4;i+=_5){
_7.push(_2[i]);
}
return _7;
};
function _8(_9,_a){
var _b=[];
function _c(_d){
if(_a){
if(_a===true&&!(_d instanceof Array)){
_b.push(_d);
}else{
if(_d[_a]){
_b.push(_d[_a]);
}
}
}
for(var i in _d){
var _e=_d[i];
if(!_a){
_b.push(_e);
}else{
if(_e&&typeof _e=="object"){
_c(_e);
}
}
}
};
if(_a instanceof Array){
if(_a.length==1){
return _9[_a[0]];
}
for(var i=0;i<_a.length;i++){
_b.push(_9[_a[i]]);
}
}else{
_c(_9);
}
return _b;
};
function _f(_10,_11){
var _12=[];
var _13={};
for(var i=0,l=_10.length;i<l;++i){
var _14=_10[i];
if(_11(_14,i,_10)){
if((typeof _14=="object")&&_14){
if(!_14.__included){
_14.__included=true;
_12.push(_14);
}
}else{
if(!_13[_14+typeof _14]){
_13[_14+typeof _14]=true;
_12.push(_14);
}
}
}
}
for(i=0,l=_12.length;i<l;++i){
if(_12[i]){
delete _12[i].__included;
}
}
return _12;
};
dojox.json.query=function(_15,obj){
var _16=0;
var str=[];
_15=_15.replace(/"(\\.|[^"\\])*"|'(\\.|[^'\\])*'|[\[\]]/g,function(t){
_16+=t=="["?1:t=="]"?-1:0;
return (t=="]"&&_16>0)?"`]":(t.charAt(0)=="\""||t.charAt(0)=="'")?"`"+(str.push(t)-1):t;
});
var _17="";
function _18(_19){
_17=_19+"("+_17;
};
function _1a(t,a,b,c,d,e,f,g){
return str[g].match(/[\*\?]/)||f=="~"?"/^"+str[g].substring(1,str[g].length-1).replace(/\\([btnfr\\"'])|([^\w\*\?])/g,"\\$1$2").replace(/([\*\?])/g,"[\\w\\W]$1")+(f=="~"?"$/i":"$/")+".test("+a+")":t;
};
_15.replace(/(\]|\)|push|pop|shift|splice|sort|reverse)\s*\(/,function(){
throw new Error("Unsafe function call");
});
_15=_15.replace(/([^=]=)([^=])/g,"$1=$2").replace(/@|(\.\s*)?[a-zA-Z\$_]+(\s*:)?/g,function(t){
return t.charAt(0)=="."?t:t=="@"?"$obj":(t.match(/:|^(\$|Math|true|false|null)$/)?"":"$obj.")+t;
}).replace(/\.?\.?\[(`\]|[^\]])*\]|\?.*|\.\.([\w\$_]+)|\.\*/g,function(t,a,b){
var _1b=t.match(/^\.?\.?(\[\s*\^?\?|\^?\?|\[\s*==)(.*?)\]?$/);
if(_1b){
var _1c="";
if(t.match(/^\./)){
_18("expand");
_1c=",true)";
}
_18(_1b[1].match(/\=/)?"dojo.map":_1b[1].match(/\^/)?"distinctFilter":"dojo.filter");
return _1c+",function($obj){return "+_1b[2]+"})";
}
_1b=t.match(/^\[\s*([\/\\].*)\]/);
if(_1b){
return ".concat().sort(function(a,b){"+_1b[1].replace(/\s*,?\s*([\/\\])\s*([^,\\\/]+)/g,function(t,a,b){
return "var av= "+b.replace(/\$obj/,"a")+",bv= "+b.replace(/\$obj/,"b")+";if(av>bv||bv==null){return "+(a=="/"?1:-1)+";}\n"+"if(bv>av||av==null){return "+(a=="/"?-1:1)+";}\n";
})+"return 0;})";
}
_1b=t.match(/^\[(-?[0-9]*):(-?[0-9]*):?(-?[0-9]*)\]/);
if(_1b){
_18("slice");
return ","+(_1b[1]||0)+","+(_1b[2]||0)+","+(_1b[3]||1)+")";
}
if(t.match(/^\.\.|\.\*|\[\s*\*\s*\]|,/)){
_18("expand");
return (t.charAt(1)=="."?",'"+b+"'":t.match(/,/)?","+t:"")+")";
}
return t;
}).replace(/(\$obj\s*((\.\s*[\w_$]+\s*)|(\[\s*`([0-9]+)\s*`\]))*)(==|~)\s*`([0-9]+)/g,_1a).replace(/`([0-9]+)\s*(==|~)\s*(\$obj\s*((\.\s*[\w_$]+)|(\[\s*`([0-9]+)\s*`\]))*)/g,function(t,a,b,c,d,e,f,g){
return _1a(t,c,d,e,f,g,b,a);
});
_15=_17+(_15.charAt(0)=="$"?"":"$")+_15.replace(/`([0-9]+|\])/g,function(t,a){
return a=="]"?"]":str[a];
});
var _1d=eval("1&&function($,$1,$2,$3,$4,$5,$6,$7,$8,$9){var $obj=$;return "+_15+"}");
for(var i=0;i<arguments.length-1;i++){
arguments[i]=arguments[i+1];
}
return obj?_1d.apply(this,arguments):_1d;
};
})();
}
