/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.html.format"]){
dojo._hasResource["dojox.html.format"]=true;
dojo.provide("dojox.html.format");
dojo.require("dojox.html.entities");
(function(){
dojox.html.format.prettyPrint=function(_1,_2,_3,_4,_5){
var _6=[];
var _7=0;
var _8=[];
var _9="\t";
var _a="";
var _b=[];
var i;
var _c=/[=]([^"']+?)(\s|>)/g;
var _d=/style=("[^"]*"|'[^']*'|\S*)/gi;
var _e=/\s\w+=("[^"]*"|'[^']*'|\S*)/gi;
if(_2&&_2>0&&_2<10){
_9="";
for(i=0;i<_2;i++){
_9+=" ";
}
}
var _f=dojo.doc.createElement("div");
_f.innerHTML=_1;
var _10=dojox.html.entities.encode;
var _11=dojox.html.entities.decode;
var _12=function(tag){
switch(tag){
case "a":
case "b":
case "strong":
case "s":
case "strike":
case "i":
case "u":
case "em":
case "sup":
case "sub":
case "span":
case "font":
case "big":
case "cite":
case "q":
case "small":
return true;
default:
return false;
}
};
var div=_f.ownerDocument.createElement("div");
var _13=function(_14){
var _15=_14.cloneNode(false);
div.appendChild(_15);
var _16=div.innerHTML;
div.innerHTML="";
return _16;
};
var _17=function(){
var i;
for(i=0;i<_7;i++){
_6.push(_9);
}
};
var _18=function(){
_6.push("\n");
};
var _19=function(n){
_a+=_10(n.nodeValue,_4);
};
var _1a=function(txt){
var i;
var _1b;
var _1c=txt.split("\n");
for(i=0;i<_1c.length;i++){
_1c[i]=dojo.trim(_1c[i]);
}
txt=_1c.join(" ");
txt=dojo.trim(txt);
if(txt!==""){
var _1d=[];
if(_3&&_3>0){
while(txt){
if(txt.length>_3){
for(i=_3;(i<txt.length&&txt.charAt(i)!==" ");i++){
}
var _1e=txt.substring(0,i);
_1e=dojo.trim(_1e);
txt=dojo.trim(txt.substring((i==txt.length)?txt.length:i+1,txt.length));
if(_1e){
_1b="";
for(i=0;i<_7;i++){
_1b+=_9;
}
_1e=_1b+_1e+"\n";
}
_1d.push(_1e);
}else{
_1b="";
for(i=0;i<_7;i++){
_1b+=_9;
}
txt=_1b+txt+"\n";
_1d.push(txt);
txt=null;
}
}
return _1d.join("");
}else{
_1b="";
for(i=0;i<_7;i++){
_1b+=_9;
}
txt=_1b+txt+"\n";
return txt;
}
}else{
return "";
}
};
var _1f=function(txt){
if(txt){
txt=txt.replace(/&quot;/gi,"\"");
txt=txt.replace(/&gt;/gi,">");
txt=txt.replace(/&lt;/gi,"<");
txt=txt.replace(/&amp;/gi,"&");
}
return txt;
};
var _20=function(txt){
if(txt){
txt=_1f(txt);
var i,t,c,_21;
var _22=0;
var _23=txt.split("\n");
var _24=[];
for(i=0;i<_23.length;i++){
var _25=_23[i];
var _26=(_25.indexOf("\n")>-1);
_25=dojo.trim(_25);
if(_25){
var _27=_22;
for(c=0;c<_25.length;c++){
var ch=_25.charAt(c);
if(ch==="{"){
_22++;
}else{
if(ch==="}"){
_22--;
_27=_22;
}
}
}
_21="";
for(t=0;t<_7+_27;t++){
_21+=_9;
}
_24.push(_21+_25+"\n");
}else{
if(_26&&i===0){
_24.push("\n");
}
}
}
txt=_24.join("");
}
return txt;
};
var _28=function(_29){
var _2a=_29.nodeName.toLowerCase();
var _2b=dojo.trim(_13(_29));
var tag=_2b.substring(0,_2b.indexOf(">")+1);
tag=tag.replace(_c,"=\"$1\"$2");
tag=tag.replace(_d,function(_2c){
var sL=_2c.substring(0,6);
var _2d=_2c.substring(6,_2c.length);
var _2e=_2d.charAt(0);
_2d=dojo.trim(_2d.substring(1,_2d.length-1));
_2d=_2d.split(";");
var _2f=[];
dojo.forEach(_2d,function(s){
s=dojo.trim(s);
if(s){
s=s.substring(0,s.indexOf(":")).toLowerCase()+s.substring(s.indexOf(":"),s.length);
_2f.push(s);
}
});
_2f=_2f.sort();
_2d=_2f.join("; ");
var ts=dojo.trim(_2d);
if(!ts||ts===";"){
return "";
}else{
_2d+=";";
return sL+_2e+_2d+_2e;
}
});
var _30=[];
tag=tag.replace(_e,function(_31){
_30.push(dojo.trim(_31));
return "";
});
_30=_30.sort();
tag="<"+_2a;
if(_30.length){
tag+=" "+_30.join(" ");
}
if(_2b.indexOf("</")!=-1){
_8.push(_2a);
tag+=">";
}else{
if(_5){
tag+=" />";
}else{
tag+=">";
}
_8.push(false);
}
var _32=_12(_2a);
_b.push(_32);
if(_a&&!_32){
_6.push(_1a(_a));
_a="";
}
if(!_32){
_17();
_6.push(tag);
_18();
_7++;
}else{
_a+=tag;
}
};
var _33=function(){
var _34=_b.pop();
if(_a&&!_34){
_6.push(_1a(_a));
_a="";
}
var ct=_8.pop();
if(ct){
ct="</"+ct+">";
if(!_34){
_7--;
_17();
_6.push(ct);
_18();
}else{
_a+=ct;
}
}else{
_7--;
}
};
var _35=function(n){
var _36=_11(n.nodeValue,_4);
_17();
_6.push("<!--");
_18();
_7++;
_6.push(_1a(_36));
_7--;
_17();
_6.push("-->");
_18();
};
var _37=function(_38){
var _39=_38.childNodes;
if(_39){
var i;
for(i=0;i<_39.length;i++){
var n=_39[i];
if(n.nodeType===1){
var tg=dojo.trim(n.tagName.toLowerCase());
if(dojo.isIE&&n.parentNode!=_38){
continue;
}
if(tg&&tg.charAt(0)==="/"){
continue;
}else{
_28(n);
if(tg==="script"){
_6.push(_20(n.innerHTML));
}else{
if(tg==="pre"){
var _3a=n.innerHTML;
if(dojo.isMoz){
_3a=_3a.replace("<br>","\n");
_3a=_3a.replace("<pre>","");
_3a=_3a.replace("</pre>","");
}
if(_3a.charAt(_3a.length-1)!=="\n"){
_3a+="\n";
}
_6.push(_3a);
}else{
_37(n);
}
}
_33();
}
}else{
if(n.nodeType===3||n.nodeType===4){
_19(n);
}else{
if(n.nodeType===8){
_35(n);
}
}
}
}
}
};
_37(_f);
return _6.join("");
};
})();
}
