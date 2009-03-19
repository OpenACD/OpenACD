/*
	Copyright (c) 2004-2008, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.io.windowName"]){
dojo._hasResource["dojox.io.windowName"]=true;
dojo.provide("dojox.io.windowName");
dojox.io.windowName={send:function(_1,_2){
_2.url+=(_2.url.match(/\?/)?"&":"?")+"windowname="+(_2.authElement?"auth":true);
var _3=_2.authElement;
var _4=function(_5){
try{
var _6=_7.ioArgs.frame.contentWindow.document;
_6.write(" ");
_6.close();
}
catch(e){
}
(_3||dojo.body()).removeChild(_7.ioArgs.outerFrame);
return _5;
};
var _7=dojo._ioSetArgs(_2,_4,_4,_4);
if(_2.timeout){
setTimeout(function(){
if(_7.fired==-1){
_7.callback(new Error("Timeout"));
}
},_2.timeout);
}
var _8=dojox.io.windowName;
if(dojo.body()){
_8._send(_7,_1,_3,_2.onAuthLoad);
}else{
dojo.addOnLoad(function(){
_8._send(_7,_1,_3,_2.onAuthLoad);
});
}
return _7;
},_send:function(_9,_a,_b,_c){
var _d=_9.ioArgs;
var _e=dojox.io.windowName._frameNum++;
var _f=(dojo.config["dojoCallbackUrl"]||dojo.moduleUrl("dojo","resources/blank.html"))+"#"+_e;
var _10=new dojo._Url(window.location,_f);
var doc=dojo.doc;
var _12=_b||dojo.body();
function styleFrame(_13){
_13.style.width="100%";
_13.style.height="100%";
_13.style.border="0px";
};
if(dojo.isMoz&&![].reduce){
var _14=doc.createElement("iframe");
styleFrame(_14);
if(!_b){
_14.style.display="none";
}
_12.appendChild(_14);
var _15=_14.contentWindow;
doc=_15.document;
doc.write("<html><body margin='0px'><iframe style='width:100%;height:100%;border:0px' name='protectedFrame'></iframe></body></html>");
doc.close();
var _16=_15[0];
_15.__defineGetter__(0,function(){
});
_15.__defineGetter__("protectedFrame",function(){
});
doc=_16.document;
doc.write("<html><body margin='0px'></body></html>");
doc.close();
_12=doc.body;
}
var _17=_d.frame=_17=doc.createElement(dojo.isIE?"<iframe name=\""+_10+"\" onload=\"dojox.io.windowName["+_e+"]()\">":"iframe");
styleFrame(_17);
_d.outerFrame=_14=_14||_17;
if(!_b){
_14.style.display="none";
}
var _18=0;
function getData(){
var _19=_17.contentWindow.name;
if(typeof _19=="string"){
if(_19!=_10){
_18=2;
_9.ioArgs.hash=_17.contentWindow.location.hash;
_9.callback(_19);
}
}
};
dojox.io.windowName[_e]=_17.onload=function(){
try{
if(!dojo.isMoz&&_17.contentWindow.location=="about:blank"){
return;
}
}
catch(e){
}
if(!_18){
_18=1;
if(_b){
if(_c){
_c();
}
}else{
_17.contentWindow.location=_f;
}
}
try{
if(_18<2){
getData();
}
}
catch(e){
}
};
_17.name=_10;
if(_a.match(/GET/i)){
dojo._ioAddQueryToUrl(_d);
_17.src=_d.url;
_12.appendChild(_17);
if(_17.contentWindow){
_17.contentWindow.location.replace(_d.url);
}
}else{
if(_a.match(/POST/i)){
_12.appendChild(_17);
var _1a=dojo.doc.createElement("form");
dojo.body().appendChild(_1a);
var _1b=dojo.queryToObject(_d.query);
for(var i in _1b){
var _1d=_1b[i];
_1d=_1d instanceof Array?_1d:[_1d];
for(j=0;j<_1d.length;j++){
var _1e=doc.createElement("input");
_1e.type="hidden";
_1e.name=i;
_1e.value=_1d[j];
_1a.appendChild(_1e);
}
}
_1a.method="POST";
_1a.action=_d.url;
_1a.target=_10;
_1a.submit();
_1a.parentNode.removeChild(_1a);
}else{
throw new Error("Method "+_a+" not supported with the windowName transport");
}
}
if(_17.contentWindow){
_17.contentWindow.name=_10;
}
},_frameNum:0};
}
