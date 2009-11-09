/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.off.files"]){
dojo._hasResource["dojox.off.files"]=true;
dojo.provide("dojox.off.files");
dojox.off.files={versionURL:"version.js",listOfURLs:[],refreshing:false,_cancelID:null,_error:false,_errorMessages:[],_currentFileIndex:0,_store:null,_doSlurp:false,slurp:function(){
this._doSlurp=true;
},cache:function(_1){
if(dojo.isString(_1)){
var _2=this._trimAnchor(_1+"");
if(!this.isAvailable(_2)){
this.listOfURLs.push(_2);
}
}else{
if(_1 instanceof dojo._Url){
var _2=this._trimAnchor(_1.uri);
if(!this.isAvailable(_2)){
this.listOfURLs.push(_2);
}
}else{
dojo.forEach(_1,function(_3){
_3=this._trimAnchor(_3);
if(!this.isAvailable(_3)){
this.listOfURLs.push(_3);
}
},this);
}
}
},printURLs:function(){
dojo.forEach(this.listOfURLs,function(i){
});
},remove:function(_4){
for(var i=0;i<this.listOfURLs.length;i++){
if(this.listOfURLs[i]==_4){
this.listOfURLs=this.listOfURLs.splice(i,1);
break;
}
}
},isAvailable:function(_5){
for(var i=0;i<this.listOfURLs.length;i++){
if(this.listOfURLs[i]==_5){
return true;
}
}
return false;
},refresh:function(_6){
try{
if(dojo.config.isDebug){
this.printURLs();
}
this.refreshing=true;
if(this.versionURL){
this._getVersionInfo(function(_7,_8,_9){
if(dojo.config.isDebug||!_8||_9||!_7||_7!=_8){
console.warn("Refreshing offline file list");
this._doRefresh(_6,_8);
}else{
console.warn("No need to refresh offline file list");
_6(false,[]);
}
});
}else{
console.warn("Refreshing offline file list");
this._doRefresh(_6);
}
}
catch(e){
this.refreshing=false;
dojox.off.coreOpFailed=true;
dojox.off.enabled=false;
dojox.off.onFrameworkEvent("coreOperationFailed");
}
},abortRefresh:function(){
if(!this.refreshing){
return;
}
this._store.abortCapture(this._cancelID);
this.refreshing=false;
},_slurp:function(){
if(!this._doSlurp){
return;
}
var _a=dojo.hitch(this,function(_b){
if(this._sameLocation(_b)){
this.cache(_b);
}
});
_a(window.location.href);
dojo.query("script").forEach(function(i){
try{
_a(i.getAttribute("src"));
}
catch(exp){
}
});
dojo.query("link").forEach(function(i){
try{
if(!i.getAttribute("rel")||i.getAttribute("rel").toLowerCase()!="stylesheet"){
return;
}
_a(i.getAttribute("href"));
}
catch(exp){
}
});
dojo.query("img").forEach(function(i){
try{
_a(i.getAttribute("src"));
}
catch(exp){
}
});
dojo.query("a").forEach(function(i){
try{
_a(i.getAttribute("href"));
}
catch(exp){
}
});
dojo.forEach(document.styleSheets,function(_c){
try{
if(_c.cssRules){
dojo.forEach(_c.cssRules,function(_d){
var _e=_d.cssText;
if(_e){
var _f=_e.match(/url\(\s*([^\) ]*)\s*\)/i);
if(!_f){
return;
}
for(var i=1;i<_f.length;i++){
_a(_f[i]);
}
}
});
}else{
if(_c.cssText){
var _10;
var _11=_c.cssText.toString();
var _12=_11.split(/\f|\r|\n/);
for(var i=0;i<_12.length;i++){
_10=_12[i].match(/url\(\s*([^\) ]*)\s*\)/i);
if(_10&&_10.length){
_a(_10[1]);
}
}
}
}
}
catch(exp){
}
});
},_sameLocation:function(url){
if(!url){
return false;
}
if(url.length&&url.charAt(0)=="#"){
return false;
}
url=new dojo._Url(url);
if(!url.scheme&&!url.port&&!url.host){
return true;
}
if(!url.scheme&&url.host&&url.port&&window.location.hostname==url.host&&window.location.port==url.port){
return true;
}
if(!url.scheme&&url.host&&!url.port&&window.location.hostname==url.host&&window.location.port==80){
return true;
}
return window.location.protocol==(url.scheme+":")&&window.location.hostname==url.host&&(window.location.port==url.port||!window.location.port&&!url.port);
},_trimAnchor:function(url){
return url.replace(/\#.*$/,"");
},_doRefresh:function(_13,_14){
var _15;
try{
_15=google.gears.factory.create("beta.localserver","1.0");
}
catch(exp){
dojo.setObject("google.gears.denied",true);
dojox.off.onFrameworkEvent("coreOperationFailed");
throw "Google Gears must be allowed to run";
}
var _16="dot_store_"+window.location.href.replace(/[^0-9A-Za-z_]/g,"_");
if(_16.length>=64){
_16=_16.substring(0,63);
}
_15.removeStore(_16);
_15.openStore(_16);
var _17=_15.createStore(_16);
this._store=_17;
var _18=this;
this._currentFileIndex=0;
this._cancelID=_17.capture(this.listOfURLs,function(url,_19,_1a){
if(!_19&&_18.refreshing){
_18._cancelID=null;
_18.refreshing=false;
var _1b=[];
_1b.push("Unable to capture: "+url);
_13(true,_1b);
return;
}else{
if(_19){
_18._currentFileIndex++;
}
}
if(_19&&_18._currentFileIndex>=_18.listOfURLs.length){
_18._cancelID=null;
_18.refreshing=false;
if(_14){
dojox.storage.put("oldVersion",_14,null,dojox.off.STORAGE_NAMESPACE);
}
dojox.storage.put("justDebugged",dojo.config.isDebug,null,dojox.off.STORAGE_NAMESPACE);
_13(false,[]);
}
});
},_getVersionInfo:function(_1c){
var _1d=dojox.storage.get("justDebugged",dojox.off.STORAGE_NAMESPACE);
var _1e=dojox.storage.get("oldVersion",dojox.off.STORAGE_NAMESPACE);
var _1f=null;
_1c=dojo.hitch(this,_1c);
dojo.xhrGet({url:this.versionURL+"?browserbust="+new Date().getTime(),timeout:5*1000,handleAs:"javascript",error:function(err){
dojox.storage.remove("oldVersion",dojox.off.STORAGE_NAMESPACE);
dojox.storage.remove("justDebugged",dojox.off.STORAGE_NAMESPACE);
_1c(_1e,_1f,_1d);
},load:function(_20){
if(_20){
_1f=_20;
}
_1c(_1e,_1f,_1d);
}});
}};
}
