/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.form.FileUploader"]){
dojo._hasResource["dojox.form.FileUploader"]=true;
dojo.provide("dojox.form.FileUploader");
dojo.require("dojox.embed.Flash");
dojo.require("dojo.io.iframe");
dojo.require("dojox.html.styles");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dojox.embed.flashVars");
dojo.require("dijit._Contained");
dojo.experimental("dojox.form.FileUploader");
(function(){
var _1=dojo.config.uploaderPath||dojo.moduleUrl("dojox.form","resources/uploader.swf");
var _2=function(o1,o2){
var o={},nm;
for(nm in o1){
if(dojo.isObject(o1[nm])){
o[nm]=_2({},o1[nm]);
}else{
o[nm]=o1[nm];
}
}
for(nm in o2){
if(dojo.isObject(o2[nm])){
if(dojo.isObject(o[nm])){
_2(o[nm],o2[nm]);
}else{
_2({},o2[nm]);
}
}else{
o[nm]=o2[nm];
}
}
return o;
};
var _3=function(_4){
if(!_4||_4=="none"){
return false;
}
return _4.replace(/:/g,"||").replace(/\./g,"^^").replace("url(","").replace(")","").replace(/'/g,"").replace(/"/g,"");
};
var _5=function(_6){
var tn=_6.tagName.toLowerCase();
return tn=="button"||tn=="input";
};
var _7=function(_8){
var o={};
o.ff=dojo.style(_8,"fontFamily");
if(o.ff){
o.ff=o.ff.replace(", ",",");
o.ff=o.ff.replace(/\"|\'/g,"");
o.ff=o.ff=="sans-serif"?"Arial":o.ff;
o.fw=dojo.style(_8,"fontWeight");
o.fi=dojo.style(_8,"fontStyle");
o.fs=parseInt(dojo.style(_8,"fontSize"),10);
if(dojo.style(_8,"fontSize").indexOf("%")>-1){
var n=_8;
while(n.tagName){
if(dojo.style(n,"fontSize").indexOf("%")==-1){
o.fs=parseInt(dojo.style(n,"fontSize"),10);
break;
}
if(n.tagName.toLowerCase()=="body"){
o.fs=16*0.01*parseInt(dojo.style(n,"fontSize"),10);
}
n=n.parentNode;
}
}
o.fc=new dojo.Color(dojo.style(_8,"color")).toHex();
o.fc=parseInt(o.fc.substring(1,Infinity),16);
}
o.lh=dojo.style(_8,"lineHeight");
o.ta=dojo.style(_8,"textAlign");
o.ta=o.ta=="start"||!o.ta?"left":o.ta;
o.va=_5(_8)?"middle":o.lh==o.h?"middle":dojo.style(_8,"verticalAlign");
return o;
};
var _9=function(_a){
var cn=dojo.trim(_a.innerHTML);
if(cn.indexOf("<")>-1){
cn=escape(cn);
}
return cn;
};
var _b=function(_c){
var o={};
var _d=dojo.contentBox(_c);
var _e=dojo._getPadExtents(_c);
o.p=[_e.t,_e.w-_e.l,_e.h-_e.t,_e.l];
o.w=_d.w+_e.w;
o.h=_d.h+_e.h;
o.d=dojo.style(_c,"display");
var _f=new dojo.Color(dojo.style(_c,"backgroundColor"));
o.bc=_f.a==0?"#ffffff":_f.toHex();
o.bc=parseInt(o.bc.substring(1,Infinity),16);
var url=_3(dojo.style(_c,"backgroundImage"));
if(url){
o.bi={url:url,rp:dojo.style(_c,"backgroundRepeat"),pos:escape(dojo.style(_c,"backgroundPosition"))};
if(!o.bi.pos){
var rx=dojo.style(_c,"backgroundPositionX");
var ry=dojo.style(_c,"backgroundPositionY");
rx=(rx=="left")?"0%":(rx=="right")?"100%":rx;
ry=(ry=="top")?"0%":(ry=="bottom")?"100%":ry;
o.bi.pos=escape(rx+" "+ry);
}
}
return _2(o,_7(_c));
};
var _10=function(_11,_12,_13){
var _14,_15;
if(_13){
_14=dojo.place("<"+_11.tagName+"><span>"+_11.innerHTML+"</span></"+_11.tagName+">",_11.parentNode);
var _16=_14.firstChild;
dojo.addClass(_16,_11.className);
dojo.addClass(_14,_12);
_15=_b(_16);
}else{
_14=dojo.place("<"+_11.tagName+">"+_11.innerHTML+"</"+_11.tagName+">",_11.parentNode);
dojo.addClass(_14,_11.className);
dojo.addClass(_14,_12);
_14.id=_11.id;
_15=_b(_14);
}
dojo.destroy(_14);
return _15;
};
var _17=function(ltr){
return ltr.charCodeAt(0)<91;
};
dojo.declare("dojox.form.FileUploader",[dijit._Widget,dijit._Templated,dijit._Contained],{templateString:"<div><div dojoAttachPoint=\"progNode\"><div dojoAttachPoint=\"progTextNode\"></div></div><div dojoAttachPoint=\"insideNode\" class=\"uploaderInsideNode\"></div></div>",uploadUrl:"",isDebug:false,devMode:false,baseClass:"dojoxUploaderNorm",hoverClass:"dojoxUploaderHover",activeClass:"dojoxUploaderActive",disabledClass:"dojoxUploaderDisabled",force:"",uploaderType:"",flashObject:null,flashMovie:null,flashDiv:null,insideNode:null,deferredUploading:0,fileListId:"",uploadOnChange:false,selectMultipleFiles:true,htmlFieldName:"uploadedfile",flashFieldName:"flashUploadFiles",fileMask:null,minFlashVersion:9,tabIndex:-1,showProgress:false,progressMessage:"Loading",progressBackgroundUrl:dojo.moduleUrl("dijit","themes/tundra/images/buttonActive.png"),progressBackgroundColor:"#ededed",progressWidgetId:"",skipServerCheck:false,log:function(){
if(this.isDebug){
console.log.apply(console,arguments);
}
},constructor:function(){
this._subs=[];
},postMixInProperties:function(){
this.fileList=[];
this._cons=[];
this.fileMask=[];
this.fileInputs=[];
this.fileCount=0;
this.flashReady=false;
this._disabled=false;
this.force=this.force.toLowerCase();
this.uploaderType=((dojox.embed.Flash.available>=this.minFlashVersion||this.force=="flash")&&this.force!="html")?"flash":"html";
if(!this.swfPath){
this.swfPath=_1;
}
this._refNode=this.srcNodeRef;
this.getButtonStyle();
},startup:function(){
},postCreate:function(){
this.inherited(arguments);
this.setButtonStyle();
if(this.uploaderType=="flash"){
this.uploaderType="flash";
this.createFlashUploader();
}else{
this.uploaderType="html";
this.createHtmlUploader();
}
if(this.fileListId){
this.connect(dojo.byId(this.fileListId),"click",function(evt){
var p=evt.target.parentNode.parentNode.parentNode;
if(p.id&&p.id.indexOf("file_")>-1){
this.removeFile(p.id.split("file_")[1]);
}
});
}
dojo.addOnUnload(this,this.destroy);
},getButtonStyle:function(){
var _18=this.srcNodeRef;
if(this.button){
console.warn("DEPRECATED: FileUploader.button - will be removed in 1.5. FileUploader should be created as widget.");
}
if(!_18&&this.button&&this.button.domNode){
var _19=true;
var cls=this.button.domNode.className+" dijitButtonNode";
var txt=_9(dojo.query(".dijitButtonText",this.button.domNode)[0]);
var _1a="<button id=\""+this.button.id+"\" class=\""+cls+"\">"+txt+"</button>";
_18=dojo.place(_1a,this.button.domNode,"after");
this.srcNodeRef=_18;
this.button.destroy();
this.baseClass="dijitButton";
this.hoverClass="dijitButtonHover";
this.pressClass="dijitButtonActive";
this.disabledClass="dijitButtonDisabled";
}else{
if(!this.srcNodeRef&&this.button){
_18=this.button;
}
}
if(dojo.attr(_18,"class")){
this.baseClass+=" "+dojo.attr(_18,"class");
}
dojo.attr(_18,"class",this.baseClass);
this.norm=_b(_18);
this.width=this.norm.w;
this.height=this.norm.h;
if(this.uploaderType=="flash"){
this.over=_10(_18,this.baseClass+" "+this.hoverClass,_19);
this.down=_10(_18,this.baseClass+" "+this.activeClass,_19);
this.dsbl=_10(_18,this.baseClass+" "+this.disabledClass,_19);
this.fhtml={cn:_9(_18),nr:this.norm,ov:this.over,dn:this.down,ds:this.dsbl};
}else{
this.fhtml={cn:_9(_18),nr:this.norm};
if(this.norm.va=="middle"){
this.norm.lh=this.norm.h;
}
}
if(this.devMode){
this.log("classes - base:",this.baseClass," hover:",this.hoverClass,"active:",this.activeClass);
this.log("fhtml:",this.fhtml);
this.log("norm:",this.norm);
this.log("over:",this.over);
this.log("down:",this.down);
this.log("norm:",this.norm.fs);
}
},setButtonStyle:function(){
dojo.style(this.domNode,{width:this.fhtml.nr.w+"px",height:(this.fhtml.nr.h)+"px",padding:"0px",lineHeight:"normal",position:"relative"});
if(this.uploaderType=="html"&&this.norm.va=="middle"){
dojo.style(this.domNode,"lineHeight",this.norm.lh+"px");
}
if(this.showProgress){
this.progTextNode.innerHTML=this.progressMessage;
dojo.style(this.progTextNode,{width:this.fhtml.nr.w+"px",height:(this.fhtml.nr.h+0)+"px",padding:"0px",margin:"0px",left:"0px",lineHeight:(this.fhtml.nr.h+0)+"px",position:"absolute"});
dojo.style(this.progNode,{width:this.fhtml.nr.w+"px",height:(this.fhtml.nr.h+0)+"px",padding:"0px",margin:"0px",left:"0px",position:"absolute",display:"none",backgroundImage:"url("+this.progressBackgroundUrl+")",backgroundPosition:"bottom",backgroundRepeat:"repeat-x",backgroundColor:this.progressBackgroundColor});
}else{
dojo.destroy(this.progNode);
}
dojo.style(this.insideNode,{position:"absolute",top:"0px",left:"0px",display:""});
dojo.addClass(this.domNode,this.srcNodeRef.className);
if(this.fhtml.nr.d.indexOf("inline")>-1){
dojo.addClass(this.domNode,"dijitInline");
}
try{
this.insideNode.innerHTML=this.fhtml.cn;
}
catch(e){
if(this.uploaderType=="flash"){
this.insideNode=this.insideNode.parentNode.removeChild(this.insideNode);
dojo.body().appendChild(this.insideNode);
this.insideNode.innerHTML=this.fhtml.cn;
var c=dojo.connect(this,"onReady",this,function(){
dojo.disconnect(c);
this.insideNode=this.insideNode.parentNode.removeChild(this.insideNode);
this.domNode.appendChild(this.insideNode);
});
}else{
this.insideNode.appendChild(document.createTextNode(this.fhtml.cn));
}
}
this.flashDiv=this.insideNode;
},onChange:function(_1b){
},onProgress:function(_1c){
},onComplete:function(_1d){
},onCancel:function(){
},onError:function(_1e){
},onReady:function(_1f){
},onLoad:function(_20){
},submit:function(_21){
var _22=_21?dojo.formToObject(_21):null;
this.upload(_22);
return false;
},upload:function(_23){
if(!this.fileList.length){
return false;
}
if(!this.uploadUrl){
console.warn("uploadUrl not provided. Aborting.");
return false;
}
if(!this.showProgress){
this.attr("disabled",true);
}
if(this.progressWidgetId){
var _24=dijit.byId(this.progressWidgetId).domNode;
if(dojo.style(_24,"display")=="none"){
this.restoreProgDisplay="none";
dojo.style(_24,"display","block");
}
if(dojo.style(_24,"visibility")=="hidden"){
this.restoreProgDisplay="hidden";
dojo.style(_24,"visibility","visible");
}
}
if(_23&&!_23.target){
this.postData=_23;
}
this.log("upload type:",this.uploaderType," - postData:",this.postData);
for(var i=0;i<this.fileList.length;i++){
var f=this.fileList[i];
f.bytesLoaded=0;
f.bytesTotal=f.size||100000;
f.percent=0;
}
if(this.uploaderType=="flash"){
this.uploadFlash();
}else{
this.uploadHTML();
}
return false;
},removeFile:function(_25,_26){
var i;
for(i=0;i<this.fileList.length;i++){
if(this.fileList[i].name==_25){
if(!_26){
this.fileList.splice(i,1);
}
break;
}
}
if(this.uploaderType=="flash"){
this.flashMovie.removeFile(_25);
}else{
if(!_26){
dojo.destroy(this.fileInputs[i]);
this.fileInputs.splice(i,1);
}
}
if(this.fileListId){
dojo.destroy("file_"+_25);
}
},destroyAll:function(){
console.warn("DEPRECATED for 1.5 - use destroy() instead");
this.destroy();
},destroy:function(){
if(this.uploaderType=="flash"&&!this.flashMovie){
this._cons.push(dojo.connect(this,"onLoad",this,"destroy"));
return;
}
dojo.forEach(this._subs,dojo.unsubscribe,dojo);
dojo.forEach(this._cons,dojo.disconnect,dojo);
if(this.scrollConnect){
dojo.disconnect(this.scrollConnect);
}
if(this.uploaderType=="flash"){
this.flashObject.destroy();
dojo.destroy(this.flashDiv);
}else{
dojo.destroy("dojoIoIframe");
dojo.destroy(this._fileInput);
dojo.destroy(this._formNode);
}
this.inherited(arguments);
},hide:function(){
console.warn("DEPRECATED for 1.5 - use dojo.style(domNode, 'display', 'none' instead");
dojo.style(this.domNode,"display","none");
},show:function(){
console.warn("DEPRECATED for 1.5 - use dojo.style(domNode, 'display', '') instead");
dojo.style(this.domNode,"display","");
},disable:function(_27){
console.warn("DEPRECATED: FileUploader.disable() - will be removed in 1.5. Use attr('disable', true) instead.");
this.attr("disable",_27);
},_displayProgress:function(_28){
if(_28===true){
if(this.uploaderType=="flash"){
dojo.style(this.insideNode,"left","-2500px");
}else{
dojo.style(this.insideNode,"display","none");
}
dojo.style(this.progNode,"display","");
}else{
if(_28===false){
dojo.style(this.insideNode,{display:"",left:"0px"});
dojo.style(this.progNode,"display","none");
}else{
var w=_28*this.fhtml.nr.w;
dojo.style(this.progNode,"width",w+"px");
}
}
},_animateProgress:function(){
this._displayProgress(true);
var _29=false;
var c=dojo.connect(this,"_complete",function(){
dojo.disconnect(c);
_29=true;
});
var w=0;
var _2a=setInterval(dojo.hitch(this,function(){
w+=5;
if(w>this.fhtml.nr.w){
w=0;
_29=true;
}
this._displayProgress(w/this.fhtml.nr.w);
if(_29){
clearInterval(_2a);
setTimeout(dojo.hitch(this,function(){
this._displayProgress(false);
}),500);
}
}),50);
},_error:function(evt){
if(typeof (evt)=="string"){
evt=new Error(evt);
}
this.onError(evt);
},_addToFileList:function(){
if(this.fileListId){
var str="";
dojo.forEach(this.fileList,function(d){
str+="<table id=\"file_"+d.name+"\" class=\"fileToUpload\"><tr><td class=\"fileToUploadClose\"></td><td class=\"fileToUploadName\">"+d.name+"</td><td class=\"fileToUploadSize\">"+Math.ceil(d.size*0.001)+"kb</td></tr></table>";
},this);
dojo.byId(this.fileListId).innerHTML=str;
}
},_change:function(_2b){
if(dojo.isIE){
dojo.forEach(_2b,function(f){
f.name=f.name.split("\\")[f.name.split("\\").length-1];
});
}
if(this.selectMultipleFiles){
this.fileList=this.fileList.concat(_2b);
}else{
if(this.fileList[0]){
this.removeFile(this.fileList[0].name);
}
this.fileList=_2b;
}
this._addToFileList();
this.onChange(_2b);
if(this.uploadOnChange){
this.upload();
}else{
if(this.uploaderType=="html"&&this.selectMultipleFiles){
this._buildFileInput();
this._connectInput();
}
}
},_complete:function(_2c){
_2c=dojo.isArray(_2c)?_2c:[_2c];
dojo.forEach(_2c,function(f){
if(f.ERROR){
this._error(f.ERROR);
}
},this);
dojo.forEach(this.fileList,function(f){
f.bytesLoaded=1;
f.bytesTotal=1;
f.percent=100;
this._progress(f);
},this);
dojo.forEach(this.fileList,function(f){
this.removeFile(f.name,true);
},this);
this.onComplete(_2c);
this.fileList=[];
this._resetHTML();
this.attr("disabled",false);
if(this.restoreProgDisplay){
setTimeout(dojo.hitch(this,function(){
dojo.style(dijit.byId(this.progressWidgetId).domNode,this.restoreProgDisplay=="none"?"display":"visibility",this.restoreProgDisplay);
}),500);
}
},_progress:function(_2d){
var _2e=0;
var _2f=0;
for(var i=0;i<this.fileList.length;i++){
var f=this.fileList[i];
if(f.name==_2d.name){
f.bytesLoaded=_2d.bytesLoaded;
f.bytesTotal=_2d.bytesTotal;
f.percent=Math.ceil(f.bytesLoaded/f.bytesTotal*100);
this.log(f.name,"percent:",f.percent);
}
_2f+=Math.ceil(0.001*f.bytesLoaded);
_2e+=Math.ceil(0.001*f.bytesTotal);
}
var _30=Math.ceil(_2f/_2e*100);
if(this.progressWidgetId){
dijit.byId(this.progressWidgetId).update({progress:_30+"%"});
}
if(this.showProgress){
this._displayProgress(_30*0.01);
}
this.onProgress(this.fileList);
},_getDisabledAttr:function(){
return this._disabled;
},_setDisabledAttr:function(_31){
if(this._disabled==_31){
return;
}
if(this.uploaderType=="flash"){
if(!this.flashReady){
var _32=dojo.connect(this,"onReady",this,function(){
dojo.disconnect(_32);
this._setDisabledAttr(_31);
});
return;
}
this._disabled=_31;
this.flashMovie.doDisable(_31);
}else{
this._disabled=_31;
dojo.style(this._fileInput,"display",this._disabled?"none":"");
}
dojo.toggleClass(this.domNode,this.disabledClass,_31);
},_onFlashBlur:function(){
this.flashMovie.blur();
if(!this.nextFocusObject&&this.tabIndex){
var _33=dojo.query("[tabIndex]");
for(var i=0;i<_33.length;i++){
if(_33[i].tabIndex>=Number(this.tabIndex)+1){
this.nextFocusObject=_33[i];
break;
}
}
}
this.nextFocusObject.focus();
},_disconnect:function(){
dojo.forEach(this._cons,dojo.disconnect,dojo);
},uploadHTML:function(){
if(this.selectMultipleFiles){
dojo.destroy(this._fileInput);
}
this._setHtmlPostData();
if(this.showProgress){
this._animateProgress();
}
dojo.io.iframe.send({url:this.uploadUrl,form:this._formNode,handleAs:"json",handle:dojo.hitch(this,function(_34,_35,_36){
this._complete(_34);
})});
},createHtmlUploader:function(){
this._buildForm();
this._setFormStyle();
this._buildFileInput();
this._connectInput();
this._styleContent();
dojo.style(this.insideNode,"visibility","visible");
this.onReady();
},_connectInput:function(){
this._disconnect();
this._cons.push(dojo.connect(this._fileInput,"mouseover",this,function(evt){
dojo.addClass(this.domNode,this.hoverClass);
this.onMouseOver(evt);
}));
this._cons.push(dojo.connect(this._fileInput,"mouseout",this,function(evt){
dojo.removeClass(this.domNode,this.activeClass);
dojo.removeClass(this.domNode,this.hoverClass);
this.onMouseOut(evt);
this._checkHtmlCancel("off");
}));
this._cons.push(dojo.connect(this._fileInput,"mousedown",this,function(evt){
dojo.addClass(this.domNode,this.activeClass);
dojo.removeClass(this.domNode,this.hoverClass);
this.onMouseDown(evt);
}));
this._cons.push(dojo.connect(this._fileInput,"mouseup",this,function(evt){
dojo.removeClass(this.domNode,this.activeClass);
this.onMouseUp(evt);
this.onClick(evt);
this._checkHtmlCancel("up");
}));
this._cons.push(dojo.connect(this._fileInput,"change",this,function(){
this._checkHtmlCancel("change");
this._change([{name:this._fileInput.value,type:"",size:0}]);
}));
if(this.tabIndex>=0){
dojo.attr(this.domNode,"tabIndex",this.tabIndex);
}
},_checkHtmlCancel:function(_37){
if(_37=="change"){
this.dialogIsOpen=false;
}
if(_37=="up"){
this.dialogIsOpen=true;
}
if(_37=="off"){
if(this.dialogIsOpen){
this.onCancel();
}
this.dialogIsOpen=false;
}
},_styleContent:function(){
var o=this.fhtml.nr;
dojo.style(this.insideNode,{width:o.w+"px",height:o.va=="middle"?o.h+"px":"auto",textAlign:o.ta,paddingTop:o.p[0]+"px",paddingRight:o.p[1]+"px",paddingBottom:o.p[2]+"px",paddingLeft:o.p[3]+"px"});
try{
dojo.style(this.insideNode,"lineHeight","inherit");
}
catch(e){
}
},_resetHTML:function(){
if(this.uploaderType=="html"&&this._formNode){
dojo.query("*",this._formNode).forEach(function(n){
dojo.destroy(n);
});
this.fileCount=0;
this._buildFileInput();
this._connectInput();
}
},_buildForm:function(){
if(this._formNode){
return;
}
if(dojo.isIE){
this._formNode=document.createElement("<form enctype=\"multipart/form-data\" method=\"post\">");
this._formNode.encoding="multipart/form-data";
}else{
this._formNode=document.createElement("form");
this._formNode.setAttribute("enctype","multipart/form-data");
}
this._formNode.id=dijit.getUniqueId("FileUploaderForm");
this.domNode.appendChild(this._formNode);
},_buildFileInput:function(){
if(this._fileInput){
this._disconnect();
this._fileInput.id=this._fileInput.id+this.fileCount;
dojo.style(this._fileInput,"display","none");
}
this._fileInput=document.createElement("input");
this.fileInputs.push(this._fileInput);
var nm=this.htmlFieldName;
var _38=this.id;
if(this.selectMultipleFiles){
nm+=this.fileCount;
_38+=this.fileCount;
this.fileCount++;
}
dojo.attr(this._fileInput,{id:this.id,name:nm,type:"file"});
dojo.addClass(this._fileInput,"dijitFileInputReal");
this._formNode.appendChild(this._fileInput);
var _39=dojo.marginBox(this._fileInput);
dojo.style(this._fileInput,{position:"relative",left:(this.fhtml.nr.w-_39.w)+"px",opacity:0});
},_setFormStyle:function(){
var _3a=Math.max(2,Math.max(Math.ceil(this.fhtml.nr.w/60),Math.ceil(this.fhtml.nr.h/15)));
dojox.html.insertCssRule("#"+this._formNode.id+" input","font-size:"+_3a+"em");
dojo.style(this.domNode,{overflow:"hidden",position:"relative"});
dojo.style(this.insideNode,"position","absolute");
},_setHtmlPostData:function(){
if(this.postData){
for(var nm in this.postData){
dojo.create("input",{type:"hidden",name:nm,value:this.postData[nm]},this._formNode);
}
}
},uploadFlash:function(){
try{
if(this.showProgress){
this._displayProgress(true);
var c=dojo.connect(this,"_complete",this,function(){
dojo.disconnect(c);
this._displayProgress(false);
});
}
this.flashMovie.doUpload(this.postData);
}
catch(err){
this._error("FileUploader - Sorry, the SWF failed to initialize."+err);
}
},createFlashUploader:function(){
this.uploadUrl=this.uploadUrl.toString();
if(this.uploadUrl){
if(this.uploadUrl.toLowerCase().indexOf("http")<0&&this.uploadUrl.indexOf("/")!=0){
var loc=window.location.href.split("/");
loc.pop();
loc=loc.join("/")+"/";
this.uploadUrl=loc+this.uploadUrl;
this.log("SWF Fixed - Relative loc:",loc," abs loc:",this.uploadUrl);
}else{
this.log("SWF URL unmodified:",this.uploadUrl);
}
}else{
console.warn("Warning: no uploadUrl provided.");
}
var w=this.fhtml.nr.w;
var h=this.fhtml.nr.h;
var _3b={expressInstall:true,path:this.swfPath.uri||this.swfPath,width:w,height:h,allowScriptAccess:"always",allowNetworking:"all",vars:{uploadDataFieldName:this.flashFieldName,uploadUrl:this.uploadUrl,uploadOnSelect:this.uploadOnChange,deferredUploading:this.deferredUploading||"",selectMultipleFiles:this.selectMultipleFiles,id:this.id,isDebug:this.isDebug,devMode:this.devMode,flashButton:dojox.embed.flashVars.serialize("fh",this.fhtml),fileMask:dojox.embed.flashVars.serialize("fm",this.fileMask),noReturnCheck:this.skipServerCheck},params:{scale:"noscale",wmode:"opaque"}};
this.flashObject=new dojox.embed.Flash(_3b,this.insideNode);
this.flashObject.onError=function(msg){
this._error("Flash Error: "+msg);
};
this.flashObject.onReady=dojo.hitch(this,function(){
dojo.style(this.insideNode,"visibility","visible");
this.log("FileUploader flash object ready");
this.onReady(this);
});
this.flashObject.onLoad=dojo.hitch(this,function(mov){
this.flashMovie=mov;
this.flashReady=true;
this.onLoad(this);
});
this._connectFlash();
},_connectFlash:function(){
this._doSub("/filesSelected","_change");
this._doSub("/filesUploaded","_complete");
this._doSub("/filesProgress","_progress");
this._doSub("/filesError","_error");
this._doSub("/filesCanceled","onCancel");
this._doSub("/stageBlur","_onFlashBlur");
this._doSub("/up","onMouseUp");
this._doSub("/down","onMouseDown");
this._doSub("/over","onMouseOver");
this._doSub("/out","onMouseOut");
this.connect(this.domNode,"focus",function(){
this.flashMovie.focus();
this.flashMovie.doFocus();
});
if(this.tabIndex>=0){
dojo.attr(this.domNode,"tabIndex",this.tabIndex);
}
},_doSub:function(_3c,_3d){
this._subs.push(dojo.subscribe(this.id+_3c,this,_3d));
}});
})();
}
