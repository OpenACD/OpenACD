/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.off.sync"]){
dojo._hasResource["dojox.off.sync"]=true;
dojo.provide("dojox.off.sync");
dojo.require("dojox.storage.GearsStorageProvider");
dojo.require("dojox.off._common");
dojo.require("dojox.off.files");
dojo.mixin(dojox.off.sync,{isSyncing:false,cancelled:false,successful:true,details:[],error:false,actions:null,autoSync:true,onSync:function(_1){
},synchronize:function(){
if(this.isSyncing||dojox.off.goingOnline||(!dojox.off.isOnline)){
return;
}
this.isSyncing=true;
this.successful=false;
this.details=[];
this.cancelled=false;
this.start();
},cancel:function(){
if(!this.isSyncing){
return;
}
this.cancelled=true;
if(dojox.off.files.refreshing){
dojox.off.files.abortRefresh();
}
this.onSync("cancel");
},finishedDownloading:function(_2,_3){
if(typeof _2=="undefined"){
_2=true;
}
if(!_2){
this.successful=false;
this.details.push(_3);
this.error=true;
}
this.finished();
},start:function(){
if(this.cancelled){
this.finished();
return;
}
this.onSync("start");
this.refreshFiles();
},refreshFiles:function(){
if(this.cancelled){
this.finished();
return;
}
this.onSync("refreshFiles");
dojox.off.files.refresh(dojo.hitch(this,function(_4,_5){
if(_4){
this.error=true;
this.successful=false;
for(var i=0;i<_5.length;i++){
this.details.push(_5[i]);
}
}
this.upload();
}));
},upload:function(){
if(this.cancelled){
this.finished();
return;
}
this.onSync("upload");
dojo.connect(this.actions,"onReplayFinished",this,this.download);
this.actions.replay();
},download:function(){
if(this.cancelled){
this.finished();
return;
}
this.onSync("download");
},finished:function(){
this.isSyncing=false;
this.successful=(!this.cancelled&&!this.error);
this.onSync("finished");
},_save:function(_6){
this.actions._save(function(){
_6();
});
},_load:function(_7){
this.actions._load(function(){
_7();
});
}});
dojo.declare("dojox.off.sync.ActionLog",null,{entries:[],reasonHalted:null,isReplaying:false,autoSave:true,add:function(_8){
if(this.isReplaying){
throw "Programming error: you can not call "+"dojox.off.sync.actions.add() while "+"we are replaying an action log";
}
this.entries.push(_8);
if(this.autoSave){
this._save();
}
},onReplay:function(_9,_a){
},length:function(){
return this.entries.length;
},haltReplay:function(_b){
if(!this.isReplaying){
return;
}
if(_b){
this.reasonHalted=_b.toString();
}
if(this.autoSave){
var _c=this;
this._save(function(){
_c.isReplaying=false;
_c.onReplayFinished();
});
}else{
this.isReplaying=false;
this.onReplayFinished();
}
},continueReplay:function(){
if(!this.isReplaying){
return;
}
this.entries.shift();
if(!this.entries.length){
if(this.autoSave){
var _d=this;
this._save(function(){
_d.isReplaying=false;
_d.onReplayFinished();
});
return;
}else{
this.isReplaying=false;
this.onReplayFinished();
return;
}
}
var _e=this.entries[0];
this.onReplay(_e,this);
},clear:function(){
if(this.isReplaying){
return;
}
this.entries=[];
if(this.autoSave){
this._save();
}
},replay:function(){
if(this.isReplaying){
return;
}
this.reasonHalted=null;
if(!this.entries.length){
this.onReplayFinished();
return;
}
this.isReplaying=true;
var _f=this.entries[0];
this.onReplay(_f,this);
},onReplayFinished:function(){
},toString:function(){
var _10="";
_10+="[";
for(var i=0;i<this.entries.length;i++){
_10+="{";
for(var j in this.entries[i]){
_10+=j+": \""+this.entries[i][j]+"\"";
_10+=", ";
}
_10+="}, ";
}
_10+="]";
return _10;
},_save:function(_11){
if(!_11){
_11=function(){
};
}
try{
var _12=this;
var _13=function(_14,key,_15){
if(_14==dojox.storage.FAILED){
dojox.off.onFrameworkEvent("save",{status:dojox.storage.FAILED,isCoreSave:true,key:key,value:_15,namespace:dojox.off.STORAGE_NAMESPACE});
_11();
}else{
if(_14==dojox.storage.SUCCESS){
_11();
}
}
};
dojox.storage.put("actionlog",this.entries,_13,dojox.off.STORAGE_NAMESPACE);
}
catch(exp){
dojox.off.onFrameworkEvent("save",{status:dojox.storage.FAILED,isCoreSave:true,key:"actionlog",value:this.entries,namespace:dojox.off.STORAGE_NAMESPACE});
_11();
}
},_load:function(_16){
var _17=dojox.storage.get("actionlog",dojox.off.STORAGE_NAMESPACE);
if(!_17){
_17=[];
}
this.entries=_17;
_16();
}});
dojox.off.sync.actions=new dojox.off.sync.ActionLog();
}
