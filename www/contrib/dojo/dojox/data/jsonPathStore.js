/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.data.jsonPathStore"]){
dojo._hasResource["dojox.data.jsonPathStore"]=true;
dojo.provide("dojox.data.jsonPathStore");
dojo.require("dojox.jsonPath");
dojo.require("dojo.date");
dojo.require("dojo.date.locale");
dojo.require("dojo.date.stamp");
dojox.data.ASYNC_MODE=0;
dojox.data.SYNC_MODE=1;
dojo.declare("dojox.data.jsonPathStore",null,{mode:dojox.data.ASYNC_MODE,metaLabel:"_meta",hideMetaAttributes:false,autoIdPrefix:"_auto_",autoIdentity:true,idAttribute:"_id",indexOnLoad:true,labelAttribute:"",url:"",_replaceRegex:/'\]/gi,noRevert:false,constructor:function(_1){
this.byId=this.fetchItemByIdentity;
if(_1){
dojo.mixin(this,_1);
}
this._dirtyItems=[];
this._autoId=0;
this._referenceId=0;
this._references={};
this._fetchQueue=[];
this.index={};
var _2="("+this.metaLabel+"'])";
this.metaRegex=new RegExp(_2);
if(!this.data&&!this.url){
this.setData({});
}
if(this.data&&!this.url){
this.setData(this.data);
delete this.data;
}
if(this.url){
dojo.xhrGet({url:_1.url,handleAs:"json",load:dojo.hitch(this,"setData"),sync:this.mode});
}
},_loadData:function(_3){
if(this._data){
delete this._data;
}
if(dojo.isString(_3)){
this._data=dojo.fromJson(_3);
}else{
this._data=_3;
}
if(this.indexOnLoad){
this.buildIndex();
}
this._updateMeta(this._data,{path:"$"});
this.onLoadData(this._data);
},onLoadData:function(_4){
while(this._fetchQueue.length>0){
var _5=this._fetchQueue.shift();
this.fetch(_5);
}
},setData:function(_6){
this._loadData(_6);
},buildIndex:function(_7,_8){
if(!this.idAttribute){
throw new Error("buildIndex requires idAttribute for the store");
}
_8=_8||this._data;
var _9=_7;
_7=_7||"$";
_7+="[*]";
var _a=this.fetch({query:_7,mode:dojox.data.SYNC_MODE});
for(var i=0;i<_a.length;i++){
var _b,_c;
if(dojo.isObject(_a[i])){
var _d=_a[i][this.metaLabel]["path"];
if(_9){
_b=_9.split("['");
_c=_b[_b.length-1].replace(this._replaceRegex,"");
if(!dojo.isArray(_a[i])){
this._addReference(_a[i],{parent:_8,attribute:_c});
this.buildIndex(_d,_a[i]);
}else{
this.buildIndex(_d,_8);
}
}else{
_b=_d.split("['");
_c=_b[_b.length-1].replace(this._replaceRegex,"");
this._addReference(_a[i],{parent:this._data,attribute:_c});
this.buildIndex(_d,_a[i]);
}
}
}
},_correctReference:function(_e){
if(this.index[_e[this.idAttribute]]&&(this.index[_e[this.idAttribute]][this.metaLabel]===_e[this.metaLabel])){
return this.index[_e[this.idAttribute]];
}
return _e;
},getValue:function(_f,_10){
_f=this._correctReference(_f);
return _f[_10];
},getValues:function(_11,_12){
_11=this._correctReference(_11);
return dojo.isArray(_11[_12])?_11[_12]:[_11[_12]];
},getAttributes:function(_13){
_13=this._correctReference(_13);
var res=[];
for(var i in _13){
if(this.hideMetaAttributes&&(i==this.metaLabel)){
continue;
}
res.push(i);
}
return res;
},hasAttribute:function(_14,_15){
_14=this._correctReference(_14);
if(_15 in _14){
return true;
}
return false;
},containsValue:function(_16,_17,_18){
_16=this._correctReference(_16);
if(_16[_17]&&_16[_17]==_18){
return true;
}
if(dojo.isObject(_16[_17])||dojo.isObject(_18)){
if(this._shallowCompare(_16[_17],_18)){
return true;
}
}
return false;
},_shallowCompare:function(a,b){
if((dojo.isObject(a)&&!dojo.isObject(b))||(dojo.isObject(b)&&!dojo.isObject(a))){
return false;
}
if(a["getFullYear"]||b["getFullYear"]){
if((a["getFullYear"]&&!b["getFullYear"])||(b["getFullYear"]&&!a["getFullYear"])){
return false;
}else{
if(!dojo.date.compare(a,b)){
return true;
}
return false;
}
}
for(var i in b){
if(dojo.isObject(b[i])){
if(!a[i]||!dojo.isObject(a[i])){
return false;
}
if(b[i]["getFullYear"]){
if(!a[i]["getFullYear"]){
return false;
}
if(dojo.date.compare(a,b)){
return false;
}
}else{
if(!this._shallowCompare(a[i],b[i])){
return false;
}
}
}else{
if(!b[i]||(a[i]!=b[i])){
return false;
}
}
}
for(i in a){
if(!b[i]){
return false;
}
}
return true;
},isItem:function(_19){
if(!dojo.isObject(_19)||!_19[this.metaLabel]){
return false;
}
if(this.requireId&&this._hasId&&!_19[this._id]){
return false;
}
return true;
},isItemLoaded:function(_1a){
_1a=this._correctReference(_1a);
return this.isItem(_1a);
},loadItem:function(_1b){
return true;
},_updateMeta:function(_1c,_1d){
if(_1c&&_1c[this.metaLabel]){
dojo.mixin(_1c[this.metaLabel],_1d);
return;
}
_1c[this.metaLabel]=_1d;
},cleanMeta:function(_1e,_1f){
_1e=_1e||this._data;
if(_1e[this.metaLabel]){
if(_1e[this.metaLabel].autoId){
delete _1e[this.idAttribute];
}
delete _1e[this.metaLabel];
}
if(dojo.isArray(_1e)){
for(var i=0;i<_1e.length;i++){
if(dojo.isObject(_1e[i])||dojo.isArray(_1e[i])){
this.cleanMeta(_1e[i]);
}
}
}else{
if(dojo.isObject(_1e)){
for(i in _1e){
if(dojo.isObject(_1e[i])){
this.cleanMeta(_1e[i]);
}
}
}
}
},fetch:function(_20){
if(!this._data){
this._fetchQueue.push(_20);
return _20;
}
if(dojo.isString(_20)){
_21=_20;
_20={query:_21,mode:dojox.data.SYNC_MODE};
}
var _21;
if(!_20||!_20.query){
if(!_20){
var _20={};
}
if(!_20.query){
_20.query="$..*";
_21=_20.query;
}
}
if(dojo.isObject(_20.query)){
if(_20.query.query){
_21=_20.query.query;
}else{
_21=_20.query="$..*";
}
if(_20.query.queryOptions){
_20.queryOptions=_20.query.queryOptions;
}
}else{
_21=_20.query;
}
if(!_20.mode){
_20.mode=this.mode;
}
if(!_20.queryOptions){
_20.queryOptions={};
}
_20.queryOptions.resultType="BOTH";
var _22=dojox.jsonPath.query(this._data,_21,_20.queryOptions);
var tmp=[];
var _23=0;
for(var i=0;i<_22.length;i++){
if(_20.start&&i<_20.start){
continue;
}
if(_20.count&&(_23>=_20.count)){
continue;
}
var _24=_22[i]["value"];
var _25=_22[i]["path"];
if(!dojo.isObject(_24)){
continue;
}
if(this.metaRegex.exec(_25)){
continue;
}
this._updateMeta(_24,{path:_22[i].path});
if(this.autoIdentity&&!_24[this.idAttribute]){
var _26=this.autoIdPrefix+this._autoId++;
_24[this.idAttribute]=_26;
_24[this.metaLabel].autoId=true;
}
if(_24[this.idAttribute]){
this.index[_24[this.idAttribute]]=_24;
}
_23++;
tmp.push(_24);
}
_22=tmp;
var _27=_20.scope||dojo.global;
if("sort" in _20){
}
if(_20.mode==dojox.data.SYNC_MODE){
return _22;
}
if(_20.onBegin){
_20["onBegin"].call(_27,_22.length,_20);
}
if(_20.onItem){
for(var i=0;i<_22.length;i++){
_20["onItem"].call(_27,_22[i],_20);
}
}
if(_20.onComplete){
_20["onComplete"].call(_27,_22,_20);
}
return _20;
},dump:function(_28){
var _28=_28||{};
var d=_28.data||this._data;
if(!_28.suppressExportMeta&&_28.clone){
_29=dojo.clone(d);
if(_29[this.metaLabel]){
_29[this.metaLabel]["clone"]=true;
}
}else{
var _29=d;
}
if(!_28.suppressExportMeta&&_29[this.metaLabel]){
_29[this.metaLabel]["last_export"]=new Date().toString();
}
if(_28.cleanMeta){
this.cleanMeta(_29);
}
switch(_28.type){
case "raw":
return _29;
case "json":
default:
return dojo.toJson(_29,_28.pretty||false);
}
},getFeatures:function(){
return {"dojo.data.api.Read":true,"dojo.data.api.Identity":true,"dojo.data.api.Write":true,"dojo.data.api.Notification":true};
},getLabel:function(_2a){
_2a=this._correctReference(_2a);
var _2b="";
if(dojo.isFunction(this.createLabel)){
return this.createLabel(_2a);
}
if(this.labelAttribute){
if(dojo.isArray(this.labelAttribute)){
for(var i=0;i<this.labelAttribute.length;i++){
if(i>0){
_2b+=" ";
}
_2b+=_2a[this.labelAttribute[i]];
}
return _2b;
}else{
return _2a[this.labelAttribute];
}
}
return _2a.toString();
},getLabelAttributes:function(_2c){
_2c=this._correctReference(_2c);
return dojo.isArray(this.labelAttribute)?this.labelAttribute:[this.labelAttribute];
},sort:function(a,b){
},getIdentity:function(_2d){
if(this.isItem(_2d)){
return _2d[this.idAttribute];
}
throw new Error("Id not found for item");
},getIdentityAttributes:function(_2e){
return [this.idAttribute];
},fetchItemByIdentity:function(_2f){
var id;
if(dojo.isString(_2f)){
id=_2f;
_2f={identity:id,mode:dojox.data.SYNC_MODE};
}else{
if(_2f){
id=_2f["identity"];
}
if(!_2f.mode){
_2f.mode=this.mode;
}
}
if(this.index&&(this.index[id]||this.index["identity"])){
if(_2f.mode==dojox.data.SYNC_MODE){
return this.index[id];
}
if(_2f.onItem){
_2f["onItem"].call(_2f.scope||dojo.global,this.index[id],_2f);
}
return _2f;
}else{
if(_2f.mode==dojox.data.SYNC_MODE){
return false;
}
}
if(_2f.onError){
_2f["onItem"].call(_2f.scope||dojo.global,new Error("Item Not Found: "+id),_2f);
}
return _2f;
},_makeItAnItem:function(_30,_31){
var _32={};
if(this.idAttribute&&!_30[this.idAttribute]){
if(this.requireId){
throw new Error("requireId is enabled, new items must have an id defined to be added");
}
if(this.autoIdentity){
var _33=this.autoIdPrefix+this._autoId++;
_30[this.idAttribute]=_33;
_32.autoId=true;
}
}
if(!_31&&!_31.attribute&&!this.idAttribute&&!_30[this.idAttribute]){
throw new Error("Adding a new item requires, at a minimum, either the pInfo information, including the pInfo.attribute, or an id on the item in the field identified by idAttribute");
}
if(!_31.attribute){
_31.attribute=_30[this.idAttribute];
}
if(_30[this.idAttribute]){
this.index[_30[this.idAttribute]]=_30;
}
this._updateMeta(_30,_32);
this._addReference(_30,{parent:_31.item,attribute:_31.attribute});
this._setDirty(_30);
if(_30[_31.attribute]&&dojo.isArray(_30[_31.attribute])){
for(var i=0;i<_30[_31.attribute].length;i++){
this._makeItAnItem(_30[_31.attribute][i],{item:_30,attribute:_31.attribute});
}
}
return _30;
},newItem:function(_34,_35){
var _36={item:this._data};
if(_35){
if(_35.parent){
_35.item=_35.parent;
}
dojo.mixin(_36,_35);
}
this._makeItAnItem(_34,_36);
_36.oldValue=this._trimItem(_36.item[_36.attribute]);
this._setDirty(_36.item);
if(dojo.isArray(_36.item[_36.attribute])){
_36.item[_36.attribute].push(_34);
}else{
_36.item[_36.attribute]=_34;
}
_36.newValue=_36.item[_36.attribute];
this.onNew(_34,_36);
if(_34[_36.attribute]&&dojo.isArray(_34[_36.attribute])){
for(var i=0;i<_34[_36.attribute].length;i++){
this.onNew(_34[_36.attribute][i],{item:_34,attribute:_36.attribute});
}
}
return _34;
},_addReference:function(_37,_38){
var rid="_ref_"+this._referenceId++;
if(!_37[this.metaLabel]["referenceIds"]){
_37[this.metaLabel]["referenceIds"]=[];
}
_37[this.metaLabel]["referenceIds"].push(rid);
this._references[rid]=_38;
},deleteItem:function(_39){
_39=this._correctReference(_39);
if(this.isItem(_39)){
while(_39[this.metaLabel]["referenceIds"].length>0){
var rid=_39[this.metaLabel]["referenceIds"].pop();
var _3a=this._references[rid];
var _3b=_3a.parent;
var _3c=_3a.attribute;
if(_3b&&_3b[_3c]&&!dojo.isArray(_3b[_3c])){
this._setDirty(_3b);
this.unsetAttribute(_3b,_3c);
delete _3b[_3c];
}
if(dojo.isArray(_3b[_3c])){
var _3d=this._trimItem(_3b[_3c]);
var _3e=false;
for(var i=0;i<_3b[_3c].length&&!_3e;i++){
if(_3b[_3c][i][this.metaLabel]===_39[this.metaLabel]){
_3e=true;
}
}
if(_3e){
this._setDirty(_3b);
var del=_3b[_3c].splice(i-1,1);
delete del;
}
var _3f=this._trimItem(_3b[_3c]);
this.onSet(_3b,_3c,_3d,_3f);
}
delete this._references[rid];
}
this.onDelete(_39);
delete this.index[_39[this.idAttribute]];
}
},_setDirty:function(_40){
if(this.noRevert){
return;
}
for(var i=0;i<this._dirtyItems.length;i++){
if(_40[this.idAttribute]==this._dirtyItems[i][this.idAttribute]){
return;
}
}
this._dirtyItems.push({item:_40,old:this._trimItem(_40)});
this._updateMeta(_40,{isDirty:true});
},setValue:function(_41,_42,_43){
_41=this._correctReference(_41);
this._setDirty(_41);
var old=_41[_42]|undefined;
_41[_42]=_43;
this.onSet(_41,_42,old,_43);
},setValues:function(_44,_45,_46){
_44=this._correctReference(_44);
if(!dojo.isArray(_46)){
throw new Error("setValues expects to be passed an Array object as its value");
}
this._setDirty(_44);
var old=_44[_45]||null;
_44[_45]=_46;
this.onSet(_44,_45,old,_46);
},unsetAttribute:function(_47,_48){
_47=this._correctReference(_47);
this._setDirty(_47);
var old=_47[_48];
delete _47[_48];
this.onSet(_47,_48,old,null);
},save:function(_49){
var _4a=[];
if(!_49){
_49={};
}
while(this._dirtyItems.length>0){
var _4b=this._dirtyItems.pop()["item"];
var t=this._trimItem(_4b);
var d;
switch(_49.format){
case "json":
d=dojo.toJson(t);
break;
case "raw":
default:
d=t;
}
_4a.push(d);
this._markClean(_4b);
}
this.onSave(_4a);
},_markClean:function(_4c){
if(_4c&&_4c[this.metaLabel]&&_4c[this.metaLabel]["isDirty"]){
delete _4c[this.metaLabel]["isDirty"];
}
},revert:function(){
while(this._dirtyItems.length>0){
var d=this._dirtyItems.pop();
this._mixin(d.item,d.old);
}
this.onRevert();
},_mixin:function(_4d,_4e){
var mix;
if(dojo.isObject(_4e)){
if(dojo.isArray(_4e)){
while(_4d.length>0){
_4d.pop();
}
for(var i=0;i<_4e.length;i++){
if(dojo.isObject(_4e[i])){
if(dojo.isArray(_4e[i])){
mix=[];
}else{
mix={};
if(_4e[i][this.metaLabel]&&_4e[i][this.metaLabel]["type"]&&_4e[i][this.metaLabel]["type"]=="reference"){
_4d[i]=this.index[_4e[i][this.idAttribute]];
continue;
}
}
this._mixin(mix,_4e[i]);
_4d.push(mix);
}else{
_4d.push(_4e[i]);
}
}
}else{
for(var i in _4d){
if(i in _4e){
continue;
}
delete _4d[i];
}
for(var i in _4e){
if(dojo.isObject(_4e[i])){
if(dojo.isArray(_4e[i])){
mix=[];
}else{
if(_4e[i][this.metaLabel]&&_4e[i][this.metaLabel]["type"]&&_4e[i][this.metaLabel]["type"]=="reference"){
_4d[i]=this.index[_4e[i][this.idAttribute]];
continue;
}
mix={};
}
this._mixin(mix,_4e[i]);
_4d[i]=mix;
}else{
_4d[i]=_4e[i];
}
}
}
}
},isDirty:function(_4f){
_4f=this._correctReference(_4f);
return _4f&&_4f[this.metaLabel]&&_4f[this.metaLabel]["isDirty"];
},_createReference:function(_50){
var obj={};
obj[this.metaLabel]={type:"reference"};
obj[this.idAttribute]=_50[this.idAttribute];
return obj;
},_trimItem:function(_51){
var _52;
if(dojo.isArray(_51)){
_52=[];
for(var i=0;i<_51.length;i++){
if(dojo.isArray(_51[i])){
_52.push(this._trimItem(_51[i]));
}else{
if(dojo.isObject(_51[i])){
if(_51[i]["getFullYear"]){
_52.push(dojo.date.stamp.toISOString(_51[i]));
}else{
if(_51[i][this.idAttribute]){
_52.push(this._createReference(_51[i]));
}else{
_52.push(this._trimItem(_51[i]));
}
}
}else{
_52.push(_51[i]);
}
}
}
return _52;
}
if(dojo.isObject(_51)){
_52={};
for(var _53 in _51){
if(!_51[_53]){
_52[_53]=undefined;
continue;
}
if(dojo.isArray(_51[_53])){
_52[_53]=this._trimItem(_51[_53]);
}else{
if(dojo.isObject(_51[_53])){
if(_51[_53]["getFullYear"]){
_52[_53]=dojo.date.stamp.toISOString(_51[_53]);
}else{
if(_51[_53][this.idAttribute]){
_52[_53]=this._createReference(_51[_53]);
}else{
_52[_53]=this._trimItem(_51[_53]);
}
}
}else{
_52[_53]=_51[_53];
}
}
}
return _52;
}
},onSet:function(_54,_55,_56,_57){
},onNew:function(_58,_59){
},onDelete:function(_5a){
},onSave:function(_5b){
},onRevert:function(){
}});
dojox.data.jsonPathStore.byId=dojox.data.jsonPathStore.fetchItemByIdentity;
}
