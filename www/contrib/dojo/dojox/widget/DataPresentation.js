/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.widget.DataPresentation"]){
dojo._hasResource["dojox.widget.DataPresentation"]=true;
dojo.provide("dojox.widget.DataPresentation");
dojo.experimental("dojox.widget.DataPresentation");
dojo.require("dojox.grid.DataGrid");
dojo.require("dojox.charting.Chart2D");
dojo.require("dojox.charting.widget.Legend");
dojo.require("dojox.charting.action2d.Tooltip");
dojo.require("dojox.charting.action2d.Highlight");
dojo.require("dojo.colors");
dojo.require("dojo.data.ItemFileWriteStore");
(function(){
var _1=function(_2,_3,_4,_5,_6){
var _7=[],_8=_3;
_7[0]={value:0,text:""};
var _9=_2.slice(0);
if(_4){
_9.reverse();
}
var _a=_9.length;
if((_5!=="ClusteredBars")&&(_5!=="StackedBars")){
var _b=_6.offsetWidth;
var _c=(""+_9[0]).length*_9.length*7;
if(_8==1){
for(var z=1;z<500;++z){
if((_c/z)<_b){
break;
}
++_8;
}
}
}
for(var i=0;i<_a;i++){
if(i%_8==0){
_7.push({value:(i+1),text:_9[i]});
}else{
_7.push({value:(i+1),text:""});
}
}
_7.push({value:(_a+1),text:""});
return _7;
};
var _d=function(_e,_f){
var _10={vertical:false,labels:_f,min:0,max:_f.length-1,majorTickStep:1,minorTickStep:1};
if((_e==="ClusteredBars")||(_e==="StackedBars")){
_10.vertical=true;
}
if((_e==="Lines")||(_e==="Areas")||(_e==="StackedAreas")){
_10.min++;
_10.max--;
}
return _10;
};
var _11=function(_12,_13,_14,_15){
var _16={vertical:true,fixLower:"major",fixUpper:"major"};
if(_13==="secondary"){
_16.leftBottom=false;
}
if((_12==="ClusteredBars")||(_12==="StackedBars")){
_16.vertical=false;
}
if((_12==="ClusteredBars")||(_12==="StackedBars")||(_12==="ClusteredColumns")||(_12==="Hybrid")){
_16.natural=true;
_16.min=0;
}
if((_12==="Areas")||(_12==="Lines")){
var mts=10,_17=_15-_14,_18=100;
if(_17>100){
mts=20;
}
while(_17>5*_18){
mts=_18;
_18*=10;
}
_16.min=_14-(_14%mts);
_16.max=_15-(_15%mts)+mts;
_16.majorTickStep=mts;
}
return _16;
};
var _19=function(_1a,_1b){
var _1c={type:_1a,hAxis:"independent",vAxis:"dependent-"+_1b,gap:4,lines:false,areas:false,markers:false};
if((_1a==="ClusteredBars")||(_1a==="StackedBars")){
_1c.hAxis=_1c.vAxis;
_1c.vAxis="independent";
}
if((_1a==="Lines")||(_1a==="Hybrid-Lines")||(_1a==="Areas")||(_1a==="StackedAreas")){
_1c.lines=true;
}
if((_1a==="Areas")||(_1a==="StackedAreas")){
_1c.areas=true;
}
if(_1a==="Lines"){
_1c.markers=true;
}
if(_1a==="Hybrid-Lines"){
_1c.shadows={dx:2,dy:2,dw:2};
_1c.type="Lines";
}
if(_1a==="Hybrid-ClusteredColumns"){
_1c.type="ClusteredColumns";
}
return _1c;
};
var _1d=function(_1e,_1f,_20,_21,_22,_23,_24,_25,_26){
var _27=_1f;
if(!_27){
_1e.innerHTML="";
_27=new dojox.charting.Chart2D(_1e);
}
if(_23){
_23._clone=function(){
var _28=new dojox.charting.Theme({chart:this.chart,plotarea:this.plotarea,axis:this.axis,series:this.series,marker:this.marker,antiAlias:this.antiAlias,assignColors:this.assignColors,assignMarkers:this.assigneMarkers,colors:dojo.delegate(this.colors)});
_28.markers=this.markers;
_28._buildMarkerArray();
return _28;
};
_27.setTheme(_23);
}
var _29=_1(_24.series_data[0],_22,_21,_20,_1e);
var _2a={};
var _2b=0;
var _2c=10000000;
var _2d=_24.series_name.length;
for(var i=0;i<_2d;i++){
if(_24.series_chart[i]&&(_24.series_data[i].length>0)){
var _2e=_20;
var _2f=_24.series_axis[i];
if(_2e=="Hybrid"){
if(_24.series_charttype[i]=="line"){
_2e="Hybrid-Lines";
}else{
_2e="Hybrid-ClusteredColumns";
}
}
if(!_2a[_2f]){
_2a[_2f]={};
}
if(!_2a[_2f][_2e]){
var _30=_2f+"-"+_2e;
_27.addPlot(_30,_19(_2e,_2f));
new dojox.charting.action2d.Tooltip(_27,_30);
if((_2e!=="Lines")&&(_2e!=="Hybrid-Lines")){
new dojox.charting.action2d.Highlight(_27,_30);
}
_2a[_2f][_2e]=true;
}
var _31=[];
var _32=_24.series_data[i].length;
for(var j=0;j<_32;j++){
var val=_24.series_data[i][j];
_31.push(val);
if(val>_2b){
_2b=val;
}
if(val<_2c){
_2c=val;
}
}
if(_21){
_31.reverse();
}
_27.addSeries(_24.series_name[i],_31,{plot:_2f+"-"+_2e});
}
}
_27.addAxis("independent",_d(_20,_29));
_27.addAxis("dependent-primary",_11(_20,"primary",_2c,_2b));
_27.addAxis("dependent-secondary",_11(_20,"secondary",_2c,_2b));
_27.render();
return _27;
};
var _33=function(_34,_35,_36,_37){
var _38=_35;
if(!_38){
if(_37){
_38=new dojox.charting.widget.Legend({chart:_36,horizontal:false},_34);
}else{
_38=new dojox.charting.widget.Legend({chart:_36,vertical:false},_34);
}
}
return _38;
};
var _39=function(_3a,_3b,_3c,_3d,_3e){
var _3f=_3b||new dojox.grid.DataGrid({},_3a);
_3f.startup();
_3f.setStore(_3c,_3d,_3e);
var _40=[];
for(var ser=0;ser<_3c.series_name.length;ser++){
if(_3c.series_grid[ser]&&(_3c.series_data[ser].length>0)){
_40.push({field:"data."+ser,name:_3c.series_name[ser],width:"auto",formatter:_3c.series_gridformatter[ser]});
}
}
_3f.setStructure(_40);
_3f.render();
return _3f;
};
var _41=function(_42,_43){
if(_43.title){
_42.innerHTML=_43.title;
}
};
var _44=function(_45,_46){
if(_46.footer){
_45.innerHTML=_46.footer;
}
};
var _47=function(_48,_49){
var _4a=_48;
if(_49){
var _4b=_49.split(/[.\[\]]+/);
for(var _4c in _4b){
if(_4a){
_4a=_4a[_4b[_4c]];
}
}
}
return _4a;
};
dojo.declare("dojox.widget.DataPresentation",null,{type:"chart",chartType:"clusteredBars",reverse:false,labelMod:1,legendVertical:false,constructor:function(_4d,_4e){
dojo.mixin(this,_4e);
this.domNode=dojo.byId(_4d);
this[this.type+"Node"]=this.domNode;
if(typeof this.theme=="string"){
this.theme=dojo.getObject(this.theme);
}
this.chartNode=dojo.byId(this.chartNode);
this.legendNode=dojo.byId(this.legendNode);
this.gridNode=dojo.byId(this.gridNode);
this.titleNode=dojo.byId(this.titleNode);
this.footerNode=dojo.byId(this.footerNode);
if(this.url){
this.setURL(null,this.refreshInterval);
}else{
if(this.data){
this.setData(null,this.refreshInterval);
}else{
this.setStore();
}
}
},setURL:function(url,_4f){
if(_4f){
this.cancelRefresh();
}
this.url=url||this.url;
this.refreshInterval=_4f||this.refreshInterval;
var me=this;
dojo.xhrGet({url:this.url,handleAs:"json-comment-optional",load:function(_50,_51){
me.setData(_50);
},error:function(xhr,_52){
if(me.urlError&&(typeof me.urlError=="function")){
me.urlError(xhr,_52);
}
}});
if(_4f&&(this.refreshInterval>0)){
this.refreshIntervalPending=setInterval(function(){
me.setURL();
},this.refreshInterval);
}
},setData:function(_53,_54){
if(_54){
this.cancelRefresh();
}
this.data=_53||this.data;
this.refreshInterval=_54||this.refreshInterval;
var _55=(typeof this.series=="function")?this.series(this.data):this.series;
var _56=[];
var _57=[];
var _58=[];
var _59=[];
var _5a=[];
var _5b=[];
var _5c=[];
var _5d=[];
var _5e=0;
for(var ser=0;ser<_55.length;ser++){
_56[ser]=_47(this.data,_55[ser].datapoints);
if(_56[ser]&&(_56[ser].length>_5e)){
_5e=_56[ser].length;
}
_57[ser]=[];
_58[ser]=_55[ser].name||(_55[ser].namefield?_47(this.data,_55[ser].namefield):null)||("series "+ser);
_59[ser]=(_55[ser].chart!==false);
_5a[ser]=_55[ser].charttype||"bar";
_5b[ser]=_55[ser].axis||"primary";
_5c[ser]=(_55[ser].grid!==false);
_5d[ser]=_55[ser].gridformatter;
}
var _5f,_60,_61,_62;
var _63=[];
for(_5f=0;_5f<_5e;_5f++){
_60={index:_5f};
for(ser=0;ser<_55.length;ser++){
if(_56[ser]&&(_56[ser].length>_5f)){
_61=_47(_56[ser][_5f],_55[ser].field);
if(_59[ser]){
_62=parseFloat(_61);
if(!isNaN(_62)){
_61=_62;
}
}
_60["data."+ser]=_61;
_57[ser].push(_61);
}
}
_63.push(_60);
}
if(_5e<=0){
_63.push({index:0});
}
var _64=new dojo.data.ItemFileWriteStore({data:{identifier:"index",items:_63}});
if(this.data.title){
_64.title=this.data.title;
}
if(this.data.footer){
_64.footer=this.data.footer;
}
_64.series_data=_57;
_64.series_name=_58;
_64.series_chart=_59;
_64.series_charttype=_5a;
_64.series_axis=_5b;
_64.series_grid=_5c;
_64.series_gridformatter=_5d;
this.setPreparedStore(_64);
if(_54&&(this.refreshInterval>0)){
var me=this;
this.refreshIntervalPending=setInterval(function(){
me.setData();
},this.refreshInterval);
}
},refresh:function(){
if(this.url){
this.setURL(this.url,this.refreshInterval);
}else{
if(this.data){
this.setData(this.data,this.refreshInterval);
}
}
},cancelRefresh:function(){
if(this.refreshIntervalPending){
clearInterval(this.refreshIntervalPending);
this.refreshIntervalPending=undefined;
}
},setStore:function(_65,_66,_67){
this.setPreparedStore(_65,_66,_67);
},setPreparedStore:function(_68,_69,_6a){
this.preparedstore=_68||this.store;
this.query=_69||this.query;
this.queryOptions=_6a||this.queryOptions;
if(this.preparedstore){
if(this.chartNode){
this.chartWidget=_1d(this.chartNode,this.chartWidget,this.chartType,this.reverse,this.labelMod,this.theme,this.preparedstore,this.query,this,_6a);
}
if(this.legendNode){
this.legendWidget=_33(this.legendNode,this.legendWidget,this.chartWidget,this.legendVertical);
}
if(this.gridNode){
this.gridWidget=_39(this.gridNode,this.gridWidget,this.preparedstore,this.query,this.queryOptions);
}
if(this.titleNode){
_41(this.titleNode,this.preparedstore);
}
if(this.footerNode){
_44(this.footerNode,this.preparedstore);
}
}
},getChartWidget:function(){
return this.chartWidget;
},getGridWidget:function(){
return this.gridWidget;
},destroy:function(){
this.cancelRefresh();
if(this.chartWidget){
this.chartWidget.destroy();
this.chartWidget=undefined;
}
if(this.legendWidget){
this.legendWidget=undefined;
}
if(this.gridWidget){
this.gridWidget=undefined;
}
if(this.chartNode){
this.chartNode.innerHTML="";
}
if(this.legendNode){
this.legendNode.innerHTML="";
}
if(this.gridNode){
this.gridNode.innerHTML="";
}
if(this.titleNode){
this.titleNode.innerHTML="";
}
if(this.footerNode){
this.footerNode.innerHTML="";
}
}});
})();
}
