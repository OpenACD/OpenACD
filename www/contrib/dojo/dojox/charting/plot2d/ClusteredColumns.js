/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.charting.plot2d.ClusteredColumns"]){
dojo._hasResource["dojox.charting.plot2d.ClusteredColumns"]=true;
dojo.provide("dojox.charting.plot2d.ClusteredColumns");
dojo.require("dojox.charting.plot2d.common");
dojo.require("dojox.charting.plot2d.Columns");
dojo.require("dojox.lang.functional");
dojo.require("dojox.lang.functional.reversed");
(function(){
var df=dojox.lang.functional,dc=dojox.charting.plot2d.common,_1=df.lambda("item.purgeGroup()");
dojo.declare("dojox.charting.plot2d.ClusteredColumns",dojox.charting.plot2d.Columns,{render:function(_2,_3){
this.dirty=this.isDirty();
if(this.dirty){
dojo.forEach(this.series,_1);
this.cleanGroup();
var s=this.group;
df.forEachRev(this.series,function(_4){
_4.cleanGroup(s);
});
}
var t=this.chart.theme,_5,_6,_7,f,_8,_9,_a,ht=this._hScaler.scaler.getTransformerFromModel(this._hScaler),vt=this._vScaler.scaler.getTransformerFromModel(this._vScaler),_b=Math.max(0,this._vScaler.bounds.lower),_c=vt(_b),_d=this.events();
f=dc.calculateBarSize(this._hScaler.bounds.scale,this.opt,this.series.length);
_8=f.gap;
_9=_a=f.size;
this.resetEvents();
for(var i=0;i<this.series.length;++i){
var _e=this.series[i],_f=_a*i;
if(!this.dirty&&!_e.dirty){
continue;
}
_e.cleanGroup();
var s=_e.group;
if(!_e.fill||!_e.stroke){
_5=_e.dyn.color=new dojo.Color(t.next("color"));
}
_6=_e.stroke?_e.stroke:dc.augmentStroke(t.series.stroke,_5);
_7=_e.fill?_e.fill:dc.augmentFill(t.series.fill,_5);
for(var j=0;j<_e.data.length;++j){
var v=_e.data[j],vv=vt(v),_10=vv-_c,h=Math.abs(_10);
if(_9>=1&&h>=1){
var _11=s.createRect({x:_3.l+ht(j+0.5)+_8+_f,y:_2.height-_3.b-(v>_b?vv:_c),width:_9,height:h}).setFill(_7).setStroke(_6);
_e.dyn.fill=_11.getFill();
_e.dyn.stroke=_11.getStroke();
if(_d){
var o={element:"column",index:j,run:_e,plot:this,hAxis:this.hAxis||null,vAxis:this.vAxis||null,shape:_11,x:j+0.5,y:v};
this._connectEvents(_11,o);
}
}
}
_e.dirty=false;
}
this.dirty=false;
return this;
}});
})();
}
