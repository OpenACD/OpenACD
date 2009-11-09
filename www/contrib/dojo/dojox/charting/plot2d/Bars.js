/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.charting.plot2d.Bars"]){
dojo._hasResource["dojox.charting.plot2d.Bars"]=true;
dojo.provide("dojox.charting.plot2d.Bars");
dojo.require("dojox.charting.plot2d.common");
dojo.require("dojox.charting.plot2d.Base");
dojo.require("dojox.lang.utils");
dojo.require("dojox.lang.functional");
dojo.require("dojox.lang.functional.reversed");
(function(){
var df=dojox.lang.functional,du=dojox.lang.utils,dc=dojox.charting.plot2d.common,_1=df.lambda("item.purgeGroup()");
dojo.declare("dojox.charting.plot2d.Bars",dojox.charting.plot2d.Base,{defaultParams:{hAxis:"x",vAxis:"y",gap:0,shadows:null},optionalParams:{minBarSize:1,maxBarSize:1},constructor:function(_2,_3){
this.opt=dojo.clone(this.defaultParams);
du.updateWithObject(this.opt,_3);
du.updateWithPattern(this.opt,_3,this.optionalParams);
this.series=[];
this.hAxis=this.opt.hAxis;
this.vAxis=this.opt.vAxis;
},calculateAxes:function(_4){
var _5=dc.collectSimpleStats(this.series),t;
_5.hmin-=0.5;
_5.hmax+=0.5;
t=_5.hmin,_5.hmin=_5.vmin,_5.vmin=t;
t=_5.hmax,_5.hmax=_5.vmax,_5.vmax=t;
this._calc(_4,_5);
return this;
},render:function(_6,_7){
this.dirty=this.isDirty();
if(this.dirty){
dojo.forEach(this.series,_1);
this.cleanGroup();
var s=this.group;
df.forEachRev(this.series,function(_8){
_8.cleanGroup(s);
});
}
var t=this.chart.theme,_9,_a,_b,f,_c,_d,ht=this._hScaler.scaler.getTransformerFromModel(this._hScaler),vt=this._vScaler.scaler.getTransformerFromModel(this._vScaler),_e=Math.max(0,this._hScaler.bounds.lower),_f=ht(_e),_10=this.events();
f=dc.calculateBarSize(this._vScaler.bounds.scale,this.opt);
_c=f.gap;
_d=f.size;
this.resetEvents();
for(var i=this.series.length-1;i>=0;--i){
var run=this.series[i];
if(!this.dirty&&!run.dirty){
continue;
}
run.cleanGroup();
var s=run.group;
if(!run.fill||!run.stroke){
_9=run.dyn.color=new dojo.Color(t.next("color"));
}
_a=run.stroke?run.stroke:dc.augmentStroke(t.series.stroke,_9);
_b=run.fill?run.fill:dc.augmentFill(t.series.fill,_9);
for(var j=0;j<run.data.length;++j){
var v=run.data[j],hv=ht(v),_11=hv-_f,w=Math.abs(_11);
if(w>=1&&_d>=1){
var _12=s.createRect({x:_7.l+(v<_e?hv:_f),y:_6.height-_7.b-vt(j+1.5)+_c,width:w,height:_d}).setFill(_b).setStroke(_a);
run.dyn.fill=_12.getFill();
run.dyn.stroke=_12.getStroke();
if(_10){
var o={element:"bar",index:j,run:run,plot:this,hAxis:this.hAxis||null,vAxis:this.vAxis||null,shape:_12,x:v,y:j+1.5};
this._connectEvents(_12,o);
}
}
}
run.dirty=false;
}
this.dirty=false;
return this;
}});
})();
}
