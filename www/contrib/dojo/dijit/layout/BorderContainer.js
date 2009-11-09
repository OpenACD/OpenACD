/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dijit.layout.BorderContainer"]){
dojo._hasResource["dijit.layout.BorderContainer"]=true;
dojo.provide("dijit.layout.BorderContainer");
dojo.require("dijit.layout._LayoutWidget");
dojo.require("dojo.cookie");
dojo.declare("dijit.layout.BorderContainer",dijit.layout._LayoutWidget,{design:"headline",gutters:true,liveSplitters:true,persist:false,baseClass:"dijitBorderContainer",_splitterClass:"dijit.layout._Splitter",postMixInProperties:function(){
if(!this.gutters){
this.baseClass+="NoGutter";
}
this.inherited(arguments);
},postCreate:function(){
this.inherited(arguments);
this._splitters={};
this._splitterThickness={};
},startup:function(){
if(this._started){
return;
}
dojo.forEach(this.getChildren(),this._setupChild,this);
this.inherited(arguments);
},_setupChild:function(_1){
var _2=_1.region;
if(_2){
this.inherited(arguments);
dojo.addClass(_1.domNode,this.baseClass+"Pane");
var _3=this.isLeftToRight();
if(_2=="leading"){
_2=_3?"left":"right";
}
if(_2=="trailing"){
_2=_3?"right":"left";
}
this["_"+_2]=_1.domNode;
this["_"+_2+"Widget"]=_1;
if((_1.splitter||this.gutters)&&!this._splitters[_2]){
var _4=dojo.getObject(_1.splitter?this._splitterClass:"dijit.layout._Gutter");
var _5={left:"right",right:"left",top:"bottom",bottom:"top",leading:"trailing",trailing:"leading"};
var _6=new _4({container:this,child:_1,region:_2,oppNode:this["_"+_5[_1.region]],live:this.liveSplitters});
_6.isSplitter=true;
this._splitters[_2]=_6.domNode;
dojo.place(this._splitters[_2],_1.domNode,"after");
_6.startup();
}
_1.region=_2;
}
},_computeSplitterThickness:function(_7){
this._splitterThickness[_7]=this._splitterThickness[_7]||dojo.marginBox(this._splitters[_7])[(/top|bottom/.test(_7)?"h":"w")];
},layout:function(){
for(var _8 in this._splitters){
this._computeSplitterThickness(_8);
}
this._layoutChildren();
},addChild:function(_9,_a){
this.inherited(arguments);
if(this._started){
this._layoutChildren();
}
},removeChild:function(_b){
var _c=_b.region;
var _d=this._splitters[_c];
if(_d){
dijit.byNode(_d).destroy();
delete this._splitters[_c];
delete this._splitterThickness[_c];
}
this.inherited(arguments);
delete this["_"+_c];
delete this["_"+_c+"Widget"];
if(this._started){
this._layoutChildren(_b.region);
}
dojo.removeClass(_b.domNode,this.baseClass+"Pane");
},getChildren:function(){
return dojo.filter(this.inherited(arguments),function(_e){
return !_e.isSplitter;
});
},getSplitter:function(_f){
var _10=this._splitters[_f];
return _10?dijit.byNode(_10):null;
},resize:function(_11,_12){
if(!this.cs||!this.pe){
var _13=this.domNode;
this.cs=dojo.getComputedStyle(_13);
this.pe=dojo._getPadExtents(_13,this.cs);
this.pe.r=dojo._toPixelValue(_13,this.cs.paddingRight);
this.pe.b=dojo._toPixelValue(_13,this.cs.paddingBottom);
dojo.style(_13,"padding","0px");
}
this.inherited(arguments);
},_layoutChildren:function(_14){
if(!this._borderBox||!this._borderBox.h){
return;
}
var _15=(this.design=="sidebar");
var _16=0,_17=0,_18=0,_19=0;
var _1a={},_1b={},_1c={},_1d={},_1e=(this._center&&this._center.style)||{};
var _1f=/left|right/.test(_14);
var _20=!_14||(!_1f&&!_15);
var _21=!_14||(_1f&&_15);
if(this._top){
_1a=_21&&this._top.style;
_16=dojo.marginBox(this._top).h;
}
if(this._left){
_1b=_20&&this._left.style;
_18=dojo.marginBox(this._left).w;
}
if(this._right){
_1c=_20&&this._right.style;
_19=dojo.marginBox(this._right).w;
}
if(this._bottom){
_1d=_21&&this._bottom.style;
_17=dojo.marginBox(this._bottom).h;
}
var _22=this._splitters;
var _23=_22.top,_24=_22.bottom,_25=_22.left,_26=_22.right;
var _27=this._splitterThickness;
var _28=_27.top||0,_29=_27.left||0,_2a=_27.right||0,_2b=_27.bottom||0;
if(_29>50||_2a>50){
setTimeout(dojo.hitch(this,function(){
this._splitterThickness={};
for(var _2c in this._splitters){
this._computeSplitterThickness(_2c);
}
this._layoutChildren();
}),50);
return false;
}
var pe=this.pe;
var _2d={left:(_15?_18+_29:0)+pe.l+"px",right:(_15?_19+_2a:0)+pe.r+"px"};
if(_23){
dojo.mixin(_23.style,_2d);
_23.style.top=_16+pe.t+"px";
}
if(_24){
dojo.mixin(_24.style,_2d);
_24.style.bottom=_17+pe.b+"px";
}
_2d={top:(_15?0:_16+_28)+pe.t+"px",bottom:(_15?0:_17+_2b)+pe.b+"px"};
if(_25){
dojo.mixin(_25.style,_2d);
_25.style.left=_18+pe.l+"px";
}
if(_26){
dojo.mixin(_26.style,_2d);
_26.style.right=_19+pe.r+"px";
}
dojo.mixin(_1e,{top:pe.t+_16+_28+"px",left:pe.l+_18+_29+"px",right:pe.r+_19+_2a+"px",bottom:pe.b+_17+_2b+"px"});
var _2e={top:_15?pe.t+"px":_1e.top,bottom:_15?pe.b+"px":_1e.bottom};
dojo.mixin(_1b,_2e);
dojo.mixin(_1c,_2e);
_1b.left=pe.l+"px";
_1c.right=pe.r+"px";
_1a.top=pe.t+"px";
_1d.bottom=pe.b+"px";
if(_15){
_1a.left=_1d.left=_18+_29+pe.l+"px";
_1a.right=_1d.right=_19+_2a+pe.r+"px";
}else{
_1a.left=_1d.left=pe.l+"px";
_1a.right=_1d.right=pe.r+"px";
}
var _2f=this._borderBox.h-pe.t-pe.b,_30=_2f-(_16+_28+_17+_2b),_31=_15?_2f:_30;
var _32=this._borderBox.w-pe.l-pe.r,_33=_32-(_18+_29+_19+_2a),_34=_15?_33:_32;
var dim={top:{w:_34,h:_16},bottom:{w:_34,h:_17},left:{w:_18,h:_31},right:{w:_19,h:_31},center:{h:_30,w:_33}};
var _35=dojo.isIE<8||(dojo.isIE&&dojo.isQuirks)||dojo.some(this.getChildren(),function(_36){
return _36.domNode.tagName=="TEXTAREA"||_36.domNode.tagName=="INPUT";
});
if(_35){
var _37=function(_38,_39,_3a){
if(_38){
(_38.resize?_38.resize(_39,_3a):dojo.marginBox(_38.domNode,_39));
}
};
if(_25){
_25.style.height=_31;
}
if(_26){
_26.style.height=_31;
}
_37(this._leftWidget,{h:_31},dim.left);
_37(this._rightWidget,{h:_31},dim.right);
if(_23){
_23.style.width=_34;
}
if(_24){
_24.style.width=_34;
}
_37(this._topWidget,{w:_34},dim.top);
_37(this._bottomWidget,{w:_34},dim.bottom);
_37(this._centerWidget,dim.center);
}else{
var _3b={};
if(_14){
_3b[_14]=_3b.center=true;
if(/top|bottom/.test(_14)&&this.design!="sidebar"){
_3b.left=_3b.right=true;
}else{
if(/left|right/.test(_14)&&this.design=="sidebar"){
_3b.top=_3b.bottom=true;
}
}
}
dojo.forEach(this.getChildren(),function(_3c){
if(_3c.resize&&(!_14||_3c.region in _3b)){
_3c.resize(null,dim[_3c.region]);
}
},this);
}
},destroy:function(){
for(var _3d in this._splitters){
var _3e=this._splitters[_3d];
dijit.byNode(_3e).destroy();
dojo.destroy(_3e);
}
delete this._splitters;
delete this._splitterThickness;
this.inherited(arguments);
}});
dojo.extend(dijit._Widget,{region:"",splitter:false,minSize:0,maxSize:Infinity});
dojo.require("dijit._Templated");
dojo.declare("dijit.layout._Splitter",[dijit._Widget,dijit._Templated],{live:true,templateString:"<div class=\"dijitSplitter\" dojoAttachEvent=\"onkeypress:_onKeyPress,onmousedown:_startDrag,onmouseenter:_onMouse,onmouseleave:_onMouse\" tabIndex=\"0\" waiRole=\"separator\"><div class=\"dijitSplitterThumb\"></div></div>",postCreate:function(){
this.inherited(arguments);
this.horizontal=/top|bottom/.test(this.region);
dojo.addClass(this.domNode,"dijitSplitter"+(this.horizontal?"H":"V"));
this._factor=/top|left/.test(this.region)?1:-1;
this._minSize=this.child.minSize;
this.child.domNode._recalc=true;
this.connect(this.container,"resize",function(){
this.child.domNode._recalc=true;
});
this._cookieName=this.container.id+"_"+this.region;
if(this.container.persist){
var _3f=dojo.cookie(this._cookieName);
if(_3f){
this.child.domNode.style[this.horizontal?"height":"width"]=_3f;
}
}
},_computeMaxSize:function(){
var dim=this.horizontal?"h":"w",_40=this.container._splitterThickness[this.region];
var _41=dojo.contentBox(this.container.domNode)[dim]-(this.oppNode?dojo.marginBox(this.oppNode)[dim]:0)-20-_40*2;
this._maxSize=Math.min(this.child.maxSize,_41);
},_startDrag:function(e){
if(this.child.domNode._recalc){
this._computeMaxSize();
this.child.domNode._recalc=false;
}
if(!this.cover){
this.cover=dojo.doc.createElement("div");
dojo.addClass(this.cover,"dijitSplitterCover");
dojo.place(this.cover,this.child.domNode,"after");
}
dojo.addClass(this.cover,"dijitSplitterCoverActive");
if(this.fake){
dojo.destroy(this.fake);
}
if(!(this._resize=this.live)){
(this.fake=this.domNode.cloneNode(true)).removeAttribute("id");
dojo.addClass(this.domNode,"dijitSplitterShadow");
dojo.place(this.fake,this.domNode,"after");
}
dojo.addClass(this.domNode,"dijitSplitterActive");
dojo.addClass(this.domNode,"dijitSplitter"+(this.horizontal?"H":"V")+"Active");
if(this.fake){
dojo.removeClass(this.fake,"dijitSplitterHover");
dojo.removeClass(this.fake,"dijitSplitter"+(this.horizontal?"H":"V")+"Hover");
}
var _42=this._factor,max=this._maxSize,min=this._minSize||20,_43=this.horizontal,_44=_43?"pageY":"pageX",_45=e[_44],_46=this.domNode.style,dim=_43?"h":"w",_47=dojo.marginBox(this.child.domNode)[dim],_48=this.region,_49=parseInt(this.domNode.style[_48],10),_4a=this._resize,mb={},_4b=this.child.domNode,_4c=dojo.hitch(this.container,this.container._layoutChildren),de=dojo.doc.body;
this._handlers=(this._handlers||[]).concat([dojo.connect(de,"onmousemove",this._drag=function(e,_4d){
var _4e=e[_44]-_45,_4f=_42*_4e+_47,_50=Math.max(Math.min(_4f,max),min);
if(_4a||_4d){
mb[dim]=_50;
dojo.marginBox(_4b,mb);
_4c(_48);
}
_46[_48]=_42*_4e+_49+(_50-_4f)+"px";
}),dojo.connect(dojo.doc,"ondragstart",dojo.stopEvent),dojo.connect(dojo.body(),"onselectstart",dojo.stopEvent),dojo.connect(de,"onmouseup",this,"_stopDrag")]);
dojo.stopEvent(e);
},_onMouse:function(e){
var o=(e.type=="mouseover"||e.type=="mouseenter");
dojo.toggleClass(this.domNode,"dijitSplitterHover",o);
dojo.toggleClass(this.domNode,"dijitSplitter"+(this.horizontal?"H":"V")+"Hover",o);
},_stopDrag:function(e){
try{
if(this.cover){
dojo.removeClass(this.cover,"dijitSplitterCoverActive");
}
if(this.fake){
dojo.destroy(this.fake);
}
dojo.removeClass(this.domNode,"dijitSplitterActive");
dojo.removeClass(this.domNode,"dijitSplitter"+(this.horizontal?"H":"V")+"Active");
dojo.removeClass(this.domNode,"dijitSplitterShadow");
this._drag(e);
this._drag(e,true);
}
finally{
this._cleanupHandlers();
if(this.oppNode){
this.oppNode._recalc=true;
}
delete this._drag;
}
if(this.container.persist){
dojo.cookie(this._cookieName,this.child.domNode.style[this.horizontal?"height":"width"],{expires:365});
}
},_cleanupHandlers:function(){
dojo.forEach(this._handlers,dojo.disconnect);
delete this._handlers;
},_onKeyPress:function(e){
if(this.child.domNode._recalc){
this._computeMaxSize();
this.child.domNode._recalc=false;
}
this._resize=true;
var _51=this.horizontal;
var _52=1;
var dk=dojo.keys;
switch(e.charOrCode){
case _51?dk.UP_ARROW:dk.LEFT_ARROW:
_52*=-1;
case _51?dk.DOWN_ARROW:dk.RIGHT_ARROW:
break;
default:
return;
}
var _53=dojo.marginBox(this.child.domNode)[_51?"h":"w"]+this._factor*_52;
var mb={};
mb[this.horizontal?"h":"w"]=Math.max(Math.min(_53,this._maxSize),this._minSize);
dojo.marginBox(this.child.domNode,mb);
if(this.oppNode){
this.oppNode._recalc=true;
}
this.container._layoutChildren(this.region);
dojo.stopEvent(e);
},destroy:function(){
this._cleanupHandlers();
delete this.child;
delete this.container;
delete this.cover;
delete this.fake;
this.inherited(arguments);
}});
dojo.declare("dijit.layout._Gutter",[dijit._Widget,dijit._Templated],{templateString:"<div class=\"dijitGutter\" waiRole=\"presentation\"></div>",postCreate:function(){
this.horizontal=/top|bottom/.test(this.region);
dojo.addClass(this.domNode,"dijitGutter"+(this.horizontal?"H":"V"));
}});
}
