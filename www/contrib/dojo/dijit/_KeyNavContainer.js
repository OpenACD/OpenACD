/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dijit._KeyNavContainer"]){
dojo._hasResource["dijit._KeyNavContainer"]=true;
dojo.provide("dijit._KeyNavContainer");
dojo.require("dijit._Container");
dojo.declare("dijit._KeyNavContainer",dijit._Container,{tabIndex:"0",_keyNavCodes:{},connectKeyNavHandlers:function(_1,_2){
var _3=this._keyNavCodes={};
var _4=dojo.hitch(this,this.focusPrev);
var _5=dojo.hitch(this,this.focusNext);
dojo.forEach(_1,function(_6){
_3[_6]=_4;
});
dojo.forEach(_2,function(_7){
_3[_7]=_5;
});
this.connect(this.domNode,"onkeypress","_onContainerKeypress");
this.connect(this.domNode,"onfocus","_onContainerFocus");
},startupKeyNavChildren:function(){
dojo.forEach(this.getChildren(),dojo.hitch(this,"_startupChild"));
},addChild:function(_8,_9){
dijit._KeyNavContainer.superclass.addChild.apply(this,arguments);
this._startupChild(_8);
},focus:function(){
this.focusFirstChild();
},focusFirstChild:function(){
this.focusChild(this._getFirstFocusableChild());
},focusNext:function(){
if(this.focusedChild&&this.focusedChild.hasNextFocalNode&&this.focusedChild.hasNextFocalNode()){
this.focusedChild.focusNext();
return;
}
var _a=this._getNextFocusableChild(this.focusedChild,1);
if(_a.getFocalNodes){
this.focusChild(_a,_a.getFocalNodes()[0]);
}else{
this.focusChild(_a);
}
},focusPrev:function(){
if(this.focusedChild&&this.focusedChild.hasPrevFocalNode&&this.focusedChild.hasPrevFocalNode()){
this.focusedChild.focusPrev();
return;
}
var _b=this._getNextFocusableChild(this.focusedChild,-1);
if(_b.getFocalNodes){
var _c=_b.getFocalNodes();
this.focusChild(_b,_c[_c.length-1]);
}else{
this.focusChild(_b);
}
},focusChild:function(_d,_e){
if(_d){
if(this.focusedChild&&_d!==this.focusedChild){
this._onChildBlur(this.focusedChild);
}
this.focusedChild=_d;
if(_e&&_d.focusFocalNode){
_d.focusFocalNode(_e);
}else{
_d.focus();
}
}
},_startupChild:function(_f){
if(_f.getFocalNodes){
dojo.forEach(_f.getFocalNodes(),function(_10){
dojo.attr(_10,"tabIndex",-1);
this._connectNode(_10);
},this);
}else{
var _11=_f.focusNode||_f.domNode;
if(_f.isFocusable()){
dojo.attr(_11,"tabIndex",-1);
}
this._connectNode(_11);
}
},_connectNode:function(_12){
this.connect(_12,"onfocus","_onNodeFocus");
this.connect(_12,"onblur","_onNodeBlur");
},_onContainerFocus:function(evt){
if(evt.target!==this.domNode){
return;
}
this.focusFirstChild();
dojo.attr(this.domNode,"tabIndex","-1");
},_onBlur:function(evt){
if(this.tabIndex){
dojo.attr(this.domNode,"tabIndex",this.tabIndex);
}
this.inherited(arguments);
},_onContainerKeypress:function(evt){
if(evt.ctrlKey||evt.altKey){
return;
}
var _13=this._keyNavCodes[evt.charOrCode];
if(_13){
_13();
dojo.stopEvent(evt);
}
},_onNodeFocus:function(evt){
var _14=dijit.getEnclosingWidget(evt.target);
if(_14&&_14.isFocusable()){
this.focusedChild=_14;
}
dojo.stopEvent(evt);
},_onNodeBlur:function(evt){
dojo.stopEvent(evt);
},_onChildBlur:function(_15){
},_getFirstFocusableChild:function(){
return this._getNextFocusableChild(null,1);
},_getNextFocusableChild:function(_16,dir){
if(_16){
_16=this._getSiblingOfChild(_16,dir);
}
var _17=this.getChildren();
for(var i=0;i<_17.length;i++){
if(!_16){
_16=_17[(dir>0)?0:(_17.length-1)];
}
if(_16.isFocusable()){
return _16;
}
_16=this._getSiblingOfChild(_16,dir);
}
return null;
}});
}
