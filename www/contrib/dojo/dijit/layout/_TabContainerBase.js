/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dijit.layout._TabContainerBase"]){
dojo._hasResource["dijit.layout._TabContainerBase"]=true;
dojo.provide("dijit.layout._TabContainerBase");
dojo.require("dijit.layout.StackContainer");
dojo.require("dijit._Templated");
dojo.declare("dijit.layout._TabContainerBase",[dijit.layout.StackContainer,dijit._Templated],{tabPosition:"top",baseClass:"dijitTabContainer",tabStrip:false,nested:false,templateString:dojo.cache("dijit.layout","templates/TabContainer.html","<div class=\"dijitTabContainer\">\n\t<div class=\"dijitTabListWrapper\" dojoAttachPoint=\"tablistNode\"></div>\n\t<div dojoAttachPoint=\"tablistSpacer\" class=\"dijitTabSpacer ${baseClass}-spacer\"></div>\n\t<div class=\"dijitTabPaneWrapper ${baseClass}-container\" dojoAttachPoint=\"containerNode\"></div>\n</div>\n"),postMixInProperties:function(){
this.baseClass+=this.tabPosition.charAt(0).toUpperCase()+this.tabPosition.substr(1).replace(/-.*/,"");
this.srcNodeRef&&dojo.style(this.srcNodeRef,"visibility","hidden");
this.inherited(arguments);
},postCreate:function(){
this.inherited(arguments);
this.tablist=this._makeController(this.tablistNode);
if(!this.doLayout){
dojo.addClass(this.domNode,"dijitTabContainerNoLayout");
}
if(this.nested){
dojo.addClass(this.domNode,"dijitTabContainerNested");
dojo.addClass(this.tablist.containerNode,"dijitTabContainerTabListNested");
dojo.addClass(this.tablistSpacer,"dijitTabContainerSpacerNested");
dojo.addClass(this.containerNode,"dijitTabPaneWrapperNested");
}else{
dojo.addClass(this.domNode,"tabStrip-"+(this.tabStrip?"enabled":"disabled"));
}
},_setupChild:function(_1){
dojo.addClass(_1.domNode,"dijitTabPane");
this.inherited(arguments);
},startup:function(){
if(this._started){
return;
}
this.tablist.startup();
this.inherited(arguments);
},layout:function(){
if(!this._contentBox||typeof (this._contentBox.l)=="undefined"){
return;
}
if(this.doLayout){
var _2=this.tabPosition.replace(/-h/,"");
this.tablist.layoutAlign=_2;
var _3=[this.tablist,{domNode:this.tablistSpacer,layoutAlign:_2},{domNode:this.containerNode,layoutAlign:"client"}];
dijit.layout.layoutChildren(this.domNode,this._contentBox,_3);
this._containerContentBox=dijit.layout.marginBox2contentBox(this.containerNode,_3[2]);
if(this.selectedChildWidget){
if(this.selectedChildWidget.resize){
this.selectedChildWidget.resize(this._containerContentBox);
}
}
}else{
if(this.tablist.resize){
this.tablist.resize({w:dojo.contentBox(this.domNode).w});
}
this.selectedChildWidget.resize();
}
},destroy:function(){
if(this.tablist){
this.tablist.destroy();
}
this.inherited(arguments);
}});
}
