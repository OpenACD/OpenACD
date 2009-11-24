/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.editor.plugins.Breadcrumb"]){
dojo._hasResource["dojox.editor.plugins.Breadcrumb"]=true;
dojo.provide("dojox.editor.plugins.Breadcrumb");
dojo.require("dijit._editor._Plugin");
dojo.require("dijit._editor.range");
dojo.require("dojo.i18n");
dojo.require("dojo.string");
dojo.require("dijit.Toolbar");
dojo.require("dijit.form.Button");
dojo.require("dijit._editor.selection");
dojo.require("dijit.Menu");
dojo.require("dijit.MenuItem");
dojo.require("dijit.MenuSeparator");
dojo.experimental("dojox.editor.plugins.Breadcrumb");
dojo.requireLocalization("dojox.editor.plugins","Breadcrumb",null,"ROOT");
dojo.declare("dojox.editor.plugins._BreadcrumbMenuTitle",[dijit._Widget,dijit._Templated,dijit._Contained],{templateString:"<tr><td dojoAttachPoint=\"title\" colspan=\"4\" class=\"dijitToolbar\" style=\"font-weight: bold; padding: 3px;\"></td></tr>",menuTitle:"",postCreate:function(){
dojo.setSelectable(this.domNode,false);
var _1=this.id+"_text";
dijit.setWaiState(this.domNode,"labelledby",_1);
},_setMenuTitleAttr:function(_2){
this.title.innerHTML=_2;
},_getMenuTitleAttr:function(_3){
return this.title.innerHTML;
}});
dojo.declare("dojox.editor.plugins.Breadcrumb",dijit._editor._Plugin,{_menu:null,breadcrumbBar:null,setEditor:function(_4){
this.editor=_4;
this._buttons=[];
this.breadcrumbBar=new dijit.Toolbar();
dojo.style(this.breadcrumbBar.domNode,"height","1.5em");
var _5=dojo.i18n.getLocalization("dojox.editor.plugins","Breadcrumb");
this._titleTemplate=_5.nodeActions;
dojo.place(this.breadcrumbBar.domNode,this.editor.iframe,"after");
this.editor.onLoadDeferred.addCallback(dojo.hitch(this,function(){
this._menu=new dijit.Menu({});
dojo.addClass(this.breadcrumbBar.domNode,"dojoxEditorBreadcrumbArrow");
var _6=this;
var _7=new dijit.form.ComboButton({showLabel:true,label:"body",_selNode:_4.editNode,dropDown:this._menu,onClick:function(){
_6._menuTarget=_4.editNode;
_6._selectContents();
}});
this._menuTitle=new dojox.editor.plugins._BreadcrumbMenuTitle({menuTitle:_5.nodeActions});
this._selCMenu=new dijit.MenuItem({label:_5.selectContents,onClick:dojo.hitch(this,this._selectContents)});
this._delCMenu=new dijit.MenuItem({label:_5.deleteContents,onClick:dojo.hitch(this,this._deleteContents)});
this._selEMenu=new dijit.MenuItem({label:_5.selectElement,onClick:dojo.hitch(this,this._selectElement)});
this._delEMenu=new dijit.MenuItem({label:_5.deleteElement,onClick:dojo.hitch(this,this._deleteElement)});
this._moveSMenu=new dijit.MenuItem({label:_5.moveStart,onClick:dojo.hitch(this,this._moveCToStart)});
this._moveEMenu=new dijit.MenuItem({label:_5.moveEnd,onClick:dojo.hitch(this,this._moveCToEnd)});
this._menu.addChild(this._menuTitle);
this._menu.addChild(this._selCMenu);
this._menu.addChild(this._delCMenu);
this._menu.addChild(new dijit.MenuSeparator({}));
this._menu.addChild(this._selEMenu);
this._menu.addChild(this._delEMenu);
this._menu.addChild(new dijit.MenuSeparator({}));
this._menu.addChild(this._moveSMenu);
this._menu.addChild(this._moveEMenu);
_7._ddConnect=dojo.connect(_7,"openDropDown",dojo.hitch(this,function(){
this._menuTarget=_7._selNode;
this._menuTitle.attr("menuTitle",dojo.string.substitute(this._titleTemplate,{"nodeName":"&lt;body&gt;"}));
this._selEMenu.attr("disabled",true);
this._delEMenu.attr("disabled",true);
}));
this.breadcrumbBar.addChild(_7);
this.connect(this.editor,"onNormalizedDisplayChanged","updateState");
}));
this.breadcrumbBar.startup();
},_selectContents:function(){
this.editor.focus();
if(this._menuTarget){
dojo.withGlobal(this.editor.window,"collapse",dijit._editor.selection,[null]);
dojo.withGlobal(this.editor.window,"selectElementChildren",dijit._editor.selection,[this._menuTarget]);
this.editor.onDisplayChanged();
}
},_deleteContents:function(){
if(this._menuTarget){
this.editor.beginEditing();
this._selectContents();
dojo.withGlobal(this.editor.window,"remove",dijit._editor.selection,[this._menuTarget]);
this.editor.endditing();
this._updateBreadcrumb();
this.editor.onDisplayChanged();
}
},_selectElement:function(){
this.editor.focus();
if(this._menuTarget){
dojo.withGlobal(this.editor.window,"collapse",dijit._editor.selection,[null]);
dojo.withGlobal(this.editor.window,"selectElement",dijit._editor.selection,[this._menuTarget]);
this.editor.onDisplayChanged();
}
},_deleteElement:function(){
if(this._menuTarget){
this.editor.beginEditing();
this._selectElement();
dojo.withGlobal(this.editor.window,"remove",dijit._editor.selection,[this._menuTarget]);
this.editor.endEditing();
this._updateBreadcrumb();
this.editor.onDisplayChanged();
}
},_moveCToStart:function(){
this.editor.focus();
if(this._menuTarget){
this._selectContents();
dojo.withGlobal(this.editor.window,"collapse",dijit._editor.selection,[true]);
}
},_moveCToEnd:function(){
this.editor.focus();
if(this._menuTarget){
this._selectContents();
dojo.withGlobal(this.editor.window,"collapse",dijit._editor.selection,[false]);
}
},_updateBreadcrumb:function(){
var ed=this.editor;
if(ed.window){
var _8=dijit.range.getSelection(ed.window);
if(_8&&_8.rangeCount>0){
var _9=_8.getRangeAt(0);
var _a=_9.startContainer;
var _b=[];
if(_a&&_a.ownerDocument===ed.document){
while(_a&&_a!==ed.editNode){
if(_a.nodeType===1){
_b.push({type:_a.tagName.toLowerCase(),node:_a});
}
_a=_a.parentNode;
}
_b=_b.reverse();
while(this._buttons.length){
var db=this._buttons.pop();
dojo.disconnect(db._ddConnect);
this.breadcrumbBar.removeChild(db);
}
this._buttons=[];
var i;
var _c=this;
for(i=0;i<_b.length;i++){
var bc=_b[i];
var b=new dijit.form.ComboButton({showLabel:true,label:bc.type,_selNode:bc.node,dropDown:this._menu,onClick:function(){
_c._menuTarget=this._selNode;
_c._selectContents();
}});
b._ddConnect=dojo.connect(b,"openDropDown",dojo.hitch(b,function(){
_c._menuTarget=this._selNode;
var _d=dojo.string.substitute(_c._titleTemplate,{"nodeName":"&lt;"+_c._menuTarget.tagName.toLowerCase()+"&gt;"});
_c._menuTitle.attr("menuTitle",_d);
_c._selEMenu.attr("disabled",false);
_c._delEMenu.attr("disabled",false);
}));
this._buttons.push(b);
this.breadcrumbBar.addChild(b);
}
}
}
}
},updateState:function(){
if(dojo.style(this.editor.iframe,"display")==="none"){
dojo.style(this.breadcrumbBar.domNode,"display","none");
}else{
if(dojo.style(this.breadcrumbBar.domNode,"display")==="none"){
dojo.style(this.breadcrumbBar.domNode,"display","block");
}
this._updateBreadcrumb();
}
},destroy:function(){
if(this.breadcrumbBar){
this.breadcrumbBar.destroy();
this.breadcrumbBar=null;
}
this._buttons=null;
delete this.editor.breadcrumbBar;
this.inherited(arguments);
}});
dojo.subscribe(dijit._scopeName+".Editor.getPlugin",null,function(o){
if(o.plugin){
return;
}
var _e=o.args.name.toLowerCase();
if(_e==="breadcrumb"){
o.plugin=new dojox.editor.plugins.Breadcrumb({});
}
});
}
