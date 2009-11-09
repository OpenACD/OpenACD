/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dijit.form.Select"]){
dojo._hasResource["dijit.form.Select"]=true;
dojo.provide("dijit.form.Select");
dojo.require("dijit.form._FormSelectWidget");
dojo.require("dijit._HasDropDown");
dojo.require("dijit.Menu");
dojo.requireLocalization("dijit.form","validate",null,"ROOT,ar,ca,cs,da,de,el,es,fi,fr,he,hu,it,ja,ko,nb,nl,pl,pt,pt-pt,ru,sk,sl,sv,th,tr,zh,zh-tw");
dojo.declare("dijit.form._SelectMenu",dijit.Menu,{buildRendering:function(){
this.inherited(arguments);
var o=this.menuTableNode=this.domNode;
var n=this.domNode=dojo.doc.createElement("div");
if(o.parentNode){
o.parentNode.replaceChild(n,o);
}
dojo.removeClass(o,"dijitMenuTable");
n.className=o.className+" dijitSelectMenu";
o.className="dijitReset dijitMenuTable";
dijit.setWaiRole(o,"listbox");
dijit.setWaiRole(n,"presentation");
n.appendChild(o);
this.tabIndex=null;
},resize:function(mb){
if(mb){
dojo.marginBox(this.domNode,mb);
var w=dojo.contentBox(this.domNode).w;
if(dojo.isMoz&&this.domNode.scrollHeight>this.domNode.clientHeight){
w--;
}else{
if(dojo.isIE<8||(dojo.isIE&&dojo.isQuirks)){
w-=16;
}
}
dojo.marginBox(this.menuTableNode,{w:w});
}
}});
dojo.declare("dijit.form.Select",[dijit.form._FormSelectWidget,dijit._HasDropDown],{attributeMap:dojo.mixin(dojo.clone(dijit.form._FormSelectWidget.prototype.attributeMap),{value:"valueNode",name:"valueNode"}),baseClass:"dijitSelect",templateString:dojo.cache("dijit.form","templates/Select.html","<table class='dijit dijitReset dijitInline dijitLeft'\n\tdojoAttachPoint=\"_buttonNode,tableNode\" cellspacing='0' cellpadding='0' waiRole=\"presentation\"\n\tdojoAttachEvent=\"onmouseenter:_onMouse,onmouseleave:_onMouse,onmousedown:_onMouse\"\n\t><tbody waiRole=\"presentation\"><tr waiRole=\"presentation\"\n\t\t><td class=\"dijitReset dijitStretch dijitButtonContents dijitButtonNode\" dojoAttachPoint=\"focusNode\"\n\t\t\twaiRole=\"combobox\" waiState=\"haspopup-true\"\n\t\t\t><span class=\"dijitReset dijitInline dijitButtonText\"  dojoAttachPoint=\"containerNode,_popupStateNode\"></span\n\t\t\t><input type=\"hidden\" ${nameAttrSetting} dojoAttachPoint=\"valueNode\" value=\"${value}\" waiState=\"hidden-true\" />\n\t\t</td><td class=\"dijitReset dijitRight dijitButtonNode dijitArrowButton dijitDownArrowButton\"\n\t\t\t\tdojoAttachPoint=\"titleNode\" waiRole=\"presentation\"\n\t\t\t><div class=\"dijitReset dijitArrowButtonInner\" waiRole=\"presentation\">&thinsp;</div\n\t\t\t><div class=\"dijitReset dijitArrowButtonChar\" waiRole=\"presentation\">&#9660;</div\n\t\t></td\n\t></tr></tbody\n></table>\n"),attributeMap:dojo.mixin(dojo.clone(dijit.form._FormSelectWidget.prototype.attributeMap),{style:"tableNode"}),required:false,state:"",tooltipPosition:[],emptyLabel:"",_isLoaded:false,_childrenLoaded:false,_fillContent:function(){
this.inherited(arguments);
if(this.options.length&&!this.value&&this.srcNodeRef){
var si=this.srcNodeRef.selectedIndex;
this.value=this.options[si!=-1?si:0].value;
}
this.dropDown=new dijit.form._SelectMenu();
dojo.addClass(this.dropDown.domNode,this.baseClass+"Menu");
},_getMenuItemForOption:function(_1){
if(!_1.value){
return new dijit.MenuSeparator();
}else{
var _2=dojo.hitch(this,"_setValueAttr",_1);
var _3=new dijit.MenuItem({option:_1,label:_1.label,onClick:_2,disabled:_1.disabled||false});
dijit.setWaiRole(_3.focusNode,"listitem");
return _3;
}
},_addOptionItem:function(_4){
if(this.dropDown){
this.dropDown.addChild(this._getMenuItemForOption(_4));
}
},_getChildren:function(){
if(!this.dropDown){
return [];
}
return this.dropDown.getChildren();
},_loadChildren:function(_5){
if(_5===true){
if(this.dropDown){
delete this.dropDown.focusedChild;
}
this.inherited(arguments);
}else{
this._updateSelection();
}
var _6=this.options.length;
this._isLoaded=false;
this._childrenLoaded=true;
if(!this._iReadOnly){
this.attr("readOnly",(_6===1));
delete this._iReadOnly;
}
if(!this._iDisabled){
this.attr("disabled",(_6===0));
delete this._iDisabled;
}
if(!this._loadingStore){
this._setValueAttr(this.value);
}
},_setValueAttr:function(_7){
this.inherited(arguments);
dojo.attr(this.valueNode,"value",this.attr("value"));
},_setDisplay:function(_8){
this.containerNode.innerHTML="<span class=\"dijitReset dijitInline "+this.baseClass+"Label\">"+(_8||this.emptyLabel||"&nbsp;")+"</span>";
dijit.setWaiState(this.focusNode,"valuenow",(_8||this.emptyLabel||"&nbsp;"));
},validate:function(_9){
var _a=this.isValid(_9);
this.state=_a?"":"Error";
this._setStateClass();
dijit.setWaiState(this.focusNode,"invalid",_a?"false":"true");
var _b=_a?"":this._missingMsg;
if(this._message!==_b){
this._message=_b;
dijit.hideTooltip(this.domNode);
if(_b){
dijit.showTooltip(_b,this.domNode,this.tooltipPosition);
}
}
return _a;
},isValid:function(_c){
return (!this.required||!(/^\s*$/.test(this.value)));
},reset:function(){
this.inherited(arguments);
dijit.hideTooltip(this.domNode);
this.state="";
this._setStateClass();
delete this._message;
},postMixInProperties:function(){
this.inherited(arguments);
this._missingMsg=dojo.i18n.getLocalization("dijit.form","validate",this.lang).missingMessage;
},postCreate:function(){
this.inherited(arguments);
if(this.srcNodeRef&&dojo.attr(this.srcNodeRef,"disabled")){
this.attr("disabled",true);
}
if(this.tableNode.style.width){
dojo.addClass(this.domNode,this.baseClass+"FixedWidth");
}
},startup:function(){
if(this._started){
return;
}
if(!this.dropDown){
var _d=dojo.query("[widgetId]",this.dropDownContainer)[0];
this.dropDown=dijit.byNode(_d);
delete this.dropDownContainer;
}
this.inherited(arguments);
},isLoaded:function(){
return this._isLoaded;
},loadDropDown:function(_e){
this._loadChildren(true);
this._isLoaded=true;
_e();
},_setReadOnlyAttr:function(_f){
this._iReadOnly=_f;
if(!_f&&this._childrenLoaded&&this.options.length===1){
return;
}
this.readOnly=_f;
},_setDisabledAttr:function(_10){
this._iDisabled=_10;
if(!_10&&this._childrenLoaded&&this.options.length===0){
return;
}
this.inherited(arguments);
},uninitialize:function(_11){
if(this.dropDown&&!this.dropDown._destroyed){
this.dropDown.destroyRecursive(_11);
delete this.dropDown;
}
this.inherited(arguments);
}});
}
