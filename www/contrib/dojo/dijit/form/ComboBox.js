/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dijit.form.ComboBox"]){
dojo._hasResource["dijit.form.ComboBox"]=true;
dojo.provide("dijit.form.ComboBox");
dojo.require("dijit.form._FormWidget");
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dojo.data.util.simpleFetch");
dojo.require("dojo.data.util.filter");
dojo.require("dojo.regexp");
dojo.requireLocalization("dijit.form","ComboBox",null,"ROOT,ar,ca,cs,da,de,el,es,fi,fr,he,hu,it,ja,ko,nb,nl,pl,pt,pt-pt,ru,sk,sl,sv,th,tr,zh,zh-tw");
dojo.declare("dijit.form.ComboBoxMixin",null,{item:null,pageSize:Infinity,store:null,fetchProperties:{},query:{},autoComplete:true,highlightMatch:"first",searchDelay:100,searchAttr:"name",labelAttr:"",labelType:"text",queryExpr:"${0}*",ignoreCase:true,hasDownArrow:true,templateString:dojo.cache("dijit.form","templates/ComboBox.html","<div class=\"dijit dijitReset dijitInlineTable dijitLeft\"\n\tid=\"widget_${id}\"\n\tdojoAttachEvent=\"onmouseenter:_onMouse,onmouseleave:_onMouse,onmousedown:_onMouse\" dojoAttachPoint=\"comboNode\" waiRole=\"combobox\" tabIndex=\"-1\"\n\t><div style=\"overflow:hidden;\"\n\t\t><div class='dijitReset dijitRight dijitButtonNode dijitArrowButton dijitDownArrowButton'\n\t\t\tdojoAttachPoint=\"downArrowNode\" waiRole=\"presentation\"\n\t\t\tdojoAttachEvent=\"onmousedown:_onArrowMouseDown,onmouseup:_onMouse,onmouseenter:_onMouse,onmouseleave:_onMouse\"\n\t\t\t><div class=\"dijitArrowButtonInner\">&thinsp;</div\n\t\t\t><div class=\"dijitArrowButtonChar\">&#9660;</div\n\t\t></div\n\t\t><div class=\"dijitReset dijitValidationIcon\"><br></div\n\t\t><div class=\"dijitReset dijitValidationIconText\">&Chi;</div\n\t\t><div class=\"dijitReset dijitInputField\"\n\t\t\t><input ${nameAttrSetting} type=\"text\" autocomplete=\"off\" class='dijitReset'\n\t\t\tdojoAttachEvent=\"onkeypress:_onKeyPress,compositionend\"\n\t\t\tdojoAttachPoint=\"textbox,focusNode\" waiRole=\"textbox\" waiState=\"haspopup-true,autocomplete-list\"\n\t\t/></div\n\t></div\n></div>\n"),baseClass:"dijitComboBox",_getCaretPos:function(_1){
var _2=0;
if(typeof (_1.selectionStart)=="number"){
_2=_1.selectionStart;
}else{
if(dojo.isIE){
var tr=dojo.doc.selection.createRange().duplicate();
var _3=_1.createTextRange();
tr.move("character",0);
_3.move("character",0);
try{
_3.setEndPoint("EndToEnd",tr);
_2=String(_3.text).replace(/\r/g,"").length;
}
catch(e){
}
}
}
return _2;
},_setCaretPos:function(_4,_5){
_5=parseInt(_5);
dijit.selectInputText(_4,_5,_5);
},_setDisabledAttr:function(_6){
this.inherited(arguments);
dijit.setWaiState(this.comboNode,"disabled",_6);
},_abortQuery:function(){
if(this.searchTimer){
clearTimeout(this.searchTimer);
this.searchTimer=null;
}
if(this._fetchHandle){
if(this._fetchHandle.abort){
this._fetchHandle.abort();
}
this._fetchHandle=null;
}
},_onKeyPress:function(_7){
var _8=_7.charOrCode;
if(_7.altKey||((_7.ctrlKey||_7.metaKey)&&(_8!="x"&&_8!="v"))||_8==dojo.keys.SHIFT){
return;
}
var _9=false;
var _a="_startSearchFromInput";
var pw=this._popupWidget;
var dk=dojo.keys;
var _b=null;
this._prev_key_backspace=false;
this._abortQuery();
if(this._isShowingNow){
pw.handleKey(_8);
_b=pw.getHighlightedOption();
}
switch(_8){
case dk.PAGE_DOWN:
case dk.DOWN_ARROW:
case dk.PAGE_UP:
case dk.UP_ARROW:
if(!this._isShowingNow){
this._arrowPressed();
_9=true;
_a="_startSearchAll";
}else{
this._announceOption(_b);
}
dojo.stopEvent(_7);
break;
case dk.ENTER:
if(_b){
if(_b==pw.nextButton){
this._nextSearch(1);
dojo.stopEvent(_7);
break;
}else{
if(_b==pw.previousButton){
this._nextSearch(-1);
dojo.stopEvent(_7);
break;
}
}
}else{
this._setBlurValue();
this._setCaretPos(this.focusNode,this.focusNode.value.length);
}
_7.preventDefault();
case dk.TAB:
var _c=this.attr("displayedValue");
if(pw&&(_c==pw._messages["previousMessage"]||_c==pw._messages["nextMessage"])){
break;
}
if(_b){
this._selectOption();
}
if(this._isShowingNow){
this._lastQuery=null;
this._hideResultList();
}
break;
case " ":
if(_b){
dojo.stopEvent(_7);
this._selectOption();
this._hideResultList();
}else{
_9=true;
}
break;
case dk.ESCAPE:
if(this._isShowingNow){
dojo.stopEvent(_7);
this._hideResultList();
}
break;
case dk.DELETE:
case dk.BACKSPACE:
this._prev_key_backspace=true;
_9=true;
break;
default:
_9=typeof _8=="string"||_8==229;
}
if(_9){
this.item=undefined;
this.searchTimer=setTimeout(dojo.hitch(this,_a),1);
}
},_autoCompleteText:function(_d){
var fn=this.focusNode;
dijit.selectInputText(fn,fn.value.length);
var _e=this.ignoreCase?"toLowerCase":"substr";
if(_d[_e](0).indexOf(this.focusNode.value[_e](0))==0){
var _f=this._getCaretPos(fn);
if((_f+1)>fn.value.length){
fn.value=_d;
dijit.selectInputText(fn,_f);
}
}else{
fn.value=_d;
dijit.selectInputText(fn);
}
},_openResultList:function(_10,_11){
this._fetchHandle=null;
if(this.disabled||this.readOnly||(_11.query[this.searchAttr]!=this._lastQuery)){
return;
}
this._popupWidget.clearResultList();
if(!_10.length){
this._hideResultList();
return;
}
_11._maxOptions=this._maxOptions;
var _12=this._popupWidget.createOptions(_10,_11,dojo.hitch(this,"_getMenuLabelFromItem"));
this._showResultList();
if(_11.direction){
if(1==_11.direction){
this._popupWidget.highlightFirstOption();
}else{
if(-1==_11.direction){
this._popupWidget.highlightLastOption();
}
}
this._announceOption(this._popupWidget.getHighlightedOption());
}else{
if(this.autoComplete&&!this._prev_key_backspace&&!/^[*]+$/.test(_11.query[this.searchAttr])){
this._announceOption(_12[1]);
}
}
},_showResultList:function(){
this._hideResultList();
this._arrowPressed();
this.displayMessage("");
dojo.style(this._popupWidget.domNode,{width:"",height:""});
var _13=this.open();
var _14=dojo.marginBox(this._popupWidget.domNode);
this._popupWidget.domNode.style.overflow=((_13.h==_14.h)&&(_13.w==_14.w))?"hidden":"auto";
var _15=_13.w;
if(_13.h<this._popupWidget.domNode.scrollHeight){
_15+=16;
}
dojo.marginBox(this._popupWidget.domNode,{h:_13.h,w:Math.max(_15,this.domNode.offsetWidth)});
dijit.setWaiState(this.comboNode,"expanded","true");
},_hideResultList:function(){
this._abortQuery();
if(this._isShowingNow){
dijit.popup.close(this._popupWidget);
this._arrowIdle();
this._isShowingNow=false;
dijit.setWaiState(this.comboNode,"expanded","false");
dijit.removeWaiState(this.focusNode,"activedescendant");
}
},_setBlurValue:function(){
var _16=this.attr("displayedValue");
var pw=this._popupWidget;
if(pw&&(_16==pw._messages["previousMessage"]||_16==pw._messages["nextMessage"])){
this._setValueAttr(this._lastValueReported,true);
}else{
if(typeof this.item=="undefined"){
this.item=null;
this.attr("displayedValue",_16);
}else{
if(this.value!=this._lastValueReported){
dijit.form._FormValueWidget.prototype._setValueAttr.call(this,this.value,true);
}
}
}
},_onBlur:function(){
this._hideResultList();
this._arrowIdle();
this.inherited(arguments);
},_setItemAttr:function(_17,_18,_19){
if(!_19){
_19=this.labelFunc(_17,this.store);
}
this.value=this._getValueField()!=this.searchAttr?this.store.getIdentity(_17):_19;
this.item=_17;
dijit.form.ComboBox.superclass._setValueAttr.call(this,this.value,_18,_19);
},_announceOption:function(_1a){
if(!_1a){
return;
}
var _1b=this._getCaretPos(this.focusNode);
var _1c;
if(_1a==this._popupWidget.nextButton||_1a==this._popupWidget.previousButton){
_1c=_1a.innerHTML;
this.item=undefined;
this.value="";
}else{
_1c=this.labelFunc(_1a.item,this.store);
this.attr("item",_1a.item,false,_1c);
}
this.focusNode.value=this.focusNode.value.substring(0,_1b);
dijit.setWaiState(this.focusNode,"activedescendant",dojo.attr(_1a,"id"));
this._autoCompleteText(_1c);
},_selectOption:function(evt){
if(evt){
this._announceOption(evt.target);
}
this._hideResultList();
this._setCaretPos(this.focusNode,this.focusNode.value.length);
dijit.form._FormValueWidget.prototype._setValueAttr.call(this,this.value,true);
},_onArrowMouseDown:function(evt){
if(this.disabled||this.readOnly){
return;
}
dojo.stopEvent(evt);
this.focus();
if(this._isShowingNow){
this._hideResultList();
}else{
this._startSearch("");
}
},_startSearchAll:function(){
this._startSearch("");
},_startSearchFromInput:function(){
this._startSearch(this.focusNode.value.replace(/([\\\*\?])/g,"\\$1"));
},_getQueryString:function(_1d){
return dojo.string.substitute(this.queryExpr,[_1d]);
},_startSearch:function(key){
if(!this._popupWidget){
var _1e=this.id+"_popup";
this._popupWidget=new dijit.form._ComboBoxMenu({onChange:dojo.hitch(this,this._selectOption),id:_1e});
dijit.removeWaiState(this.focusNode,"activedescendant");
dijit.setWaiState(this.textbox,"owns",_1e);
}
var _1f=dojo.clone(this.query);
this._lastInput=key;
this._lastQuery=_1f[this.searchAttr]=this._getQueryString(key);
this.searchTimer=setTimeout(dojo.hitch(this,function(_20,_21){
this.searchTimer=null;
var _22={queryOptions:{ignoreCase:this.ignoreCase,deep:true},query:_20,onBegin:dojo.hitch(this,"_setMaxOptions"),onComplete:dojo.hitch(this,"_openResultList"),onError:function(_23){
_21._fetchHandle=null;
console.error("dijit.form.ComboBox: "+_23);
dojo.hitch(_21,"_hideResultList")();
},start:0,count:this.pageSize};
dojo.mixin(_22,_21.fetchProperties);
this._fetchHandle=_21.store.fetch(_22);
var _24=function(_25,_26){
_25.start+=_25.count*_26;
_25.direction=_26;
this._fetchHandle=this.store.fetch(_25);
};
this._nextSearch=this._popupWidget.onPage=dojo.hitch(this,_24,this._fetchHandle);
},_1f,this),this.searchDelay);
},_setMaxOptions:function(_27,_28){
this._maxOptions=_27;
},_getValueField:function(){
return this.searchAttr;
},_arrowPressed:function(){
if(!this.disabled&&!this.readOnly&&this.hasDownArrow){
dojo.addClass(this.downArrowNode,"dijitArrowButtonActive");
}
},_arrowIdle:function(){
if(!this.disabled&&!this.readOnly&&this.hasDownArrow){
dojo.removeClass(this.downArrowNode,"dojoArrowButtonPushed");
}
},compositionend:function(evt){
this._onKeyPress({charOrCode:229});
},constructor:function(){
this.query={};
this.fetchProperties={};
},postMixInProperties:function(){
if(!this.hasDownArrow){
this.baseClass="dijitTextBox";
}
if(!this.store){
var _29=this.srcNodeRef;
this.store=new dijit.form._ComboBoxDataStore(_29);
if(!this.value||((typeof _29.selectedIndex=="number")&&_29.selectedIndex.toString()===this.value)){
var _2a=this.store.fetchSelectedItem();
if(_2a){
var _2b=this._getValueField();
this.value=_2b!=this.searchAttr?this.store.getValue(_2a,_2b):this.labelFunc(_2a,this.store);
}
}
}
this.inherited(arguments);
},postCreate:function(){
var _2c=dojo.query("label[for=\""+this.id+"\"]");
if(_2c.length){
_2c[0].id=(this.id+"_label");
var cn=this.comboNode;
dijit.setWaiState(cn,"labelledby",_2c[0].id);
}
this.inherited(arguments);
},uninitialize:function(){
if(this._popupWidget&&!this._popupWidget._destroyed){
this._hideResultList();
this._popupWidget.destroy();
}
this.inherited(arguments);
},_getMenuLabelFromItem:function(_2d){
var _2e=this.labelAttr?this.store.getValue(_2d,this.labelAttr):this.labelFunc(_2d,this.store);
var _2f=this.labelType;
if(this.highlightMatch!="none"&&this.labelType=="text"&&this._lastInput){
_2e=this.doHighlight(_2e,this._escapeHtml(this._lastInput));
_2f="html";
}
return {html:_2f=="html",label:_2e};
},doHighlight:function(_30,_31){
var _32="i"+(this.highlightMatch=="all"?"g":"");
var _33=this._escapeHtml(_30);
_31=dojo.regexp.escapeString(_31);
var ret=_33.replace(new RegExp("(^|\\s)("+_31+")",_32),"$1<span class=\"dijitComboBoxHighlightMatch\">$2</span>");
return ret;
},_escapeHtml:function(str){
str=String(str).replace(/&/gm,"&amp;").replace(/</gm,"&lt;").replace(/>/gm,"&gt;").replace(/"/gm,"&quot;");
return str;
},open:function(){
this._isShowingNow=true;
return dijit.popup.open({popup:this._popupWidget,around:this.domNode,parent:this});
},reset:function(){
this.item=null;
this.inherited(arguments);
},labelFunc:function(_34,_35){
return _35.getValue(_34,this.searchAttr).toString();
}});
dojo.declare("dijit.form._ComboBoxMenu",[dijit._Widget,dijit._Templated],{templateString:"<ul class='dijitReset dijitMenu' dojoAttachEvent='onmousedown:_onMouseDown,onmouseup:_onMouseUp,onmouseover:_onMouseOver,onmouseout:_onMouseOut' tabIndex='-1' style='overflow: \"auto\"; overflow-x: \"hidden\";'>"+"<li class='dijitMenuItem dijitMenuPreviousButton' dojoAttachPoint='previousButton' waiRole='option'></li>"+"<li class='dijitMenuItem dijitMenuNextButton' dojoAttachPoint='nextButton' waiRole='option'></li>"+"</ul>",_messages:null,postMixInProperties:function(){
this._messages=dojo.i18n.getLocalization("dijit.form","ComboBox",this.lang);
this.inherited(arguments);
},_setValueAttr:function(_36){
this.value=_36;
this.onChange(_36);
},onChange:function(_37){
},onPage:function(_38){
},postCreate:function(){
this.previousButton.innerHTML=this._messages["previousMessage"];
this.nextButton.innerHTML=this._messages["nextMessage"];
this.inherited(arguments);
},onClose:function(){
this._blurOptionNode();
},_createOption:function(_39,_3a){
var _3b=_3a(_39);
var _3c=dojo.doc.createElement("li");
dijit.setWaiRole(_3c,"option");
if(_3b.html){
_3c.innerHTML=_3b.label;
}else{
_3c.appendChild(dojo.doc.createTextNode(_3b.label));
}
if(_3c.innerHTML==""){
_3c.innerHTML="&nbsp;";
}
_3c.item=_39;
return _3c;
},createOptions:function(_3d,_3e,_3f){
this.previousButton.style.display=(_3e.start==0)?"none":"";
dojo.attr(this.previousButton,"id",this.id+"_prev");
dojo.forEach(_3d,function(_40,i){
var _41=this._createOption(_40,_3f);
_41.className="dijitReset dijitMenuItem";
dojo.attr(_41,"id",this.id+i);
this.domNode.insertBefore(_41,this.nextButton);
},this);
var _42=false;
if(_3e._maxOptions&&_3e._maxOptions!=-1){
if((_3e.start+_3e.count)<_3e._maxOptions){
_42=true;
}else{
if((_3e.start+_3e.count)>(_3e._maxOptions-1)){
if(_3e.count==_3d.length){
_42=true;
}
}
}
}else{
if(_3e.count==_3d.length){
_42=true;
}
}
this.nextButton.style.display=_42?"":"none";
dojo.attr(this.nextButton,"id",this.id+"_next");
return this.domNode.childNodes;
},clearResultList:function(){
while(this.domNode.childNodes.length>2){
this.domNode.removeChild(this.domNode.childNodes[this.domNode.childNodes.length-2]);
}
},_onMouseDown:function(evt){
dojo.stopEvent(evt);
},_onMouseUp:function(evt){
if(evt.target===this.domNode){
return;
}else{
if(evt.target==this.previousButton){
this.onPage(-1);
}else{
if(evt.target==this.nextButton){
this.onPage(1);
}else{
var tgt=evt.target;
while(!tgt.item){
tgt=tgt.parentNode;
}
this._setValueAttr({target:tgt},true);
}
}
}
},_onMouseOver:function(evt){
if(evt.target===this.domNode){
return;
}
var tgt=evt.target;
if(!(tgt==this.previousButton||tgt==this.nextButton)){
while(!tgt.item){
tgt=tgt.parentNode;
}
}
this._focusOptionNode(tgt);
},_onMouseOut:function(evt){
if(evt.target===this.domNode){
return;
}
this._blurOptionNode();
},_focusOptionNode:function(_43){
if(this._highlighted_option!=_43){
this._blurOptionNode();
this._highlighted_option=_43;
dojo.addClass(this._highlighted_option,"dijitMenuItemSelected");
}
},_blurOptionNode:function(){
if(this._highlighted_option){
dojo.removeClass(this._highlighted_option,"dijitMenuItemSelected");
this._highlighted_option=null;
}
},_highlightNextOption:function(){
var fc=this.domNode.firstChild;
if(!this.getHighlightedOption()){
this._focusOptionNode(fc.style.display=="none"?fc.nextSibling:fc);
}else{
var ns=this._highlighted_option.nextSibling;
if(ns&&ns.style.display!="none"){
this._focusOptionNode(ns);
}
}
dijit.scrollIntoView(this._highlighted_option);
},highlightFirstOption:function(){
this._focusOptionNode(this.domNode.firstChild.nextSibling);
dijit.scrollIntoView(this._highlighted_option);
},highlightLastOption:function(){
this._focusOptionNode(this.domNode.lastChild.previousSibling);
dijit.scrollIntoView(this._highlighted_option);
},_highlightPrevOption:function(){
var lc=this.domNode.lastChild;
if(!this.getHighlightedOption()){
this._focusOptionNode(lc.style.display=="none"?lc.previousSibling:lc);
}else{
var ps=this._highlighted_option.previousSibling;
if(ps&&ps.style.display!="none"){
this._focusOptionNode(ps);
}
}
dijit.scrollIntoView(this._highlighted_option);
},_page:function(up){
var _44=0;
var _45=this.domNode.scrollTop;
var _46=dojo.style(this.domNode,"height");
if(!this.getHighlightedOption()){
this._highlightNextOption();
}
while(_44<_46){
if(up){
if(!this.getHighlightedOption().previousSibling||this._highlighted_option.previousSibling.style.display=="none"){
break;
}
this._highlightPrevOption();
}else{
if(!this.getHighlightedOption().nextSibling||this._highlighted_option.nextSibling.style.display=="none"){
break;
}
this._highlightNextOption();
}
var _47=this.domNode.scrollTop;
_44+=(_47-_45)*(up?-1:1);
_45=_47;
}
},pageUp:function(){
this._page(true);
},pageDown:function(){
this._page(false);
},getHighlightedOption:function(){
var ho=this._highlighted_option;
return (ho&&ho.parentNode)?ho:null;
},handleKey:function(key){
switch(key){
case dojo.keys.DOWN_ARROW:
this._highlightNextOption();
break;
case dojo.keys.PAGE_DOWN:
this.pageDown();
break;
case dojo.keys.UP_ARROW:
this._highlightPrevOption();
break;
case dojo.keys.PAGE_UP:
this.pageUp();
break;
}
}});
dojo.declare("dijit.form.ComboBox",[dijit.form.ValidationTextBox,dijit.form.ComboBoxMixin],{_setValueAttr:function(_48,_49,_4a){
this.item=null;
if(!_48){
_48="";
}
dijit.form.ValidationTextBox.prototype._setValueAttr.call(this,_48,_49,_4a);
}});
dojo.declare("dijit.form._ComboBoxDataStore",null,{constructor:function(_4b){
this.root=_4b;
dojo.query("> option",_4b).forEach(function(_4c){
_4c.innerHTML=dojo.trim(_4c.innerHTML);
});
},getValue:function(_4d,_4e,_4f){
return (_4e=="value")?_4d.value:(_4d.innerText||_4d.textContent||"");
},isItemLoaded:function(_50){
return true;
},getFeatures:function(){
return {"dojo.data.api.Read":true,"dojo.data.api.Identity":true};
},_fetchItems:function(_51,_52,_53){
if(!_51.query){
_51.query={};
}
if(!_51.query.name){
_51.query.name="";
}
if(!_51.queryOptions){
_51.queryOptions={};
}
var _54=dojo.data.util.filter.patternToRegExp(_51.query.name,_51.queryOptions.ignoreCase),_55=dojo.query("> option",this.root).filter(function(_56){
return (_56.innerText||_56.textContent||"").match(_54);
});
if(_51.sort){
_55.sort(dojo.data.util.sorter.createSortFunction(_51.sort,this));
}
_52(_55,_51);
},close:function(_57){
return;
},getLabel:function(_58){
return _58.innerHTML;
},getIdentity:function(_59){
return dojo.attr(_59,"value");
},fetchItemByIdentity:function(_5a){
var _5b=dojo.query("option[value='"+_5a.identity+"']",this.root)[0];
_5a.onItem(_5b);
},fetchSelectedItem:function(){
var _5c=this.root,si=_5c.selectedIndex;
return dojo.query("> option:nth-child("+(si!=-1?si+1:1)+")",_5c)[0];
}});
dojo.extend(dijit.form._ComboBoxDataStore,dojo.data.util.simpleFetch);
}
