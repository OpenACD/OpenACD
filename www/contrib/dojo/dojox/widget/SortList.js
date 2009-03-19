/*
	Copyright (c) 2004-2008, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.widget.SortList"]){
dojo._hasResource["dojox.widget.SortList"]=true;
dojo.provide("dojox.widget.SortList");
dojo.experimental("dojox.widget.SortList");
dojo.require("dijit.layout._LayoutWidget");
dojo.require("dijit._Templated");
dojo.declare("dojox.widget.SortList",[dijit.layout._LayoutWidget,dijit._Templated],{title:"",heading:"",descending:true,selected:null,sortable:true,store:"",key:"name",baseClass:"dojoxSortList",templateString:"<div class=\"sortList\" id=\"${id}\">\n\t\t<div class=\"sortListTitle\" dojoAttachPoint=\"titleNode\">\n\t\t<div class=\"dijitInline sortListIcon\">&thinsp;</div>\n\t\t<span dojoAttachPoint=\"focusNode\">${title}</span>\n\t\t</div>\n\t\t<div class=\"sortListBodyWrapper\" dojoAttachEvent=\"onmouseover: _set, onmouseout: _unset, onclick:_handleClick\" dojoAttachPoint=\"bodyWrapper\">\n\t\t<ul dojoAttachPoint=\"containerNode\" class=\"sortListBody\"></ul>\n\t</div>\n</div>\n",_addItem:function(_1){
var _2=dojo.doc.createElement("li");
var _3=this.store.getValue(_1,this.key);
_2.innerHTML=_3;
this.containerNode.appendChild(_2);
},postCreate:function(){
if(this.store){
this.store=dojo.getObject(this.store);
var _4={onItem:dojo.hitch(this,"_addItem"),onComplete:dojo.hitch(this,"onSort")};
this.store.fetch(_4);
}else{
this.onSort();
}
this.inherited(arguments);
},startup:function(){
this.inherited(arguments);
if(this.heading){
this.setTitle(this.heading);
this.title=this.heading;
}
setTimeout(dojo.hitch(this,"resize"),5);
if(this.sortable){
this.connect(this.titleNode,"onclick","onSort");
}
},resize:function(){
this.inherited(arguments);
var _5=((this._contentBox.h)-(dojo.style(this.titleNode,"height")))-10;
this.bodyWrapper.style.height=Math.abs(_5)+"px";
},onSort:function(e){
var _7=dojo.query("li",this.domNode);
if(this.sortable){
this.descending=!this.descending;
dojo.addClass(this.titleNode,((this.descending)?"sortListDesc":"sortListAsc"));
dojo.removeClass(this.titleNode,((this.descending)?"sortListAsc":"sortListDesc"));
_7.sort(this._sorter);
if(this.descending){
_7.reverse();
}
}
var i=0;
dojo.forEach(_7,function(_9){
dojo[(i++)%2===0?"addClass":"removeClass"](_9,"sortListItemOdd");
this.containerNode.appendChild(_9);
},this);
},_set:function(e){
if(e.target!==this.bodyWrapper){
dojo.addClass(e.target,"sortListItemHover");
}
},_unset:function(e){
dojo.removeClass(e.target,"sortListItemHover");
},_handleClick:function(e){
dojo.toggleClass(e.target,"sortListItemSelected");
e.target.focus();
this._updateValues(e.target.innerHTML);
},_updateValues:function(){
this._selected=dojo.query("li.sortListItemSelected",this.containerNode);
this.selected=[];
dojo.forEach(this._selected,function(_d){
this.selected.push(_d.innerHTML);
},this);
this.onChanged(arguments);
},_sorter:function(a,b){
var _10=a.innerHTML;
var _11=b.innerHTML;
if(_10>_11){
return 1;
}
if(_10<_11){
return -1;
}
return 0;
},setTitle:function(_12){
this.focusNode.innerHTML=this.title=_12;
},onChanged:function(){
}});
}
