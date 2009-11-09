/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.grid._Grid"]){
dojo._hasResource["dojox.grid._Grid"]=true;
dojo.provide("dojox.grid._Grid");
dojo.require("dijit.dijit");
dojo.require("dijit.Menu");
dojo.require("dojox.html.metrics");
dojo.require("dojox.grid.util");
dojo.require("dojox.grid._Scroller");
dojo.require("dojox.grid._Layout");
dojo.require("dojox.grid._View");
dojo.require("dojox.grid._ViewManager");
dojo.require("dojox.grid._RowManager");
dojo.require("dojox.grid._FocusManager");
dojo.require("dojox.grid._EditManager");
dojo.require("dojox.grid.Selection");
dojo.require("dojox.grid._RowSelector");
dojo.require("dojox.grid._Events");
dojo.requireLocalization("dijit","loading",null,"ROOT,ar,ca,cs,da,de,el,es,fi,fr,he,hu,it,ja,ko,nb,nl,pl,pt,pt-pt,ru,sk,sl,sv,th,tr,zh,zh-tw");
(function(){
if(!dojo.isCopyKey){
dojo.isCopyKey=dojo.dnd.getCopyKeyState;
}
dojo.declare("dojox.grid._Grid",[dijit._Widget,dijit._Templated,dojox.grid._Events],{templateString:"<div class=\"dojoxGrid\" hidefocus=\"hidefocus\" wairole=\"grid\" dojoAttachEvent=\"onmouseout:_mouseOut\">\n\t<div class=\"dojoxGridMasterHeader\" dojoAttachPoint=\"viewsHeaderNode\" wairole=\"presentation\"></div>\n\t<div class=\"dojoxGridMasterView\" dojoAttachPoint=\"viewsNode\" wairole=\"presentation\"></div>\n\t<div class=\"dojoxGridMasterMessages\" style=\"display: none;\" dojoAttachPoint=\"messagesNode\"></div>\n\t<span dojoAttachPoint=\"lastFocusNode\" tabindex=\"0\"></span>\n</div>\n",classTag:"dojoxGrid",get:function(_1){
},rowCount:5,keepRows:75,rowsPerPage:25,autoWidth:false,initialWidth:"",autoHeight:"",rowHeight:0,autoRender:true,defaultHeight:"15em",height:"",structure:null,elasticView:-1,singleClickEdit:false,selectionMode:"extended",rowSelector:"",columnReordering:false,headerMenu:null,placeholderLabel:"GridColumns",selectable:false,_click:null,loadingMessage:"<span class='dojoxGridLoading'>${loadingState}</span>",errorMessage:"<span class='dojoxGridError'>${errorState}</span>",noDataMessage:"",escapeHTMLInData:true,formatterScope:null,editable:false,sortInfo:0,themeable:true,_placeholders:null,_layoutClass:dojox.grid._Layout,buildRendering:function(){
this.inherited(arguments);
if(this.get==dojox.grid._Grid.prototype.get){
this.get=null;
}
if(!this.domNode.getAttribute("tabIndex")){
this.domNode.tabIndex="0";
}
this.createScroller();
this.createLayout();
this.createViews();
this.createManagers();
this.createSelection();
this.connect(this.selection,"onSelected","onSelected");
this.connect(this.selection,"onDeselected","onDeselected");
this.connect(this.selection,"onChanged","onSelectionChanged");
dojox.html.metrics.initOnFontResize();
this.connect(dojox.html.metrics,"onFontResize","textSizeChanged");
dojox.grid.util.funnelEvents(this.domNode,this,"doKeyEvent",dojox.grid.util.keyEvents);
if(this.selectionMode!="none"){
dojo.attr(this.domNode,"aria-multiselectable",this.selectionMode=="single"?"false":"true");
}
},postMixInProperties:function(){
this.inherited(arguments);
var _2=dojo.i18n.getLocalization("dijit","loading",this.lang);
this.loadingMessage=dojo.string.substitute(this.loadingMessage,_2);
this.errorMessage=dojo.string.substitute(this.errorMessage,_2);
if(this.srcNodeRef&&this.srcNodeRef.style.height){
this.height=this.srcNodeRef.style.height;
}
this._setAutoHeightAttr(this.autoHeight,true);
},postCreate:function(){
this._placeholders=[];
this._setHeaderMenuAttr(this.headerMenu);
this._setStructureAttr(this.structure);
this._click=[];
this.inherited(arguments);
if(this.domNode&&this.autoWidth&&this.initialWidth){
this.domNode.style.width=this.initialWidth;
}
if(this.domNode&&!this.editable){
dojo.attr(this.domNode,"aria-readonly","true");
}
},destroy:function(){
this.domNode.onReveal=null;
this.domNode.onSizeChange=null;
delete this._click;
this.edit.destroy();
delete this.edit;
this.views.destroyViews();
if(this.scroller){
this.scroller.destroy();
delete this.scroller;
}
if(this.focus){
this.focus.destroy();
delete this.focus;
}
if(this.headerMenu&&this._placeholders.length){
dojo.forEach(this._placeholders,function(p){
p.unReplace(true);
});
this.headerMenu.unBindDomNode(this.viewsHeaderNode);
}
this.inherited(arguments);
},_setAutoHeightAttr:function(ah,_3){
if(typeof ah=="string"){
if(!ah||ah=="false"){
ah=false;
}else{
if(ah=="true"){
ah=true;
}else{
ah=window.parseInt(ah,10);
}
}
}
if(typeof ah=="number"){
if(isNaN(ah)){
ah=false;
}
if(ah<0){
ah=true;
}else{
if(ah===0){
ah=false;
}
}
}
this.autoHeight=ah;
if(typeof ah=="boolean"){
this._autoHeight=ah;
}else{
if(typeof ah=="number"){
this._autoHeight=(ah>=this.attr("rowCount"));
}else{
this._autoHeight=false;
}
}
if(this._started&&!_3){
this.render();
}
},_getRowCountAttr:function(){
return this.updating&&this.invalidated&&this.invalidated.rowCount!=undefined?this.invalidated.rowCount:this.rowCount;
},textSizeChanged:function(){
this.render();
},sizeChange:function(){
this.update();
},createManagers:function(){
this.rows=new dojox.grid._RowManager(this);
this.focus=new dojox.grid._FocusManager(this);
this.edit=new dojox.grid._EditManager(this);
},createSelection:function(){
this.selection=new dojox.grid.Selection(this);
},createScroller:function(){
this.scroller=new dojox.grid._Scroller();
this.scroller.grid=this;
this.scroller.renderRow=dojo.hitch(this,"renderRow");
this.scroller.removeRow=dojo.hitch(this,"rowRemoved");
},createLayout:function(){
this.layout=new this._layoutClass(this);
this.connect(this.layout,"moveColumn","onMoveColumn");
},onMoveColumn:function(){
this.render();
},onResizeColumn:function(_4){
},createViews:function(){
this.views=new dojox.grid._ViewManager(this);
this.views.createView=dojo.hitch(this,"createView");
},createView:function(_5,_6){
var c=dojo.getObject(_5);
var _7=new c({grid:this,index:_6});
this.viewsNode.appendChild(_7.domNode);
this.viewsHeaderNode.appendChild(_7.headerNode);
this.views.addView(_7);
return _7;
},buildViews:function(){
for(var i=0,vs;(vs=this.layout.structure[i]);i++){
this.createView(vs.type||dojox._scopeName+".grid._View",i).setStructure(vs);
}
this.scroller.setContentNodes(this.views.getContentNodes());
},_setStructureAttr:function(_8){
var s=_8;
if(s&&dojo.isString(s)){
dojo.deprecated("dojox.grid._Grid.attr('structure', 'objVar')","use dojox.grid._Grid.attr('structure', objVar) instead","2.0");
s=dojo.getObject(s);
}
this.structure=s;
if(!s){
if(this.layout.structure){
s=this.layout.structure;
}else{
return;
}
}
this.views.destroyViews();
if(s!==this.layout.structure){
this.layout.setStructure(s);
}
this._structureChanged();
},setStructure:function(_9){
dojo.deprecated("dojox.grid._Grid.setStructure(obj)","use dojox.grid._Grid.attr('structure', obj) instead.","2.0");
this._setStructureAttr(_9);
},getColumnTogglingItems:function(){
return dojo.map(this.layout.cells,function(_a){
if(!_a.menuItems){
_a.menuItems=[];
}
var _b=this;
var _c=new dijit.CheckedMenuItem({label:_a.name,checked:!_a.hidden,_gridCell:_a,onChange:function(_d){
if(_b.layout.setColumnVisibility(this._gridCell.index,_d)){
var _e=this._gridCell.menuItems;
if(_e.length>1){
dojo.forEach(_e,function(_f){
if(_f!==this){
_f.setAttribute("checked",_d);
}
},this);
}
_d=dojo.filter(_b.layout.cells,function(c){
if(c.menuItems.length>1){
dojo.forEach(c.menuItems,"item.attr('disabled', false);");
}else{
c.menuItems[0].attr("disabled",false);
}
return !c.hidden;
});
if(_d.length==1){
dojo.forEach(_d[0].menuItems,"item.attr('disabled', true);");
}
}
},destroy:function(){
var _10=dojo.indexOf(this._gridCell.menuItems,this);
this._gridCell.menuItems.splice(_10,1);
delete this._gridCell;
dijit.CheckedMenuItem.prototype.destroy.apply(this,arguments);
}});
_a.menuItems.push(_c);
return _c;
},this);
},_setHeaderMenuAttr:function(_11){
if(this._placeholders&&this._placeholders.length){
dojo.forEach(this._placeholders,function(p){
p.unReplace(true);
});
this._placeholders=[];
}
if(this.headerMenu){
this.headerMenu.unBindDomNode(this.viewsHeaderNode);
}
this.headerMenu=_11;
if(!_11){
return;
}
this.headerMenu.bindDomNode(this.viewsHeaderNode);
if(this.headerMenu.getPlaceholders){
this._placeholders=this.headerMenu.getPlaceholders(this.placeholderLabel);
}
},setHeaderMenu:function(_12){
dojo.deprecated("dojox.grid._Grid.setHeaderMenu(obj)","use dojox.grid._Grid.attr('headerMenu', obj) instead.","2.0");
this._setHeaderMenuAttr(_12);
},setupHeaderMenu:function(){
if(this._placeholders&&this._placeholders.length){
dojo.forEach(this._placeholders,function(p){
if(p._replaced){
p.unReplace(true);
}
p.replace(this.getColumnTogglingItems());
},this);
}
},_fetch:function(_13){
this.setScrollTop(0);
},getItem:function(_14){
return null;
},showMessage:function(_15){
if(_15){
this.messagesNode.innerHTML=_15;
this.messagesNode.style.display="";
}else{
this.messagesNode.innerHTML="";
this.messagesNode.style.display="none";
}
},_structureChanged:function(){
this.buildViews();
if(this.autoRender&&this._started){
this.render();
}
},hasLayout:function(){
return this.layout.cells.length;
},resize:function(_16,_17){
this._pendingChangeSize=_16;
this._pendingResultSize=_17;
this.sizeChange();
},_getPadBorder:function(){
this._padBorder=this._padBorder||dojo._getPadBorderExtents(this.domNode);
return this._padBorder;
},_getHeaderHeight:function(){
var vns=this.viewsHeaderNode.style,t=vns.display=="none"?0:this.views.measureHeader();
vns.height=t+"px";
this.views.normalizeHeaderNodeHeight();
return t;
},_resize:function(_18,_19){
_18=_18||this._pendingChangeSize;
_19=_19||this._pendingResultSize;
delete this._pendingChangeSize;
delete this._pendingResultSize;
if(!this.domNode){
return;
}
var pn=this.domNode.parentNode;
if(!pn||pn.nodeType!=1||!this.hasLayout()||pn.style.visibility=="hidden"||pn.style.display=="none"){
return;
}
var _1a=this._getPadBorder();
var hh=undefined;
var h;
if(this._autoHeight){
this.domNode.style.height="auto";
this.viewsNode.style.height="";
}else{
if(typeof this.autoHeight=="number"){
h=hh=this._getHeaderHeight();
h+=(this.scroller.averageRowHeight*this.autoHeight);
this.domNode.style.height=h+"px";
}else{
if(this.domNode.clientHeight<=_1a.h){
if(pn==document.body){
this.domNode.style.height=this.defaultHeight;
}else{
if(this.height){
this.domNode.style.height=this.height;
}else{
this.fitTo="parent";
}
}
}
}
}
if(_19){
_18=_19;
}
if(_18){
dojo.marginBox(this.domNode,_18);
this.height=this.domNode.style.height;
delete this.fitTo;
}else{
if(this.fitTo=="parent"){
h=this._parentContentBoxHeight=this._parentContentBoxHeight||dojo._getContentBox(pn).h;
this.domNode.style.height=Math.max(0,h)+"px";
}
}
var _1b=dojo.some(this.views.views,function(v){
return v.flexCells;
});
if(!this._autoHeight&&(h||dojo._getContentBox(this.domNode).h)===0){
this.viewsHeaderNode.style.display="none";
}else{
this.viewsHeaderNode.style.display="block";
if(!_1b&&hh===undefined){
hh=this._getHeaderHeight();
}
}
if(_1b){
hh=undefined;
}
this.adaptWidth();
this.adaptHeight(hh);
this.postresize();
},adaptWidth:function(){
var _1c=(!this.initialWidth&&this.autoWidth);
var w=_1c?0:this.domNode.clientWidth||(this.domNode.offsetWidth-this._getPadBorder().w),vw=this.views.arrange(1,w);
this.views.onEach("adaptWidth");
if(_1c){
this.domNode.style.width=vw+"px";
}
},adaptHeight:function(_1d){
var t=_1d===undefined?this._getHeaderHeight():_1d;
var h=(this._autoHeight?-1:Math.max(this.domNode.clientHeight-t,0)||0);
this.views.onEach("setSize",[0,h]);
this.views.onEach("adaptHeight");
if(!this._autoHeight){
var _1e=0,_1f=0;
var _20=dojo.filter(this.views.views,function(v){
var has=v.hasHScrollbar();
if(has){
_1e++;
}else{
_1f++;
}
return (!has);
});
if(_1e>0&&_1f>0){
dojo.forEach(_20,function(v){
v.adaptHeight(true);
});
}
}
if(this.autoHeight===true||h!=-1||(typeof this.autoHeight=="number"&&this.autoHeight>=this.attr("rowCount"))){
this.scroller.windowHeight=h;
}else{
this.scroller.windowHeight=Math.max(this.domNode.clientHeight-t,0);
}
},startup:function(){
if(this._started){
return;
}
this.inherited(arguments);
if(this.autoRender){
this.render();
}
},render:function(){
if(!this.domNode){
return;
}
if(!this._started){
return;
}
if(!this.hasLayout()){
this.scroller.init(0,this.keepRows,this.rowsPerPage);
return;
}
this.update=this.defaultUpdate;
this._render();
},_render:function(){
this.scroller.init(this.attr("rowCount"),this.keepRows,this.rowsPerPage);
this.prerender();
this.setScrollTop(0);
this.postrender();
},prerender:function(){
this.keepRows=this._autoHeight?0:this.keepRows;
this.scroller.setKeepInfo(this.keepRows);
this.views.render();
this._resize();
},postrender:function(){
this.postresize();
this.focus.initFocusView();
dojo.setSelectable(this.domNode,this.selectable);
},postresize:function(){
if(this._autoHeight){
var _21=Math.max(this.views.measureContent())+"px";
this.viewsNode.style.height=_21;
}
},renderRow:function(_22,_23){
this.views.renderRow(_22,_23,this._skipRowRenormalize);
},rowRemoved:function(_24){
this.views.rowRemoved(_24);
},invalidated:null,updating:false,beginUpdate:function(){
this.invalidated=[];
this.updating=true;
},endUpdate:function(){
this.updating=false;
var i=this.invalidated,r;
if(i.all){
this.update();
}else{
if(i.rowCount!=undefined){
this.updateRowCount(i.rowCount);
}else{
for(r in i){
this.updateRow(Number(r));
}
}
}
this.invalidated=[];
},defaultUpdate:function(){
if(!this.domNode){
return;
}
if(this.updating){
this.invalidated.all=true;
return;
}
var _25=this.scrollTop;
this.prerender();
this.scroller.invalidateNodes();
this.setScrollTop(_25);
this.postrender();
},update:function(){
this.render();
},updateRow:function(_26){
_26=Number(_26);
if(this.updating){
this.invalidated[_26]=true;
}else{
this.views.updateRow(_26);
this.scroller.rowHeightChanged(_26);
}
},updateRows:function(_27,_28){
_27=Number(_27);
_28=Number(_28);
var i;
if(this.updating){
for(i=0;i<_28;i++){
this.invalidated[i+_27]=true;
}
}else{
for(i=0;i<_28;i++){
this.views.updateRow(i+_27,this._skipRowRenormalize);
}
this.scroller.rowHeightChanged(_27);
}
},updateRowCount:function(_29){
if(this.updating){
this.invalidated.rowCount=_29;
}else{
this.rowCount=_29;
this._setAutoHeightAttr(this.autoHeight,true);
if(this.layout.cells.length){
this.scroller.updateRowCount(_29);
}
this._resize();
if(this.layout.cells.length){
this.setScrollTop(this.scrollTop);
}
}
},updateRowStyles:function(_2a){
this.views.updateRowStyles(_2a);
},getRowNode:function(_2b){
if(this.focus.focusView&&!(this.focus.focusView instanceof dojox.grid._RowSelector)){
return this.focus.focusView.rowNodes[_2b];
}else{
for(var i=0,_2c;(_2c=this.views.views[i]);i++){
if(!(_2c instanceof dojox.grid._RowSelector)){
return _2c.rowNodes[_2b];
}
}
}
return null;
},rowHeightChanged:function(_2d){
this.views.renormalizeRow(_2d);
this.scroller.rowHeightChanged(_2d);
},fastScroll:true,delayScroll:false,scrollRedrawThreshold:(dojo.isIE?100:50),scrollTo:function(_2e){
if(!this.fastScroll){
this.setScrollTop(_2e);
return;
}
var _2f=Math.abs(this.lastScrollTop-_2e);
this.lastScrollTop=_2e;
if(_2f>this.scrollRedrawThreshold||this.delayScroll){
this.delayScroll=true;
this.scrollTop=_2e;
this.views.setScrollTop(_2e);
if(this._pendingScroll){
window.clearTimeout(this._pendingScroll);
}
var _30=this;
this._pendingScroll=window.setTimeout(function(){
delete _30._pendingScroll;
_30.finishScrollJob();
},200);
}else{
this.setScrollTop(_2e);
}
},finishScrollJob:function(){
this.delayScroll=false;
this.setScrollTop(this.scrollTop);
},setScrollTop:function(_31){
this.scroller.scroll(this.views.setScrollTop(_31));
},scrollToRow:function(_32){
this.setScrollTop(this.scroller.findScrollTop(_32)+1);
},styleRowNode:function(_33,_34){
if(_34){
this.rows.styleRowNode(_33,_34);
}
},_mouseOut:function(e){
this.rows.setOverRow(-2);
},getCell:function(_35){
return this.layout.cells[_35];
},setCellWidth:function(_36,_37){
this.getCell(_36).unitWidth=_37;
},getCellName:function(_38){
return "Cell "+_38.index;
},canSort:function(_39){
},sort:function(){
},getSortAsc:function(_3a){
_3a=_3a==undefined?this.sortInfo:_3a;
return Boolean(_3a>0);
},getSortIndex:function(_3b){
_3b=_3b==undefined?this.sortInfo:_3b;
return Math.abs(_3b)-1;
},setSortIndex:function(_3c,_3d){
var si=_3c+1;
if(_3d!=undefined){
si*=(_3d?1:-1);
}else{
if(this.getSortIndex()==_3c){
si=-this.sortInfo;
}
}
this.setSortInfo(si);
},setSortInfo:function(_3e){
if(this.canSort(_3e)){
this.sortInfo=_3e;
this.sort();
this.update();
}
},doKeyEvent:function(e){
e.dispatch="do"+e.type;
this.onKeyEvent(e);
},_dispatch:function(m,e){
if(m in this){
return this[m](e);
}
return false;
},dispatchKeyEvent:function(e){
this._dispatch(e.dispatch,e);
},dispatchContentEvent:function(e){
this.edit.dispatchEvent(e)||e.sourceView.dispatchContentEvent(e)||this._dispatch(e.dispatch,e);
},dispatchHeaderEvent:function(e){
e.sourceView.dispatchHeaderEvent(e)||this._dispatch("doheader"+e.type,e);
},dokeydown:function(e){
this.onKeyDown(e);
},doclick:function(e){
if(e.cellNode){
this.onCellClick(e);
}else{
this.onRowClick(e);
}
},dodblclick:function(e){
if(e.cellNode){
this.onCellDblClick(e);
}else{
this.onRowDblClick(e);
}
},docontextmenu:function(e){
if(e.cellNode){
this.onCellContextMenu(e);
}else{
this.onRowContextMenu(e);
}
},doheaderclick:function(e){
if(e.cellNode){
this.onHeaderCellClick(e);
}else{
this.onHeaderClick(e);
}
},doheaderdblclick:function(e){
if(e.cellNode){
this.onHeaderCellDblClick(e);
}else{
this.onHeaderDblClick(e);
}
},doheadercontextmenu:function(e){
if(e.cellNode){
this.onHeaderCellContextMenu(e);
}else{
this.onHeaderContextMenu(e);
}
},doStartEdit:function(_3f,_40){
this.onStartEdit(_3f,_40);
},doApplyCellEdit:function(_41,_42,_43){
this.onApplyCellEdit(_41,_42,_43);
},doCancelEdit:function(_44){
this.onCancelEdit(_44);
},doApplyEdit:function(_45){
this.onApplyEdit(_45);
},addRow:function(){
this.updateRowCount(this.attr("rowCount")+1);
},removeSelectedRows:function(){
if(this.allItemsSelected){
this.updateRowCount(0);
}else{
this.updateRowCount(Math.max(0,this.attr("rowCount")-this.selection.getSelected().length));
}
this.selection.clear();
}});
dojox.grid._Grid.markupFactory=function(_46,_47,_48,_49){
var d=dojo;
var _4a=function(n){
var w=d.attr(n,"width")||"auto";
if((w!="auto")&&(w.slice(-2)!="em")&&(w.slice(-1)!="%")){
w=parseInt(w,10)+"px";
}
return w;
};
if(!_46.structure&&_47.nodeName.toLowerCase()=="table"){
_46.structure=d.query("> colgroup",_47).map(function(cg){
var sv=d.attr(cg,"span");
var v={noscroll:(d.attr(cg,"noscroll")=="true")?true:false,__span:(!!sv?parseInt(sv,10):1),cells:[]};
if(d.hasAttr(cg,"width")){
v.width=_4a(cg);
}
return v;
});
if(!_46.structure.length){
_46.structure.push({__span:Infinity,cells:[]});
}
d.query("thead > tr",_47).forEach(function(tr,_4b){
var _4c=0;
var _4d=0;
var _4e;
var _4f=null;
d.query("> th",tr).map(function(th){
if(!_4f){
_4e=0;
_4f=_46.structure[0];
}else{
if(_4c>=(_4e+_4f.__span)){
_4d++;
_4e+=_4f.__span;
var _50=_4f;
_4f=_46.structure[_4d];
}
}
var _51={name:d.trim(d.attr(th,"name")||th.innerHTML),colSpan:parseInt(d.attr(th,"colspan")||1,10),type:d.trim(d.attr(th,"cellType")||""),id:d.trim(d.attr(th,"id")||"")};
_4c+=_51.colSpan;
var _52=d.attr(th,"rowspan");
if(_52){
_51.rowSpan=_52;
}
if(d.hasAttr(th,"width")){
_51.width=_4a(th);
}
if(d.hasAttr(th,"relWidth")){
_51.relWidth=window.parseInt(dojo.attr(th,"relWidth"),10);
}
if(d.hasAttr(th,"hidden")){
_51.hidden=d.attr(th,"hidden")=="true";
}
if(_49){
_49(th,_51);
}
_51.type=_51.type?dojo.getObject(_51.type):dojox.grid.cells.Cell;
if(_51.type&&_51.type.markupFactory){
_51.type.markupFactory(th,_51);
}
if(!_4f.cells[_4b]){
_4f.cells[_4b]=[];
}
_4f.cells[_4b].push(_51);
});
});
}
return new _48(_46,_47);
};
})();
}
