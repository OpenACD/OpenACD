/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.grid.enhanced.dnd._DndMovingManager"]){
dojo._hasResource["dojox.grid.enhanced.dnd._DndMovingManager"]=true;
dojo.provide("dojox.grid.enhanced.dnd._DndMovingManager");
dojo.require("dojox.grid.enhanced.dnd._DndSelectingManager");
dojo.require("dojox.grid.enhanced.dnd._DndMover");
dojo.require("dojo.dnd.move");
dojo.declare("dojox.grid.enhanced.dnd._DndMovingManager",dojox.grid.enhanced.dnd._DndSelectingManager,{exceptRowsTo:-1,exceptColumnsTo:-1,coverDIVs:[],movers:[],constructor:function(_1){
if(this.grid.indirectSelection){
this.exceptColumnsTo=this.grid.pluginMgr.getFixedCellNumber()-1;
}
this.coverDIVs=this.movers=[];
dojo.subscribe("CTRL_KEY_DOWN",dojo.hitch(this,function(_2,_3){
if(_2==this.grid&&_2!=this){
this.keyboardMove(_3);
}
}));
dojo.forEach(this.grid.views.views,function(_4){
dojo.connect(_4.scrollboxNode,"onscroll",dojo.hitch(this,function(){
this.clearDrugDivs();
}));
},this);
},getGridWidth:function(){
return dojo.contentBox(this.grid.domNode).w-this.grid.views.views[0].getWidth().replace("px","");
},isColSelected:function(_5){
return this.selectedColumns[_5]&&_5>this.exceptColumnsTo;
},getHScrollBarHeight:function(){
this.scrollbarHeight=0;
dojo.forEach(this.grid.views.views,function(_6,_7){
if(_6.scrollboxNode){
var _8=_6.scrollboxNode.offsetHeight-_6.scrollboxNode.clientHeight;
this.scrollbarHeight=_8>this.scrollbarHeight?_8:this.scrollbarHeight;
}
},this);
return this.scrollbarHeight;
},getExceptionalColOffsetWidth:function(){
if(!this.grid.indirectSelection||!this.grid.rowSelectCell){
return 0;
}
var _9=(normalizedOffsetWidth=0),_a=this.grid.rowSelectCell.view.scrollboxNode;
dojo.forEach(this.getHeaderNodes(),function(_b,_c){
if(_c<=this.exceptColumnsTo){
var _d=dojo.coords(_b);
_9+=_d.w;
}
},this);
normalizedOffsetWidth=_9-_a.scrollLeft*(dojo._isBodyLtr()?1:(dojo.isMoz?-1:1));
return normalizedOffsetWidth>0?normalizedOffsetWidth:0;
},getGridCoords:function(_e){
if(!this.gridCoords||_e){
this.gridCoords=new Object();
if(!this.headerHeight){
this.headerHeight=dojo.coords(this.getHeaderNodes()[0]).h;
}
var _f=dojo.coords(this.grid.views.views[0].domNode);
var _10=dojo.coords(this.grid.domNode);
var _11=dojo.contentBox(this.grid.domNode);
this.gridCoords.h=_11.h-this.headerHeight-this.getHScrollBarHeight();
this.gridCoords.t=_10.y;
this.gridCoords.l=dojo._isBodyLtr()?(_10.x+_f.w):_10.x;
this.gridCoords.w=_11.w-_f.w;
}
return this.gridCoords;
},createAvatar:function(_12,_13,_14,top,_15){
this.gridCoords=null;
var _16=this.getGridCoords();
var _17=dojo.doc.createElement("DIV");
_17.className="dojoxGridSelectedDIV";
_17.id="grid_dnd_cover_div_"+_14+"_"+top;
_17.style.width=_12+"px";
var _18=dojo._docScroll();
var _19=top<_16.t+this.headerHeight?_16.t+this.headerHeight-top:0;
var _1a=_16.t+_16.h+this.headerHeight;
var _1b=0;
if(top<_16.t+this.headerHeight){
_1b=(_16.t+this.headerHeight);
}else{
if(top>_1a){
_1b=10000;
}else{
_1b=top;
}
}
_17.style.top=_1b+_18.y+"px";
_17.style.left=(_14+_18.x)+"px";
var _1c=_1b+_13-_19;
if(_1c>_1a+(_15?this.scrollbarHeight:0)){
_1c=_1a;
}
_17.style.height=((_1c-_1b)>=0?(_1c-_1b):0)+"px";
dojo.doc.body.appendChild(_17);
_17.connections=[];
_17.connections.push(dojo.connect(_17,"onmouseout",this,function(){
this.clearDrugDivs();
}));
_17.connections.push(dojo.connect(_17,"onclick",this,"avataDivClick"));
_17.connections.push(dojo.connect(_17,"keydown",this,function(e){
this.handleESC(e,this);
}));
this.coverDIVs.push(_17);
return _17;
},handleESC:function(e,_1d){
var dk=dojo.keys;
switch(e.keyCode){
case dk.ESCAPE:
try{
this.cancelDND();
}
catch(e){
}
break;
}
},cancelDND:function(){
this.cleanAll();
this.clearDrugDivs();
if(this.mover){
this.mover.destroy();
}
this.cleanAll();
},createCoverMover:function(_1e,_1f,_20,top,_21){
var _22=this.getGridCoords(),_23=(_21=="col"?true:false);
var _24={box:{l:(_21=="row"?_20:_22.l)+dojo._docScroll().x,t:(_21=="col"?top:_22.t+this.headerHeight)+dojo._docScroll().y,w:_21=="row"?1:_22.w,h:_21=="col"?1:_22.h},within:true,movingType:_21,mover:dojox.grid.enhanced.dnd._DndMover};
return new dojox.grid.enhanced.dnd._DndBoxConstrainedMoveable(this.createAvatar(_1e,_1f,_20,top,_23),_24);
},getBorderDiv:function(){
var _25=dojo.byId("borderDIV"+this.grid.id);
if(_25==null){
_25=dojo.doc.createElement("DIV");
_25.id="borderDIV"+this.grid.id;
_25.className="dojoxGridBorderDIV";
dojo.doc.body.appendChild(_25);
}
return _25;
},setBorderDiv:function(_26,_27,_28,top){
var _29=this.getBorderDiv();
dojo.style(_29,{"height":_27+"px","top":top+"px","width":_26+"px","left":_28+"px"});
return _29;
},removeOtherMovers:function(id){
if(!this.coverDIVs.hasRemovedOtherMovers){
var _2a;
dojo.forEach(this.coverDIVs,function(div){
if(div.id!=id){
dojo.doc.body.removeChild(div);
}else{
_2a=div;
}
},this);
this.coverDIVs=[_2a];
this.coverDIVs.hasRemovedOtherMovers=true;
}
},addColMovers:function(){
var _2b=-1;
dojo.forEach(this.selectedColumns,function(col,_2c){
if(this.isColSelected(_2c)){
if(_2b==-1){
_2b=_2c;
}
if(this.selectedColumns[_2c+1]==null){
this.addColMover(_2b,_2c);
_2b=-1;
}
}
},this);
},addColMover:function(_2d,_2e){
if(this.lock){
return;
}
var _2f=(_30=0);
var top=null,_31=null;
if(dojo._isBodyLtr()){
dojo.forEach(this.getHeaderNodes(),function(_32,_33){
var _34=dojo.coords(_32);
if(_33==_2d){
_2f=_34.x;
top=_34.y+_34.h;
_31=_34.h;
}
if(_33==_2e){
_30=_34.x+_34.w;
}
});
}else{
dojo.forEach(this.getHeaderNodes(),function(_35,_36){
var _37=dojo.coords(_35);
if(_36==_2d){
_30=_37.x+_37.w;
_31=_37.h;
}
if(_36==_2e){
_2f=_37.x;
top=_37.y+_37.h;
}
});
}
var _38=this.normalizeColMoverCoords(_2f,_30,_2d,_2e);
var _39=_38.h,_3a=_38.w,_2f=_38.l,_30=_38.r;
var _3b=this.createCoverMover(_3a,_39,_2f,top,"col");
this.movers.push(_3b);
var _3c=this.setBorderDiv(3,_39,-1000,top+dojo._docScroll().y);
dojo.attr(_3c,"colH",_38.colH);
dojo.connect(_3b,"onMoveStart",dojo.hitch(this,function(_3d,_3e){
this.mover=_3d;
this.removeOtherMovers(_3d.node.id);
}));
dojo.connect(_3b,"onMove",dojo.hitch(this,function(_3f,_40,_41){
if(_3f.node==null||_3f.node.parentNode==null){
return;
}
this.isMoving=true;
this.moveColBorder(_3f,_41,_3c);
}));
dojo.connect(_3b,"onMoveStop",dojo.hitch(this,function(_42){
if(this.drugDestIndex==null||this.isContinuousSelection(this.selectedColumns)&&(this.drugDestIndex==_2d||this.drugDestIndex==_2e||this.drugDestIndex==(_2e+1)&&this.drugBefore)){
this.movingIgnored=true;
if(this.isMoving){
this.isMoving=false;
this.clearDrugDivs();
}
return;
}
this.isMoving=false;
this.mover=null;
this.startMoveCols();
this.drugDestIndex=null;
}));
},normalizeColMoverCoords:function(_43,_44,_45,_46){
var _47=_44-_43,_48=this.grid.views.views,_49=this.grid.pluginMgr;
var _4a={"w":_47,"h":0,"l":_43,"r":_44,"colH":0};
var _4b=this.getGridWidth()-_48[_48.length-1].getScrollbarWidth();
var rtl=!dojo._isBodyLtr();
var _4c=_49.getViewByCellIdx(!rtl?_45:_46);
var _4d=_49.getViewByCellIdx(!rtl?_46:_45);
var _4e=(_4c==_4d);
if(!_4c||!_4d){
return _4a;
}
var _4f=dojo.coords(_4c.scrollboxNode).x+(rtl&&dojo.isIE?_4c.getScrollbarWidth():0);
var _50=dojo.coords(_4d.scrollboxNode);
var _51=_50.x+_50.w-((!rtl||!dojo.isIE)?_4d.getScrollbarWidth():0);
if(_4a.l<_4f){
_4a.w=_4a.r-_4f;
_4a.l=_4f;
}
if(_4a.r>_51){
_4a.w=_51-_4a.l;
}
var i,_52=this.grid.views.views[0],_53=dojo.coords(_52.contentNode).h;
var _54=_4d,_55=_50.h;
_4a.colH=_53;
_55=!_4e?_55:(_55-(_54.scrollboxNode.offsetHeight-_54.scrollboxNode.clientHeight));
_4a.h=_53<_55?_53:_55;
return _4a;
},moveColBorder:function(_56,_57,_58){
var _59=dojo._docScroll(),rtl=!dojo._isBodyLtr();
_57.x-=_59.x;
var _5a=this.grid.views.views,_5b=this.getGridCoords();
var _5c=_5a[!rtl?1:_5a.length-1].scrollboxNode;
var _5d=_5a[!rtl?_5a.length-1:1].scrollboxNode;
var _5e=(!rtl||!dojo.isIE)?_5b.l:(_5b.l+_5c.offsetWidth-_5c.clientWidth);
var _5f=(!rtl||dojo.isMoz)?(_5b.l+_5b.w-(_5d.offsetWidth-_5d.clientWidth)):(_5b.l+_5b.w);
dojo.forEach(this.getHeaderNodes(),dojo.hitch(this,function(_60,_61){
if(_61>this.exceptColumnsTo){
var x,_62=dojo.coords(_60);
if(_57.x>=_62.x&&_57.x<=_62.x+_62.w){
if(!this.selectedColumns[_61]||!this.selectedColumns[_61-1]){
x=_62.x+_59.x+(rtl?_62.w:0);
if(_57.x<_5e||_57.x>_5f||x<_5e||x>_5f){
return;
}
dojo.style(_58,"left",x+"px");
this.drugDestIndex=_61;
this.drugBefore=true;
!dojo.isIE&&this.normalizeColBorderHeight(_58,_61);
}
}else{
if(this.getHeaderNodes()[_61+1]==null&&(!rtl?(_57.x>_62.x+_62.w):(_57.x<_62.x))){
x=_57.x<_5e?_5e:(_57.x>_5f?_5f:(_62.x+_59.x+(rtl?0:_62.w)));
dojo.style(_58,"left",x+"px");
this.drugDestIndex=_61;
this.drugBefore=false;
!dojo.isIE&&this.normalizeColBorderHeight(_58,_61);
}
}
}
}));
},normalizeColBorderHeight:function(_63,_64){
var _65=this.grid.pluginMgr.getViewByCellIdx(_64);
if(!_65){
return;
}
var _66=_65.scrollboxNode,_67=dojo.attr(_63,"colH");
var _68=dojo.coords(_66).h-(_66.offsetHeight-_66.clientHeight);
_68=_67>0&&_67<_68?_67:_68;
dojo.style(_63,"height",_68+"px");
},avataDivClick:function(e){
if(this.movingIgnored){
this.movingIgnored=false;
return;
}
this.cleanAll();
this.clearDrugDivs();
},startMoveCols:function(){
this.changeCursorState("wait");
this.srcIndexdelta=0;
deltaColAmount=0;
dojo.forEach(this.selectedColumns,dojo.hitch(this,function(col,_69){
if(this.isColSelected(_69)){
if(this.drugDestIndex>_69){
_69-=deltaColAmount;
}
deltaColAmount+=1;
var _6a=this.grid.layout.cells[_69].view.idx;
var _6b=this.grid.layout.cells[this.drugDestIndex].view.idx;
if(_69!=this.drugDestIndex){
this.grid.layout.moveColumn(_6a,_6b,_69,this.drugDestIndex,this.drugBefore);
}
if(this.drugDestIndex<=_69&&this.drugDestIndex+1<this.grid.layout.cells.length){
this.drugDestIndex+=1;
}
}
}));
var _6c=this.drugDestIndex+(this.drugBefore?0:1);
this.clearDrugDivs();
this.cleanAll();
this.resetCellIdx();
this.drugSelectionStart.colIndex=_6c-deltaColAmount;
this.drugSelectColumn(this.drugSelectionStart.colIndex+deltaColAmount-1);
},changeCursorState:function(_6d){
dojo.forEach(this.coverDIVs,function(div){
div.style.cursor="wait";
});
},addRowMovers:function(){
var _6e=-1;
dojo.forEach(this.grid.selection.selected,function(row,_6f){
var _70=this.grid.views.views[0];
if(row&&_70.rowNodes[_6f]){
if(_6e==-1){
_6e=_6f;
}
if(this.grid.selection.selected[_6f+1]==null||!_70.rowNodes[_6f+1]){
this.addRowMover(_6e,_6f);
_6e=-1;
}
}
},this);
},addRowMover:function(_71,to){
var _72=0,_73=this.grid.views.views;
dojo.forEach(_73,function(_74,_75){
_72+=_74.getScrollbarWidth();
});
var _76=_73[_73.length-1].getScrollbarWidth();
var _77=!dojo._isBodyLtr()?(dojo.isIE?_72-_76:_72):0;
var _78=this.getGridWidth()-_76;
var _79=this.grid.views.views[0];
var _7a=_79.rowNodes[_71],_7b=_79.rowNodes[to];
if(!_7a||!_7b){
return;
}
var _7c=dojo.coords(_7a),_7d=dojo.coords(_7b);
var _7e=this.getExceptionalColOffsetWidth();
var _7f=this.createCoverMover(_78-_7e,(_7d.y-_7c.y+_7d.h),dojo._isBodyLtr()?(_7c.x+_7c.w+_7e):(_7c.x-_78-_77),_7c.y,"row");
var _80=this.setBorderDiv(_78,3,(dojo._isBodyLtr()?(_7d.x+_7d.w):(_7d.x-_78-_77))+dojo._docScroll().x,-100);
var _81=dojo.connect(_7f,"onMoveStart",dojo.hitch(this,function(_82,_83){
this.mover=_82;
this.removeOtherMovers(_82.node.id);
}));
var _84=dojo.connect(_7f,"onMove",dojo.hitch(this,function(_85,_86,_87){
if(_85.node==null||_85.node.parentNode==null){
return;
}
this.isMoving=true;
this.moveRowBorder(_85,_86,_80,_87);
}));
var _88=dojo.connect(_7f,"onMoveStop",dojo.hitch(this,function(_89){
if(this.avaOnRowIndex==null||this.isContinuousSelection(this.grid.selection.selected)&&(this.avaOnRowIndex==_71||this.avaOnRowIndex==(to+1))){
this.movingIgnored=true;
if(this.isMoving){
this.isMoving=false;
this.clearDrugDivs();
}
return;
}
this.isMoving=false;
this.mover=null;
this.grid.select.outRangeY=false;
this.grid.select.moveOutTop=false;
this.grid.scroller.findScrollTop(this.grid.scroller.page*this.grid.scroller.rowsPerPage);
this.startMoveRows();
this.avaOnRowIndex=null;
delete _7f;
}));
},moveRowBorder:function(_8a,_8b,_8c,_8d){
var _8e=this.getGridCoords(true),_8f=dojo._docScroll();
var _90=_8e.t+this.headerHeight+_8e.h;
_8b.t-=_8f.y,_8d.y-=_8f.y;
if(_8d.y>=_90){
this.grid.select.outRangeY=true;
this.autoMoveToNextRow();
}else{
if(_8d.y<=_8e.t+this.headerHeight){
this.grid.select.moveOutTop=true;
this.autoMoveToPreRow();
}else{
this.grid.select.outRangeY=this.grid.select.moveOutTop=false;
var _91=this.grid.views.views[0],_92=_91.rowNodes;
var _93=dojo.coords(_91.contentNode).h;
var _94=0,_95=-1;
for(i in _92){
++_94;
if(i>_95){
_95=i;
}
}
var _96=dojo.coords(_92[_95]);
if(_93<_8e.h&&_8d.y>(_96.y+_96.h)){
this.avaOnRowIndex=_94;
dojo.style(_8c,{"top":_96.y+_96.h+_8f.y+"px"});
return;
}
var _97,_98,_99;
for(var _9a in _92){
_9a=parseInt(_9a);
if(isNaN(_9a)){
continue;
}
_98=_92[_9a];
if(!_98){
continue;
}
_97=dojo.coords(_98),_99=(_97.y<=_90);
if(_99&&_8d.y>_97.y&&_8d.y<_97.y+_97.h){
if(!this.grid.selection.selected[_9a]||!this.grid.selection.selected[_9a-1]){
this.avaOnRowIndex=_9a;
dojo.style(_8c,{"top":_97.y+_8f.y+"px"});
}
}
}
}
}
},autoMoveToPreRow:function(){
if(this.grid.select.moveOutTop){
if(this.grid.scroller.firstVisibleRow>0){
this.grid.scrollToRow(this.grid.scroller.firstVisibleRow-1);
this.autoMoveBorderDivPre();
setTimeout(dojo.hitch(this,"autoMoveToPreRow"),this.autoScrollRate);
}
}
},autoMoveBorderDivPre:function(){
var _9b=dojo._docScroll(),_9c=this.getGridCoords();
var _9d=_9c.t+this.headerHeight+_9b.y;
var _9e,_9f=this.getBorderDiv();
if(this.avaOnRowIndex-1<=0){
this.avaOnRowIndex=0;
_9e=_9d;
}else{
this.avaOnRowIndex--;
_9e=dojo.coords(this.grid.views.views[0].rowNodes[this.avaOnRowIndex]).y+_9b.y;
}
_9f.style.top=(_9e<_9d?_9d:_9e)+"px";
},autoMoveToNextRow:function(){
if(this.grid.select.outRangeY){
this.grid.scrollToRow(this.grid.scroller.firstVisibleRow+1);
this.autoMoveBorderDiv();
setTimeout(dojo.hitch(this,"autoMoveToNextRow"),this.autoScrollRate);
}
},autoMoveBorderDiv:function(){
var _a0=dojo._docScroll(),_a1=this.getGridCoords();
var _a2=_a1.t+this.headerHeight+_a1.h+_a0.y;
var _a3,_a4=this.getBorderDiv();
if(this.avaOnRowIndex+1>=this.grid.scroller.rowCount){
this.avaOnRowIndex=this.grid.scroller.rowCount;
_a3=_a2;
}else{
this.avaOnRowIndex++;
_a3=dojo.coords(this.grid.views.views[0].rowNodes[this.avaOnRowIndex]).y+_a0.y;
}
_a4.style.top=(_a3>_a2?_a2:_a3)+"px";
},startMoveRows:function(){
var _a5=-1;
var _a6=0;
dojo.forEach(this.grid.selection.selected,function(row,_a7){
if(row){
if(_a5==-1){
_a5=_a7;
}
if(this.grid.selection.selected[_a7+1]==null){
_a6=this.moveRows(_a5,_a7,_a6);
_a5=-1;
}
}
},this);
try{
this.clearDrugDivs();
this.cleanAll();
this.drugSelectionStart.rowIndex=this.avaOnRowIndex-_a6;
this.drugSelectRow(this.drugSelectionStart.rowIndex+_a6-1);
this.publishRowMove();
}
catch(e){
}
},moveRows:function(_a8,end,_a9){
if(this.avaOnRowIndex>end){
_a8-=_a9;
end-=_a9;
}
var _aa=end-_a8+1;
_a9+=_aa;
var _ab=[];
for(var i=0;i<_aa;i++){
_ab[i]=this.grid._by_idx[_a8+i];
this.grid._by_idx[_a8+i]=this.grid._by_idx[_a8+i+_aa];
}
if(this.avaOnRowIndex>end){
for(i=end+1;i<this.avaOnRowIndex-_aa;i++){
this.grid._by_idx[i]=this.grid._by_idx[i+_aa];
}
var _ac=this.avaOnRowIndex-_aa;
for(i=_ac;i<this.avaOnRowIndex;i++){
this.grid._by_idx[i]=_ab[i-_ac];
}
for(i=_a8;i<this.avaOnRowIndex;i++){
this.grid.updateRow(i);
}
}else{
if(this.avaOnRowIndex<end){
for(i=end;i>this.avaOnRowIndex+_aa-1;i--){
this.grid._by_idx[i]=this.grid._by_idx[i-_aa];
}
for(i=this.avaOnRowIndex;i<this.avaOnRowIndex+_aa;i++){
this.grid._by_idx[i]=_ab[i-this.avaOnRowIndex];
}
for(i=this.avaOnRowIndex;i<=end;i++){
this.grid.updateRow(i);
}
}
}
if(this.avaOnRowIndex<=_a8){
this.avaOnRowIndex+=_aa;
}
return _a9;
},clearDrugDivs:function(){
if(!this.isMoving){
var _ad=this.getBorderDiv();
_ad.style.top=-100+"px";
_ad.style.height="0px";
_ad.style.left=-100+"px";
dojo.forEach(this.coverDIVs,function(div){
dojo.forEach(div.connections,function(_ae){
dojo.disconnect(_ae);
});
dojo.doc.body.removeChild(div);
delete div;
},this);
this.coverDIVs=[];
}
},setDrugCoverDivs:function(_af,_b0){
if(!this.isMoving){
if(this.isColSelected(_af)){
this.addColMovers();
}else{
if(this.grid.selection.selected[_b0]){
this.addRowMovers();
}else{
this.clearDrugDivs();
}
}
}
},resetCellIdx:function(){
var _b1=0;
var _b2=-1;
dojo.forEach(this.grid.views.views,function(_b3,_b4){
if(_b4==0){
return;
}
if(_b3.structure.cells&&_b3.structure.cells[0]){
dojo.forEach(_b3.structure.cells[0],function(_b5,_b6){
var _b7=_b5.markup[2].split(" ");
var idx=_b1+_b6;
_b7[1]="idx=\""+idx+"\"";
_b5.markup[2]=_b7.join(" ");
});
}
for(i in _b3.rowNodes){
if(!_b3.rowNodes[i]){
return;
}
dojo.forEach(_b3.rowNodes[i].firstChild.rows[0].cells,function(_b8,_b9){
if(_b8&&_b8.attributes){
if(_b9+_b1>_b2){
_b2=_b9+_b1;
}
var idx=document.createAttribute("idx");
idx.value=_b9+_b1;
_b8.attributes.setNamedItem(idx);
}
});
}
_b1=_b2+1;
});
},publishRowMove:function(){
dojo.publish(this.grid.rowMovedTopic,[this]);
},keyboardMove:function(_ba){
var _bb=this.selectedColumns.length>0;
var _bc=dojo.hitch(this.grid.selection,dojox.grid.Selection.prototype["getFirstSelected"])()>=0;
var i,_bd,dk=dojo.keys,_be=_ba.keyCode;
if(!dojo._isBodyLtr()){
_be=(_ba.keyCode==dk.LEFT_ARROW)?dk.RIGHT_ARROW:(_ba.keyCode==dk.RIGHT_ARROW?dk.LEFT_ARROW:_be);
}
switch(_be){
case dk.LEFT_ARROW:
if(!_bb){
return;
}
_bd=this.getHeaderNodes().length;
for(i=0;i<_bd;i++){
if(this.isColSelected(i)){
this.drugDestIndex=i-1;
this.drugBefore=true;
break;
}
}
var _bf=this.grid.indirectSelection?1:0;
(this.drugDestIndex>=_bf)?this.startMoveCols():(this.drugDestIndex=_bf);
break;
case dk.RIGHT_ARROW:
if(!_bb){
return;
}
_bd=this.getHeaderNodes().length;
this.drugBefore=true;
for(i=0;i<_bd;i++){
if(this.isColSelected(i)&&!this.isColSelected(i+1)){
this.drugDestIndex=i+2;
if(this.drugDestIndex==_bd){
this.drugDestIndex--;
this.drugBefore=false;
}
break;
}
}
if(this.drugDestIndex<_bd){
this.startMoveCols();
}
break;
case dk.UP_ARROW:
if(!_bc){
return;
}
this.avaOnRowIndex=dojo.hitch(this.grid.selection,dojox.grid.Selection.prototype["getFirstSelected"])()-1;
if(this.avaOnRowIndex>-1){
this.startMoveRows();
}
break;
case dk.DOWN_ARROW:
if(!_bc){
return;
}
for(i=0;i<this.grid.rowCount;i++){
if(this.grid.selection.selected[i]&&!this.grid.selection.selected[i+1]){
this.avaOnRowIndex=i+2;
break;
}
}
if(this.avaOnRowIndex<=this.grid.rowCount){
this.startMoveRows();
}
}
}});
}
