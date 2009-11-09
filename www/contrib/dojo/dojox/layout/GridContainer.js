/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dojox.layout.GridContainer"]){
dojo._hasResource["dojox.layout.GridContainer"]=true;
dojo.provide("dojox.layout.GridContainer");
dojo.experimental("dojox.layout.GridContainer");
dojo.require("dijit._base.focus");
dojo.require("dijit._Templated");
dojo.require("dijit._Container");
dojo.require("dijit._Contained");
dojo.require("dojo.dnd.move");
dojo.require("dojox.layout.dnd.PlottedDnd");
dojo.declare("dojox.layout.GridContainer",[dijit._Widget,dijit._Templated,dijit._Container,dijit._Contained],{templateString:dojo.cache("dojox.layout","resources/GridContainer.html","<div id=\"${id}\" class=\"gridContainer\" dojoAttachPoint=\"containerNode\" tabIndex=\"0\" dojoAttachEvent=\"onkeypress:_selectFocus\">\n\t<table class=\"gridContainerTable\" dojoAttachPoint=\"gridContainerTable\" cellspacing=\"0\" cellpadding=\"0\">\n\t\t<tbody class=\"gridContainerBody\">\n\t\t\t<tr class=\"gridContainerRow\" dojoAttachPoint=\"gridNode\"></tr>\n\t\t</tbody>\n\t</table>\n</div>\n"),isContainer:true,isAutoOrganized:true,isRightFixed:false,isLeftFixed:false,hasResizableColumns:true,nbZones:1,opacity:1,colWidths:[],minColWidth:20,minChildWidth:150,acceptTypes:[],mode:"right",allowAutoScroll:false,timeDisplayPopup:1500,isOffset:false,offsetDrag:{},withHandles:false,handleClasses:[],_draggedWidget:null,_isResized:false,_activeGrip:null,_oldwidth:0,_oldheight:0,_a11yOn:false,_canDisplayPopup:true,constructor:function(_1,_2){
_1=_1||{};
this.acceptTypes=_1.acceptTypes||["dijit.layout.ContentPane"];
this.dragOffset=_1.dragOffset||{x:0,y:0};
this._children=[];
},_createCells:function(){
if(this.nbZones===0){
this.nbZones=1;
}
if(dojo.isIE&&dojo.marginBox(this.gridNode).height){
var _3=document.createTextNode(" ");
this.gridNode.appendChild(_3);
}
var _4=[];
this.cell=[];
var i=0;
while(i<this.nbZones){
var _5=dojo.create("td",{id:this.id+"_dz"+i,className:"gridContainerZone",style:{width:this._getColWidth(i)+"%"}},this.gridNode);
this.cell[i]=_5;
i++;
}
},startup:function(){
this.inherited(arguments);
this._createCells();
dojo.forEach(this.getChildren(),function(_6){
if(!_6.started&&!_6._started){
_6.startup();
}
});
if(this.usepref!==true){
this[(this.isAutoOrganized?"_organizeServices":"_organizeServicesManually")]();
}else{
return;
}
this.init();
},init:function(){
this.grid=this._createGrid();
this.connect(dojo.global,"onresize","onResized");
this.connect(this,"onDndDrop","_placeGrips");
this.dropHandler=dojo.subscribe("/dnd/drop",this,"_placeGrips");
this._oldwidth=this.domNode.offsetWidth;
if(this.hasResizableColumns){
this._initPlaceGrips();
this._placeGrips();
}
},destroy:function(){
for(var i=0;i<this.handleDndStart;i++){
dojo.disconnect(this.handleDndStart[i]);
}
dojo.unsubscribe(this.dropHandler);
this.inherited(arguments);
},resize:function(){
dojo.forEach(this._children,function(_7){
if(_7.resize){
_7.resize();
}
},this);
},onResized:function(){
if(this.hasResizableColumns){
this._placeGrips();
this._oldwidth=this.domNode.offsetWidth;
this._oldheight=this.domNode.offsetHeight;
}
},_organizeServices:function(){
var _8=this.nbZones;
var _9=this.getChildren().length;
var _a=Math.floor(_9/_8);
var _b=_9%_8;
var i=0;
for(var z=0;z<_8;z++){
for(var r=0;r<_a;r++){
this._insertService(z,i++,0,true);
}
if(_b>0){
try{
this._insertService(z,i++,0,true);
}
catch(e){
console.error("Unable to insert service in grid container",e,this.getChildren());
}
_b--;
}else{
if(_a===0){
break;
}
}
}
},_organizeServicesManually:function(){
var _c=this.getChildren();
for(var i=0;i<_c.length;i++){
try{
this._insertService(_c[i].column-1,i,0,true);
}
catch(e){
console.error("Unable to insert service in grid container",e,_c[i]);
}
}
},_insertService:function(z,p,i,_d){
var _e=this.cell[z];
var _f=_e.childNodes.length;
var _10=this.getChildren()[(i?i:0)];
this._children.push(_10);
if(typeof (p)=="undefined"||p>_f){
p=_f;
}
var _11=dojo.place(_10.domNode,_e,p);
_10.domNode.setAttribute("tabIndex",0);
if(!_10.dragRestriction){
dojo.addClass(_10.domNode,"dojoDndItem");
}
if(!_10.domNode.getAttribute("dndType")){
_10.domNode.setAttribute("dndType",_10.declaredClass);
}
dojox.layout.dnd._setGcDndHandle(_10,this.withHandles,this.handleClasses,_d);
if(this.hasResizableColumns){
if(_10.onLoad){
this.connect(_10,"onLoad","_placeGrips");
}
if(_10.onExecError){
this.connect(_10,"onExecError","_placeGrips");
}
if(_10.onUnLoad){
this.connect(_10,"onUnLoad","_placeGrips");
}
}
this._placeGrips();
return _10.id;
},addService:function(_12,z,p){
_12.domNode.id=_12.id;
this.addChild(_12);
if(p<=0){
p=0;
}
var _13=this._insertService(z,p);
this.grid[z].setItem(_12.id,{data:_12.domNode,type:[_12.domNode.getAttribute("dndType")]});
return _13;
},_createGrid:function(){
var _14=[];
var i=0;
this.tabDZ=[];
while(i<this.nbZones){
var _15=this.cell[i];
this.tabDZ[i]=this._createZone(_15);
if(this.hasResizableColumns&&i!=(this.nbZones-1)){
this._createGrip(this.tabDZ[i]);
}
_14.push(this.tabDZ[i]);
i++;
}
if(this.hasResizableColumns){
this.handleDndStart=[];
for(var j=0;j<this.tabDZ.length;j++){
var dz=this.tabDZ[j];
var _16=this;
this.handleDndStart.push(dojo.connect(dz,"onDndStart",dz,function(_17){
if(_17==this){
_16.handleDndInsertNodes=[];
for(i=0;i<_16.tabDZ.length;i++){
_16.handleDndInsertNodes.push(dojo.connect(_16.tabDZ[i],"insertNodes",_16,function(){
_16._disconnectDnd();
}));
}
_16.handleDndInsertNodes.push(dojo.connect(dz,"onDndCancel",_16,_16._disconnectDnd));
_16.onResized();
}
}));
}
}
return _14;
},_disconnectDnd:function(){
dojo.forEach(this.handleDndInsertNodes,dojo.disconnect);
setTimeout(dojo.hitch(this,"onResized"),0);
},_createZone:function(_18){
var dz=null;
dz=new dojox.layout.dnd.PlottedDnd(_18.id,{accept:this.acceptTypes,withHandles:this.withHandles,handleClasses:this.handleClasses,singular:true,hideSource:true,opacity:this.opacity,dom:this.domNode,allowAutoScroll:this.allowAutoScroll,isOffset:this.isOffset,offsetDrag:this.offsetDrag});
this.connect(dz,"insertDashedZone","_placeGrips");
this.connect(dz,"deleteDashedZone","_placeGrips");
return dz;
},_createGrip:function(dz){
var _19=document.createElement("div");
_19.className="gridContainerGrip";
_19.setAttribute("tabIndex","0");
var _1a=this;
this.onMouseOver=this.connect(_19,"onmouseover",function(e){
var _1b=false;
for(var i=0;i<_1a.grid.length-1;i++){
if(dojo.hasClass(_1a.grid[i].grip,"gridContainerGripShow")){
_1b=true;
break;
}
}
if(!_1b){
dojo.removeClass(e.target,"gridContainerGrip");
dojo.addClass(e.target,"gridContainerGripShow");
}
});
this.connect(_19,"onmouseout",function(e){
if(!_1a._isResized){
dojo.removeClass(e.target,"gridContainerGripShow");
dojo.addClass(e.target,"gridContainerGrip");
}
});
this.connect(_19,"onmousedown",function(e){
_1a._a11yOn=false;
_1a._activeGrip=e.target;
_1a.resizeColumnOn(e);
});
this.domNode.appendChild(_19);
dz.grip=_19;
},_initPlaceGrips:function(){
var dcs=dojo.getComputedStyle(this.domNode);
var gcs=dojo.getComputedStyle(this.gridContainerTable);
this._x=parseInt(dcs.paddingLeft);
this._topGrip=parseInt(dcs.paddingTop);
if(dojo.isIE||gcs.borderCollapse!="collapse"){
var ex=dojo._getBorderExtents(this.gridContainerTable);
this._x+=ex.l;
this._topGrip+=ex.t;
}
this._topGrip+="px";
dojo.forEach(this.grid,function(_1c){
if(_1c.grip){
var _1d=_1c.grip;
if(!dojo.isIE){
_1c.pad=dojo._getPadBorderExtents(_1c.node).w;
}
_1d.style.top=this._topGrip;
}
},this);
},_placeGrips:function(){
var _1e;
if(this.allowAutoScroll){
_1e=this.gridNode.scrollHeight;
}else{
_1e=dojo.contentBox(this.gridNode).h;
}
var _1f=this._x;
dojo.forEach(this.grid,function(_20){
if(_20.grip){
var _21=_20.grip;
_1f+=dojo[(dojo.isIE?"marginBox":"contentBox")](_20.node).w+(dojo.isIE?0:_20.pad);
dojo.style(_21,{left:_1f+"px",height:_1e+"px"});
}
},this);
},_getZoneByIndex:function(n){
return this.grid[(n>=0&&n<this.grid.length?n:0)];
},getIndexZone:function(_22){
for(var z=0;z<this.grid.length;z++){
if(this.grid[z].domNode==_22){
return z;
}
}
return -1;
},resizeColumnOn:function(e){
var k=dojo.keys;
var i;
if(!(this._a11yOn&&e.keyCode!=k.LEFT_ARROW&&e.keyCode!=k.RIGHT_ARROW)){
e.preventDefault();
dojo.body().style.cursor="ew-resize";
this._isResized=true;
this.initX=e.pageX;
var _23=[];
for(i=0;i<this.grid.length;i++){
_23[i]=dojo.contentBox(this.grid[i].node).w;
}
this.oldTabSize=_23;
for(i=0;i<this.grid.length;i++){
if(this._activeGrip==this.grid[i].grip){
this.currentColumn=this.grid[i].node;
this.currentColumnWidth=_23[i];
this.nextColumn=this.currentColumn.nextSibling;
this.nextColumnWidth=_23[i+1];
}
this.grid[i].node.style.width=_23[i]+"px";
}
var _24=function(_25,_26){
var _27=0;
var _28=0;
dojo.forEach(_25,function(_29){
if(_29.nodeType==1){
var _2a=dojo.getComputedStyle(_29);
var _2b=(dojo.isIE?_26:parseInt(_2a.minWidth));
_28=_2b+parseInt(_2a.marginLeft)+parseInt(_2a.marginRight);
if(_27<_28){
_27=_28;
}
}
});
return _27;
};
var _2c=_24(this.currentColumn.childNodes,this.minChildWidth);
var _2d=_24(this.nextColumn.childNodes,this.minChildWidth);
var _2e=Math.round((dojo.marginBox(this.gridContainerTable).w*this.minColWidth)/100);
this.currentMinCol=_2c;
this.nextMinCol=_2d;
if(_2e>this.currentMinCol){
this.currentMinCol=_2e;
}
if(_2e>this.nextMinCol){
this.nextMinCol=_2e;
}
if(this._a11yOn){
this.connectResizeColumnMove=this.connect(dojo.doc,"onkeypress","resizeColumnMove");
}else{
this.connectResizeColumnMove=this.connect(dojo.doc,"onmousemove","resizeColumnMove");
this.connectResizeColumnOff=this.connect(document,"onmouseup","resizeColumnOff");
}
}
},resizeColumnMove:function(e){
var d=0;
if(this._a11yOn){
var k=dojo.keys;
switch(e.keyCode){
case k.LEFT_ARROW:
d=-10;
break;
case k.RIGHT_ARROW:
d=10;
break;
}
}else{
e.preventDefault();
d=e.pageX-this.initX;
}
if(d==0){
return;
}
if(!(this.currentColumnWidth+d<this.currentMinCol||this.nextColumnWidth-d<this.nextMinCol)){
this.currentColumnWidth+=d;
this.nextColumnWidth-=d;
this.initX=e.pageX;
this.currentColumn.style["width"]=this.currentColumnWidth+"px";
this.nextColumn.style["width"]=this.nextColumnWidth+"px";
this._activeGrip.style.left=parseInt(this._activeGrip.style.left)+d+"px";
this._placeGrips();
}
if(this._a11yOn){
this.resizeColumnOff(e);
}
},resizeColumnOff:function(e){
dojo.body().style.cursor="default";
if(this._a11yOn){
this.disconnect(this.connectResizeColumnMove);
this._a11yOn=false;
}else{
this.disconnect(this.connectResizeColumnMove);
this.disconnect(this.connectResizeColumnOff);
}
var _2f=[];
var _30=[];
var _31=this.gridContainerTable.clientWidth;
var i;
for(i=0;i<this.grid.length;i++){
var _32=dojo.contentBox(this.grid[i].node);
if(dojo.isIE){
_2f[i]=dojo.marginBox(this.grid[i].node).w;
_30[i]=_32.w;
}else{
_2f[i]=_32.w;
_30=_2f;
}
}
var _33=false;
for(i=0;i<_30.length;i++){
if(_30[i]!=this.oldTabSize[i]){
_33=true;
break;
}
}
if(_33){
var mul=dojo.isIE?100:10000;
for(i=0;i<this.grid.length;i++){
this.grid[i].node.style.width=Math.round((100*mul*_2f[i])/_31)/mul+"%";
}
this._placeGrips();
}
if(this._activeGrip){
dojo.removeClass(this._activeGrip,"gridContainerGripShow");
dojo.addClass(this._activeGrip,"gridContainerGrip");
}
this._isResized=false;
},setColumns:function(_34){
var _35;
if(_34>0){
var _36=this.grid.length-_34;
if(_36>0){
var _37=[];
var _38,end,z,_39,j;
if(this.mode=="right"){
end=(this.isLeftFixed&&this.grid.length>0)?1:0;
_38=this.grid.length-(this.isRightFixed?2:1);
for(z=_38;z>=end;z--){
_39=0;
_35=this.grid[z].node;
for(j=0;j<_35.childNodes.length;j++){
if(_35.childNodes[j].nodeType==1&&!(_35.childNodes[j].id=="")){
_39++;
break;
}
}
if(_39==0){
_37[_37.length]=z;
}
if(_37.length>=_36){
this._deleteColumn(_37);
break;
}
}
if(_37.length<_36){
console.error("Move boxes in first columns, in all tabs before changing the organization of the page");
}
}else{
_38=(this.isLeftFixed&&this.grid.length>0)?1:0;
end=this.grid.length;
if(this.isRightFixed){
end--;
}
for(z=_38;z<end;z++){
_39=0;
_35=this.grid[z].node;
for(j=0;j<_35.childNodes.length;j++){
if(_35.childNodes[j].nodeType==1&&!(_35.childNodes[j].id=="")){
_39++;
break;
}
}
if(_39==0){
_37[_37.length]=z;
}
if(_37.length>=_36){
this._deleteColumn(_37);
break;
}
}
if(_37.length<_36){
alert("Move boxes in last columns, in all tabs before changing the organization of the page");
}
}
}else{
if(_36<0){
this._addColumn(Math.abs(_36));
}
}
this._initPlaceGrips();
this._placeGrips();
}
},_addColumn:function(_3a){
var _3b;
if(this.hasResizableColumns&&!this.isRightFixed&&this.mode=="right"){
_3b=this.grid[this.grid.length-1];
this._createGrip(_3b);
}
for(var i=0;i<_3a;i++){
_3b=dojo.doc.createElement("td");
dojo.addClass(_3b,"gridContainerZone");
_3b.id=this.id+"_dz"+this.nbZones;
var dz;
if(this.mode=="right"){
if(this.isRightFixed){
this.grid[this.grid.length-1].node.parentNode.insertBefore(_3b,this.grid[this.grid.length-1].node);
dz=this._createZone(_3b);
this.tabDZ.splice(this.tabDZ.length-1,0,dz);
this.grid.splice(this.grid.length-1,0,dz);
this.cell.splice(this.cell.length-1,0,_3b);
}else{
var _3c=this.gridNode.appendChild(_3b);
dz=this._createZone(_3b);
this.tabDZ.push(dz);
this.grid.push(dz);
this.cell.push(_3b);
}
}else{
if(this.isLeftFixed){
(this.grid.length==1)?this.grid[0].node.parentNode.appendChild(_3b,this.grid[0].node):this.grid[1].node.parentNode.insertBefore(_3b,this.grid[1].node);
dz=this._createZone(_3b);
this.tabDZ.splice(1,0,dz);
this.grid.splice(1,0,dz);
this.cell.splice(1,0,_3b);
}else{
this.grid[this.grid.length-this.nbZones].node.parentNode.insertBefore(_3b,this.grid[this.grid.length-this.nbZones].node);
dz=this._createZone(_3b);
this.tabDZ.splice(this.tabDZ.length-this.nbZones,0,dz);
this.grid.splice(this.grid.length-this.nbZones,0,dz);
this.cell.splice(this.cell.length-this.nbZones,0,_3b);
}
}
if(this.hasResizableColumns){
var _3d=this;
var _3e=dojo.connect(dz,"onDndStart",dz,function(_3f){
if(_3f==this){
_3d.handleDndInsertNodes=[];
for(var o=0;o<_3d.tabDZ.length;o++){
_3d.handleDndInsertNodes.push(dojo.connect(_3d.tabDZ[o],"insertNodes",_3d,function(){
_3d._disconnectDnd();
}));
}
_3d.handleDndInsertNodes.push(dojo.connect(dz,"onDndCancel",_3d,_3d._disconnectDnd));
_3d.onResized();
}
});
if(this.mode=="right"){
if(this.isRightFixed){
this.handleDndStart.splice(this.handleDndStart.length-1,0,_3e);
}else{
this.handleDndStart.push(_3e);
}
}else{
if(this.isLeftFixed){
this.handleDndStart.splice(1,0,_3e);
}else{
this.handleDndStart.splice(this.handleDndStart.length-this.nbZones,0,_3e);
}
}
this._createGrip(dz);
}
this.nbZones++;
}
this._updateColumnsWidth();
},_deleteColumn:function(_40){
var _41,_42,_43;
_43=0;
for(var i=0;i<_40.length;i++){
var idx=_40[i];
if(this.mode=="right"){
_41=this.grid[idx];
}else{
_41=this.grid[idx-_43];
}
for(var j=0;j<_41.node.childNodes.length;j++){
if(_41.node.childNodes[j].nodeType!=1){
continue;
}
_42=dijit.byId(_41.node.childNodes[j].id);
for(var x=0;x<this.getChildren().length;x++){
if(this.getChildren()[x]===_42){
this.getChildren().splice(x,1);
break;
}
}
}
_41.node.parentNode.removeChild(_41.node);
if(this.mode=="right"){
if(this.hasResizableColumns){
dojo.disconnect(this.handleDndStart[idx]);
}
this.grid.splice(idx,1);
this.tabDZ.splice(idx,1);
this.cell.splice(idx,1);
}else{
if(this.hasResizableColumns){
dojo.disconnect(this.handleDndStart[idx-_43]);
}
this.grid.splice(idx-_43,1);
this.tabDZ.splice(idx-_43,1);
this.cell.splice(idx-_43,1);
}
this.nbZones--;
_43++;
if(_41.grip){
this.domNode.removeChild(_41.grip);
}
}
this._updateColumnsWidth();
},_getColWidth:function(idx){
if(idx<this.colWidths.length){
return this.colWidths[idx];
}
var _44=100;
dojo.forEach(this.colWidths,function(_45){
_44-=_45;
});
return _44/(this.nbZones-this.colWidths.length);
},_updateColumnsWidth:function(){
var _46;
for(var z=0;z<this.grid.length;z++){
this.grid[z].node.style.width=this._getColWidth(z)+"%";
}
},_selectFocus:function(_47){
var e=_47.keyCode;
var _48=null;
var _49=dijit.getFocus();
var _4a=_49.node;
var k=dojo.keys;
var i,_4b,_4c,r,z,_4d;
var _4e=(e==k.UP_ARROW||e==k.LEFT_ARROW)?"lastChild":"firstChild";
var pos=(e==k.UP_ARROW||e==k.LEFT_ARROW)?"previousSibling":"nextSibling";
if(_4a==this.containerNode){
switch(e){
case k.DOWN_ARROW:
case k.RIGHT_ARROW:
for(i=0;i<this.gridNode.childNodes.length;i++){
_48=this.gridNode.childNodes[i].firstChild;
_4b=false;
while(!_4b){
if(_48!=null){
if(_48.style.display!=="none"){
dijit.focus(_48);
dojo.stopEvent(_47);
_4b=true;
}else{
_48=_48[pos];
}
}else{
break;
}
}
if(_4b){
break;
}
}
break;
case k.UP_ARROW:
case k.LEFT_ARROW:
for(i=this.gridNode.childNodes.length-1;i>=0;i--){
_48=this.gridNode.childNodes[i].lastChild;
_4b=false;
while(!_4b){
if(_48!=null){
if(_48.style.display!=="none"){
dijit.focus(_48);
dojo.stopEvent(_47);
_4b=true;
}else{
_48=_48[pos];
}
}else{
break;
}
}
if(_4b){
break;
}
}
break;
}
}else{
if(_4a.parentNode.parentNode==this.gridNode){
switch(e){
case k.UP_ARROW:
case k.DOWN_ARROW:
dojo.stopEvent(_47);
var _4f=0;
dojo.forEach(_4a.parentNode.childNodes,function(_50){
if(_50.style.display!=="none"){
_4f++;
}
});
if(_4f==1){
return;
}
_4b=false;
_48=_4a[pos];
while(!_4b){
if(_48==null){
_48=_4a.parentNode[_4e];
if(_48.style.display!=="none"){
_4b=true;
}else{
_48=_48[pos];
}
}else{
if(_48.style.display!=="none"){
_4b=true;
}else{
_48=_48[pos];
}
}
}
if(_47.shiftKey){
if(dijit.byNode(_4a).dragRestriction){
return;
}
_4d=_4a.getAttribute("dndtype");
_4c=false;
for(i=0;i<this.acceptTypes.length;i++){
if(_4d==this.acceptTypes[i]){
_4c=true;
break;
}
}
if(_4c){
var _51=_4a.parentNode;
var _52=_51.firstChild;
var _53=_51.lastChild;
while(_52.style.display=="none"||_53.style.display=="none"){
if(_52.style.display=="none"){
_52=_52.nextSibling;
}
if(_53.style.display=="none"){
_53=_53.previousSibling;
}
}
if(e==k.UP_ARROW){
r=_51.removeChild(_4a);
if(r==_52){
_51.appendChild(r);
}else{
_51.insertBefore(r,_48);
}
r.setAttribute("tabIndex","0");
dijit.focus(r);
}else{
if(_4a==_53){
r=_51.removeChild(_4a);
_51.insertBefore(r,_48);
r.setAttribute("tabIndex","0");
dijit.focus(r);
}else{
r=_51.removeChild(_48);
_51.insertBefore(r,_4a);
_4a.setAttribute("tabIndex","0");
dijit.focus(_4a);
}
}
}else{
this._displayPopup();
}
}else{
dijit.focus(_48);
}
break;
case k.RIGHT_ARROW:
case k.LEFT_ARROW:
dojo.stopEvent(_47);
if(_47.shiftKey){
if(dijit.byNode(_4a).dragRestriction){
return;
}
z=0;
if(_4a.parentNode[pos]==null){
if(e==k.LEFT_ARROW){
z=this.gridNode.childNodes.length-1;
}
}else{
if(_4a.parentNode[pos].nodeType==3){
z=this.gridNode.childNodes.length-2;
}else{
for(i=0;i<this.gridNode.childNodes.length;i++){
if(_4a.parentNode[pos]==this.gridNode.childNodes[i]){
break;
}
z++;
}
}
}
_4d=_4a.getAttribute("dndtype");
_4c=false;
for(i=0;i<this.acceptTypes.length;i++){
if(_4d==this.acceptTypes[i]){
_4c=true;
break;
}
}
if(_4c){
var _54=_4a.parentNode;
var _55=dijit.byNode(_4a);
r=_54.removeChild(_4a);
var _56=(e==k.RIGHT_ARROW?0:this.gridNode.childNodes[z].length);
this.addService(_55,z,_56);
r.setAttribute("tabIndex","0");
dijit.focus(r);
this._placeGrips();
}else{
this._displayPopup();
}
}else{
var _57=_4a.parentNode;
while(_48===null){
if(_57[pos]!==null&&_57[pos].nodeType!==3){
_57=_57[pos];
}else{
if(pos==="previousSibling"){
_57=_57.parentNode.childNodes[_57.parentNode.childNodes.length-1];
}else{
_57=_57.parentNode.childNodes[0];
}
}
_4b=false;
var _58=_57[_4e];
while(!_4b){
if(_58!=null){
if(_58.style.display!=="none"){
_48=_58;
_4b=true;
}else{
_58=_58[pos];
}
}else{
break;
}
}
}
dijit.focus(_48);
}
break;
}
}else{
if(dojo.hasClass(_4a,"gridContainerGrip")||dojo.hasClass(_4a,"gridContainerGripShow")){
this._activeGrip=_47.target;
this._a11yOn=true;
this.resizeColumnOn(_47);
}
}
}
},_displayPopup:function(){
if(this._canDisplayPopup){
var _59=dojo.doc.createElement("div");
dojo.addClass(_59,"gridContainerPopup");
_59.innerHTML="this widget type is not accepted to be moved!";
var _5a=this.containerNode.appendChild(_59);
this._canDisplayPopup=false;
setTimeout(dojo.hitch(this,function(){
this.containerNode.removeChild(_5a);
dojo.destroy(_5a);
this._canDisplayPopup=true;
}),this.timeDisplayPopup);
}
}});
dojo.extend(dijit._Widget,{dragRestriction:false,column:"1",group:""});
}
