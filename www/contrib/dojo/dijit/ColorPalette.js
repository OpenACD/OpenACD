/*
	Copyright (c) 2004-2009, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/


if(!dojo._hasResource["dijit.ColorPalette"]){
dojo._hasResource["dijit.ColorPalette"]=true;
dojo.provide("dijit.ColorPalette");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dojo.colors");
dojo.require("dojo.i18n");
dojo.requireLocalization("dojo","colors",null,"ROOT,ar,ca,cs,da,de,el,es,fi,fr,he,hu,it,ja,ko,nb,nl,pl,pt,pt-pt,ru,sk,sl,sv,th,tr,zh,zh-tw");
dojo.declare("dijit.ColorPalette",[dijit._Widget,dijit._Templated],{defaultTimeout:500,timeoutChangeRate:0.9,palette:"7x10",value:null,_currentFocus:0,_xDim:null,_yDim:null,_palettes:{"7x10":[["white","seashell","cornsilk","lemonchiffon","lightyellow","palegreen","paleturquoise","lightcyan","lavender","plum"],["lightgray","pink","bisque","moccasin","khaki","lightgreen","lightseagreen","lightskyblue","cornflowerblue","violet"],["silver","lightcoral","sandybrown","orange","palegoldenrod","chartreuse","mediumturquoise","skyblue","mediumslateblue","orchid"],["gray","red","orangered","darkorange","yellow","limegreen","darkseagreen","royalblue","slateblue","mediumorchid"],["dimgray","crimson","chocolate","coral","gold","forestgreen","seagreen","blue","blueviolet","darkorchid"],["darkslategray","firebrick","saddlebrown","sienna","olive","green","darkcyan","mediumblue","darkslateblue","darkmagenta"],["black","darkred","maroon","brown","darkolivegreen","darkgreen","midnightblue","navy","indigo","purple"]],"3x4":[["white","lime","green","blue"],["silver","yellow","fuchsia","navy"],["gray","red","purple","black"]]},_imagePaths:{"7x10":dojo.moduleUrl("dijit.themes","a11y/colors7x10.png"),"3x4":dojo.moduleUrl("dijit.themes","a11y/colors3x4.png")},_paletteCoords:{"leftOffset":3,"topOffset":3,"cWidth":20,"cHeight":20},templateString:dojo.cache("dijit","templates/ColorPalette.html","<div class=\"dijitInline dijitColorPalette\">\n\t<div class=\"dijitColorPaletteInner\" dojoAttachPoint=\"divNode\" waiRole=\"grid\" tabIndex=\"${tabIndex}\">\n\t\t<img class=\"dijitColorPaletteUnder\" dojoAttachPoint=\"imageNode\" waiRole=\"presentation\" alt=\"\">\n\t</div>\n</div>\n"),_paletteDims:{"7x10":{"width":"206px","height":"145px"},"3x4":{"width":"86px","height":"64px"}},tabIndex:"0",postCreate:function(){
dojo.mixin(this.divNode.style,this._paletteDims[this.palette]);
this.imageNode.setAttribute("src",this._imagePaths[this.palette].toString());
var _1=this._palettes[this.palette];
this.domNode.style.position="relative";
this._cellNodes=[];
this.colorNames=dojo.i18n.getLocalization("dojo","colors",this.lang);
var _2=this._blankGif,_3=new dojo.Color(),_4=this._paletteCoords;
for(var _5=0;_5<_1.length;_5++){
var _6=dojo.create("div",{role:"row"});
dojo.place(_6,this.divNode);
for(var _7=0;_7<_1[_5].length;_7++){
var _8=_1[_5][_7],_9=_3.setColor(dojo.Color.named[_8]);
var _a=dojo.create("span",{"class":"dijitPaletteCell",tabIndex:"-1",title:this.colorNames[_8],style:{top:_4.topOffset+(_5*_4.cHeight)+"px",left:_4.leftOffset+(_7*_4.cWidth)+"px"}});
var _b=dojo.create("img",{src:_2,"class":"dijitPaletteImg",alt:this.colorNames[_8]},_a);
_b.color=_9.toHex();
var _c=_b.style;
_c.color=_c.backgroundColor=_b.color;
dojo.forEach(["Dijitclick","MouseEnter","Focus"],function(_d){
this.connect(_a,"on"+_d.toLowerCase(),"_onCell"+_d);
},this);
dojo.place(_a,_6);
dijit.setWaiRole(_a,"gridcell");
_a.index=this._cellNodes.length;
this._cellNodes.push(_a);
}
}
this._xDim=_1[0].length;
this._yDim=_1.length;
this.connect(this.divNode,"onfocus","_onDivNodeFocus");
var _e={UP_ARROW:-this._xDim,DOWN_ARROW:this._xDim,RIGHT_ARROW:1,LEFT_ARROW:-1};
for(var _f in _e){
this._connects.push(dijit.typematic.addKeyListener(this.domNode,{charOrCode:dojo.keys[_f],ctrlKey:false,altKey:false,shiftKey:false},this,function(){
var _10=_e[_f];
return function(_11){
this._navigateByKey(_10,_11);
};
}(),this.timeoutChangeRate,this.defaultTimeout));
}
},focus:function(){
this._focusFirst();
},onChange:function(_12){
},_focusFirst:function(){
this._currentFocus=0;
var _13=this._cellNodes[this._currentFocus];
window.setTimeout(function(){
dijit.focus(_13);
},0);
},_onDivNodeFocus:function(evt){
this._focusFirst();
},_onFocus:function(){
dojo.attr(this.divNode,"tabIndex","-1");
},_onBlur:function(){
this._removeCellHighlight(this._currentFocus);
dojo.attr(this.divNode,"tabIndex",this.tabIndex);
},_onCellDijitclick:function(evt){
var _14=evt.currentTarget;
if(this._currentFocus!=_14.index){
this._currentFocus=_14.index;
window.setTimeout(function(){
dijit.focus(_14);
},0);
}
this._selectColor(_14);
dojo.stopEvent(evt);
},_onCellMouseEnter:function(evt){
var _15=evt.currentTarget;
this._setCurrent(_15);
window.setTimeout(function(){
dijit.focus(_15);
},0);
},_onCellFocus:function(evt){
this._setCurrent(evt.currentTarget);
},_setCurrent:function(_16){
this._removeCellHighlight(this._currentFocus);
this._currentFocus=_16.index;
dojo.addClass(_16,"dijitPaletteCellHighlight");
},_removeCellHighlight:function(_17){
dojo.removeClass(this._cellNodes[_17],"dijitPaletteCellHighlight");
},_selectColor:function(_18){
var img=_18.getElementsByTagName("img")[0];
this.onChange(this.value=img.color);
},_navigateByKey:function(_19,_1a){
if(_1a==-1){
return;
}
var _1b=this._currentFocus+_19;
if(_1b<this._cellNodes.length&&_1b>-1){
var _1c=this._cellNodes[_1b];
_1c.focus();
}
}});
}
