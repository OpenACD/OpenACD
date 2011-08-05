//>>built
require({cache:{
'url:dojox/grid/enhanced/templates/Pagination.html':"<div dojoAttachPoint=\"paginatorBar\"\n\t><table cellpadding=\"0\" cellspacing=\"0\"  class=\"dojoxGridPaginator\"\n\t\t><tr\n\t\t\t><td dojoAttachPoint=\"descriptionTd\" class=\"dojoxGridDescriptionTd\"\n\t\t\t\t><div dojoAttachPoint=\"descriptionDiv\" class=\"dojoxGridDescription\"></div\n\t\t\t></div></td\n\t\t\t><td dojoAttachPoint=\"sizeSwitchTd\"></td\n\t\t\t><td dojoAttachPoint=\"pageStepperTd\" class=\"dojoxGridPaginatorFastStep\"\n\t\t\t\t><div dojoAttachPoint=\"pageStepperDiv\" class=\"dojoxGridPaginatorStep\"></div\n\t\t\t></td\n\t\t></tr\n\t></table\n></div>\n"}});
define("dojox/grid/enhanced/plugins/Pagination", [
	"dojo/_base/declare",
	"dojo/_base/array",
	"dojo/_base/connect",
	"dojo/_base/lang",
	"dojo/_base/html",
	"dojo/_base/event",
	"dojo/_base/window",
	"dojo/query",
	"dojo/string",
	"dojo/i18n",
	"dojo/keys",
	"dojo/text!../templates/Pagination.html",
	"./Dialog",
	"./_StoreLayer",
	"../_Plugin",
	"../../EnhancedGrid",
	"dijit/form/Button",
	"dijit/form/NumberTextBox",
	"dijit/focus",
	"dijit/_Widget",
	"dijit/_TemplatedMixin",
	"dojox/html/metrics",
	"dojo/i18n!../nls/Pagination"
], function(declare, array, connect, lang, html, evt, win, query, 
	string, i18n, keys, template, Dialog, layers, _Plugin, EnhancedGrid, 
	Button, NumberTextBox, dijitFocus, _Widget, _TemplatedMixin, metrics){

var _GotoPageDialog = declare("dojox.grid.enhanced.plugins.pagination._GotoPageDialog", null, {
	
	pageCount: 0,
	
	constructor: function(plugin){
		this.plugin = plugin;
		this.pageCount = this.plugin.paginators[0]._getPageCount();
		this._dialogNode = html.create("div", {}, win.body(), "last");
		this._gotoPageDialog = new Dialog({
			"refNode": plugin.grid.domNode,
			"title": this.plugin.nls.dialogTitle
		}, this._dialogNode);
		this._createDialogContent();
		this._gotoPageDialog.startup();
	},
	
	_createDialogContent: function(){
		// summary:
		//		Create the dialog content
		this._specifyNode = html.create("div", {innerHTML: this.plugin.nls.dialogIndication}, this._gotoPageDialog.containerNode, "last");
		
		this._pageInputDiv = html.create("div", {}, this._gotoPageDialog.containerNode, "last");
		this._pageTextBox = new NumberTextBox();
		this._pageTextBox.constraints = {fractional:false, min:1, max:this.pageCount};
		this.plugin.connect(this._pageTextBox.textbox, "onkeyup", lang.hitch(this, "_setConfirmBtnState"));
		
		this._pageInputDiv.appendChild(this._pageTextBox.domNode);
		this._pageLabel = html.create("label", {innerHTML: string.substitute(this.plugin.nls.pageCountIndication, [this.pageCount])}, this._pageInputDiv, "last");
		
		this._buttonDiv = html.create("div", {}, this._gotoPageDialog.containerNode, "last");
		this._confirmBtn = new Button({label: this.plugin.nls.dialogConfirm, onClick: lang.hitch(this, this._onConfirm)});
		this._confirmBtn.set("disabled", true);
		
		this._cancelBtn = new Button({label: this.plugin.nls.dialogCancel, onClick: lang.hitch(this, this._onCancel)});
		this._buttonDiv.appendChild(this._confirmBtn.domNode);
		this._buttonDiv.appendChild(this._cancelBtn.domNode);
		this._styleContent();
		this._gotoPageDialog.onCancel = lang.hitch(this, this._onCancel);
		this.plugin.connect(this._gotoPageDialog, "_onKey", lang.hitch(this, "_onKeyDown"));
	},
	
	_styleContent: function(){
		html.addClass(this._specifyNode, "dojoxGridDialogMargin");
		html.addClass(this._pageInputDiv, "dojoxGridDialogMargin");
		html.addClass(this._buttonDiv, "dojoxGridDialogButton");
		html.style(this._pageTextBox.domNode, "width", "50px");
	},
	
	updatePageCount: function(){
		this.pageCount = this.plugin.paginators[0]._getPageCount();
		this._pageTextBox.constraints = {fractional:false, min:1, max:this.pageCount};
		this._pageLabel.innerHTML = string.substitute(this.plugin.nls.pageCountIndication, [this.pageCount]);
	},
	
	showDialog: function(){
		this._gotoPageDialog.show();
	},
	
	_onConfirm: function(event){
		// summary:
		//		Jump to the given page
		if(this._pageTextBox.isValid() && this._pageTextBox.getDisplayedValue() !== ""){
			this.plugin.gotoPage(this._pageTextBox.parse(this._pageTextBox.getDisplayedValue()));
			this._gotoPageDialog.hide();
			this._pageTextBox.reset();
		}
		this.plugin._stopEvent(event);
	},
	
	_onCancel: function(event){
		// summary:
		//		Cancel action and hide the dialog
		this._pageTextBox.reset();
		this._gotoPageDialog.hide();
		this.plugin._stopEvent(event);
	},
	
	_onKeyDown: function(event){
		if(event.altKey || event.metaKey){
			return;
		}
		if(event.keyCode === keys.ENTER){
			this._onConfirm(event);
		}
	},
	
	_setConfirmBtnState: function(){
		if(this._pageTextBox.isValid() && this._pageTextBox.getDisplayedValue() !== ""){
			this._confirmBtn.set("disabled", false);
		}else{
			this._confirmBtn.set("disabled", true);
		}
	},
	
	destroy: function(){
		this._pageTextBox.destroy();
		this._confirmBtn.destroy();
		this._cancelBtn.destroy();
		this._gotoPageDialog.destroy();
		
		html.destroy(this._specifyNode);
		html.destroy(this._pageInputDiv);
		html.destroy(this._pageLabel);
		html.destroy(this._buttonDiv);
		html.destroy(this._dialogNode);
	}
});

var _ForcedPageStoreLayer = declare("dojox.grid.enhanced.plugins._ForcedPageStoreLayer", layers._StoreLayer, {
	tags: ["presentation"],
	
	constructor: function(plugin){
		this._plugin = plugin;
	},
	
	_fetch: function(request){
		var self = this,
			plugin = self._plugin,
			grid = plugin.grid,
			scope = request.scope || win.global,
			onBegin = request.onBegin;
		
		request.start = plugin._currentPage * plugin._pageSize + request.start;
		self.startIdx = request.start;
		self.endIdx = request.start + plugin._pageSize - 1;
		if(onBegin && (plugin.showAll || array.every(plugin.paginators, function(p){
			plugin.showAll = !p.sizeSwitch && !p.pageStepper && !p.gotoButton;
			return plugin.showAll;
		}))){
			request.onBegin = function(size, req){
				plugin._maxSize = plugin._pageSize = size;
				self.startIdx = 0;
				self.endIdx = size - 1;
				array.forEach(plugin.paginators, function(f){
					f.update();
				});
				req.onBegin = onBegin;
				req.onBegin.call(scope, size, req);
			};
		}else if(onBegin){
			request.onBegin = function(size, req){
				req.start = 0;
				req.count = plugin._pageSize;
				plugin._maxSize = size;
				self.endIdx = self.endIdx >= size ? (size - 1) : self.endIdx;
				if(self.startIdx > size && size !== 0){
					grid._pending_requests[req.start] = false;
					plugin.gotoFirstPage();
				}
				array.forEach(plugin.paginators, function(f){
					f.update();
				});
				req.onBegin = onBegin;
				req.onBegin.call(scope, Math.min(plugin._pageSize, (size - self.startIdx)), req);
			};
		}
		return lang.hitch(this._store, this._originFetch)(request);
	}
});

var _Paginator = declare("dojox.grid.enhanced.plugins._Paginator", [_Widget, _TemplatedMixin], {
	templateString: template,
		
	// pagination bar position - "bottom"|"top"
	position: "bottom",
	
	// max data item size
	_maxItemSize: 0,
	
	// description message status params
	description: true,
	
	// fast step page status params
	pageStepper: true,
	
	maxPageStep: 7,
	
	// items per page size switch params
	sizeSwitch: true,
	
	pageSizes: ["10", "25", "50", "100", "All"],
	
	gotoButton: false,
	
	constructor: function(params){
		lang.mixin(this, params);
		this.grid = this.plugin.grid;
		this.singularItemTitle = this.itemTitle ? this.itemTitle : this.plugin.nls.singularItemTitle;
		this.itemTitle = this.itemTitle ? this.itemTitle : this.plugin.nls.itemTitle;
		if(!this.singularItemTitle){
			//just for safe in case missed in some locales
			this.singularItemTitle = this.itemTitle;
		}
		this.descTemplate = this.descTemplate ? this.descTemplate : this.plugin.nls.descTemplate;
		array.forEach(this.pageSizes, function(size, idx){
			size = parseInt(size, 10);
			if(isNaN(size) || size <= 0){
				size = "All";
			}
			this.pageSizes[idx] = String(size);
		}, this);
	},
	
	postCreate: function(){
		this.inherited(arguments);
		this._setWidthValue();
		var self = this;
		var g = this.grid;
		this.plugin.connect(g, "_resize", lang.hitch(this, "_resetGridHeight"));
		this._originalResize = lang.hitch(g, "resize");
		g.resize = function(changeSize, resultSize){
			self._changeSize = g._pendingChangeSize = changeSize;
			self._resultSize = g._pendingResultSize = resultSize;
			g.sizeChange();
		};
		this._placeSelf();
	},
	
	destroy: function(){
		this.inherited(arguments);
		this.grid.focus.removeArea("pagination" + this.position.toLowerCase());
		if(this._gotoPageDialog){
			this._gotoPageDialog.destroy();
			html.destroy(this.gotoPageTd);
			delete this.gotoPageTd;
			delete this._gotoPageDialog;
		}
		this.grid.resize = this._originalResize;
		this.pageSizes = null;
	},
	
	update: function(){
		// summary:
		//		Function to update paging information and update
		//		pagination bar display.
		this.currentPageSize = this.plugin._pageSize;
		this._maxItemSize = this.plugin._maxSize;
		
		// update pagination bar display information
		this._updateDescription();
		this._updatePageStepper();
		this._updateSizeSwitch();
		this._updateGotoButton();
	},
	
	_setWidthValue: function(){
		var type = ["description", "sizeSwitch", "pageStepper"];
		var endWith = function(str1, str2){
			var reg = new RegExp(str2+"$");
			return reg.test(str1);
		};
		array.forEach(type, function(t){
			var width, flag = this[t];
			if(flag === undefined || typeof flag === "boolean"){
				return;
			}
			if(lang.isString(flag)){
				width = endWith(flag, "px") || endWith(flag, "%") || endWith(flag, "em") ? flag : parseInt(flag, 10) > 0 ? parseInt(flag, 10) + "px" : null;
			}else if(typeof flag === "number" && flag > 0){
				width = flag + "px";
			}
			this[t] = width ? true : false;
			this[t + "Width"] = width;
		}, this);
	},
	
	_regFocusMgr: function(position){
		// summary:
		//		Function to register pagination bar to focus manager.
		this.grid.focus.addArea({
			name: "pagination" + position,
			onFocus: lang.hitch(this, this._onFocusPaginator),
			onBlur: lang.hitch(this, this._onBlurPaginator),
			onMove: lang.hitch(this, this._moveFocus),
			onKeyDown: lang.hitch(this, this._onKeyDown)
		});
		switch(position){
			case "top":
				this.grid.focus.placeArea("pagination" + position, "before", "header");
				break;
			case "bottom":
			default:
				this.grid.focus.placeArea("pagination" + position, "after", "content");
				break;
		}
	},
	
	_placeSelf: function(){
		// summary:
		//		Place pagination bar to a position.
		//		There are two options, top of the grid, bottom of the grid.
		var g = this.grid;
		var	position = lang.trim(this.position.toLowerCase());
		switch(position){
			case "top":
				this.placeAt(g.viewsHeaderNode, "before");
				this._regFocusMgr("top");
				break;
			case "bottom":
			default:
				this.placeAt(g.viewsNode, "after");
				this._regFocusMgr("bottom");
				break;
		}
	},
	
	_resetGridHeight: function(changeSize, resultSize){
		// summary:
		//		Function of resize grid height to place this pagination bar.
		//		Since the grid would be able to add other element in its domNode, we have
		//		change the grid view size to place the pagination bar.
		//		This function will resize the grid viewsNode height, scorllboxNode height
		var g = this.grid;
		changeSize = changeSize || this._changeSize;
		resultSize = resultSize || this._resultSize;
		delete this._changeSize;
		delete this._resultSize;
		if(g._autoHeight){
			return;
		}
		var padBorder = g._getPadBorder().h;
		if(!this.plugin.gh){
			this.plugin.gh = html.contentBox(g.domNode).h + 2 * padBorder;
		}
		if(resultSize){
			changeSize = resultSize;
		}
		if(changeSize){
			this.plugin.gh = html.contentBox(g.domNode).h + 2 * padBorder;
		}
		var gh = this.plugin.gh,
			hh = g._getHeaderHeight(),
			ph = html.marginBox(this.domNode).h;
		ph = this.plugin.paginators[1] ? ph * 2 : ph;
		if(typeof g.autoHeight === "number"){
			var cgh = gh + ph - padBorder;
			html.style(g.domNode, "height", cgh + "px");
			html.style(g.viewsNode, "height", (cgh - ph - hh) + "px");
			
			this._styleMsgNode(hh, html.marginBox(g.viewsNode).w, cgh - ph - hh);
		}else{
			var h = gh - ph - hh - padBorder;
			html.style(g.viewsNode, "height", h + "px");
			var hasHScroller = array.some(g.views.views, function(v){
				return v.hasHScrollbar();
			});
			array.forEach(g.viewsNode.childNodes, function(c){
				html.style(c, "height", h + "px");
			});
			array.forEach(g.views.views, function(v){
				if(v.scrollboxNode){
					if(!v.hasHScrollbar() && hasHScroller){
						html.style(v.scrollboxNode, "height", (h - metrics.getScrollbar().h) + "px");
					}else{
						html.style(v.scrollboxNode, "height", h + "px");
					}
				}
			});
			this._styleMsgNode(hh, html.marginBox(g.viewsNode).w, h);
		}
	},
	
	_styleMsgNode: function(top, width, height){
		var messagesNode = this.grid.messagesNode;
		html.style(messagesNode, {"position": "absolute", "top": top + "px", "width": width + "px", "height": height + "px", "z-Index": "100"});
	},
	
	_updateDescription: function(){
		// summary:
		//		Update size information.
		var s = this.plugin.forcePageStoreLayer;
		var title = this[this._maxItemSize > 1 ? 'itemTitle' : 'singularItemTitle'];
		if(this.description && this.descriptionDiv){
			this.descriptionDiv.innerHTML = this._maxItemSize > 0 ?
				string.substitute(this.descTemplate, [title, this._maxItemSize, s.startIdx + 1, s.endIdx + 1]) : "0 " + title;
		}
		if(this.descriptionWidth){
			html.style(this.descriptionTd, "width", this.descriptionWidth);
		}
	},
	
	_updateSizeSwitch: function(){
		// summary:
		//		Update "items per page" information.
		if(!this.sizeSwitchTd){
			return;
		}
		if(!this.sizeSwitch || this._maxItemSize <= 0){
			html.style(this.sizeSwitchTd, "display", "none");
			return;
		}else{
			html.style(this.sizeSwitchTd, "display", "");
		}
		if(this.sizeSwitchTd.childNodes.length < 1){
			this._createSizeSwitchNodes();
		}
		this._updateSwitchNodeClass();
		
		// move focus to next activable node
		this._moveToNextActivableNode(this._getAllPageSizeNodes(), this.pageSizeValue);
		this.pageSizeValue = null;
	},
	
	_createSizeSwitchNodes: function(){
		// summary:
		//		The function to create the size switch nodes
		var node = null;
		if(!this.pageSizes || this.pageSizes.length < 1){
			return;
		}
		array.forEach(this.pageSizes, function(size){
			// create page size switch node
			var labelValue = size.toLowerCase() === "all" ? this.plugin.nls.allItemsLabelTemplate : string.substitute(this.plugin.nls.pageSizeLabelTemplate, [size]);
			node = html.create("span", {innerHTML: size, title: labelValue, value: size, tabindex: 0}, this.sizeSwitchTd, "last");
			// for accessibility
			node.setAttribute("aria-label", labelValue);
			// connect event
			this.plugin.connect(node, "onclick", lang.hitch(this, "_onSwitchPageSize"));
			this.plugin.connect(node, "onmouseover", function(e){
				html.addClass(e.target, "dojoxGridPageTextHover");
			});
			this.plugin.connect(node, "onmouseout", function(e){
				html.removeClass(e.target, "dojoxGridPageTextHover");
			});
			// create a separation node
			node = html.create("span", {innerHTML: "|"}, this.sizeSwitchTd, "last");
			html.addClass(node, "dojoxGridSeparator");
		}, this);
		// delete last separation node
		html.destroy(node);
		if(this.sizeSwitchWidth){
			html.style(this.sizeSwitchTd, "width", this.sizeSwitchWidth);
		}
	},
	
	_updateSwitchNodeClass: function(){
		// summary:
		//		Update the switch nodes style
		var size = null;
		var hasActivedNode = false;
		var styleNode = function(node, status){
			if(status){
				html.addClass(node, "dojoxGridActivedSwitch");
				node.setAttribute("tabindex", "-1");
				hasActivedNode = true;
			}else{
				html.addClass(node, "dojoxGridInactiveSwitch");
				node.setAttribute("tabindex", "0");
			}
		};
		array.forEach(this.sizeSwitchTd.childNodes, function(node){
			if(node.value){
				size = node.value;
				html.removeClass(node);
				if(this.plugin._pageSizeValue){
					styleNode(node, size === this.plugin._pageSizeValue && !hasActivedNode);
				}else{
					if(size.toLowerCase() == "all"){
						size = this._maxItemSize;
					}
					styleNode(node, this.currentPageSize === parseInt(size, 10) && !hasActivedNode);
				}
			}
		}, this);
	},
	
	_updatePageStepper: function(){
		// summary:
		//		Update the page step nodes
		if(!this.pageStepperTd){
			return;
		}
		if(!this.pageStepper || this._maxItemSize <= 0){
			html.style(this.pageStepperTd, "display", "none");
			return;
		}else{
			html.style(this.pageStepperTd, "display", "");
		}
		if(this.pageStepperDiv.childNodes.length < 1){
			this._createPageStepNodes();
			this._createWardBtns();
		}else{
			this._resetPageStepNodes();
		}
		this._updatePageStepNodeClass();
		
		this._moveToNextActivableNode(this._getAllPageStepNodes(), this.pageStepValue);
		this.pageStepValue = null;
	},
	
	_createPageStepNodes: function(){
		// summary:
		//		Create the page step nodes if they do not exist
		var startPage = this._getStartPage(),
			stepSize = this._getStepPageSize(),
			label = "",
			node = null,
			i = startPage;
		for(; i < startPage + this.maxPageStep + 1; i++){
			label = string.substitute(this.plugin.nls.pageStepLabelTemplate, [i]);
			node = html.create("div", {innerHTML: i, value: i, title: label, tabindex: i < startPage + stepSize ? 0 : -1}, this.pageStepperDiv, "last");
			node.setAttribute("aria-label", label);
			// connect event
			this.plugin.connect(node, "onclick", lang.hitch(this, "_onPageStep"));
			this.plugin.connect(node, "onmouseover", function(e){
				html.addClass(e.target, "dojoxGridPageTextHover");
			});
			this.plugin.connect(node, "onmouseout", function(e){
				html.removeClass(e.target, "dojoxGridPageTextHover");
			});
			html.style(node, "display", i < startPage + stepSize ? "block" : "none");
		}
		if(this.pageStepperWidth){
			html.style(this.pageStepperTd, "width", this.pageStepperWidth);
		}
	},
	
	_createWardBtns: function(){
		// summary:
		//		Create the previous/next/first/last button
		var self = this;
		var highContrastLabel = {prevPage: "&#60;", firstPage: "&#171;", nextPage: "&#62;", lastPage: "&#187;"};
		var createWardBtn = function(value, label, position){
			var node = html.create("div", {value: value, title: label, tabindex: 1}, self.pageStepperDiv, position);
			self.plugin.connect(node, "onclick", lang.hitch(self, "_onPageStep"));
			node.setAttribute("aria-label", label);
			// for high contrast
			var highConrastNode = html.create("span", {value: value, title: label, innerHTML: highContrastLabel[value]}, node, position);
			html.addClass(highConrastNode, "dojoxGridWardButtonInner");
		};
		createWardBtn("prevPage", this.plugin.nls.prevTip, "first");
		createWardBtn("firstPage", this.plugin.nls.firstTip, "first");
		createWardBtn("nextPage", this.plugin.nls.nextTip, "last");
		createWardBtn("lastPage", this.plugin.nls.lastTip, "last");
	},
	
	_resetPageStepNodes: function(){
		// summary:
		//		The page step nodes might be changed when fetch data, we need to
		//		update/reset them
		var startPage = this._getStartPage(),
			stepSize = this._getStepPageSize(),
			stepNodes = this.pageStepperDiv.childNodes,
			node = null, i = startPage, j = 2, tip;
		for(; j < stepNodes.length - 2; j++, i++){
			node = stepNodes[j];
			if(i < startPage + stepSize){
				tip = string.substitute(this.plugin.nls.pageStepLabelTemplate, [i]);
				html.attr(node, {
					"innerHTML": i,
					"title": tip,
					"value": i
				});
				html.style(node, "display", "block");
				node.setAttribute("aria-label", tip);
			}else{
				html.style(node, "display", "none");
			}
		}
	},
	
	_updatePageStepNodeClass: function(){
		// summary:
		//		Update the style of the page step nodes
		var value = null,
			curPage = this._getCurrentPageNo(),
			pageCount = this._getPageCount();
			
		var updateClass = function(node, isWardBtn, status){
			var value = node.value,
				enableClass = isWardBtn ? "dojoxGrid" + value + "Btn" : "dojoxGridInactived",
				disableClass = isWardBtn ? "dojoxGrid" + value + "BtnDisable" : "dojoxGridActived";
			if(status){
				html.addClass(node, disableClass);
				node.setAttribute("tabindex", "-1");
			}else{
				html.addClass(node, enableClass);
				node.setAttribute("tabindex", "0");
			}
		};
		array.forEach(this.pageStepperDiv.childNodes, function(node){
			html.removeClass(node);
			if(isNaN(parseInt(node.value, 10))){
				html.addClass(node, "dojoxGridWardButton");
				var disablePageNum = node.value == "prevPage" || node.value == "firstPage" ? 1 : pageCount;
				updateClass(node, true, (curPage === disablePageNum));
			}else{
				value = parseInt(node.value, 10);
				updateClass(node, false, (value === curPage || html.style(node, "display") === "none"));
			}
		}, this);
	},
	
	_showGotoButton: function(flag){
		this.gotoButton = flag;
		this._updateGotoButton();
	},
	
	_updateGotoButton: function(){
		// summary:
		//		Create/destroy the goto page button
		if(!this.gotoButton){
			if(this.gotoPageTd){
				if(this._gotoPageDialog){
					this._gotoPageDialog.destroy();
				}
				html.destroy(this.gotoPageDiv);
				html.destroy(this.gotoPageTd);
				delete this.gotoPageDiv;
				delete this.gotoPageTd;
			}
			return;
		}
		if(!this.gotoPageTd){
			this._createGotoNode();
		}
		html.toggleClass(this.gotoPageDiv, "dojoxGridPaginatorGotoDivDisabled", this.plugin._pageSize >= this.plugin._maxSize);
		this.gotoPageDiv.setAttribute("tabindex", "-1");
	},
	
	_createGotoNode: function(){
		// summary:
		//		Create the goto page button
		this.gotoPageTd = html.create("td", {}, query("tr", this.domNode)[0], "last");
		html.addClass(this.gotoPageTd, "dojoxGridPaginatorGotoTd");
		this.gotoPageDiv = html.create("div", {tabindex: "0", title: this.plugin.nls.gotoButtonTitle}, this.gotoPageTd, "first");
		html.addClass(this.gotoPageDiv, "dojoxGridPaginatorGotoDiv");
		this.plugin.connect(this.gotoPageDiv, "onclick", lang.hitch(this, "_openGotopageDialog"));
		// for high contrast
		var highConrastNode = html.create("span", {title: this.plugin.nls.gotoButtonTitle, innerHTML: "&#8869;"}, this.gotoPageDiv, "last");
		html.addClass(highConrastNode, "dojoxGridWardButtonInner");
	},
	
	_openGotopageDialog: function(event){
		// summary:
		//		Show the goto page dialog
		if(this._getPageCount() <= 1){
			return;
		}
		if(!this._gotoPageDialog){
			this._gotoPageDialog = new _GotoPageDialog(this.plugin);
		}
		// focus
		if(!this._currentFocusNode){
			this.grid.focus.focusArea("pagination" + this.position, event);
		}else{
			this._currentFocusNode = this.gotoPageDiv;
		}
		if(this.focusArea != "pageStep"){
			this.focusArea = "pageStep";
		}
		this._gotoPageDialog.updatePageCount();
		this._gotoPageDialog.showDialog();
	},
	
	// ===== focus handlers ===== //
	_onFocusPaginator: function(event, step){
		// summary:
		//		Focus handler
		if(!this._currentFocusNode){
			if(step > 0){
				return this._onFocusPageSizeNode(event) ? true : this._onFocusPageStepNode(event);
			}else if(step < 0){
				return this._onFocusPageStepNode(event) ? true : this._onFocusPageSizeNode(event);
			}else{
				return false;
			}
		}else{
			if(step > 0){
				return this.focusArea === "pageSize" ? this._onFocusPageStepNode(event) : false;
			}else if(step < 0){
				return this.focusArea === "pageStep" ? this._onFocusPageSizeNode(event) : false;
			}else{
				return false;
			}
		}
	},
	
	_onFocusPageSizeNode: function(event){
		// summary:
		//		Focus the page size area, if there is no focusable node, return false
		var pageSizeNodes = this._getPageSizeActivableNodes();
		if(event && event.type !== "click"){
			if(pageSizeNodes[0]){
				dijitFocus.focus(pageSizeNodes[0]);
				this._currentFocusNode = pageSizeNodes[0];
				this.focusArea = "pageSize";
				this.plugin._stopEvent(event);
				return true;
			}else{
				return false;
			}
		}
		if(event && event.type == "click"){
			if(array.indexOf(this._getPageSizeActivableNodes(), event.target) > -1){
				this.focusArea = "pageSize";
				this.plugin._stopEvent(event);
				return true;
			}
		}
		return false;
	},
	
	_onFocusPageStepNode: function(event){
		// summary:
		//		Focus the page step area, if there is no focusable node, return false
		var pageStepNodes = this._getPageStepActivableNodes();
		if(event && event.type !== "click"){
			if(pageStepNodes[0]){
				dijitFocus.focus(pageStepNodes[0]);
				this._currentFocusNode = pageStepNodes[0];
				this.focusArea = "pageStep";
				this.plugin._stopEvent(event);
				return true;
			}else if(this.gotoPageDiv){
				dijitFocus.focus(this.gotoPageDiv);
				this._currentFocusNode = this.gotoPageDiv;
				this.focusArea = "pageStep";
				this.plugin._stopEvent(event);
				return true;
			}else{
				return false;
			}
		}
		if(event && event.type == "click"){
			if(array.indexOf(this._getPageStepActivableNodes(), event.target) > -1){
				this.focusArea = "pageStep";
				this.plugin._stopEvent(event);
				return true;
			}else if(event.target === this.gotoPageDiv){
				dijitFocus.focus(this.gotoPageDiv);
				this._currentFocusNode = this.gotoPageDiv;
				this.focusArea = "pageStep";
				this.plugin._stopEvent(event);
				return true;
			}
		}
		return false;
	},
	
	_onFocusGotoPageNode: function(event){
		// summary:
		//		Focus the goto page button, if there is no focusable node, return false
		if(!this.gotoButton || !this.gotoPageTd){
			return false;
		}
		if(event && event.type !== "click" || (event.type == "click" && event.target == this.gotoPageDiv)){
			dijitFocus.focus(this.gotoPageDiv);
			this._currentFocusNode = this.gotoPageDiv;
			this.focusArea = "gotoButton";
			this.plugin._stopEvent(event);
			return true;
		}
		return true;
	},
	
	_onBlurPaginator: function(event, step){
		var pageSizeNodes = this._getPageSizeActivableNodes(),
			pageStepNodes = this._getPageStepActivableNodes();
		
		if(step > 0 && this.focusArea === "pageSize" && (pageStepNodes.length > 1 || this.gotoButton)){
			return false;
		}else if(step < 0 && this.focusArea === "pageStep" && pageSizeNodes.length > 1){
			return false;
		}
		this._currentFocusNode = null;
		this.focusArea = null;
		return true;
	},
	
	_onKeyDown: function(event, isBubble){
		// summary:
		//		Focus navigation
		if(isBubble){
			return;
		}
		if(event.altKey || event.metaKey){
			return;
		}
		if(event.keyCode === keys.ENTER || event.keyCode === keys.SPACE){
			if(array.indexOf(this._getPageStepActivableNodes(), this._currentFocusNode) > -1){
				this._onPageStep(event);
			}else if(array.indexOf(this._getPageSizeActivableNodes(), this._currentFocusNode) > -1){
				this._onSwitchPageSize(event);
			}else if(this._currentFocusNode === this.gotoPageDiv){
				this._openGotopageDialog(event);
			}
		}
		this.plugin._stopEvent(event);
	},
	
	_moveFocus: function(rowDelta, colDelta, evt){
		// summary:
		//		Move focus according row delta&column delta
		var nodes;
		if(this.focusArea == "pageSize"){
			nodes = this._getPageSizeActivableNodes();
		}else if(this.focusArea == "pageStep"){
			nodes = this._getPageStepActivableNodes();
			if(this.gotoPageDiv){
				nodes.push(this.gotoPageDiv);
			}
		}
		if(nodes.length < 1){
			return;
		}
		var currentIdx = array.indexOf(nodes, this._currentFocusNode);
		var focusIdx = currentIdx + colDelta;
		if(focusIdx >= 0 && focusIdx < nodes.length){
			dijitFocus.focus(nodes[focusIdx]);
			this._currentFocusNode = nodes[focusIdx];
		}
		this.plugin._stopEvent(evt);
	},
	
	_getPageSizeActivableNodes: function(){
		return query("span[tabindex='0']", this.sizeSwitchTd);
	},
	
	_getPageStepActivableNodes: function(){
		return query("div[tabindex='0']", this.pageStepperDiv);
	},
	
	_getAllPageSizeNodes: function(){
		var nodeList = [];
		array.forEach(this.sizeSwitchTd.childNodes, function(node){
			if(node.value){
				nodeList.push(node);
			}
		});
		return nodeList;
	},
	
	_getAllPageStepNodes: function(){
		var nodeList = [],
			i = 0, len = this.pageStepperDiv.childNodes.length;
		for(; i < len; i++){
			nodeList.push(this.pageStepperDiv.childNodes[i]);
		}
		return nodeList;
	},
	
	_moveToNextActivableNode: function(nodeList, curNodeValue){
		// summary:
		//		Need to move the focus to next node since current node is inactive and unfocusable
		if(!curNodeValue){
			return;
		}
		if(nodeList.length < 2){
			this.grid.focus.tab(1);
		}
		var nl = [],
			node = null,
			index = 0;
		array.forEach(nodeList, function(n){
			if(n.value === curNodeValue){
				nl.push(n);
				node = n;
			}else if(n.getAttribute("tabindex") === "0"){
				nl.push(n);
			}
		});
		if(nl.length < 2){
			this.grid.focus.tab(1);
		}
		index = array.indexOf(nl, node);
		if(node.getAttribute("tabindex") !== "0"){
			node = nl[index + 1] ? nl[index + 1] : nl[index - 1];
		}
		dijitFocus.focus(node);
		this._currentFocusNode = node;
	},
	
	// ===== pagination events handlers ===== //
	_onSwitchPageSize: function(/*Event*/e){
		// summary:
		//		The handler of switch the page size
		var size = this.plugin._pageSizeValue = this.pageSizeValue = e.target.value;
		if(!size){
			return;
		}
		if(lang.trim(size.toLowerCase()) == "all"){
			size = this._maxItemSize;
			this.plugin.showAll = true;
		}else{
			this.plugin.showAll = false;
		}
		this.plugin.grid.usingPagination = !this.plugin.showAll;
		
		size = parseInt(size, 10);
		if(isNaN(size) || size <= 0){
			return;
		}
		
		if(!this._currentFocusNode){
			this.grid.focus.currentArea("pagination" + this.position);
		}
		if(this.focusArea != "pageSize"){
			this.focusArea = "pageSize";
		}
		this.plugin.changePageSize(size);
	},
	
	_onPageStep: function(/*Event*/e){
		// summary:
		//		The handler jump page event
		var p = this.plugin,
			value = this.pageStepValue = e.target.value;
		
		if(!this._currentFocusNode){
			this.grid.focus.currentArea("pagination" + this.position);
		}
		if(this.focusArea != "pageStep"){
			this.focusArea = "pageStep";
		}
		if(!isNaN(parseInt(value, 10))){
			p.gotoPage(value);
		}else{
			switch(e.target.value){
				case "prevPage":
					p.prevPage();
					break;
				case "nextPage":
					p.nextPage();
					break;
				case "firstPage":
					p.gotoFirstPage();
					break;
				case "lastPage":
					p.gotoLastPage();
			}
		}
	},
	
	// ===== information getters ===== //
	_getCurrentPageNo: function(){
		return this.plugin._currentPage + 1;
	},
	
	_getPageCount: function(){
		if(!this._maxItemSize || !this.currentPageSize){
			return 0;
		}
		return Math.ceil(this._maxItemSize / this.currentPageSize);
	},
	
	_getStartPage: function(){
		var cp = this._getCurrentPageNo();
		var ms = parseInt(this.maxPageStep / 2, 10);
		var pc = this._getPageCount();
		if(cp < ms || (cp - ms) < 1){
			return 1;
		}else if(pc <= this.maxPageStep){
			return 1;
		}else{
			if(pc - cp < ms && cp - this.maxPageStep >= 0){
				return pc - this.maxPageStep + 1;
			}else{
				return (cp - ms);
			}
		}
	},
	
	_getStepPageSize: function(){
		var sp = this._getStartPage();
		var count = this._getPageCount();
		if((sp + this.maxPageStep) > count){
			return count - sp + 1;
		}else{
			return this.maxPageStep;
		}
	}

});

var Pagination = declare("dojox.grid.enhanced.plugins.Pagination", _Plugin, {
	// summary:
	//		The typical pagination way as an alternative to deal with huge data set besides the default virtual scrolling way
	
	name: "pagination",
	// The page size used with the store, default = 25.
	_pageSize: 25,
	
	_defaultRowsPerPage: 25,
	
	//current page we are at
	_currentPage: 0,

	//The currently obtained max # of rows to page through.
	_maxSize: 0,
	
	init: function(){
		this.gh = null;
		this._defaultRowsPerPage = this.grid.rowsPerPage;
		var size = this.option.defaultPageSize > 0 ? this.option.defaultPageSize :
				(this.grid.rowsPerPage ? this.grid.rowsPerPage : this._pageSize);
		this.grid.rowsPerPage = this._pageSize = size;
		this._currentPage = this.option.defaultPage > 0 ? this.option.defaultPage - 1 : 0;
		this.grid.usingPagination = true;
		this.nls = i18n.getLocalization("dojox.grid.enhanced", "Pagination");
		
		this._wrapStoreLayer();
		this._createPaginators(this.option);
		
		this._regApis();
	},
	
	_createPaginators: function(paginationArgs){
		// summary:
		//		Function to create the pagination control bar.
		this.paginators = [];
		if(paginationArgs.position === "both"){
			this.paginators = [
				new _Paginator(lang.mixin(paginationArgs, {position: "bottom", plugin: this})),
				new _Paginator(lang.mixin(paginationArgs, {position: "top", plugin: this}))
			];
		}else{
			this.paginators = [new _Paginator(lang.mixin(paginationArgs, {plugin: this}))];
		}
	},
	 
	_wrapStoreLayer: function(){
		var g = this.grid; 
		this._store = g.store;
		
		this.forcePageStoreLayer = new _ForcedPageStoreLayer(this);
		layers.wrap(g, "_storeLayerFetch", this.forcePageStoreLayer);
	},
	
	_stopEvent: function(event){
		try{
			evt.stop(event);
		}catch(e){}
	},
	
	_onNew: function(item, parentInfo){
		var totalPages = Math.ceil(this._maxSize / this._pageSize);
		if(((this._currentPage + 1 === totalPages || totalPages === 0) && this.grid.rowCount < this._pageSize) || this.showAll){
			lang.hitch(this.grid, this._originalOnNew)(item, parentInfo);
			this.forcePageStoreLayer.endIdx++;
		}
		this._maxSize++;
		if(this.showAll){
			this._pageSize++;
		}
		if(this.showAll && this.grid.autoHeight){
			this.grid._refresh();
		}else{
			array.forEach(this.paginators, function(p){
				p.update();
			});
		}
	},
	
	_removeSelectedRows: function(){
		this._multiRemoving = true;
		this._originalRemove();
		this._multiRemoving = false;
		this.grid.resize();
		this.grid._refresh();
	},
	
	_onDelete: function(){
		if(!this._multiRemoving){
			this.grid.resize();
			if(this.showAll){
				this.grid._refresh();
			}
		}
		if(this.grid.get('rowCount') === 0){
			this.prevPage();
		}
	},
	
	_regApis: function(){
		// summary:
		//		register pagination public APIs to grid.
		var g = this.grid;
		// New added APIs
		g.gotoPage = lang.hitch(this, this.gotoPage);
		g.nextPage = lang.hitch(this, this.nextPage);
		g.prevPage = lang.hitch(this, this.prevPage);
		g.gotoFirstPage = lang.hitch(this, this.gotoFirstPage);
		g.gotoLastPage = lang.hitch(this, this.gotoLastPage);
		g.changePageSize = lang.hitch(this, this.changePageSize);
		g.showGotoPageButton = lang.hitch(this, this.showGotoPageButton);
		g.getTotalRowCount = lang.hitch(this, this.getTotalRowCount);
		// Changed APIs
		this.originalScrollToRow = lang.hitch(g, g.scrollToRow);
		g.scrollToRow = lang.hitch(this, this.scrollToRow);
		this._originalOnNew = lang.hitch(g, g._onNew);
		this._originalRemove = lang.hitch(g, g.removeSelectedRows);
		g.removeSelectedRows = lang.hitch(this, this._removeSelectedRows);
		g._onNew = lang.hitch(this, this._onNew);
		this.connect(g, "_onDelete", lang.hitch(this, this._onDelete));
	},
	
	destroy: function(){
		this.inherited(arguments);
		var g = this.grid;
		try{
			array.forEach(this.paginators, function(p){
				p.destroy();
			});
			g.unwrap(this.forcePageStoreLayer.name());
			g._onNew = this._originalOnNew;
			g.removeSelectedRows = this._originalRemove;
			g.scrollToRow = this.originalScrollToRow;
			this.paginators = null;
			this.nls = null;
		}catch(e){
			console.warn("Pagination.destroy() error: ", e);
		}
	},
	
	nextPage: function(){
		// summary:
		//		Function to handle shifting to the next page in the list.
		if(this._maxSize > ((this._currentPage + 1) * this._pageSize)){
			//Current page is indexed at 0 and gotoPage expects 1-X.  So to go
			//up  one, pass current page + 2!
			this.gotoPage(this._currentPage + 2);
		}
	},

	prevPage: function(){
		// summary:
		//		Function to handle shifting to the previous page in the list.
		if(this._currentPage > 0){
			//Current page is indexed at 0 and gotoPage expects 1-X.  So to go
			//back one, pass current page!
			this.gotoPage(this._currentPage);
		}
	},

	gotoPage: function(page){
		// summary:
		//		Function to handle shifting to an arbirtary page in the list.
		//	page:
		//		The page to go to, starting at 1.
		var totalPages = Math.ceil(this._maxSize / this._pageSize);
		page--;
		if(page < totalPages && page >= 0 && this._currentPage !== page){
			this._currentPage = page;
			// this._updateSelected();
			this.grid._refresh(true);
			this.grid.resize();
		}
	},
	
	gotoFirstPage: function(){
		// summary:
		//		Go to the first page
		this.gotoPage(1);
	},
	
	gotoLastPage: function(){
		// summary:
		//		Go to the last page
		var totalPages = Math.ceil(this._maxSize / this._pageSize);
		this.gotoPage(totalPages);
	},
	
	changePageSize: function(size){
		// summary:
		//		Change size of items per page.
		//		This function will only be called by _Paginator
		if(typeof size === "string"){
			size = parseInt(size, 10);
		}
		var startIndex = this._pageSize * this._currentPage;
		array.forEach(this.paginators, function(f){
			f.currentPageSize = this.grid.rowsPerPage = this._pageSize = size;
			if(size >= this._maxSize){
				this.grid.rowsPerPage = this._defaultRowsPerPage;
				this.showAll = true;
				this.grid.usingPagination = false;
			}else{
				this.grid.usingPagination = true;
			}
		}, this);
		var endIndex = startIndex + Math.min(this._pageSize, this._maxSize);
		if(endIndex > this._maxSize){
			this.gotoLastPage();
		}else{
			var cp = Math.ceil(startIndex / this._pageSize);
			if(cp !== this._currentPage){
				this.gotoPage(cp + 1);
			}else{
				this.grid._refresh(true);
			}
		}
		this.grid.resize();
	},
	
	showGotoPageButton: function(flag){
		// summary:
		//		For show/hide the go to page button dynamically
		// flag: boolean
		//		Show the go to page button when flag is true, otherwise hide it
		array.forEach(this.paginators, function(p){
			p._showGotoButton(flag);
		});
	},
	
	scrollToRow: function(inRowIndex){
		// summary:
		//		Override the grid.scrollToRow(), could jump to the right page
		//		and scroll to the specific row
		// inRowIndex: integer
		//		The row index
		var page = parseInt(inRowIndex / this._pageSize, 10),
			totalPages = Math.ceil(this._maxSize / this._pageSize);
		if(page > totalPages){
			return;
		}
		this.gotoPage(page + 1);
		var rowIdx = inRowIndex % this._pageSize;
		this.grid.setScrollTop(this.grid.scroller.findScrollTop(rowIdx) + 1);
	},
	
	getTotalRowCount: function(){
		// summary:
		//		Function for get total row count
		return this._maxSize;
	}
});

EnhancedGrid.registerPlugin(Pagination/*name:'pagination'*/);

return Pagination;
});
