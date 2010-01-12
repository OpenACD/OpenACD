
if(typeof(supervisorView) == "undefined"){

	supervisorView = function(){
		return {};
	};

	supervisorView.healthData = {identifier:"id",
	label:"display",
	items:[]
	};

	supervisorView.dataStore = new dojo.data.ItemFileWriteStore({
		data: supervisorView.healthData,
		typeMap:{
			"details":{
				"type":Object,
				"deserialize":function(obj){return obj;}
			}
		}
	});

	supervisorView.systemStack = [];

	supervisorView.node = "*";
	supervisorView.queueGroupsStack = {clear:function(){ return true;}};
	supervisorView.queuesStack = {clear:function(){ return true;}};
	supervisorView.agentsStack = {clear:function(){ return true;}};
	supervisorView.agentProfilesStack = {clear:function(){ return true;}};
	supervisorView.callsStack = {clear:function(){return true;}};
	supervisorView.suppressPoll = false;
	supervisorView.hpCalcInterval = 100;
	supervisorView.showEmptyProfiles = false;
	/*
	To use the dndManager:
		item to drag should implement:
			impelement dojox.gfx.Moveable.
			event onFirstMove should call startDrag(this);
			impelement setDroppable(bool) for a visual indicator on if it's droppable.
		item to accept drops:
			impelement pointCollision(pt) -> bool
			impelement dragOver(obj) -> bool
			impelement dropped(obj)
			registerCollision
	*/
	
	supervisorView.dndManager = {
		_dragging: null,
		_eventHandle:null,
		_dropCandidate: null,
		_collisionCandidates: [],
		checkCollision:function(pt){
			var out = false;
			for(var i = 0; i < supervisorView.dndManager._collisionCandidates.length; i++){
				var obj = supervisorView.dndManager._collisionCandidates[i];
				if(obj && obj.pointCollision){
					if(obj.pointCollision(pt)){
						return obj;
					}
				}
			}
						
			return false;
		},
		startDrag:function(obj){
			supervisorView.dndManager._eventHandle = supervisorView.surface.connect('onmousemove', function(ev){
				var point = {
					x:ev.layerX,
					y:ev.layerY
				};
				debug(["checking collision", point]);
				var collided = supervisorView.dndManager.checkCollision(point);
				if(collided && collided.dragOver){
					var setDrop = false;
					if(collided.dragOver(obj)){
						supervisorView.dndManager._dropCandidate = collided;
						setDrop = true;
					} else{
						debug(["collided.dragOver false", collided, obj]);
						supervisorView.dndManager._dropCandidate = false;
						setDrop = false;
					}
					
					if(obj.setDroppable){
						obj.setDroppable(setDrop);
					}
				} else{
					debug(["one of them's false", collided, collided.dragOver]);
					supervisorView.dndManager._dropCandidate = null;
				}
			});
			supervisorView.suppressPoll = true;
			supervisorView.dndManager._dragging = obj;
			if(typeof(obj.startDrag) == 'function'){
				obj.startDrag();
			}
		},
		endDrag:function(){
			debug("ending drag");
			supervisorView.suppressPoll = false;
			supervisorView.surface.disconnect(supervisorView.dndManager._eventHandle);
			if(supervisorView.dndManager._dropCandidate){
				supervisorView.dndManager._dropCandidate.dropped(supervisorView.dndManager._dragging);
			}
			if(typeof(supervisorView.dndManager._dragging.endDrag) == 'function'){
				supervisorView.dndManager._dragging.endDrag();
			}
		},
		registerCollider:function(obj){
			if(obj.pointCollision && obj.dragOver){
				var i = 0;
				while(supervisorView.dndManager._collisionCandidates[i]){
					i++;
				}
			
				supervisorView.dndManager._collisionCandidates[i] = obj;
				return i;
			}
			
			warning(["rejected register", obj.pointCollision, obj.dragOver, obj]);
			return false;
		},
		unregisterCollider:function(index){
			supervisorView.dndManager._collisionCandidates[index] = false;
		}
	};

	//=====
	// And so begins defining useful classes
	//=====	
	
	//=====
	// Bubble
	//=====
	
	supervisorView.Bubble = function(conf){
		this.defaultConf = {
			point:{x:100,y:100},
			scale:1,
			parent:supervisorView.surface,
			data:{"display":"bubble", "health":50, "type":"text", "id":"id"},
			image: false,
			onmouseenter:function(){ return true;},
			onmouseleave:function(){return true;},
			onclick:function(){ return false;},
			startDrag: false,
			endDrag: false,
			dropped: function(){ return false;},
			dragOver: function(){ debug(["default bubble dragOver"]); return false;},
			subscriptions:[],
			moveable: false,
			menu:false,
			menuBlur:function(){return true}
		};
		conf.data = dojo.mixin(this.defaultConf.data, conf.data);
		conf = dojo.mixin(this.defaultConf, conf);
		
		this.group = conf.parent.createGroup();
		this.subscriptions = [];
		for(var i = 0; i < conf.subscriptions.length; i++){
			var subObj = conf.subscriptions[i];
			this.subscriptions.push(dojo.subscribe(subObj.channel, this, subObj.callback));
		}
		
		debug(["conf.data", conf.data]);
		this.hpline = this.group.createLine({
			x1: conf.point.x + (conf.data.health * 2),
			x2: conf.point.x + (conf.data.health * 2),
			y1: conf.point.y - 3,
			y2: (conf.point.y + 3 + 20)
		}).setStroke({width:7, color:"black", cap:"round"});
		
		this.bubble = this.group.createRect({
			x: conf.point.x,
			y: conf.point.y,
			width: 200,
			height: 20,
			r: 10
		}).setStroke("black");
		
		this.data = conf.data;
		
		this.group.connect("onmouseenter", this, conf.onmouseenter);
		this.group.connect("onmouseleave", this, conf.onmouseleave);
		this.group.connect("onclick", this, function(ev){
			conf.onclick(ev);
		});
		
		var textdisp = conf.data.display;
		if(textdisp.length > 18){
			textdisp = textdisp.substring(0, 16) + "...";
		}
		
		var text = this.group.createText({
			x: conf.point.x + 100,
			y: conf.point.y + 15,
			align: "middle",
			text: textdisp
		});
		
		this.text = text;
		
		var rect = this.bubble;
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		};
		
		if(conf.image){
			this.image = this.group.createImage({
				x: rect.getShape().x + 2,
				y: rect.getShape().y + 2,
				width: rect.getShape().height - 4,
				height: rect.getShape().height - 4,
				src: conf.image
			});
		}
		
		this.dropped = conf.dropped;
		this.dragOver = conf.dragOver;

		this.setHp(conf.data.health);
		
		if(conf.menu){
			/*var menu = dijit.byId(conf.menu);
			if(menu){
				debug(["menu found on bubble creation"]);				
			}
			else{
				warning(["menu id not found", conf.menu, menu]);
			}
			this.boundMenu = conf.menu;*/
			//this.group.rawNode.oncontextmenu = function(ev){
			this._contextMenuConnect = dojo.connect(this.group.rawNode, 'oncontextmenu', this, function(ev){
				debug(ev);
				var menu = this._buildMenu(conf.menu, conf.menuBlur);
				//dijit.byId(conf.menu)._openMyself(ev);
				menu._openMyself(ev);
				if (dojo.isIE) {
					ev.returnValue = false;
				} else {
					ev.preventDefault();
					ev.stopPropagation();
				}
			});
			
		}
		
		if(conf.moveable){
			this.moveable = new dojox.gfx.Moveable(this.group, {delay:1});
			if (navigator.appVersion.indexOf("Mac")!=-1 && dojo.isFF) {
				dojo.connect(this.moveable, 'onMouseDown', this.moveable, function(ev){
					if(ev.button == 2 && conf.menu){
						// Firefox is weird.  If a moveable is defined, it overrides
						// the menu.  Safari does not have this issue.  Other browsers
						// are untested.
						//var menu = dijit.byId(conf.menu);
						var menu = this._buildMenu(conf.menu, conf.menuBlur);
						menu._openMyself(ev);
						ev.preventDefault();
						ev.stopPropagation();
					}
				});
			}
			if(conf.menu){
				dojo.connect(this.moveable, 'onMouseDown', this, function(ev){
					if(ev.button == 2){
						this.moveable.destroy();
					}
				});
				dojo.connect(this.group, 'onMouseUp', this, function(ev){
					if(! this.moveable){
						makeMoveable();
					}
				});
			}
			if(conf.startDrag){
				this.startDrag = conf.startDrag;
			}
			if(conf.endDrag){
				this.endDrag = conf.endDrag;
			}
		}
		
		
		
		this.group.setTransform([dojox.gfx.matrix.scaleAt(conf.scale, p)]);
	};
	
	supervisorView.Bubble.prototype.size = function(scale){
		var rect = this.bubble;
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		};
		debug(["size point", p]);
		this.group.setTransform([dojox.gfx.matrix.scaleAt(scale, p)]);
	};
		
	supervisorView.Bubble.prototype.shrink = function(){
		this.size(1);
	};
	
	supervisorView.Bubble.prototype.grow = function(){
		this.size(1.4);
	};

	supervisorView.Bubble.prototype.setHp = function(hp){
		var rmod;
		var gmod;
		var bmod;
		
		if(hp < 1){
			rmod = 0;
			gmod = 1;
			bmod = 1;
		}
		else if(hp < 50){
			rmod = 0;
			gmod = 1;
			bmod = 1 - (hp / 50);
		}
		else if(hp < 75){
			rmod = (hp - 50) / 25;
			gmod = 1;
			bmod = 0;
		}
		else if(hp < 100){
			rmod = 1;
			gmod = 1 - ( (hp - 75) / 25);
			bmod = 0;
		}
		else{
			rmod = 1;
			gmod = 0;
			bmod = 0;
		}
	
		var r = Math.round(255 * rmod);
		var g = Math.round(255 * gmod);
		var b = Math.round(255 * bmod);
		
		var textcolors = "black";
		
		var bubblefill = [r, g, b, 100];
		
		this.text.setStroke(textcolors);
		this.text.setFill(textcolors);
		this.bubble.setFill(bubblefill);
		
		var bshape = this.bubble.getShape();
		
		var thex = (hp * bshape.width)/100 + bshape.x;
		this.hpline.setShape({x1: thex, x2:thex});
	};
	
	supervisorView.Bubble.prototype.pointCollision = function(point){
		var arr = this.bubble.getTransformedBoundingBox();
		debug(["pointCollision", point, arr]);
		var topleft = arr[0];
		var topright = arr[1];
		var bottomright = arr[2];
		var bottomleft = arr[3];
		return (topleft.x <= point.x && bottomright.x >= point.x) && (topleft.y <= point.y && bottomright.y >= point.y);
	};
	
	supervisorView.Bubble.prototype.boxCollision = function(intopleft, inbottomright){
		var arr = this.bubble.getTransformedBoundingBox();
		var topleft = arr[0];
		var topright = arr[1];
		var bottomright = arr[2];
		var bottomleft = arr[3];
		return (!(topleft.x > inbottomright.x || intopleft.x > bottomright.x || topleft.y > inbottomright.y || inbottomright.y > bottomright.y));
	};
	
	supervisorView.Bubble.prototype.clear = function(){
		for(var i = 0; i < this.subscriptions.length; i++){
			dojo.unsubscribe(this.subscriptions[i]);
		}
		
		if(this.boundMenu){
			var menu = dijit.byId(this.boundMenu);
			menu.unBindDomNode(this.group.rawNode);
		}
		
		var parent = this.group.rawNode.parentNode;
		this.group.clear();
		if(parent){
			parent.removeChild(this.group.rawNode);
		}
		else{
			info(["parent not defined", parent, this.group.rawNode]);
		}
	};
	
	supervisorView.Bubble.prototype.connect = function(ev, scope, fun){
		this.group.connect(ev, scope, fun);
	};
	
	supervisorView.Bubble.prototype.setImage = function(imageSrc){
		if(imageSrc === false){
			if(this.image){
				this.image.removeShape();
				this.image = false;
				return true;
			}
		}
		
		if(this.image){
			this.image.removeShape();
		}
		
		this.image = this.group.createImage({
			x: this.bubble.getShape().x + 2,
			y: this.bubble.getShape().y + 2,
			width: this.bubble.getShape().height - 4,
			height: this.bubble.getShape().height - 4,
			src: imageSrc
		});
		
		return this.image;
	};
	
	supervisorView.Bubble.prototype.setDroppable = function(bool){
		if(!this.previousImage){
			if(this.image){
				this.previousImage = this.image.getShape().src;
			}
			else{
				this.previousImage = false;
			}
		}
			
		if(bool === null){
			this.setImage(this.previousImage);
			delete this.previousImage;
		}
		else if(bool){
			this.setImage('/images/dl.png');
		}
		else{
			this.setImage('/images/redx.png');
		}
	};

	supervisorView.Bubble.prototype._buildMenu = function(menuitems, onblur){
		var bubbleMenu = new dijit.Menu({});
		for(var i = 0; i < menuitems.length; i++){
			if(menuitems[i] == 'separator'){
				bubbleMenu.addChild(new dijit.MenuSeparator());
			} else {
				bubbleMenu.addChild(new dijit.MenuItem({
					label:  menuitems[i].label,
					onClick: menuitems[i].onClick
				}));
			}
		}
		dojo.connect(this, 'destroy', this, function(){
			bubbleMenu.destroy();
		});
		dojo.connect(bubbleMenu, 'onBlur', onblur);
		return bubbleMenu;
	};
	
	//=====
	// BubbleStack
	//=====
	
	supervisorView.BubbleStack = function(conf){
		this.defaultConf = {
			parent: supervisorView.surface,
			mousept: {x:100, y:50},
			viewHeight:350,
			bubbleConfs: [],
			registerCollider: false,
			menu: false
		};
		
		conf = dojo.mixin(this.defaultConf, conf);
		this.conf = conf;
		this._hasMenus = false;
		if(this.conf.menu){
			this._hasMenus = true;
		}
		var groupHeight = conf.bubbleConfs.length * 40;
		var viewHeight = conf.viewHeight;
		
		var ratio = (groupHeight) / viewHeight;
		this.trueRatio = ratio;
		if(groupHeight < viewHeight){
			ratio = 0;
		}
		else{
			ratio = 1;
		}
		this.ratio = ratio;
		
		var parent = conf.parent;
		this.group = parent.createGroup();
		
		var yi = 0;
		var pt = {
			x:conf.mousept.x,
			y:conf.mousept.y
		};
		
		this.viewHeight = conf.viewHeight;
		
		this.bubbleConfs = conf.bubbleConfs;
		
		var thisref = this;
		
		this._menuBlurConnects = {};
		
		for(var i = 0; i < this.bubbleConfs.length; i++){
			var mixing = {
				point:{x:pt.x, y:this.indexToY(i)},
				parent:this.group,
				menu: this.conf.menu
			}
			
			if(this.bubbleConfs[i].menu){
				mixing.menu = this.bubbleConfs[i].menu;
				mixing.menuBlur = function(){ thisref.unlockScroll(); };
				this._hasMenus = true;
				if(! this._menuBlurConnects[mixing.menu]){
					this._menuBlurConnects[mixing.menu] = dojo.connect(dijit.byId(mixing.menu), 'onBlur', this, function(){
						this.unlockScroll();
					});
				}
			}
		
			this.bubbleConfs[i] = dojo.mixin(this.bubbleConfs[i], mixing);
		}
		
		var alpha = 0;
		if(getLogLevel() == "debug"){
			alpha = 1;
		}
		
		this.backing = this.group.createRect({x:pt.x, y:pt.y, width:200, height: groupHeight}).setFill([100, 100, 100, alpha]).setStroke([0, 0, 0, 0]);
		
		this.bubbles = [];
		//var hps = [];
		
		this.scrollLocked = false;
		
		this.subscriptions = [];
		this.group.connect("onmousemove", this, function(ev){
			debug(["backing onmousemove", ev]);
			this._scroll(ev.layerY - this.conf.mousept.y);
		});
		
		if(this.conf.registerCollider){
			//stub it so it's no rejected on register.
			this.dragOver = function(){ return false;};
			this.coll = supervisorView.dndManager.registerCollider(this);
		}
		
		if(this.conf.menu){
			if(! this._menuBlurConnects[this.conf.menu]){
				this._menuBlurConnects[this.conf.menu] = dojo.connect(dijit.byId(this.conf.menu), 'onBlur', this, function(){
					this.unlockScroll();
				})
			}
		}
	};
	
	supervisorView.BubbleStack.prototype.indexToY = function(index){
		return this.conf.mousept.y + index * 40;
	};
	
	supervisorView.BubbleStack.prototype.viewYtoLocalY = function(viewY){
		var stackHeight = 40 * this.bubbleConfs.length;
		debug(['viewYtoLocalY', {'stackH':stackHeight, 'viewH':this.conf.viewHeight, 'viewY':viewY}]);
		if(stackHeight <= this.conf.viewHeight){
			return viewY;
		}
		
		return Math.floor((stackHeight * viewY) / this.conf.viewHeight);
	};
	
	supervisorView.BubbleStack.prototype.yToIndex = function(localY){
		return Math.round(localY / 40);
	};
	
	supervisorView.BubbleStack.prototype.viewYToIndex = function(viewY){
		var localy = this.viewYtoLocalY(viewY);
		debug(["localy", localy]);
		return this.yToIndex(localy);
	};
	
	supervisorView.BubbleStack.prototype.lockScroll = function(){
		this.scrollLocked = true;
	};
	
	supervisorView.BubbleStack.prototype.unlockScroll = function(){
		this.scrollLocked = false;
	};
	
	supervisorView.BubbleStack.prototype._setSelected = function(index){
		debug(["_setSelected", index, this._selected]);
		if(this.scrollLocked){
			return false;
		}
		var bubConf = '';
		if(this._selected !== undefined){
			debug("shrinking old selected");
			bubConf = this.bubbleConfs[this._selected];
			if(bubConf.bubble){
				bubConf.bubble.size(1);
			}
		}
		
		if((index !== false) && this.bubbleConfs[index]){
			debug("enlarging new selected");
			bubConf = this.bubbleConfs[index];
			if(bubConf.bubble){
				bubConf.bubble.size(1.4);
			}
			this._selected = index;
			return true;
		}
		
		delete this._selected;
	};
	
	supervisorView.BubbleStack.prototype.getSelected = function(){
		if(this._selected){
			return this.bubbleConfs[this._selected];
		}
		
		return false;
	};
	
	supervisorView.BubbleStack.prototype.scroll = function(index){
		if(index >= this.bubbleConfs.length){
			index = this.bubbleConfs.length - 1;
		}
		
		if(index < 0){
			index = 0;
		}
		
		var y = this.indexToY(index);
		this._scroll(y);
	};
	
	supervisorView.BubbleStack.prototype._scroll = function(viewY){
		if(this.scrollLocked){
			return false;
		}
		
		if(isNaN(viewY)){
			return false;
		}
		var range = Math.abs(Math.ceil(this.conf.viewHeight / 40));
		var midIndex = this.viewYToIndex(viewY);
		
		var startSize = Math.ceil( (midIndex * range) / this.bubbleConfs.length);

		var min = midIndex - range;
		var max = midIndex + range;
		
		debug(["selected computeds", {
			'range':range,
			'min': min, 
			'max': max,
			'mid': midIndex,
			'startSize': startSize,
			'viewY': viewY,
			'confViewHeight': this.conf.viewHeight
		}]);
		
		var acc = [];
		if(min < 0){
			min = 0;
		}
		
		if(max > this.bubbleConfs.length){
			max = this.bubbleConfs.length;
		}
		
		for(var i = min; i < max; i++){
			acc.push(i);
		}
		
		debug(["acc", acc]);
		for(i = 0; i < min; i++){
			var obj = this.bubbleConfs[i];
			if(obj.bubble){
				obj.bubble.clear();
				delete this.bubbleConfs[i].bubble;
			}
		}
		var thisref = this;
		
		dojo.forEach(acc, function(ind){
			var obj = thisref.bubbleConfs[ind];
			if(! obj.bubble){
				var drawy = thisref.indexToY(ind);
				var bubbleConf = dojo.mixin(obj, {
					mousept: {
						x: thisref.conf.mousept.x,
						y: drawy
					},
					parent: thisref.group,
					subscriptions:[
						{channel: 'supervisorView/set/' + obj.data.id,
						callback: function(rawobj){
							debug(["got it!", obj, rawobj]);
							this.setHp(rawobj.aggregate);
						}},
						{channel: 'supervisorView/drop/' + obj.data.id,
						callback: function(){
							this.clear();
						}}
					]
				});

				thisref.bubbleConfs[ind].bubble = new supervisorView.Bubble(bubbleConf);
				thisref.bubbleConfs[ind].bubble.group.connect('onmouseenter', thisref, function(ev){
					debug(["ind hit", ind]);
					thisref._setSelected(ind);
				});
				if(thisref.bubbleConfs[ind].moveable){
					obj = thisref.bubbleConfs[ind].bubble;
					//obj.mover = new dojox.gfx.Moveable(obj.group);
					dojo.connect(obj.moveable, 'onFirstMove', function(){
						thisref.lockScroll();
						thisref.group.moveToFront();
						supervisorView.dndManager.startDrag(obj);
					});
					obj.connect('onmouseup', function(){
						thisref.unlockScroll();
						obj.setDroppable(null);
						//obj.setImage(false);
						supervisorView.dndManager.endDrag();
					});
				}
				if(thisref.bubbleConfs[ind].menu){
					thisref.bubbleConfs[ind].menuClose = function(){
						console.log('unlock scroll');
						thisref.unlockScroll();
					};
					obj = thisref.bubbleConfs[ind].bubble;
					obj.connect('onmousedown', function(ev){
						if(ev.button == 2){
							thisref.lockScroll();
						}
					});
				}
			}
		});
		
		for(i = max; i < this.bubbleConfs.length; i++){
			obj = this.bubbleConfs[i];
			if(obj && obj.bubble){
				obj.bubble.clear();
				delete this.bubbleConfs[i].bubble;
			}
		}

		debug(["viewY and ration", {'1':viewY, '2':this.ratio, '3':this.trueRatio}]);
		this.group.setTransform([dojox.gfx.matrix.translate(0, -viewY * this.trueRatio * this.ratio)]);
	};
	
	supervisorView.BubbleStack.prototype.clear = function(){
		if(this.scrollLocked){
			return false;
		}
		
		dojo.forEach(this.subscriptions, function(obj){
			dojo.unsubscribe(obj);
		});
		this.group.clear();
		
		if(this.coll){
			supervisorView.dndManager.unregisterCollider(this.coll);
		}
				
		var p = this.group.rawNode.parentNode;
		this.group.clear();
		if(p){
			p.removeChild(this.group.rawNode);
		}
	};
	
	supervisorView.BubbleStack.prototype.pointCollision = function(point){
		if(this.coll){
			var transed = this.backing.getTransformedBoundingBox();
			//var topleft = arr[0];
			//var topright = arr[1];
			//var bottomright = arr[2];
			//var bottomleft = arr[3];
			//return (topleft.x <= point.x && bottomright.x >= point.x) && (topleft.y <= point.y && bottomright.y >= point.y)
			if (transed[0].x <= point.x && transed[1].x >= point.x){
				this._scroll(point.y);
				for(var i = 0; i < this.bubbleConfs.length; i++){
					if(this.bubbleConfs[i].bubble){
						var b = this.bubbleConfs[i].bubble;
						if(b.pointCollision(point)){
							if(b.onEnter){
								b.onEnter();
							}
							this.dragOver = function(arg){
								debug(["dragOver called"]);
								return b.dragOver(arg);
							};
							this.dropped = function(arg){
								return b.dropped(arg);
							};
							return b;
						}
					}
				}
			}
		}
		
		delete this.dragOver;
		delete this.dropped;
		return null;
	};
	
	supervisorView.BubbleStack.prototype.boxCollision = function(intopleft, intopright, inbottomright, inbottomleft){
		if(! (this.backing.getShape().x > inbottomright.x || intopleft.x > (this.backing.getShape().x + this.backing.getShape().width))){
			for(var i =0; i < this.bubbleConfs.length; i++){
				var bc = this.bubbleConfs[i];
				if(bc.bubble){
					if(bc.bubble.boxCollision(intopleft, inbottomright)){
						return bc.bubble;
					}
				}
			}
		}
		return null;
	};
	
	supervisorView.BubbleStack.prototype.forEachBubble = function(callback){
		// callback is just the bubble.
		dojo.forEach(this.bubbleConfs, function(obj){
			if(obj.bubble){
				callback(obj.bubble);
			}
		});
	};
	
	//=====
	//utility functions
	//=====

	supervisorView.averageHp = function(hps){
		if(hps.length === 0){
			return 50;
		}
		
		debug(["averaging hps", hps]);
		var findweight = function(hp){
			if(hp > 50){
				// y = floor(.0039(x-50)^2 + 1)
				// I don't use the above formula for both sides because anything
				// below 50 will always have a weight of 1
				return Math.floor(0.0039 * Math.pow((hp - 50), 2) + 1);
			}
			return 1;
		};
		
		var total = 0;
		var count = 0;
		
		var f = function(obj, index, arr){
			debug(["averageing hp obj", obj]);
			var c = '';
			if(obj === undefined){
				// la la la
				//warning("obj was undefined in averageHp");
			}
			else if(typeof(obj) == "number"){
				debug("number");
				c = findweight(obj);
				count += c;
				total += c * obj;
			}
			else if(obj.health !== undefined){
				debug("health");
				c = findweight(obj.health);
				count += c;
				total += c * obj.health;
			}
			else if(obj.goal !== undefined){
				debug("goal");
				var value = 0;
				if(obj.time){
					debug("time");
					value = Math.floor(new Date().getTime()/1000) - obj.time;
				}
				else{
					debug("value");
					value = obj.value;
				}
				debug(["value", value]);
				var hp = 50;
				if(obj.goal == value){
					debug("goal match");
					c = findweight(50);
					count += c;
					total += c * 50;
				}
				else if( (obj.min < obj.max) && (obj.max <= value) ){
					debug("min < max < value");
					c = findweight(100);
					count += c;
					total += c * 100;
				}
				else if( ( obj.min < obj.max) && (value <= obj.min) ){
					debug("value < min < max");
					c = findweight(0);
					count += c;
					total += c * 0;
				}
				else if( (obj.max < obj.min) && (value <= obj.max) ){
					debug("value < max < min");
					c = findweight(100);
					count += c;
					total += c * 100;
				}
				else if( (obj.max < obj.min) && (obj.min <= value) ){
					debug("max < min < value");
					c = findweight(0);
					count += c;
					total += c * 0;
				}
				else if( (obj.min < obj.max) && ( value < obj.goal) ){
					debug("min < value < goal"); 
					hp = (50 * value - 50 * obj.min) / (obj.goal - obj.min);
					c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.min < obj.max) && ( obj.goal < value)){
					debug("min < goal < value");
					hp = (100 * obj.goal - 50 * obj.max - 50 * value) / (obj.goal - obj.max);
					c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.max < obj.min) && (value < obj.goal) ){
					debug("max < value < goal");
					hp = (50 * (2 * obj.goal - value - obj.max) ) / (obj.goal - obj.max);
					c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.max < obj.min) && (obj.goal < value) ){
					debug("everything else");
					hp = ( ( 50 * ( obj.goal - value) ) / (obj.min - obj.goal) ) + 50;
					c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else{
					warning(["obj did not match any pattern", obj]);
				}				
			}
			else{
				warning(["Not number, .health, or .goal", obj]);
			}
		};
		
		dojo.forEach(hps, f);
			
		return Math.round(total / count);
	};

	supervisorView.setHpCalcTimer = function(callback, scale){
		if(supervisorView.hpCalcInterval !== false){
			supervisorView.aggregateTimer = setTimeout(callback, supervisorView.hpCalcInterval * scale);
		}
	}
	
	supervisorView.setMediaHps = function(){
		info("setMediaHps entry");
		var setHps = function(items){
			info(["setMediaHps fetch done", items]);
			var setHp = function(obj, ind, arr){
				var healthobj = supervisorView.dataStore.getValue(obj, "health");
				var hpsvars = [];
				for(var i in healthobj){
					hpsvars.push(healthobj[i]);
				}
				debug(["setMediaHps wants avergaed:  ", hpsvars]);
				var hp = supervisorView.averageHp(hpsvars);
				supervisorView.dataStore.setValue(obj, "aggregate", hp);
				var rawobj = {
					"id":"media-" + supervisorView.dataStore.getValue(obj, "display"),
					"display":supervisorView.dataStore.getValue(obj, "display"),
					"aggregate":hp,
					"type":"media",
					"health":{},
					"details":{}
				};
				debug(["setMediaHps", obj]);
				dojo.publish("supervisorView/set/" + rawobj.id, [obj, rawobj]);				
			};
			dojo.forEach(items, setHp);
			supervisorView.dataStore.save();
			supervisorView.setHpCalcTimer(supervisorView.setQueueHps, items.length);
			info(["setMediaHps done"]);
		};
		supervisorView.dataStore.fetch({
			query:{"type":"media"},
			onComplete:setHps
		});
									  
	};

	supervisorView.setQueueHps = function(){
		info("setQueueHps entry");
		var setHp = function(item){
			info("setQueueHps setHp entry");
			var healthobj = supervisorView.dataStore.getValue(item, "health");
			var hpsvars = [];
			for(var i in healthobj){
				hpsvars.push(healthobj[i]);
			}
			var gotMedia = function(mitems){
				info("gotMedia entry");
				if(mitems.length > 0){
					dojo.forEach(mitems, function(mitem){
						hpsvars.push(supervisorView.dataStore.getValue(mitem, "aggregate"));
					});
				}
				var hp = supervisorView.averageHp(hpsvars);
				supervisorView.dataStore.setValue(item, "aggregate", hp);
				supervisorView.dataStore.save();
				var rawobj = {
					"id":"queue-" + supervisorView.dataStore.getValue(item, "display"),
					"display":supervisorView.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"queue",
					"health":{},
					"details":{}
				};
				info(["setQueueHps fetchMedias done", item]);
				dojo.publish("supervisorView/set/" + rawobj.id, [item, rawobj]);
			};
			
			supervisorView.dataStore.fetch({
				query:{
					type:"media",
					queue:supervisorView.dataStore.getValue(item, "display"),
					node:supervisorView.node
				},
				onComplete:function(got){
					gotMedia(got);
				}
			});
		};

		var setHps = function(items){
			info(["setQueueHps fetch done", items]);
			dojo.forEach(items, setHp);
			supervisorView.setHpCalcTimer(supervisorView.setQueueGroupHps, items.length * 2);
			info(["setQueueHps done(ish)"]);		
		};

		supervisorView.dataStore.fetch({
			query:{"type":"queue"},
			onComplete:setHps
		});
	};

	supervisorView.setAgentHps = function(){
		info("setAgentHps entry");
		var setHp = function(item){
			var healthobj = supervisorView.dataStore.getValue(item, "health");
			var hpsvars = [];
			for(var i in healthobj){
				hpsvars.push(healthobj[i]);
			}
			
			var gotMedia = function(mitems){
				info("setAgentHps gotMedia entry");
				if(mitems.length > 0){
					hpsvars.push(supervisorView.dataStore.getValue(mitems[0], "aggregate"));
				}
				debug(["setAgentHps wants averaged:", hpsvars]);
				var hp = supervisorView.averageHp(hpsvars);

				supervisorView.dataStore.setValue(item, "aggregate", hp);
				supervisorView.dataStore.save();
				var rawobj = {
					"id":supervisorView.dataStore.getValue(item, "id"),
					"display":supervisorView.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"agent",
					"health":{},
					"details":supervisorView.dataStore.getValue(item, "details")
				};
				dojo.publish("supervisorView/set/" + rawobj.id, [item, rawobj]);
				info("setAgentHps gotMedia exit");
			};
			
			supervisorView.dataStore.fetch({
				query:{
					"type":"media",
					"agent":supervisorView.dataStore.getValue(item, "display")
				},
				onComplete:function(got){
					gotMedia(got);
				}
			});
		};
		
		var setHps = function(items){
			info(["Fetch for setAgentHps done", items]);
			dojo.forEach(items, setHp);
			supervisorView.setHpCalcTimer(supervisorView.setAgentProfileHps, items.length);
			info("setAgentHps done(ish)");			
		};
		
		supervisorView.dataStore.fetch({
			query:{"type":"agent"},
			onComplete:setHps
		});
	};

	supervisorView.setQueueGroupHps = function(){
		info("setQueueGroupHps entry");
		var setHp = function(item){
			var hpsvars = [];
			var gotQueues = function(items){
				info("setQueueGroupHps gotQueues entry");
				var calls = 0;
				dojo.forEach(items, function(i){
					hpsvars.push(supervisorView.dataStore.getValue(i, "aggregate"));
					calls += supervisorView.dataStore.getValue(i, 'details').calls;
				});
				debug(["setQueueGroupHps wants averaged:  ", hpsvars]);
				var hp = supervisorView.averageHp(hpsvars);
				supervisorView.dataStore.setValue(item, "aggregate", hp);
				supervisorView.dataStore.setValue(item, 'details', {'totalCalls':calls});
				supervisorView.dataStore.save();
				var rawobj = {
					"id":"queuegroup-" + supervisorView.dataStore.getValue(item, "display"),
					"display":supervisorView.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"queuegroup"
				};
				dojo.publish("supervisorView/set/queuegroup-" + rawobj.display, [item, rawobj]);
				info("setQueueGroupHps gotQueues end");
			};
			
			supervisorView.dataStore.fetch({
				query:{
					"type":"queue",
					"group":supervisorView.dataStore.getValue(item, "display"),
					"node":supervisorView.node
				},
				onComplete:function(got){
					gotQueues(got);
				}
			});
		};

		var setHps = function(items){
			info(["setQueueGroupHps fetch done", items]);
			supervisorView.setHpCalcTimer(supervisorView.setGlobalQueueHp, 1);
			dojo.forEach(items, setHp);
			info("setQueueGroupHps done(ish)");
		};
		
		supervisorView.dataStore.fetch({
			query:{"type":"queuegroup"},
			onComplete:setHps
		});
	};

	supervisorView.setAgentProfileHps = function(){
		info("setAGentProfileHps entry");
		var setHp = function(item){
			info("setAGentProfileHps setHp");
			var hpsvars = [];
			var gotAgents = function(aitems){
				dojo.forEach(aitems, function(i){
					hpsvars.push(supervisorView.dataStore.getValue(i, "aggregate"));
				});
				debug(["setAGentProfileHps is averaging:", hpsvars]);
				var hp = supervisorView.averageHp(hpsvars);
				supervisorView.dataStore.setValue(item, "aggregate", hp);
				supervisorView.dataStore.setValue(item, 'details', {totalAgents: aitems.length});
				supervisorView.dataStore.save();
				var rawobj = {
					"id":"agentprofile-" + supervisorView.dataStore.getValue(item, "display"),
					"display":supervisorView.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"agentprofile",
					"health":{},
					"details":{totalAgents: aitems.length}
				};
				dojo.publish("supervisorView/set/agentprofile-" + rawobj.display, [item, rawobj]);
				info("setAGentProfileHps setHp exit");
			};
			
			supervisorView.dataStore.fetch({
				query:{
					"type":"agent",
					"profile":supervisorView.dataStore.getValue(item, "display"),
					"node":supervisorView.node
				},
				onComplete:function(got){
					gotAgents(got);
				}
			});
		};
		
		var setHps = function(items){
			info("setAgentProfileHps setHps entry");
			info(["setAgateProfileHps fetch done", items]);
			dojo.forEach(items, setHp);
			supervisorView.setHpCalcTimer(supervisorView.setGlobalAgentHp, items.length);
			info("setAgentProfileHps setHps exit");		
		};
		
		supervisorView.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHps
		});
	};

	supervisorView.setGlobalAgentHp = function(){
		info("setGlobalAgentHp entry");
		var setHp = function(items){
			info(["setGlobalAgentHp fetch done", items]);
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorView.dataStore.getValue(item, "aggregate"));
			});
			debug(["setGlobalAgentHp wants averaged:", hplist]);
			var hp = supervisorView.averageHp(hplist);
			supervisorView.agentBubble.setHp(hp);
			supervisorView.setHpCalcTimer(supervisorView.setNodeHps, 1);
			info("setGlobalAgenthp exit");
		};
		supervisorView.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHp
		});
	};

	supervisorView.setGlobalQueueHp = function(){
		info("setGlobalQueueHp entry");
		var setHp = function(items){
			debug(["setGlobalQueueHp fetch done", items]);
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorView.dataStore.getValue(item, "aggregate"));
			});
			debug(["setGlobalQueueHp wants averaged:", hplist]);
			var hp = supervisorView.averageHp(hplist);
			supervisorView.queueBubble.setHp(hp);
			supervisorView.setHpCalcTimer(supervisorView.setAgentHps, 1);
			info("setGlobalQueueHp exit");
		};
		supervisorView.dataStore.fetch({
			query:{"type":"queuegroup"},
			onComplete:setHp
		});
	};

	supervisorView.setNodeHps = function(){
		info("setNodeHps entry");
		var gotNodes = function(items){
			info(["setNodeHps fetch done", items]);
			dojo.forEach(items, function(item){
				var gotNodeItems = function(nitems){
					info("setNodeHps gotNodeItems entry");
					var hplist = [];
					dojo.forEach(items, function(nitem){
						hplist.push(supervisorView.dataStore.getValue(nitem, "aggregate"));
					});
					var hpobj = supervisorView.dataStore.getValue(item, "health");
					for(var i in hpobj){
						hplist.push(hpobj[i]);
					}
					debug(["setNodeHps wants averaged", hplist]);
					var hp = supervisorView.averageHp(hplist);
					supervisorView.dataStore.setValue(item, "aggregate", hp);
					var rawobj = {
						"id":supervisorView.dataStore.getValue(item, "id"),
						"display":supervisorView.dataStore.getValue(item, "display"),
						"aggregate":hp,
						"type":"node",
						"health":{},
						"details":supervisorView.dataStore.getValue(item, "details")
					};
					supervisorView.dataStore.save();
					dojo.publish("supervisorView/set/" + rawobj.id, [item, rawobj]);
					info("setNOdeHps gotNodeItems exit");
				};
				supervisorView.dataStore.save();
				supervisorView.dataStore.fetch({
					query:{node:supervisorView.dataStore.getValue(item, "display")},
					onComplete:gotNodeItems
				});
			});
			supervisorView.setHpCalcTimer(supervisorView.setSystemHps, items.length / 2);
			info("setNodeHps exit");			
		};
		
		supervisorView.dataStore.fetch({
			query:{type:"node"},
			onComplete:gotNodes
		});
	};

	supervisorView.setSystemHps = function(){
		info("setSystemHps entry");
		var gotNodes = function(items){
			info(["setSystemHps fetch done", items]);
			var hps = [];
			dojo.forEach(items, function(item){
				hps.push(supervisorView.dataStore.getValue(item, "aggregate"));
			});
			var hp = supervisorView.averageHp(hps);
			var rawobj = {
				"id":"system-System",
				"display":"System",
				"type":"system",
				"aggregate":hp,
				"health":{},
				"details":{}
			};
			var gotSystem = function(items){
				if(items.length < 1){
					warning(["Trying to get the system entry failed", items]);
				}
				supervisorView.dataStore.setValue(items[0], "aggreagate", hp);
				supervisorView.dataStore.save();
				supervisorView.setHpCalcTimer(supervisorView.setMediaHps, 1);
				dojo.publish("supervisorView/set/system-System", [items[0], rawobj]);
				info("setSystemHps exit");
			};
			supervisorView.dataStore.fetch({
				query:{"type":"system"},
				onComplete:gotSystem
			});
		};
		
		supervisorView.dataStore.fetch({
			query:{"type":"node"},
			onComplete:gotNodes
		});
	};
	
	/*supervisorView.setAllHps = function(){
		info(["setAllHps..."]);
		supervisorView.setMediaHps();
		supervisorView.setQueueHps();
		supervisorView.setAgentHps();
		supervisorView.setQueueGroupHps();
		supervisorView.setAgentProfileHps();
		supervisorView.setGlobalAgentHp();
		supervisorView.setGlobalQueueHp();
		supervisorView.setNodeHps();
		supervisorView.setSystemHps();
	};*/

	supervisorView.setDetails = function(fquery, filter){
		var fetchdone = function(item){
			var obj = supervisorView.dataStore.getValue(item, "details");
			var out = "";
			/*if(supervisorView.dataStore.getValue(item, "node")){
				out += "<tr><th class=\"label\">Node</th><td>" + supervisorView.dataStore.getValue(item, "node") + "</td></td>";
				//out += "<p class=\"smaller\"><label class=\"narrow\">Node:</label>" + supervisorView.dataStore.getValue(item, "node");
			}*/
			if(! filter){
				filter = [];
			}
			
			for(var i in obj){
				//out += "<p class=\"smaller\"><label class=\"narrow\">" + i + ":</label>";
				if(inArray(i, filter)){
					continue;
				}
				
				out += "<tr><th class=\"label\">" + i + "</th><td>";
				if(obj[i].timestamp){
					var date = new Date(obj[i].timestamp * 1000);
					out += date.toLocaleTimeString();
				}
				else{
					out += obj[i].toString();
				}
				
				//out += "</p>";
				out += "</td></tr>";
			}
			/*out += "<p>Health Report</p>";
			var hps = supervisorView.dataStore.getValue(item, "health");
			for(var i in hps){
				var sigdigited = Math.floor(supervisorView.averageHp([hps[i]]) * 100) / 100;
				out += "<p class=\"smaller\"><label class=\"narrow\">" + i + ":</label>" + sigdigited.toString() + "</p>";
			}*/
			dijit.byId("supervisorDetails").attr("content", "<table class=\"smaller\">" + out + "</table>");
			dijit.byId("supervisorDetails").attr("title", supervisorView.dataStore.getValue(item, "type") + ": " + supervisorView.dataStore.getValue(item, "display"));
		};
		
		supervisorView.dataStore.fetch({
				query:fquery,
				onItem:fetchdone
		});
	};

	supervisorView.drawSystemStack = function(){
	
		var fetchdone = function(items){
		
			while(supervisorView.systemStack.length){
				var popped = supervisorView.systemStack.pop();
				popped.clear();
			}
		
			var acc = [{
				data:{
					display:'System',
					id: 'system-System',
					type: 'system',
					health: 50
				},
				onmouseenter: function(){
					supervisorView.setDetails({
						display:'System'
					});
				},
				menu: [
					{'label':'Blab',
					'onClick':function(){
						supervisorView.showBlabDialog('node', 'System');
					}},
					{'label':'Set Motd...',
					'onClick':function(){
						supervisorView.showMotdDialog('system');
					}}
				]
			}];
			
			for(var i = 0; i < items.length; i++){
				var id = supervisorView.dataStore.getValue(items[i], 'id');
				var node = supervisorView.dataStore.getValue(items[i], 'display');
				acc.push({
					data:{
						'id': id,
						type:'node',
						display:supervisorView.dataStore.getValue(items[i], 'display'),
						health:50,
						aggregate:50
					},
					onmouseenter:function(){
						supervisorView.setDetails({
							display:this.data.display,
							health:this.data.health
						});
					},
					menu:[
						{'label':'Blab',
						'onClick':function(){
							supervisorView.showBlabDialog('node', node)
						}},
						{'label':'Set Motd...',
						'onClick':function(){
							supervisorView.showMotdDialog(node);
						}}
					]
				});
			}
			
			var yi = 375;
			dojo.forEach(acc, function(conf){
				var o = new supervisorView.Bubble({
					point: {x: 20, y:yi},
					data: conf.data,
					onmouseenter: conf.onmouseenter,
					onclick:function(ev){
						if(this.data.display == 'System'){
							supervisorView.node = '*';
						}
						else{
							supervisorView.node = this.data.display;
						}
						var dispRef = this.data.display;
						dojo.forEach(supervisorView.systemStack, function(obj){
							obj.size(1);
							if(obj.data.display == dispRef){
								obj.size(1.4);
							}
						});
					},
					subscriptions:[
						{channel: 'supervisorView/set/' + id,
						callback: function(rawobj){
							this.setHp(rawobj.aggregate);
						}},
						{channel: 'supervisorView/drop/' + id,
						callback: function(){
							this.clear();
						}}
					],												  
					menu: conf.menu
				});
				
				if(conf.data.display == 'System'){
					o.subscriptions.push(dojo.subscribe("supervisorView/set/system-System", function(storeref, rawobj){
						debug(["System set sub", storeref, rawobj]);
					}));
				}
				else{
					o.subscriptions.push(dojo.subscribe("supervisorView/set/node-" + conf.data.display, function(storeref, rawobj){
						debug(["node set sub", storeref, rawobj]);
					}));
					//dijit.byId("nodeAction").bindDomNode(o.rawNode);
				}
				yi = yi - 40;
				o.dragOver = function(){
					if(o.data.display == 'System'){
						supervisorView.node = '*';
					}
					else{
						supervisorView.node = o.data.display;
					}
					dojo.forEach(supervisorView.systemStack, function(obj){
						obj.size(1);
					});
					this.size(1.4);
					return false;
				};
				var coll = supervisorView.dndManager.registerCollider(o);
				dojo.connect(o, 'clear', function(){
					supervisorView.dndManager.unregisterCollider(coll);
				});
				supervisorView.systemStack.push(o);
				if(supervisorView.node == o.data.display){
					o.grow();
				}
				else if( (supervisorView.node == "*") && (o.data.display == "System") ){
					o.grow();
				}
			});
		};

		supervisorView.dataStore.fetch({
			query:{
				type:"node"
			},
			onComplete:fetchdone
		});

	};

	supervisorView.drawAgentQueueBubbles = function(agenthp, queuehp){
		var clearStacks = function(){
			var clearables = [
				"queueGroupsStack",
				"queuesStack",
				"agentProfilesStack",
				"agentsStack",
				"callsStack"
			];
			
			for(var i in clearables){
				if(supervisorView[clearables[i]].clear){
					supervisorView[clearables[i]].clear();
				}
			}
		};
		
		supervisorView.agentBubble = new supervisorView.Bubble({
			point:{x:20, y:20},
			scale: 0.75,
			data: {"health":agenthp, "display":"Agents", "id":"Agents"},
			menu: [
				{'label':'Blab...',
				'onClick':function(){
					supervisorView.showBlabDialog('all', 'all');
				}}
			],
			onmouseenter:function(){
				clearStacks();
				supervisorView.drawAgentProfilesStack();
				this.size(1);
				supervisorView.queueBubble.size(0.75);
			},
			dragOver: function(){
				clearStacks();
				supervisorView.drawAgentProfilesStack();
				supervisorView.agentBubble.size(1);
				supervisorView.queueBubble.size(0.75);
				return false;
			}
		});
		supervisorView.agentBubble.coll = supervisorView.dndManager.registerCollider(supervisorView.agentBubble);
		
		supervisorView.queueBubble = new supervisorView.Bubble({
			point:{x:20, y:60},
			scale: 0.75,
			data:{"health":queuehp, "display":"Queues", "id":"Queues"},
			onmouseenter: function(){
				clearStacks();
				supervisorView.drawQueueGroupsStack();
				this.size(1);
				supervisorView.agentBubble.size(0.75);
			},
			dragOver:  function(){
				clearStacks();
				supervisorView.drawQueueGroupsStack();
				supervisorView.queueBubble.size(1);
				supervisorView.agentBubble.size(0.75);
				return false;
			}
		});
		supervisorView.queueBubble.coll = supervisorView.dndManager.registerCollider(supervisorView.queueBubble);
	};

	supervisorView.drawQueueGroupsStack = function(){
		if(supervisorView.queueGroupsStack && supervisorView.queuesStack.scrollLocked){
			info(["queueGroupsstack exists but is scroll-locked"]);
			return false;
		}
		
		if(supervisorView.agentsStack && supervisorView.agentsStack.scrollLocked){
			info(["agent profiles stack exists but is scroll-locked"]);
			return false;
		}
		
		//supervisorView.queuesStack.clear();
		
		var fetchdone = function(items, request){
			//warning(["fetchdone"]);
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				var oldDisplay = supervisorView.dataStore.getValue(obj, 'display');
				var trueDisplay = oldDisplay;
				if(supervisorView.dataStore.getValue(obj, 'details')){
					trueDisplay = '(' + supervisorView.dataStore.getValue(obj, 'details').totalCalls + ') ' + oldDisplay;
				}
				acc.push({
					data:{
						display:trueDisplay,
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50),
						id:supervisorView.dataStore.getValue(obj, "id")
					},
					onmouseenter:function(ev){
						supervisorView.drawQueuesStack(oldDisplay, '*', acc.length);
						supervisorView.setDetails({
							type:supervisorView.dataStore.getValue(obj, "type"),
							display:supervisorView.dataStore.getValue(obj, "display")
						});
					},
					dragOver:function(){
						supervisorView.drawQueuesStack(oldDisplay, supervisorView.node, acc.length);
						supervisorView.setDetails({
							type:supervisorView.dataStore.getValue(obj, 'type'),
							display:supervisorView.dataStore.getValue(obj, 'display')
						});
						return false;
					}
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});
			
			supervisorView.callsStack.clear();

			supervisorView.queueGroupsStack = new supervisorView.BubbleStack({
				mousept:{
					x:250,
					y:20
				},
				bubbleConfs:acc,
				registerCollider:true
			});
			
			supervisorView.queueGroupsStack.scroll(0);
			supervisorView.queueGroupsStack.group.moveToBack();
		};
		
		supervisorView.dataStore.fetch({
			query:{
				type:"queuegroup"
			},
			onComplete:fetchdone
		});
	};

	supervisorView.drawQueuesStack = function(group, node, scrollIndex){
		if(supervisorView.queuesStack.scrollLocked){
			info(["queues stack scroll locked"]);
			return false;
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate", 50));
				var dispText = '(' + supervisorView.dataStore.getValue(obj, "details").calls + ') ' + supervisorView.dataStore.getValue(obj, 'display');
				acc.push({
					data:{
						display: dispText,
						health: supervisorView.dataStore.getValue(obj, "aggregate", 50),
						id: supervisorView.dataStore.getValue(obj, "id")
					},
					onmouseenter:function(ev){
						var queryObj = {
							'node':supervisorView.node,
							'queue': supervisorView.dataStore.getValue(obj, 'display')
						};
						supervisorView.drawCallStack(queryObj, acc.length);
						supervisorView.setDetails({
							type:"queue",
							display:supervisorView.dataStore.getValue(obj, "display")
						}, ['group']);
					},
					dragOver: function(testobj){
						debug(["queue bubble dragOver", testobj]);
						if(testobj.data.type == 'media'){
							return true;
						}
						
						return false;
					},
					dropped: function(droppedObj){
						debug(["queue bubble got a drop", droppedObj]);
						supervisorView.queueTransfer(droppedObj.data, supervisorView.dataStore.getValue(obj, 'display'));
					}
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});
			
			supervisorView.callsStack.clear();
			supervisorView.queuesStack.clear();
			
			supervisorView.queuesStack = new supervisorView.BubbleStack({
				mousept:{
					x:540,
					y:20
				},
				bubbleConfs: acc,
				registerCollider: true
			});
			
			supervisorView.queuesStack.scroll(scrollIndex);
			supervisorView.queuesStack.group.moveToBack();
		};
		
		var queryo = {
			type:"queue",
			'group':group,
			node:node
		};

		var sortKey = [
			{attribute: "display"}
		];
		
		supervisorView.dataStore.fetch({
			sort: sortKey,
			query:queryo,
			onComplete:fetchdone
		});
	
		return true;
	};
	
	supervisorView.drawAgentProfilesStack = function(){
		if(supervisorView.agentProfilesStack && supervisorView.agentProfilesStack.scrollLocked){
			info(["agentProfilesStack exists but is scroll-locked"]);
			return false;
		}
						
		var fetchdone = function(items, request){
			//warning(["fetchdone", items]);
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				var itemDetails = supervisorView.dataStore.getValue(obj, 'details');
				if(itemDetails && itemDetails.totalAgents !== undefined){
					if(itemDetails.totalAgents == 0 && supervisorView.showEmptyProfiles === false){
						return false;
					}
				}
				var disp = supervisorView.dataStore.getValue(obj, "display")
				acc.push({
					data:{
						display:disp,
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50),
						id:supervisorView.dataStore.getValue(obj, "id")
					},
					onmouseenter:function(ev){
						supervisorView.drawAgentsStack(this.data.display, supervisorView.node, acc.length);
						supervisorView.setDetails({
							type:supervisorView.dataStore.getValue(obj, "type"),
							display:supervisorView.dataStore.getValue(obj, "display")
						});
					},
					dragOver: function(testobj){
						 supervisorView.drawAgentsStack(this.data.display, supervisorView.node, acc.length);
						 supervisorView.setDetails({
							type:supervisorView.dataStore.getValue(obj, "type"),
							display:supervisorView.dataStore.getValue(obj, "display")
						});
						debug(["agentProfileBubble dragOver", testobj]);
						if(testobj.data.type == 'agent'){
							return true;
						}
						
						return false;
					},
					dropped: function(droppedObj){
						debug(["agentBubbledropped", droppedObj]);
						supervisorView.setProfile(this.data.display, droppedObj.data.display);
					},
					menu:[
						{'label':'Blab...',
						'onClick':function(){
							supervisorView.showBlabDialog('profile', escape(disp));
						}}
					]
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});

			supervisorView.callsStack.clear();
			supervisorView.queueGroupsStack.clear();
			supervisorView.queuesStack.clear();
			supervisorView.agentProfilesStack = new supervisorView.BubbleStack({
				mousept:{
					x:250,
					y:20
				},
				bubbleConfs:acc,
				registerCollider: true
			});
	
			supervisorView.agentProfilesStack.scroll(0);
			supervisorView.agentProfilesStack.group.moveToBack();
		};

		var sortKey = [
			{attribute: "display"}
		];


		supervisorView.dataStore.fetch({
			sort: sortKey,
			query:{
				type:"agentprofile"
			},
			onComplete:fetchdone
		});

		return true;
	};
	
	supervisorView.drawAgentsStack = function(profile, node, scrollIndex){
		if(supervisorView.agentsStack.scrollLocked){
			info(["agents stack scroll locked"]);
			return false;
		}

		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate", 50));
				var details = supervisorView.dataStore.getValue(obj, 'details', {'state':'released'});
				var imageUrl = '/images/' + details.state + '.png';
				var agentHit = supervisorView.dataStore.getValue(obj, 'display');
				acc.push({
					data:{
						display:supervisorView.dataStore.getValue(obj, "display"),
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50),
						id:supervisorView.dataStore.getValue(obj, 'id'),
						type:'agent'
					},
					onmouseenter:function(ev){
						var queryObj = {
							'agent':this.data.display
						};
						supervisorView.drawCallStack(queryObj, acc.length);
						supervisorView.setDetails({
							type:"agent",
							display:supervisorView.dataStore.getValue(obj, "display")
						}, ['login', 'profile']);
					},
					onmouseleave:function(ev){
						supervisorView.showEmptyProfiles = false;
					},
					dragOver: function(testObj){
						debug(["agentBubble dragOver", testObj]);
						if(testObj.data.type == 'media'){
							return true;
						}
						
						return false;
					},
					dropped: function(droppedObj){
						debug(["agentBubble accepted drop", droppedObj]);
						supervisorView.sendMediaToAgent(droppedObj.data, this.data.display);
					},
					startDrag: function(){
						supervisorView.agentProfilesStack.clear();
						supervisorView.showEmptyProfiles = true;
						supervisorView.drawAgentProfilesStack();
					},
					endDrag: function(){
						supervisorView.agentProfilesStack.clear();
						supervisorView.showEmptyProfiles = false;
						supervisorView.drawAgentProfilesStack();
					},
					moveable: true,
					image: imageUrl,
					menu: [
						{'label':'Released',
						'onClick':function(){
						geturl = "/supervisor/agentstate/" + escape(supervisorView.dataStore.getValue(obj, 'display')) + "/released/default";
						dojo.xhrGet({
							url:geturl,
							handleAs: "json",
							load: function(resp){
								if(resp.success){
									return true;
								}
								errMessage(["setting state to released", resp.message]);
							}
						})}},
						{'label':'Idle',
						'onClick':function(){
							var geturl = "/supervisor/agentstate/" + escape(agentHit) + "/idle";
							dojo.xhrGet({
								url:geturl,
								handleAs: "json",
								load: function(resp){
									if(resp.success){
										return true;
									}
									errMessage(["setting agent to idle", resp.message]);
								}
							})
						}},
						'separator',
						{'label':'Blab...',
						'onClick':function(){
							supervisorView.showBlabDialog('agent', escape(agentHit));
						}},
						{'label':'Spy',
						'onClick':function(){
							supervisorView.spy(agentHit);
						}},
						{'label':'Set Profile',
						'onClick':function(){
							var dialog = dijit.byId("profileSwapDialog");
							var submitSetProf = function(){
								var data = dialog.attr('value');
								supervisorView.setProfile(data.profile, escape(agentHit));
							}
							dialog.attr('execute', submitSetProf);
							dojo.xhrGet({
								url:"/supervisor/get_profiles",
								handleAs:"json",
								load:function(r){
									if(r.success){
										var span = dojo.byId('profileSwapDialogParent');
										while(span.hasChildNodes()){
											span.removeChild(span.firstChild);
										}
										var html = '<select name="profile">';
										dojo.forEach(r.profiles, function(item){
											html += '<option>' + item + '</option>';
										});
										html += '</select>';
										span.innerHTML = html;
										var modenode = span.firstChild;
										new dijit.form.ComboBox({
											name:'profile'
										}, modenode);
										dialog.show();
									} else {
										warning(["get_profiles failure", r.message]);
									}
								},
								error:function(r){
									warning(["get_profiles errored", r])
								}
							});
						}},
						'separator',
						{'label':'Kick',
						'onClick':function(){
							var geturl = "/supervisor/kick_agent/" + escape(agentHit);
							dojo.xhrGet({
								url:geturl,
								handleAs: "json",
								load:function(resp){
									if(resp.success){
										return true;
									}
									errMessage(["error kicking agent", resp.message]);
								}
							});
						}}
					]
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});

			supervisorView.callsStack.clear();
			supervisorView.agentsStack.clear();
			
			supervisorView.agentsStack = new supervisorView.BubbleStack({
				mousept:{
					x:540,
					y:20
				},
				bubbleConfs: acc,
				registerCollider:true,
			});
			supervisorView.agentsStack.group.moveToBack();
						
			supervisorView.agentsStack.scroll(scrollIndex);
		};

		var sortKey = [
			{attribute: "display"}
		];


		var queryo = {
			type:"agent",
			'profile':profile,
			'node':node
		};

		supervisorView.dataStore.fetch({
			sort: sortKey,
			query:queryo,
			onComplete:fetchdone
		});

		return true;
	};

	supervisorView.drawCallStack = function(queryObj, scrollIndex){
		if(supervisorView.callsStack.scrollLocked){
			return false;
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			var sortfunc = function(a, b){
				var aDetails = supervisorView.dataStore.getValue(a, 'details');
				var bDetails = supervisorView.dataStore.getValue(b, 'details');
				if(aDetails.priority == bDetails.priority){
					return aDetails.queued_at.timestamp - bDetails.queued_at.timestamp;
				}
				
				return aDetails.priority - bDetails.priority;
			};
			items.sort(sortfunc);
			dojo.forEach(items, function(obj){
				var imageUrl = '/images/';
				var menuConf = false;
				var mediaType = supervisorView.dataStore.getValue(obj, 'details').type;
				var datas = {
					display: supervisorView.dataStore.getValue(obj, "details").client,
					health:supervisorView.dataStore.getValue(obj, "aggregate", 50),
					id:supervisorView.dataStore.getValue(obj, "id"),
					type:"media"
				};
				var rawid = datas.id.substr(6);
				if(supervisorView.dataStore.getValue(obj, "agent")){
					datas.agent = supervisorView.dataStore.getValue(obj, "agent");
				}
				else{
					datas.queue = supervisorView.dataStore.getValue(obj, "queue");
				}
				
				switch(mediaType){
					case 'email':
						if(datas.queue){
							menuConf = [
								{'label':'Peek',
								'onClick':function(){
									supervisorView.mediaPeek(obj);
									supervisorView.callsStack.unlockScroll();
								}},
								{'label':'Remove',
								'onClick':function(){
									supervisorView.removeFromQueue(obj);
									supervisorView.callsStack.unlockScroll();

								}},
								{'label':'Send to Agent...',
								'onClick':function(){
									supervisorView.sendToAgentDialog(obj);
									supervisorView.callsStack.unlockScroll();
								}}
							];
						} else {
							menuConf = [
								{'label':'Spy',
								'onClick':function(){
									supervisorView.spy(datas.agent);
									supervisorView.callsStack.unlockScroll();
								}}
							];
						}
						imageUrl += mediaType + '.png';
						break;
					case 'dummy':
						if(datas.queue){
							menuConf = [
								{'label':'Send to Agent...',
								'onClick':function(){
									supervisorView.sendToAgentDialog(obj);
									supervisorView.callsStack.unlockScroll();
								}}
							];
						} else {
							menuConf = [
								{'label':'Spy',
								'onClick':function(){
									supervisorView.spy(datas.agent);
									supervisorView.callsStack.unlockScroll();
								}}
							];
						}
						imageUrl += mediaType + '.png';
						break;
					case 'voice':
						if(datas.queue){
							menuConf = [
								{'label':'Send to Agent...',
								'onClick':function(){
									supervisorView.sendToAgentDialog(obj);
									supervisorView.callsStack.unlockScroll();

								}},
								{'label':'Send to Voicemail...',
								'onClick':function(){
									supervisorView.sendToVoicemail(obj);
									supervisorView.callsStack.unlockScroll();
								}}
							];
						} else {
							menuConf = [
								{'label':'Spy',
								'onClick':function(){
									supervisorView.spy(datas.agent);
									supervisorView.callsStack.unlockScroll();
								}}
							]
						}
						imageUrl += mediaType + '.png';
						break;
					case 'voicemail':
						if(datas.queue){
							menuConf = [
								{'label':'Send to Agent...',
								'onClick':function(){
									supervisorView.sendToAgentDialog(obj);
									supervisorView.callsStack.unlockScroll();
								}}
							];
						} else {
							menuConf = [
								{'label':'Spy',
								'onClick':function(){
									supervisorView.spy(datas.agent);
									supervisorView.callsStack.unlockScroll();
								}}
							]
						}
						imageUrl += mediaType + '.png';
						break;
					default:
						menuConf = [
							{'label':'Send to Agent...',
							'onClick':function(){
								supervisorView.sendToAgentDialog(obj);
								supervisorView.callsStack.unlockScroll();
							}}
						];
						imageUrl += 'undefined.png';
				}
									 
				acc.push({
					data:datas,
					moveable:true,
					image: imageUrl,
					menu: menuConf,
					onmouseenter:function(ev){
						supervisorView.setDetails({
							type:"media",
							display:supervisorView.dataStore.getValue(obj, "display")
						}, ['queue', 'agent', 'ring_path', 'media_path']);
					}
				});
			});
			
			supervisorView.callsStack.clear();
			var confObj = {
				mousept:{
					x:580 + 240,
					y:20
				},
				bubbleConfs:acc
			};
			if(queryObj.queue){
				confObj = {
					mousept:{
						x:580 + 240,
						y:20
					},
					bubbleConfs:acc
				};
			}
			
			supervisorView.callsStack = new supervisorView.BubbleStack(confObj);
		
			supervisorView.callsStack.forEachBubble(function(obj){
				obj.group.connect("onmousedown", obj, function(ev){
					supervisorView.dndManager.startDrag(obj);
				});
				obj.group.connect("onmouseup", obj, function(ev){
					supervisorView.dndManager.endDrag();
				});
				
				var nom = obj.data.display;
				obj.subscriptions.push(dojo.subscribe("supervisorView/set/media-" + nom, function(storeref, rawobj){
					debug(["media sub hit", obj, rawobj]);
					obj.setHp(rawobj.aggregate);
				}));
				obj.subscriptions.push(dojo.subscribe("supervisorView/drop/media-" + nom, function(storeref, rawobj){
					obj.clear();
				}));
			});
		
			supervisorView.callsStack.scroll(scrollIndex);
		};

		/*var sortKey = [
			{attribute: "display"}
		];*/

		supervisorView.dataStore.fetch({
			query:queryObj,
			onComplete:fetchdone
		});
	
		return true;
	};
	
	supervisorView.healthDump = function(){
		var dump = function(items){
			dojo.forEach(items, function(item){
				var out = supervisorView.dataStore.getValue(item, "aggregate");
				var nom = supervisorView.dataStore.getValue(item, "display");
				debug(["healthDump", nom, out]);
			});
		};
		
		supervisorView.dataStore.fetch({
			onComplete:dump
		});
	};

	supervisorView.reloadDataStore = function(){
		dojo.xhrGet({
			url:"/supervisor/status",
			handleAs:"json",
			error:function(res){
				errMessage(["reloadDataStore errored", res]);
				supervisorView.poller.stop();
			},
			load:function(data){
				if(data.data){
					info(["1400", "store reload ajax completed"]);
					supervisorView.healthData = data.data;
					supervisorView.dataStore = new dojo.data.ItemFileWriteStore({
						data: supervisorView.healthData,
						hierarchical: false,
						typeMap:{
							"details":{
								"type":Object,
								"deserialize":function(obj){return obj;},
								"serialize":function(obj){return obj;}
							}
						}
					});
					supervisorView.drawSystemStack();
					//supervisorView.setAllHps();
				}
				else{
					debug(["1422", "stub for no data.data", data]);
				}
			}
		});
	};

	supervisorView.spy = function(agent){
		dojo.xhrGet({
			url:'/supervisor/spy/' + agent,
			handleAs:'json',
			load:function(res){
				if(res.success){
					// cool
				} else {
					errMessage(['Counldn\'t spy', res.message]);
				}
			},
			error:function(res){
				errMessage(['error spying', res]);
			}
		});
	}
	
	supervisorView.showBlabDialog = function(type, target){
		var dialog = dijit.byId("blabDialog");
		dialog.attr('title', 'Blab');
		dialog.attr('value', {'message':'Type your message here.  Url\'s get automatically interpreted'});
		var submitblab = function(){
			var data = dialog.attr('value');
			supervisorView.blab(data.message, type, target);
		};
		dialog.attr('execute', submitblab);
		dialog.show();
	}
	
	supervisorView.blab = function(message, type, target){
		dojo.xhrPost({
			handleAs:"json",
			url:"/supervisor/blab",
			content:{
				message:replaceUrls(message),
				type: type,
				value: target
			},
			load:function(res){
				debug(["blab worked", res]);
			},
			error:function(res){
				errMessage(["blab failed", res]);
			}
		});
	};
	
	supervisorView.setProfile = function(profile, agent){
		dojo.xhrGet({
			handleAs:"json",
			url:"/supervisor/set_profile/" + agent + "/" + profile,
			load:function(res){
				if(res.success){
					//kewl
					return true;
				}
				else{
					errMessage(["set profile failed", res.message]);
				}
			},
			error:function(res){
				errMessage(["set profile errored", res]);
			}
		});
	};
	
	supervisorView.queueTransfer = function(callobj, newqueue){
		errMessage("Queue transfer not implemented");
		/*var fetchdone = function(items){
			if(items.length === 0){
				return false;
			}
			
			var item = items[0];
			var healthData = supervisorView.dataStore.getValue(item, 'health');
			var url = '/supervisor/';
			var callid = supervisorView.dataStore.getValue(item, 'id').substring(6);

			if(healthData.inqueue){
				var oldqueue = supervisorView.dataStore.getValue(item, 'queue');
				url += 'queue_transfer/' + escape(callid) + '/' + escape(oldqueue) + '/' + escape(newqueue);
			}
			else if(healthData.agent_link){
				var agent = supervisorView.dataStore.getValue(item, 'agent');
				url += 'requeue/' + escape(agent) + '/' + escape(newqueue);
			}
			else{
				warning(["orphaned call?", item]);
				return false;
			}

			dojo.xhrGet({
				'url':url,
				handleAs:'json',
				laod:function(res){
					if(! res.success){
						errMessage(["queueTransfer failed", res.message]);
					}
				},
				error:function(res){
					errMessage(["queueTransfer errored", res]);
				}
			});
		};
		
		supervisorView.dataStore.fetch({
			query:{
				'id':callobj.id,
				'type':'media'
			},
			onComplete:fetchdone
		});*/
	};
	
	supervisorView.sendMediaToAgent = function(media, agent){
		if(media.queue){
			var queue = media.queue;
			var id = media.id.substring(6);
			return dojo.xhrGet({
				handleAs:"json",
				url:"/supervisor/agent_ring/" + escape(queue) + "/" + escape(id) + "/" + escape(agent),
				load:function(res){
					if(res.success){
						//kewl
						return true;
					}
					else{
						errMessage(["agent ring failed", res.message]);
					}
				},
				error:function(res){
					errMessage(["agent ring errored", res]);
				}
			});
		}
		
		if(media.agent){
			var fromAgent = media.agent;
			//var id = media.id.substring(6);
			return dojo.xhrGet({
				handleAs:'json',
				url:'/supervisor/agent_transfer/' + escape(fromAgent) + '/' + escape(agent),
				load:function(res){
					if(res.success){
						return true;
					} else{
						errMessage(["agent transfer failed", res.message]);
					}
				},
				error:function(res){
					errMessage(["agent transfer errored", res]);
				}
			});
		}
	};
	
	supervisorView.saveItem = function(item, protoitem){
		for(var i in protoitem){
			supervisorView.dataStore.setValue(item, i, protoitem[i]);
		}
		
		supervisorView.dataStore.save();
	};
	
	supervisorView.setAgent = function(agentData){
		var fetched = function(items){
			var item = [];
			if(items.length === 0){
				item = supervisorView.dataStore.newItem({
					id:agentData.id
				});
			}
			else{
				item = items[0];
			}
			
			var profile	= agentData.details.profile;
			var node = agentData.details.node;
			var display = agentData.details.login;
			
			var protoitem = {
				'display': display,
				'node': node,
				'profile': profile,
				'details': agentData.details,
				'health': agentData.health,
				'type': 'agent'
			};
			
			supervisorView.saveItem(item, protoitem);
			supervisorView.setAgentProfile(profile);
		};
		
		supervisorView.dataStore.fetch({
			query:{'id':agentData.id},
			onComplete:fetched
		});
	};
	
	supervisorView.setAgentProfile = function(profile){
		var fetched = function(items){
			if(items.length === 0){
				var item = supervisorView.dataStore.newItem({
					'id':'agentprofile-' + profile,
					'type':'agentprofile',
					'display':profile
				});
				supervisorView.dataStore.save();
			}
		};
		
		supervisorView.dataStore.fetch({
			query:{'id':'agentprofile-' + profile},
			onComplete:fetched
		});
	};
	
	supervisorView.setQueue = function(queueData){
		var fetched = function(items){
			var item = [];
			if(items.length === 0){
				item = supervisorView.dataStore.newItem({
					'id':queueData.id
				});
			}
			else{
				item = items[0];
			}
			
			var protoitem = {
				'display':queueData.display,
				'node':queueData.details.node,
				'group':queueData.details.group,
				'health':queueData.health,
				'details':queueData.details,
				'type':'queue'
			};
			
			supervisorView.saveItem(item, protoitem);
			supervisorView.setQueueGroup(queueData.details.group);
		};
		
		supervisorView.dataStore.fetch({
			query:{'id':queueData.id},
			onComplete:fetched
		});
	};
	
	supervisorView.setQueueGroup = function(group){
		var fetched = function(items){
			if(items.length === 0){
				var item = supervisorView.dataStore.newItem({
					'id':'queuegroup-' + group,
					'display':group,
					'type':'queuegroup'
				});
				supervisorView.dataStore.save();
			}
		};
		
		supervisorView.dataStore.fetch({
			query:{'id':'queuegroup-' + group},
			onComplete:fetched
		});
	};
	
	supervisorView.setMedia = function(mediaData){
		var item = [];
		var fetched = function(items){
			if(items.length === 0){
				item = supervisorView.dataStore.newItem({
					'id':mediaData.id
				});
			}
			else{
				item = items[0];
			}
			
			supervisorView.dataStore.unsetAttribute(item, 'agent');
			supervisorView.dataStore.unsetAttribute(item, 'queue');

			var protoitem = {
				'display':mediaData.display,
				'details':mediaData.details,
				'health':mediaData.health,
				'type':'media',
				'node':mediaData.details.node
			};
			
			if(mediaData.details.queue){
				protoitem.queue = mediaData.details.queue;
			}
			else if(mediaData.details.agent){
				protoitem.agent = mediaData.details.agent;
			}
			
			supervisorView.saveItem(item, protoitem);
		};
		
		supervisorView.dataStore.fetch({
			query:{'id':mediaData.id},
			onComplete:fetched
		});
	};
	
	supervisorView.setNode = function(nodeData){
		var fetched = function(items){
			var item = [];
			if(items.length === 0){
				item = supervisorView.dataStore.newItem({
					'id':nodeData.id
				});
			}
			else{
				item = items[0];
			}
			
			var protoitem = {
				'display':nodeData.display,
				'details':nodeData.details,
				'health':nodeData.health,
				'type':'node'
			};
			
			supervisorView.saveItem(item, protoitem);
		};
	};
	
	supervisorView.sendToVoicemail = function(mediaObj){
		var mediaId = supervisorView.dataStore.getValue(mediaObj, 'id');
		var erlid = mediaId.split('-');
		erlid.shift();
		erlid = erlid.join('-');
		//mediaId.shift;
		//mediaId = mediaId.join('-');
		var fetched = function(items){
			if(items.length === 0){
				return false;
			}
			
			var item = items[0];
			if(supervisorView.dataStore.getValue(item, 'queue', false)){
				var queue = supervisorView.dataStore.getValue(item, 'queue');
				dojo.xhrPost({
					url:'/supervisor/voicemail/' + escape(queue) + '/' + escape(erlid),
					handleAs:'json',
					load:function(res){
						if(res.success){
							return true;
						}
						else{
							errMessage(["sending to voicemail failed", res.message]);
						}
					},
					error:function(res){
						errMessage(["sending to voicemail errored", res]);
					}
				});
			}
		};
		
		supervisorView.dataStore.fetch({
			query:{
				'id':mediaId,
				'type':'media'
			},
			onComplete:fetched
		});
	};
	
	supervisorView.sendToAgentDialog = function(mediaObj){
		console.log(["mediaObj", mediaObj]);
		dojo.xhrGet({
			url:'/get_avail_agents',
			handleAs:'json',
			load: function(res){
				if(res.success){
					console.log(res.agents);
					var selectContent = '';
					for(var i = 0; i < res.agents.length; i++){
						selectContent += '<option value="' + res.agents[i].name + '">' + res.agents[i].name + ' (' + res.agents[i].profile + ')</option>';
					}
					var content = '<p><label>Agent:</label><select name="agent" id="supSelectAgent">' + selectContent + '</select></p><p><label>&nbsp;</label><input type="submit" dojoType="dijit.form.Button" label="Submit" /></p>';
					var dialog = new dijit.Dialog({
						title:'Select Agent',
						content: content
					});
					dialog.attr('execute', function(){
						var agentName = dojo.byId('supSelectAgent').value;
						dialog.destroy();
						console.log([agentName, arguments]);
						var simpleObj = {
							id: supervisorView.dataStore.getValue(mediaObj, 'id')
						};
						if(supervisorView.dataStore.getValue(mediaObj, 'queue')){
							simpleObj.queue = supervisorView.dataStore.getValue(mediaObj, 'queue');
						} else {
							simpleObj.agent = supervisorView.dataStore.getValue(mediaObj, 'agent');
						}
						supervisorView.sendMediaToAgent(simpleObj, agentName);
					});
					dialog.show();
					return true;
				}
				errMessage(['getting available agents failed', res.message]);
			},
			error: function(res){
				errMessage(['getting available agents errored', res]);
			}
		});
	};
	
	supervisorView.mediaPeek = function(mediaObj){
		var queue = supervisorView.dataStore.getValue(mediaObj, 'details').queue;
		if(queue){
			queue = escape(queue);
		} else {
			return false;
		}
		
		var id = supervisorView.dataStore.getValue(mediaObj, 'id').substring(6);
		id = escape(id);
		dojo.xhrGet({
			url: '/supervisor/peek/' + queue + '/' + id,
			handleAs: 'json',
			load: function(res){
				if(res.success){
					return true;
				}
				
				errMessage(["peeking at media failed", res.message]);
			},
			error: function(res){
				errMessage(["peeking at media failed", res]);
			}
		});
	}
	
	supervisorView.removeFromQueue = function(mediaObj){
		var queue = supervisorView.dataStore.getValue(mediaObj, 'queue');
		if(! queue){
			return false;
		}
		
		queue = escape(queue);
		var id = supervisorView.dataStore.getValue(mediaObj, 'id').substring(6);
		dojo.xhrGet({
			url:'/supervisor/drop_call/' + queue + '/' + id,
			handleAs: 'json',
			load: function(res){
				if(res.success){
					return true;
				}
				
				errMessage(["drop call failed", res.message]);
			},
			error: function(res){
				errMessage(["drop call errored", res]);
			}
		});
	}
	
	supervisorView.showMotdDialog = function(nodename){
		dojo.xhrGet({
			url:'/supervisor/getmotd',
			handleAs:'json',
			load:function(res){
				if(! res.success){
					errMessage(["Failed getting motd", res.message]);
					return false;
				}
				
				var dialog = dijit.byId("blabDialog");
				dialog.attr('title', 'MotD');
				if(res.motd){
					dialog.attr('value', {'message':res.motd});
				} else {
					dialog.attr('value', {'message':'Type the Message of the Day here.  Leave blank to unset.'});
				}
				var submitblab = function(){
					var data = dialog.attr('value').message;
					dojo.xhrPost({
						url:'/supervisor/motd',
						handleAs:'json',
						content:{
							message:data,
							node:nodename
						},
						load:function(res){
							if(res.success){
								return true;
							}
							errMessage(["setting motd failed", res.message]);
						},
						error:function(res){
							errMessage(["setting motd errored", res]);
						}
					});
				}
				dialog.attr('execute', submitblab);
				dialog.show();
			},
			error: function(res){
				errMessage(["Errored getting motd", res]);
			}
		});
		
		
	
		
		}
	
	supervisorView.clean = function(){
		/* finds 'g' elements with no children and removes them from the dom */
		var count = 0;
		dojo.query('g').forEach(function(elem){
			if(! elem.hasChildNodes()){
				var parent = elem.parentNode;
				parent.removeChild(elem);
				count++;
			}
		});
		console.log("cleaned " + count + " nodes");
		return count;
	};
}

supervisorView.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
supervisorView.surface.rawNode.oncontextmenu = function(ev) {
	if (dojo.isIE) {
		ev.returnValue = false;
	}else {
		ev.preventDefault();
		ev.stopPropagation();
	}
};
dojo.connect(supervisorView.surface, "ondragstart",   dojo, "stopEvent");
dojo.connect(supervisorView.surface, "onselectstart", dojo, "stopEvent");

supervisorView.reloadDataStore();

supervisorView.drawAgentQueueBubbles(0, 0);
//supervisorView.drawSystemStack();

supervisorView.masterSub = dojo.subscribe("agent/supervisortab", function(supevent){
	debug(["for supervisor view master sub", supevent]);
	if(supevent.data.action == 'drop') {
		supervisorView.dataStore.fetch({
			query:{'id':supevent.data.id},
			onComplete:function(items){
				if(items.length === 0){
					return false;
				}
				supervisorView.dataStore.deleteItem(items[0]);
				supervisorView.dataStore.save();
				dojo.publish("supervisorView/drop/" + supevent.data.id, [supevent.data]);
			}
		});
		return true;
	}
	
	if(supevent.data.action != 'set'){
		return false;
	}
	
	switch(supevent.data.type){
		case 'agent':
			supervisorView.setAgent(supevent.data);
			break;
		
		case 'queue':
			supervisorView.setQueue(supevent.data);
			break;
		
		case 'media':
			supervisorView.setMedia(supevent.data);
			break;
		
		case 'node':
			supervisorView.setNode(supevent.data);
			break;
			
		default:
			return false;
	}
});

if(supervisorView.hpCalcInterval !== false){
	supervisorView.setMediaHps();
}

supervisorView.reloadTimer = false;
supervisorView.reload = function(){
	supervisorView.reloadTimer = setTimeout(function(){
		supervisorView.reloadDataStore();
		supervisorView.reload();
	}, 300000);
};

supervisorView.reload();

window.supervisorViewKillListen = dojo.subscribe("tabPanel-removeChild", function(child){
	if(child.title == "Supervisor"){
		dropTab('supervisorTab');
		clearTimeout(supervisorView.aggregateTimer);
		clearTimeout(supervisorView.reloadTimer);
		dojo.unsubscribe(window.supervisorViewKillListen);
		dojo.unsubscribe(supervisorView.masterSub);
		delete window.supervisorViewKillListen;
		dojo.xhrGet({
			url:"/supervisor/endmonitor",
			handleAs:'json',
			load:function(res){
				if(! res.success){
					debug(["endmonitor failed", res.message]);
				}
			},
			error:function(res){
				//errMessage(["endmonitor errored", res]);
			}
		});
	}
});

//supervisorView.hpcalc();

storeTab('supervisorTab');

