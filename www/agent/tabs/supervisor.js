
if(typeof(supervisorView) == "undefined"){

	supervisorView = function(){
		{};
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
				"deserialize":function(obj){return obj}
			}
		}
	});

	supervisorView.systemStack = [];

	supervisorView.node = "*";
	supervisorView.queueGroupsStack = {clear:function(){ return true}};
	supervisorView.queuesStack = {clear:function(){ return true}};
	supervisorView.agentsStack = {clear:function(){ return true}};
	supervisorView.agentProfilesStack = {clear:function(){ return true}};
	supervisorView.callsStack = {clear:function(){return true}};
	supervisorView.suppressPoll = false;

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
				var collided = supervisorView.dndManager.checkCollision(point)
				if(collided && collided.dragOver){
					var setDrop = false;
					if(collided.dragOver(obj)){
						supervisorView.dndManager._dropCandidate = collided;
						setDrop = true;
					}
					else{
						debug(["collided.dragOver false", collided, obj]),
						supervisorView.dndManager._dropCandidate = false;
						setDrop = false;
					}
					
					if(obj.setDroppable){
						obj.setDroppable(setDrop);
					}
				}
				else{
					debug(["one of them's false", collided, collided.dragOver]),
					supervisorView.dndManager._dropCandidate = null;
				}
			});
			supervisorView.suppressPoll = true;
			supervisorView.dndManager._dragging = obj;
		},
		endDrag:function(){
			debug("ending drag");
			supervisorView.suppressPoll = false;
			supervisorView.surface.disconnect(supervisorView.dndManager._eventHandle);
			if(supervisorView.dndManager._dropCandidate){
				supervisorView.dndManager._dropCandidate.dropped(supervisorView.dndManager._dragging);
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

	/*supervisorView.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
	dojo.connect(supervisorView.surface, "ondragstart",   dojo, "stopEvent");
	dojo.connect(supervisorView.surface, "onselectstart", dojo, "stopEvent");*/

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
			data:{"display":"bubble", "health":50, "type":"text"},
			image: false,
			onmouseenter:function(){ return true},
			onmouseleave:function(){return true},
			onclick:function(){ return false},
			dropped: function(){ return false},
			dragOver: function(){ debug(["default bubble dragOver"]); return false},
			subscriptions:[],
			moveable: false
		};
		conf.data = dojo.mixin(this.defaultConf.data, conf.data);
		conf = dojo.mixin(this.defaultConf, conf);
		
		this.group = conf.parent.createGroup();
		this.subscriptions = [];
		for(var i = 0; i < conf.subscriptions.length; i++){
			var subObj = conf.subscriptions[i];
			this.subscriptions.push(dojo.subscribe(subObj.channel, this, subObj.callback));
		}
		
		var rmod = Math.abs(-(255/50) * conf.data.health + 255);
		var gmod = 255;
		var textcolors = "black";
		if(conf.data.health > 50){
			var gmod = -255/50 * 1.3 * conf.data.health + (255 * 2);
			textcolors = "white";
		}
		var bubblefill = [rmod, gmod, 0, 100];
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
		}).setFill(bubblefill).setStroke("black");
		
		this.data = conf.data;
		
		this.group.connect("onmouseenter", this, conf.onmouseenter);
		this.group.connect("onmouseleave", this, conf.onmouseleave);
		this.group.connect("onclick", this, conf.onclick);
		
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
		
		text.setStroke(textcolors);
		text.setFill(textcolors);
		
		this.text = text;
		
		var rect = this.bubble;
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		};
		
		if(conf.moveable){
			this.moveable = new dojox.gfx.Moveable(this.group, {delay:1});
			this.moveable.delay = 1;
			for(var i in conf.moveable){
				this.moveable[i] = conf.moveable[i];
			}
		}
		
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
		
		this.group.setTransform([dojox.gfx.matrix.scaleAt(conf.scale, p)]);
	};
	
	supervisorView.Bubble.prototype.size = function(scale){
		var rect = this.bubble;
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		}
		debug(["size point", p]);
		this.group.setTransform([dojox.gfx.matrix.scaleAt(scale, p)]);
	}
		
	supervisorView.Bubble.prototype.shrink = function(){
		this.size(1);
	}
	
	supervisorView.Bubble.prototype.grow = function(){
		this.size(1.4);
	}

	supervisorView.Bubble.prototype.setHp = function(hp){
		var rmod = Math.abs(-(255/50) * hp + 255);
		var gmod = 255;
		var textcolors = "black";
		if(hp > 50){
			var gmod = -255/50 * 1.3 * hp + (255 * 2);
			textcolors = "white";
		}
		var bubblefill = [rmod, gmod, 0, 100];
		this.text.setStroke(textcolors);
		this.text.setFill(textcolors);
		this.bubble.setFill(bubblefill);
		var bshape = this.bubble.getShape();
		
		var thex = (hp * bshape.width)/100 + bshape.x
		this.hpline.setShape({x1: thex, x2:thex});
	}
	
	supervisorView.Bubble.prototype.pointCollision = function(point){
		var arr = this.bubble.getTransformedBoundingBox();
		debug(["pointCollision", point, arr]);
		var topleft = arr[0];
		var topright = arr[1];
		var bottomright = arr[2];
		var bottomleft = arr[3];
		return (topleft.x <= point.x && bottomright.x >= point.x) && (topleft.y <= point.y && bottomright.y >= point.y)
	}
	
	supervisorView.Bubble.prototype.boxCollision = function(intopleft, inbottomright){
		var arr = this.bubble.getTransformedBoundingBox();
		var topleft = arr[0];
		var topright = arr[1];
		var bottomright = arr[2];
		var bottomleft = arr[3];
		return (!(topleft.x > inbottomright.x || intopleft.x > bottomright.x || topleft.y > inbottomright.y || inbottomright.y > bottomright.y));
	}
	
	supervisorView.Bubble.prototype.clear = function(){
		for(var i in this.subscriptions){
			dojo.unsubscribe(this.subscriptions[i]);
		}
		
		this.group.clear();
	}
	
	supervisorView.Bubble.prototype.connect = function(ev, scope, fun){
		this.group.connect(ev, scope, fun);
	}
	
	supervisorView.Bubble.prototype.setImage = function(imageSrc){
		if(imageSrc == false){
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
	}
	
	supervisorView.Bubble.prototype.setDroppable = function(bool){
		if(this.previousImage == null){
			if(this.image){
				this.previousImage = this.image.getShape().src;
			}
			else{
				this.previousImage = false;
			}
		}
			
		if(bool == null){
			this.setImage(this.previousImage);
			delete this.previousImage;
		}
		else if(bool){
			this.setImage('/images/dl.png');
		}
		else{
			this.setImage('/images/redx.png');
		}
	}

	//=====
	// BubbleStack
	//=====
	
	supervisorView.BubbleStack = function(conf){
		this.defaultConf = {
			parent: supervisorView.surface,
			mousept: {x:100, y:50},
			viewHeight:350,
			bubbleConfs: [],
			registerCollider: false
		}
		
		var conf = dojo.mixin(this.defaultConf, conf);
		this.conf = conf;
		
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
		
		for(var i = 0; i < this.bubbleConfs.length; i++){
			var mixing = {
				point:{x:pt.x, y:this.indexToY(i)},
				parent:this.group,
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
			this.dragOver = function(){ return false};
			this.coll = supervisorView.dndManager.registerCollider(this);
		}
	};
	
	supervisorView.BubbleStack.prototype.indexToY = function(index){
		return this.conf.mousept.y + index * 40;
	}
	
	supervisorView.BubbleStack.prototype.viewYtoLocalY = function(viewY){
		var stackHeight = 40 * this.bubbleConfs.length;
		debug(['viewYtoLocalY', {'stackH':stackHeight, 'viewH':this.conf.viewHeight, 'viewY':viewY}]);
		if(stackHeight <= this.conf.viewHeight){
			return viewY;
		}
		
		return Math.floor((stackHeight * viewY) / this.conf.viewHeight);
	}
	
	supervisorView.BubbleStack.prototype.yToIndex = function(localY){
		return Math.round(localY / 40);
	}
	
	supervisorView.BubbleStack.prototype.viewYToIndex = function(viewY){
		var localy = this.viewYtoLocalY(viewY);
		debug(["localy", localy]);
		return this.yToIndex(localy);
	}
	
	supervisorView.BubbleStack.prototype.lockScroll = function(){
		this.scrollLocked = true;
	}
	
	supervisorView.BubbleStack.prototype.unlockScroll = function(){
		this.scrollLocked = false;
	}
	
	supervisorView.BubbleStack.prototype._setSelected = function(index){
		debug(["_setSelected", index, this._selected]);
		if(this.scrollLocked){
			return false
		}
		
		if(this._selected != undefined){
			debug("shrinking old selected");
			var bubConf = this.bubbleConfs[this._selected];
			if(bubConf.bubble){
				bubConf.bubble.size(1);
			}
		}
		
		if((index !== false) && this.bubbleConfs[index]){
			debug("enlarging new selected");
			var bubConf = this.bubbleConfs[index];
			if(bubConf.bubble){
				bubConf.bubble.size(1.4);
			}
			this._selected = index;
			return true;
		}
		
		delete this._selected;
	}
	
	supervisorView.BubbleStack.prototype.getSelected = function(){
		if(this._selected){
			return this.bubbleConfs[this._selected];
		}
		
		return false
	}
	
	supervisorView.BubbleStack.prototype.scroll = function(index){
		if(index >= this.bubbleConfs.length){
			index = this.bubbleConfs.length - 1;
		}
		
		if(index < 0){
			index = 0;
		}
		
		var y = this.indexToY(index);
		this._scroll(y);
	}
	
	supervisorView.BubbleStack.prototype._scroll = function(viewY){
		if(this.scrollLocked){
			return false
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
		for(var i = 0; i < min; i++){
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
						{channel: 'supervisorView/set/' + obj.data.type + '-' + obj.data.display,
						callback: function(rawobj){
							debug(["got it!", obj, rawobj]);
							obj.setHp(rawobj.aggregate);
						}},
						{channel: 'supervisorView/drop/' + obj.data.type + '-' + obj.data.display,
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
				obj = thisref.bubbleConfs[ind].bubble;
				obj.mover = new dojox.gfx.Moveable(obj.group);
				dojo.connect(obj.mover, 'onFirstMove', function(){
					thisref.lockScroll();
					supervisorView.dndManager.startDrag(obj);
				});
				obj.connect('onmouseup', function(){
					thisref.unlockScroll();
					obj.setDroppable(null);
					//obj.setImage(false);
					supervisorView.dndManager.endDrag();
				});
			}
		})
		
		for(var i = max; i < this.bubbleConfs.length; i++){
			var obj = this.bubbleConfs[i];
			if(obj && obj.bubble){
				obj.bubble.clear();
				delete this.bubbleConfs[i].bubble;
			}
		}

		debug(["viewY and ration", {'1':viewY, '2':this.ratio, '3':this.trueRatio}]);
		this.group.setTransform([dojox.gfx.matrix.translate(0, -viewY * this.trueRatio * this.ratio)]);
	}
	
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
	}
	
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
								b.onEnter()
							}
							this.dragOver = function(arg){
								debug(["dragOver called"]);
								return b.dragOver(arg);
							}
							this.dropped = function(arg){
								return b.dropped(arg);
							}
							return b;
						}
					}
				}
			}
		}
		
		delete this.dragOver;
		delete this.dropped;
		return null;
	}
	
	supervisorView.BubbleStack.prototype.boxCollision = function(intopleft, intopright, inbottomright, inbottomright){
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
	}
	
	supervisorView.BubbleStack.prototype.forEachBubble = function(callback){
		// callback is just the bubble.
		dojo.forEach(this.bubbleConfs, function(obj){
			if(obj.bubble){
				callback(obj.bubble);
			}
		});
	}
	
	//=====
	//utility functions
	//=====

	supervisorView.averageHp = function(hps){
		if(hps.length == 0){
			return 50;
		}
		
		debug(["averaging hps", hps]);
		var findweight = function(hp){
			if(hp > 50){
				// y = floor(.0039(x-50)^2 + 1)
				// I don't use the above formula for both sides because anything
				// below 50 will always have a weight of 1
				return Math.floor(.0039 * Math.pow((hp - 50), 2) + 1);
			}
			return 1
		}
		
		var total = 0;
		var count = 0;
		
		var f = function(obj, index, arr){
			debug(["averageing hp obj", obj]);
			if(obj == undefined){
				// la la la
				//warning("obj was undefined in averageHp");
			}
			else if(typeof(obj) == "number"){
				debug("number");
				var c = findweight(obj);
				count += c;
				total += c * obj;
			}
			else if(obj.health != undefined){
				debug("health");
				var c = findweight(obj.health);
				count += c;
				total += c * obj.health;
			}
			else if(obj.goal != undefined){
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
					var c = findweight(50);
					count += c;
					total += c * 50;
				}
				else if( (obj.min < obj.max) && (obj.max <= value) ){
					debug("min < max < value");
					var c = findweight(100);
					count += c;
					total += c * 100;
				}
				else if( ( obj.min < obj.max) && (value <= obj.min) ){
					debug("value < min < max");
					var c = findweight(0);
					count += c;
					total += c * 0;
				}
				else if( (obj.max < obj.min) && (value <= obj.max) ){
					debug("value < max < min");
					var c = findweight(100);
					count += c;
					total += c * 100;
				}
				else if( (obj.max < obj.min) && (obj.min <= value) ){
					debug("max < min < value");
					var c = findweight(0);
					count += c;
					total += c * 0;
				}
				else if( (obj.min < obj.max) && ( value < obj.goal) ){
					debug("min < value < goal"); 
					var hp = (50 * value - 50 * obj.min) / (obj.goal - obj.min);
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.min < obj.max) && ( obj.goal < value)){
					debug("min < goal < value");
					var hp = (100 * obj.goal - 50 * obj.max - 50 * value) / (obj.goal - obj.max);
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.max < obj.min) && (value < obj.goal) ){
					debug("max < value < goal");
					var hp = (50 * (2 * obj.goal - value - obj.max) ) / (obj.goal - obj.max);
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.max < obj.min) && (obj.goal < value) ){
					debug("everything else");
					var hp = ( ( 50 * ( obj.goal - value) ) / (obj.min - obj.goal) ) + 50;
					var c = findweight(hp);
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
		}
		
		dojo.forEach(hps, f);
			
		return Math.round(total / count);
	}

	supervisorView.setMediaHps = function(){
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
			}
			dojo.forEach(items, setHp);
			supervisorView.dataStore.save();
			dojo.publish("supervisorView/aggregate/media", []);
		}
		supervisorView.dataStore.fetch({
			query:{"type":"media"},
			onComplete:setHps
		});
									  
	}

	supervisorView.setQueueHps = function(){
		var setHp = function(item){
			var healthobj = supervisorView.dataStore.getValue(item, "health");
			var hpsvars = [];
			for(var i in healthobj){
				hpsvars.push(healthobj[i]);
			}
			var gotMedia = function(mitems){
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
				dojo.publish("supervisorView/set/" + rawobj.id, [item, rawobj]);
			}
			
			supervisorView.dataStore.fetch({
				query:{
					type:"media",
					queue:supervisorView.dataStore.getValue(item, "display"),
					node:supervisorView.node
				},
				onComplete:function(got){
					gotMedia(got);
					dojo.publish("supervisorView/aggregate/queues", []);
				}
			});
		}

		var setHps = function(items){
			info(["setQueueHps fetch done", items]);
			dojo.forEach(items, setHp);
		}

		supervisorView.dataStore.fetch({
			query:{"type":"queue"},
			onComplete:setHps
		})
	}

	supervisorView.setAgentHps = function(){
		var setHp = function(item){
			var healthobj = supervisorView.dataStore.getValue(item, "health");
			var hpsvars = [];
			for(var i in healthobj){
				hpsvars.push(healthobj[i]);
			}
			
			var gotMedia = function(mitems){
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
			}
			
			supervisorView.dataStore.fetch({
				query:{
					"type":"media",
					"agent":supervisorView.dataStore.getValue(item, "display")
				},
				onComplete:function(got){
					gotMedia(got);
					dojo.publish("supervisorView/aggregate/agents", []);
				}
			});
		}
		
		var setHps = function(items){
			info(["Fetch for setAgentHps done", items]);
			dojo.forEach(items, setHp);
		}
		
		supervisorView.dataStore.fetch({
			query:{"type":"agent"},
			onComplete:setHps
		})
	}

	supervisorView.setQueueGroupHps = function(){
		var setHp = function(item){
			var hpsvars = [];
			var gotQueues = function(items){
				dojo.forEach(items, function(i){
					hpsvars.push(supervisorView.dataStore.getValue(i, "aggregate"));
				});
				info(["setQueueGroupHps wants averaged:  ", hpsvars]);
				var hp = supervisorView.averageHp(hpsvars);
				supervisorView.dataStore.setValue(item, "aggregate", hp);
				supervisorView.dataStore.save();
				var rawobj = {
					"id":"queuegroup-" + supervisorView.dataStore.getValue(item, "display"),
					"display":supervisorView.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"queuegroup",
					"health":{},
					"details":{}
				};
				dojo.publish("supervisorView/set/queuegroup-" + rawobj.display, [item, rawobj]);
				
			}
			
			supervisorView.dataStore.fetch({
				query:{
					"type":"queue",
					"group":supervisorView.dataStore.getValue(item, "display"),
					"node":supervisorView.node
				},
				onComplete:function(got){
					gotQueues(got);
					dojo.publish("supervisorView/aggregate/queuegroups", []);
				}
			});
		}

		var setHps = function(items){
			info(["setQueueGroupHps fetch done", items]);
			dojo.forEach(items, setHp);
		}
		
		supervisorView.dataStore.fetch({
			query:{"type":"queuegroup"},
			onComplete:setHps
		})
	}

	supervisorView.setAgentProfileHps = function(){
		var setHp = function(item){
			var hpsvars = [];
			var gotAgents = function(aitems){
				dojo.forEach(aitems, function(i){
					hpsvars.push(supervisorView.dataStore.getValue(i, "aggregate"));
				});
				debug(["setAGentProfileHps is averaging:", hpsvars]);
				var hp = supervisorView.averageHp(hpsvars);
				supervisorView.dataStore.setValue(item, "aggregate", hp);
				supervisorView.dataStore.save();
				var rawobj = {
					"id":"agentprofile-" + supervisorView.dataStore.getValue(item, "display"),
					"display":supervisorView.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"agentprofile",
					"health":{},
					"details":{}
				};
				dojo.publish("supervisorView/set/agentprofile-" + rawobj.display, [item, rawobj]);
			}
			
			supervisorView.dataStore.fetch({
				query:{
					"type":"agent",
					"profile":supervisorView.dataStore.getValue(item, "display"),
					"node":supervisorView.node
				},
				onComplete:function(got){
					gotAgents(got);
					dojo.publish("supervisorView/aggregate/agentprofiles", []);
				}
			});
		}
		
		var setHps = function(items){
			info(["setAgateProfileHps fetch done", items]);
			dojo.forEach(items, setHp);
		}
		
		supervisorView.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHps
		})
	}

	supervisorView.setGlobalAgentHp = function(){
		var setHp = function(items){
			info(["setGlobalAgentHp fetch done", items]);
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorView.dataStore.getValue(item, "aggregate"));
			});
			info(["setGlobalAgentHp wants averaged:", hplist]);
			var hp = supervisorView.averageHp(hplist);
			supervisorView.agentBubble.setHp(hp);
		}
		supervisorView.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHp
		});
	}

	supervisorView.setGlobalQueueHp = function(){
		var setHp = function(items){
			info(["setGlobalQueueHp fetch done", items]);
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorView.dataStore.getValue(item, "aggregate"));
			});
			debug(["setGlobalQueueHp wants averaged:", hplist]);
			var hp = supervisorView.averageHp(hplist);
			supervisorView.queueBubble.setHp(hp);
		}
		supervisorView.dataStore.fetch({
			query:{"type":"queuegroup"},
			onComplete:setHp
		})
	}

	supervisorView.setNodeHps = function(){
		var gotNodes = function(items){
			info(["setNodeHps fetch done", items]);
			dojo.forEach(items, function(item){
				var gotNodeItems = function(nitems){
					var hplist = [];
					dojo.forEach(nitems, function(nitem){
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
				}
				supervisorView.dataStore.save();
				//supervisorView.refreshSystemStack();
				supervisorView.dataStore.fetch({
					query:{node:supervisorView.dataStore.getValue(item, "display")},
					onComplete:gotNodeItems
				});
			});
		}
		
		supervisorView.dataStore.fetch({
			query:{type:"node"},
			onComplete:gotNodes
		});
	}

	supervisorView.setSystemHps = function(){
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
				dojo.publish("supervisorView/set/system-System", [items[0], rawobj]);
			}
			supervisorView.dataStore.fetch({
				query:{"type":"system"},
				onComplete:gotSystem
			});
		}
		
		supervisorView.dataStore.fetch({
			query:{"type":"node"},
			onComplete:gotNodes
		});
	}
	
	supervisorView.setAllHps = function(){
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
	}

	supervisorView.setDetails = function(fquery){
		var fetchdone = function(item){
			var obj = supervisorView.dataStore.getValue(item, "details");
			var out = "";
			if(supervisorView.dataStore.getValue(item, "node")){
				out += "<p class=\"smaller\"><label class=\"narrow\">Node:</label>" + supervisorView.dataStore.getValue(item, "node");
			}
			for(var i in obj){
				out += "<p class=\"smaller\"><label class=\"narrow\">" + i + ":</label>" + obj[i].toString() + "</p>";
			}
			out += "<p>Health Report</p>";
			var hps = supervisorView.dataStore.getValue(item, "health");
			for(var i in hps){
				var sigdigited = Math.floor(supervisorView.averageHp([hps[i]]) * 100) / 100;
				out += "<p class=\"smaller\"><label class=\"narrow\">" + i + ":</label>" + sigdigited.toString() + "</p>";
			}
			dijit.byId("supervisorDetails").setContent(out);
			dijit.byId("supervisorDetails").setTitle(supervisorView.dataStore.getValue(item, "type") + ": " + supervisorView.dataStore.getValue(item, "display"));
		}
		
		supervisorView.dataStore.fetch({
				query:fquery,
				onItem:fetchdone
		});
	}

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
					dijit.byId("nodeAction").nodeBubbleHit = this.data.display;
				}
			}];
			
			for(var i = 0; i < items.length; i++){
				acc.push({
					data:{
						id:supervisorView.dataStore.getValue(items[i], 'id'),
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
						dijit.byId("nodeAction").nodeBubbleHit = this.data.display;
					}
				});
			}
			
			var yi = 375;
			dojo.forEach(acc, function(conf){
				var o = new supervisorView.Bubble({
					point: {x: 20, y:yi},
					data: conf.data,
					onmouseenter: conf.onmouseenter,
					onclick:function(){
						if(this.data.display == 'System'){
							supervisorView.node = '*';
						}
						else{
							supervisorView.node = this.data.display;
						}
						dojo.forEach(supervisorView.systemStack, function(obj){
							obj.size(1);
						});
						this.size(1.4);
					}
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
				}
				var coll = supervisorView.dndManager.registerCollider(o);
				dojo.connect(o, 'clear', function(){
					supervisorView.dndManager.unregisterCollider(coll);
				});
				supervisorView.systemStack.push(o);
			});
		}

		supervisorView.dataStore.fetch({
			query:{
				type:"node"
			},
			onComplete:fetchdone
		});

	}

	supervisorView.drawAgentQueueBubbles = function(agenthp, queuehp){
		var clearStacks = function(){
			var clearables = [
				"queueGroupsStack",
				"queuesStack",
				"agentProfilesStack",
				"agentsStack",
				"callsStack"
			]
			
			for(var i in clearables){
				if(supervisorView[clearables[i]].clear){
					supervisorView[clearables[i]].clear();
				}
			}
		}
		
		supervisorView.agentBubble = new supervisorView.Bubble({
			point:{x:20, y:20},
			scale: .75,
			data: {"health":agenthp, "display":"Agents"},
			onclick:function(){
				clearStacks();
				supervisorView.drawAgentProfilesStack();
				this.size(1);
				supervisorView.queueBubble.size(.75);
			},
			dragOver: function(){
				clearStacks();
				supervisorView.drawAgentProfilesStack();
				supervisorView.agentBubble.size(1);
				supervisorView.queueBubble.size(.75);
				return false;
			}
		});
		supervisorView.agentBubble.coll = supervisorView.dndManager.registerCollider(supervisorView.agentBubble);
		
		supervisorView.queueBubble = new supervisorView.Bubble({
			point:{x:20, y:60},
			scale: .75,
			data:{"health":queuehp, "display":"Queues"},
			onclick: function(){
				clearStacks();
				supervisorView.drawQueueGroupsStack();
				this.size(1);
				supervisorView.agentBubble.size(.75);
			},
			dragOver:  function(){
				clearStacks();
				supervisorView.drawQueueGroupsStack();
				supervisorView.queueBubble.size(1);
				supervisorView.agentBubble.size(.75);
				return false;
			}
		});
		supervisorView.queueBubble.coll = supervisorView.dndManager.registerCollider(supervisorView.queueBubble);
	}

	supervisorView.drawQueueGroupsStack = function(){
		if(supervisorView.queueGroupsStack && supervisorView.queuesStack.scrollLocked){
			warning(["queueGroupsstack exists but is scroll-locked"]);
			return false;
		}
		
		if(supervisorView.agentsStack && supervisorView.agentsStack.scrollLocked){
			warning(["agent profiles stack exists but is scroll-locked"]);
			return false;
		}
		
		//supervisorView.queuesStack.clear();
		
		var fetchdone = function(items, request){
			//warning(["fetchdone"]);
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				acc.push({
					data:{
						display:supervisorView.dataStore.getValue(obj, "display"),
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50)
					},
					onmouseenter:function(ev){
						supervisorView.drawQueuesStack(this.data.display, supervisorView.node, acc.length);
						supervisorView.setDetails({
							type:supervisorView.dataStore.getValue(obj, "type"),
							display:supervisorView.dataStore.getValue(obj, "display")
						});
					},
					dragOver:function(){
						supervisorView.drawQueuesStack(this.data.display, supervisorView.node, acc.length);
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
			//warning(["calls stack cleared"]);
			
			/*supervisorView.queueGroupsStack.clear();
			supervisorView.queuesStack.clear();
			supervisorView.agentProfilesStack.clear();*/
			supervisorView.queueGroupsStack = new supervisorView.BubbleStack({
				mousept:{
					x:250,
					y:100
				},
				bubbleConfs:acc,
				registerCollider:true
			});
			
			supervisorView.queueGroupsStack.scroll(0);
			supervisorView.queueGroupsStack.group.moveToBack();
		}
		
		supervisorView.dataStore.fetch({
			query:{
				type:"queuegroup",
			},
			onComplete:fetchdone
		});
	}

	supervisorView.drawQueuesStack = function(group, node, scrollIndex){
		if(supervisorView.queuesStack.scrollLocked){
			warning(["queues stack scroll lacked"]);
			return false;
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate", 50));
				acc.push({
					data:{
						display:supervisorView.dataStore.getValue(obj, "display"),
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50)
					},
					onmouseenter:function(ev){
						var queryObj = {
							'queue':this.data.display,
							'node':supervisorView.node
						};
						supervisorView.drawCallStack(queryObj, acc.length);
						supervisorView.setDetails({
							type:"queue",
							display:supervisorView.dataStore.getValue(obj, "display")
						});
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
						supervisorView.queueTransfer(droppedObj.data.display, this.data.display);
					}
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});
			
			supervisorView.callsStack.clear();
			supervisorView.queuesStack.clear();
			
			supervisorView.queuesStack = new supervisorView.BubbleStack({
				mousept:{
					x:540,
					y:100
				},
				bubbleConfs: acc,
				registerCollider: true
			});
			
			supervisorView.queuesStack.scroll(scrollIndex);
			supervisorView.queuesStack.group.moveToBack();
		}
		
		var queryo = {
			type:"queue",
			'group':group,
			node:node
		};
		
		supervisorView.dataStore.fetch({
			query:queryo,
			onComplete:fetchdone
		});
	
		return true;
	}
	
	supervisorView.drawAgentProfilesStack = function(){
		if(supervisorView.agentProfilesStack && supervisorView.agentProfilesStack.scrollLocked){
			warning(["agentProfilesStack exists but is scroll-locked"]);
			return false;
		}
						
		var fetchdone = function(items, request){
			//warning(["fetchdone", items]);
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				acc.push({
					data:{
						display:supervisorView.dataStore.getValue(obj, "display"),
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50)
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
					}
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});

			supervisorView.callsStack.clear();
			//warning(["calls stack cleared"]);
			supervisorView.queueGroupsStack.clear();
			supervisorView.queuesStack.clear();
			supervisorView.agentProfilesStack = new supervisorView.BubbleStack({
				mousept:{
					x:250,
					y:100
				},
				bubbleConfs:acc,
				registerCollider: true
			});
	
			supervisorView.agentProfilesStack.forEachBubble(function(bubble){
				var chan = "supervisorView/set/agentprofile-" + bubble.data.display;
					bubble.subscriptions.push(dojo.subscribe(chan, function(storeref, rawobj){
					bubble.setHp(rawobj.aggregate);
				}));
			});

			supervisorView.agentProfilesStack.scroll(0);
			supervisorView.agentProfilesStack.group.moveToBack();
		}

		supervisorView.dataStore.fetch({
			query:{
				type:"agentprofile",
			},
			onComplete:fetchdone
		});

		return true;
	}
	
	supervisorView.drawAgentsStack = function(profile, node, scrollIndex){
		if(supervisorView.agentsStack.scrollLocked){
			warning(["agents stack scroll lacked"]);
			return false;
		}

		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate", 50));
				var details = supervisorView.dataStore.getValue(obj, 'details', {'state':'released'});
				var imageUrl = '/images/' + details.state + '.png';
				acc.push({
					data:{
						display:supervisorView.dataStore.getValue(obj, "display"),
						health:supervisorView.dataStore.getValue(obj, "aggregate", 50),
						type:'agent'
					},
					onmouseenter:function(ev){
						var queryObj = {
							'agent':this.data.display,
							'node':supervisorView.node
						};
						supervisorView.drawCallStack(queryObj, acc.length);
						supervisorView.setDetails({
							type:"agent",
							display:supervisorView.dataStore.getValue(obj, "display")
						});
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
						supervisorView.sendMediaToAgent(droppedObj.data.display, this.data.display)
					},
					image: imageUrl
				});
				hps.push(supervisorView.dataStore.getValue(obj, "aggregate"));
			});

			supervisorView.callsStack.clear();
			supervisorView.agentsStack.clear();
			
			supervisorView.agentsStack = new supervisorView.BubbleStack({
				mousept:{
					x:540,
					y:100
				},
				bubbleConfs: acc,
				registerCollider:true
			});
			supervisorView.agentsStack.group.moveToBack();
			
			supervisorView.agentsStack.forEachBubble(function(bub){
				bub.subscriptions.push(dojo.subscribe("supervisorView/set/agent-" + detailsObj.display, function(storeref, rawobj){
					bub.setHp(rawobj.aggregate);
				}));
				bub.subscriptions.push(dojo.subscribe("supervisorView/drop/agent-" + detailsObj.display, function(storeref, rawobj){
					bub.clear();
				}));

				bub.dropped = function(obj){
					if(obj.data.type == "media"){
						var geturl = "";
						if(obj.data.queue){
							geturl = "/supervisor/agent_ring/" + escape(obj.data.queue) + "/" + escape(obj.data.display) + "/" + escape(bub.data.display);
						}
						else if(obj.data.agent){
							geturl = "/supervisor/agent_transfer/" + escape(obj.data.agent) + "/" + escape(bub.data.display);
						}
						
						var ajaxdone = function(json, args){};
						dojo.xhrGet({
							url:geturl,
							handleAs:"json",
							load:ajaxdone
						});
					}
				}

				if(bub.data.display != "All"){
					bub.dragOver = function(){
						bub.onEnter();
						return true;
					}
				}
			});
			
			supervisorView.agentsStack.scroll(scrollIndex);
		}

		var queryo = {
			type:"agent",
			'profile':profile,
			'node':node
		};

		supervisorView.dataStore.fetch({
			query:queryo,
			onComplete:fetchdone
		});

		return true;
	}

	supervisorView.drawCallStack = function(queryObj, scrollIndex){
		if(supervisorView.callsStack.scrollLocked){
			return false;
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			dojo.forEach(items, function(obj){
				var datas = {
					display:supervisorView.dataStore.getValue(obj, "display"),
					health:supervisorView.dataStore.getValue(obj, "aggregate", 50),
					type:"media"
				};
				
				if(supervisorView.dataStore.getValue(obj, "agent")){
					datas['agent'] = supervisorView.dataStore.getValue(obj, "agent");
				}
				else{
					datas['queue'] = supervisorView.dataStore.getValue(obj, "queue");
				}
				
				acc.push({
					data:datas,
					onmouseenter:function(ev){
						supervisorView.setDetails({
							type:"media",
							display:supervisorView.dataStore.getValue(obj, "display")
						});
					}
				});
			});
			
			supervisorView.callsStack.clear();
			supervisorView.callsStack = new supervisorView.BubbleStack({
				mousept:{
					x:580 + 240,
					y:20
				},
				bubbleConfs:acc
			});
		
			supervisorView.callsStack.forEachBubble(function(obj){
				obj.group.connect("onmousedown", obj, function(ev){
					supervisorView.dndManager.startDrag(obj);
				});
				obj.group.connect("onmouseup", obj, function(ev){
					supervisorView.dndManager.endDrag()
				});
				
				var nom = obj.data.display;
				obj.subscriptions.push(dojo.subscribe("supervisorView/set/media-" + nom, function(storeref, rawobj){
					obj.setHp(rawobj.aggregate);
				}));
				obj.subscriptions.push(dojo.subscribe("supervisorView/drop/media-" + nom, function(storeref, rawobj){
					obj.clear();
				}));
			});
		
			supervisorView.callsStack.scroll(scrollIndex);
		}

		supervisorView.dataStore.fetch({
			query:queryObj,
			onComplete:fetchdone
		});
	
		return true;
	}
	
	supervisorView.healthDump = function(){
		var dump = function(items){
			dojo.forEach(items, function(item){
				var out = supervisorView.dataStore.getValue(item, "aggregate");
				var nom = supervisorView.dataStore.getValue(item, "display");
				debug(["healthDump", nom, out]);
			})
		}
		
		supervisorView.dataStore.fetch({
			onComplete:dump
		});
	}

	supervisorView.reloadDataStore = function(){
		dojo.xhrGet({
			url:"/supervisor/status",
			handleAs:"json",
			error:function(){
				supervisorView.poller.stop();
			},
			load:function(data){
				if(data.data){
					info(["1400", "store reload ajax completed"]);
					supervisorView.healthData = data.data;
					supervisorView.dataStore = new dojo.data.ItemFileWriteStore({
						data: supervisorView.healthData,
						typeMap:{
							"details":{
								"type":Object,
								"deserialize":function(obj){return obj},
								"serialize":function(obj){return obj}
							}
						}
					});
					supervisorView.refreshSystemStack();
					supervisorView.setAllHps();
					if(supervisorView.suppressPoll){
						return;
					}
				}
				else{
					debug(["1422", "stub for no data.data", data]);
				}
			}
		})
	}

	supervisorView.blab = function(message, type, target){
		dojo.xhrPost({
			handleAs:"json",
			url:"/supervisor/blab",
			content:{
				message:message,
				type: type,
				value: target
			},
			load:function(res){
				debug(["blab worked", res])
			},
			error:function(res){
				warning(["blab failed", res])
			}
		})
	}
	
	supervisorView.setProfile = function(profile, agent){
		dojo.xhrGet({
			handleAs:"json",
			url:"/supervisor/set_profile/" + agent + "/" + profile,
			load:function(res){
				if(res.success){
					//kewl
					return true
				}
				else{
					warning(["set profile failed", res.message]);
				}
			},
			error:function(res){
				warning(["set profile errored", res]);
			}
		});
	}
	
	supervisorView.queueTransfer = function(callid, newqueue){
		warning(["queueTransfer nyi"]);
		return false;
		
		/*var fetchdone = function(items){
			if(items.length == 0){
				return false
			}
			
			var item = items[0];
			var healthData = supervisorView.dataStore.getValue('health', item);
			var url = '/supervisor/';
			if(healthData.inqueue){
				url += 'queue_transfer/' + escape(callid) + '/' + escape(newqueue);
			}
			else if(healthData.{
				url += 'requeue/' + escape(
			}
		}
		
		
		dojo.xhrGet({
			url:'/supervisor/requeue/' + escape(
		});
		
		supervisorView.queuesStack.forEachBubble(function(bub){
												 bub.subscriptions.push(dojo.subscribe("supervisorView/set/queue-" + detailsObj.display, function(storeref, rawobj){
																					   bub.setHp(rawobj.aggregate);
																					   }));
												 bub.subscriptions.push(dojo.subscribe("supervisorView/drop/queue-" + detailsObj.display, function(storeref, rawobj){
																					   bub.clear();
																					   }));
												 
												 bub.dropped = function(obj){
												 if(obj.data.type == "media"){
												 var ajaxdone = function(json, args){}
												 var geturl = "/supervisor/requeue/" + escape(obj.data.agent) + "/" + escape(bub.data.display);
												 dojo.xhrGet({
															 url:geturl,
															 handleAs:"json",
															 load:ajaxdone
															 });
												 }
												 }
												 
												 if(bub.data.display != "All"){
												 bub.dragOver = function(){
												 bub.onEnter();
												 return true;
												 }
												 }
												 });*/
		
		
		
	}
	
	supervisorView.sendMediaToAgent = function(mediaid, agentlogin){
		warning("sendMediaToAgent NYI");
		return false;
	}
}

supervisorView.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
dojo.connect(supervisorView.surface, "ondragstart",   dojo, "stopEvent");
dojo.connect(supervisorView.surface, "onselectstart", dojo, "stopEvent");

supervisorView.reloadDataStore();

supervisorView.drawAgentQueueBubbles(0, 0);
supervisorView.drawSystemStack();

/*supervisorView.masterSub = dojo.subscribe("agent/supervisorView", function(data){
	debug(["1442", "master sub", data]);
	data = data.data;
	if(data.action == "set"){
		var fetched = function(items){
			warning(["fetched", items]);
			// items to be scapped out and put top leve:
			// node, profile, group, queue, agent.
			// check if profile or group exists, and if not, create a stub for them
			// finally, store the 'corrected' object, 
			// then publish.
			var scrapped = {};
			var savedata = {
				"details":{},
				"health":{},
				"id":data.id,
				"type":data.type,
				"display":data.display,
				"node":null,
				"profile":null,
				"group":null,
				"queue":null,
				"agent":null
			};
			for(var i in data.details){
				switch(i){
					case "node":
					case "profile":
					case "group":
					case "queue":
					case "agent":
						savedata[i] = data.details[i];
						break;
					default:
						savedata.details[i] = data.details[i];
				}
			}
			var hps = [];
			for(var i in data.health){
				hps.push(data.health[i]);
				savedata.health[i] = data.health[i];
			}
			var aggregate = 50;
			if(hps.length > 0){
				debug(["masterSub averaging", hps]);
				aggregate = supervisorView.averageHp(hps);
			}
			savedata.aggregate = aggregate;
			//course, there may be no item yet...
			if(items.length == 0){
				items.push(supervisorView.dataStore.newItem(savedata));
			}
			for(var i in savedata){
				if(i != "id"){
					supervisorView.dataStore.setValue(items[0], i, savedata[i]);
				}
			}
			supervisorView.dataStore.save();
			debug(["1488", "savedata", savedata]);
			dojo.publish("supervisorView/set/" + savedata.id, [items[0], savedata]);
			supervisorView.setAllHps();
		}
	
		supervisorView.dataStore.fetch({
			query:{"id":data.id},
			onComplete:fetched
		});
	}
	else if(data.action == "drop"){
		var fetched = function(items){
			supervisorView.dataStore.deleteItem(items[0]);
			dojo.publish("supervisorView/drop/" + data.id, [data]);
		}
		
		supervisorView.dataStore.fetch({
			query:{"id":data.id},
			onComplete:fetched
		});
	}
	else{
		// la la la
	}
});*/

supervisorView.hpCalcTimer = '';
supervisorView.hpcalc = function(){
	supervisorView.hpCalcTimer = setTimeout(function(){
		supervisorView.setAllHps();
		supervisorView.hpcalc()}, 5000);
}

/*supervisorView.logoutListener = dojo.subscribe("agent/logout", function(data){
	clearTimeout(supervisorView.hpCalcTimer)
});

window.supervisorViewKillListen = dojo.subscribe("tabPanel-removeChild", function(child){
	if(child.title == "Supervisor"){
		clearTimeout(supervisorView.hpCalcTimer);
		dojo.unsubscribe(window.supervisorViewKillListen);
		dojo.unsubscribe(supervisorView.logoutListener);
		dojo.unsubscribe(supervisorView.masterSub);
		dojo.xhrGet({
			url:"/supervisor/endmonitor",
			handleAs:'json',
			load:function(res){
				if(res.success){
				}
				else{
					debug(["endmonitor failed", res.message]);
				}
			},
			error:function(res){
				warning(["endmonitor errored", res]);
			}
		});
	}
});*/

supervisorView.hpcalc();