
if(typeof(supervisorTab) == "undefined"){

	supervisorTab = function(){
		{};
	};

	supervisorTab.healthData = {identifier:"id",
	label:"display",
	items:[]
	};

	supervisorTab.dataStore = new dojo.data.ItemFileWriteStore({
		data: supervisorTab.healthData,
		typeMap:{
			"details":{
				"type":Object,
				"deserialize":function(obj){return obj}
			}
		}
	});

	supervisorTab.systemStack = [];

	supervisorTab.node = "*";
	supervisorTab.queueGroupsStack = {clear:function(){ return true}};
	supervisorTab.queuesStack = {clear:function(){ return true}};
	supervisorTab.agentsStack = {clear:function(){ return true}};
	supervisorTab.agentProfilesStack = {clear:function(){ return true}};
	supervisorTab.callsStack = {clear:function(){return true}};
	supervisorTab.suppressPoll = false;

	supervisorTab.dndManager = {
		_dragging: null,
		_eventHandle:null,
		_dropCandidate: null,
		checkCollision:function(pt){
			var out;
			if(out = supervisorTab.individualsStack.pointCollision(pt)){
				return out;
			}
			
			if(out = supervisorTab.groupsStack.pointCollision(pt)){
				return out;
			}
		
			if(supervisorTab.agentBubble.pointCollision(pt)){
				return supervisorTab.agentBubble;
			}
			
			if(supervisorTab.queueBubble.pointCollision(pt)){
				return supervisorTab.queueBubble;
			}
			
			for(var i = 0; i < supervisorTab.systemStack.length; i++){
				if(out = supervisorTab.systemStack[i].pointCollision(pt)){
					return out;
				}
			}
			
			return null;
		},
		startDrag:function(obj){
			supervisorTab.dndManager._eventHandle = obj.connect("mousemove", obj, function(ev){
				var point = {
					x:ev.layerX,
					y:ev.layerY
				};
				var collided = supervisorTab.dndManager.checkCollision(point)
				if(collided && collided.dragOver){
					collided.dragOver(obj);
					supervisorTab.dndManager._dropCandidate = collided;
				}
				else{
					supervisorTab.dndManager._dropCandidate = null;
				}
			});
			supervisorTab.suppressPoll = true;
			supervisorTab.dndManager._dragging = obj;
		},
		endDrag:function(){
			supervisorTab.suppressPoll = false;
			supervisorTab.dndManager._dragging.disconnect(supervisorTab.dndManager._eventHandle);
			if(supervisorTab.dndManager._dropCandidate){
				supervisorTab.dndManager._dropCandidate.dropped(supervisorTab.dndManager._dragging);
			}
		}
	};

	/*supervisorTab.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
	dojo.connect(supervisorTab.surface, "ondragstart",   dojo, "stopEvent");
	dojo.connect(supervisorTab.surface, "onselectstart", dojo, "stopEvent");*/

	//=====
	// And so begins defining useful classes
	//=====	
	
	//=====
	// Bubble
	//=====
	
	supervisorTab.Bubble = function(conf){
		this.defaultConf = {
			point:{x:100,y:100},
			scale:1,
			parent:supervisorTab.surface,
			data:{"display":"bubble", "health":50, "type":"text"},
			onmouseenter:function(){ return false},
			onmouseleave:function(){return false},
			onclick:function(){ return false},
			subscriptions:[]
		};
		conf.data = dojo.mixin(this.defaultConf.data, conf.data);
		conf = dojo.mixin(this.defaultConf, conf);
		
		this.group = conf.parent.createGroup();
		this.subscriptions = conf.subscriptions;
		
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
		
		this.group.setTransform([dojox.gfx.matrix.scaleAt(conf.scale, p)]);
	};
	
	supervisorTab.Bubble.prototype.size = function(scale){
		var rect = this.bubble;
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		}
		this.group.setTransform([dojox.gfx.matrix.scaleAt(scale, p)]);
	}
		
	supervisorTab.Bubble.prototype.shrink = function(){
		this.size(1);
	}
	
	supervisorTab.Bubble.prototype.grow = function(){
		this.size(1.4);
	}

	supervisorTab.Bubble.prototype.setHp = function(hp){
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
	
	supervisorTab.Bubble.prototype.pointCollision = function(point){
		var arr = this.bubble.getTransformedBoundingBox();
		var topleft = arr[0];
		var topright = arr[1];
		var bottomright = arr[2];
		var bottomleft = arr[3];
		return (topleft.x <= point.x && bottomright.x >= point.x) && (topleft.y <= point.y && bottomright.y >= point.y)
	}
	
	supervisorTab.Bubble.prototype.boxCollision = function(intopleft, inbottomright){
		var arr = this.bubble.getTransformedBoundingBox();
		var topleft = arr[0];
		var topright = arr[1];
		var bottomright = arr[2];
		var bottomleft = arr[3];
		return (!(topleft.x > inbottomright.x || intopleft.x > bottomright.x || topleft.y > inbottomright.y || inbottomright.y > bottomright.y));
	}
	
	supervisorTab.Bubble.prototype.clear = function(){
		for(var i in this.subscriptions){
			dojo.unsubscribe(this.subscriptions[i]);
		}
		
		this.group.clear();
	}
	
	supervisorTab.Bubble.prototype.connect = function(ev, scope, fun){
		this.group.connect(ev, scope, fun);
	}
	

	//=====
	// BubbleStack
	//=====
	
	supervisorTab.BubbleStack = function(conf){
		this.defaultConf = {
			parent: supervisorTab.surface,
			mousept: {x:100, y:50},
			viewHeight:350,
			bubbleConfs: [],
		}
		
		var conf = dojo.mixin(this.defaultConf, conf);
		this.conf = conf;
		
		var groupHeight = conf.bubbleConfs.length * 40;
		var viewHeight = conf.viewHeight;
		
		var ratio = (groupHeight - viewHeight) / viewHeight;
		if(ratio < 1){
			ratio = 0;
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
		
		for(var i = 0; i < this.bubbleConfs.length; i++){
			var mixing = {
				point:{x:pt.x, y:this.indexToY(i)},
				parent:this.group
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
		this.backing.connect("onmousemove", this, function(ev){
			this._scroll(ev.layerY);
		});
		
	};
	
	supervisorTab.BubbleStack.prototype.indexToY = function(index){
		return this.conf.mousept.y + index * 40;
	}
	
	supervisorTab.BubbleStack.prototype.yToIndex = function(localY){
		return Math.floor((localY - this.conf.mousept.y) / 40);
	}
	
	supervisorTab.BubbleStack.prototype.lockScroll = function(){
		this.scrollLocked = true;
	}
	
	supervisorTab.BubbleStack.prototype.unlockScroll = function(){
		this.scrollLocked = false;
	}
	
	supervisorTab.BubbleStack.prototype._setSelected = function(index){
		debug(["_setSelected", index, this._selected]);
		if(this._selected){
			var bubConf = this.bubbleConfs[this._selected];
			if(bubConf.bubble){
				bubConf.bubble.size(1);
			}
		}
		
		if(index){
			var bubConf = this.bubbleConfs[index];
			if(bubConf.bubble){
				bubConf.bubble.size(1.4);
			}
			this._selected = index;
			return true;
		}
		
		delete this._selected;
	}
	
	supervisorTab.BubbleStack.prototype.getSelected = function(){
		if(this._selected){
			return this.bubbleConfs[this.selected];
		}
		
		return false
	}
	
	supervisorTab.BubbleStack.prototype._scroll = function(viewY){
		if(this.scrollLocked){
			return false
		}
		
		var deviation = Math.abs(Math.ceil(this.conf.viewHeight / 40));
		var midIndex = this.yToIndex(viewY * this.ratio);
		var min = midIndex - deviation;
		var max = midIndex + deviation;
		
		debug(["min and max", min, max]);
		var shouldDraw = function(ind){
			if( (min <= ind) && (ind <= max) ){
				return true;
			}
			
			return false;
		}
		
		for(var i = 0; i < this.bubbleConfs.length; i++){
			var obj = this.bubbleConfs[i];
			if(shouldDraw(i)){
				if(! obj.bubble){
					var drawy = this.indexToY(i);
					var bubbleConf = dojo.mixin(obj, {
						mousept: {
							x: this.conf.mousept.x,
							y: drawy
						},
						parent: this.group
					});
					this.bubbleConfs[i].bubble = new supervisorTab.Bubble(bubbleConf);
					var thei = i;
					this.bubbleConfs[i].bubble.group.connect('onmouseenter', this, function(){
						this._setSelected(thei);
					});
				}
			}
			else{
				if(obj.bubble){
					obj.bubble.clear();
					delete this.bubbleConfs[i].bubble;
				}
			}
		}

		debug(["localY and ration", viewY, this.ratio]);
		this.group.setTransform([dojox.gfx.matrix.translate(0, -viewY * this.ratio)]);
	}
	
	supervisorTab.BubbleStack.prototype.clear = function(){
		dojo.forEach(this.subscriptions, function(obj){
			dojo.unsubscribe(obj);
		});
		this.group.clear();
	}
	
	supervisorTab.BubbleStack.prototype.pointCollision = function(point){
		if (this.backing.getShape().x <= point.x && this.backing.getShape().x + this.backing.getShape().width >= point.x){
			this._scroll(point.y);
			for(var i = 0; i < this.bubbleConfs.length; i++){
				if(this.bubbleConfs[i].bubble){
					var b = this.bubbleConfs[i].bubble;
					if(b.pointCollision(point)){
						if(b.onEnter){
							b.onEnter()
						}
						return b;
					}
				}
			}
		}
		
		return null;
	}
	
	supervisorTab.BubbleStack.prototype.boxCollision = function(intopleft, intopright, inbottomright, inbottomright){
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
	
	supervisorTab.BubbleStack.prototype.forEachBubble = function(callback){
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

	supervisorTab.averageHp = function(hps){
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

	supervisorTab.setMediaHps = function(){
		var setHps = function(items){
			info(["setMediaHps fetch done", items]);
			var setHp = function(obj, ind, arr){
				var healthobj = supervisorTab.dataStore.getValue(obj, "health");
				var hpsvars = [];
				for(var i in healthobj){
					hpsvars.push(healthobj[i]);
				}
				debug(["setMediaHps wants avergaed:  ", hpsvars]);
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(obj, "aggregate", hp);
				var rawobj = {
					"id":"media-" + supervisorTab.dataStore.getValue(obj, "display"),
					"display":supervisorTab.dataStore.getValue(obj, "display"),
					"aggregate":hp,
					"type":"media",
					"health":{},
					"details":{}
				};
				debug(["setMediaHps", obj]);
				dojo.publish("supervisortab/set/" + rawobj.id, [obj, rawobj]);				
			}
			dojo.forEach(items, setHp);
			supervisorTab.dataStore.save();
			dojo.publish("supervisortab/aggregate/media", []);
		}
		supervisorTab.dataStore.fetch({
			query:{"type":"media"},
			onComplete:setHps
		});
									  
	}

	supervisorTab.setQueueHps = function(){
		var setHp = function(item){
			var healthobj = supervisorTab.dataStore.getValue(item, "health");
			var hpsvars = [];
			for(var i in healthobj){
				hpsvars.push(healthobj[i]);
			}
			var gotMedia = function(mitems){
				if(mitems.length > 0){
					dojo.forEach(mitems, function(mitem){
						hpsvars.push(supervisorTab.dataStore.getValue(mitem, "aggregate"));
					});
				}
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
				var rawobj = {
					"id":"queue-" + supervisorTab.dataStore.getValue(item, "display"),
					"display":supervisorTab.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"queue",
					"health":{},
					"details":{}
				};
				dojo.publish("supervisortab/set/" + rawobj.id, [item, rawobj]);
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					type:"media",
					queue:supervisorTab.dataStore.getValue(item, "display"),
					node:supervisorTab.node
				},
				onComplete:function(got){
					gotMedia(got);
					dojo.publish("supervisortab/aggregate/queues", []);
				}
			});
		}

		var setHps = function(items){
			info(["setQueueHps fetch done", items]);
			dojo.forEach(items, setHp);
		}

		supervisorTab.dataStore.fetch({
			query:{"type":"queue"},
			onComplete:setHps
		})
	}

	supervisorTab.setAgentHps = function(){
		var setHp = function(item){
			var healthobj = supervisorTab.dataStore.getValue(item, "health");
			var hpsvars = [];
			for(var i in healthobj){
				hpsvars.push(healthobj[i]);
			}
			
			var gotMedia = function(mitems){
				if(mitems.length > 0){
					hpsvars.push(supervisorTab.dataStore.getValue(mitems[0], "aggregate"));
				}
				debug(["setAgentHps wants averaged:", hpsvars]);
				var hp = supervisorTab.averageHp(hpsvars);

				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
				var rawobj = {
					"id":supervisorTab.dataStore.getValue(item, "id"),
					"display":supervisorTab.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"agent",
					"health":{},
					"details":supervisorTab.dataStore.getValue(item, "details")
				};
				dojo.publish("supervisortab/set/" + rawobj.id, [item, rawobj]);
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"media",
					"agent":supervisorTab.dataStore.getValue(item, "display")
				},
				onComplete:function(got){
					gotMedia(got);
					dojo.publish("supervisortab/aggregate/agents", []);
				}
			});
		}
		
		var setHps = function(items){
			info(["Fetch for setAgentHps done", items]);
			dojo.forEach(items, setHp);
		}
		
		supervisorTab.dataStore.fetch({
			query:{"type":"agent"},
			onComplete:setHps
		})
	}

	supervisorTab.setQueueGroupHps = function(){
		var setHp = function(item){
			var hpsvars = [];
			var gotQueues = function(items){
				dojo.forEach(items, function(i){
					hpsvars.push(supervisorTab.dataStore.getValue(i, "aggregate"));
				});
				info(["setQueueGroupHps wants averaged:  ", hpsvars]);
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
				var rawobj = {
					"id":"queuegroup-" + supervisorTab.dataStore.getValue(item, "display"),
					"display":supervisorTab.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"queuegroup",
					"health":{},
					"details":{}
				};
				dojo.publish("supervisortab/set/queuegroup-" + rawobj.display, [item, rawobj]);
				
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"queue",
					"group":supervisorTab.dataStore.getValue(item, "display"),
					"node":supervisorTab.node
				},
				onComplete:function(got){
					gotQueues(got);
					dojo.publish("supervisortab/aggregate/queuegroups", []);
				}
			});
		}

		var setHps = function(items){
			info(["setQueueGroupHps fetch done", items]);
			dojo.forEach(items, setHp);
		}
		
		supervisorTab.dataStore.fetch({
			query:{"type":"queuegroup"},
			onComplete:setHps
		})
	}

	supervisorTab.setAgentProfileHps = function(){
		var setHp = function(item){
			var hpsvars = [];
			var gotAgents = function(aitems){
				dojo.forEach(aitems, function(i){
					hpsvars.push(supervisorTab.dataStore.getValue(i, "aggregate"));
				});
				debug(["setAGentProfileHps is averaging:", hpsvars]);
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
				var rawobj = {
					"id":"agentprofile-" + supervisorTab.dataStore.getValue(item, "display"),
					"display":supervisorTab.dataStore.getValue(item, "display"),
					"aggregate":hp,
					"type":"agentprofile",
					"health":{},
					"details":{}
				};
				dojo.publish("supervisortab/set/agentprofile-" + rawobj.display, [item, rawobj]);
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"agent",
					"profile":supervisorTab.dataStore.getValue(item, "display"),
					"node":supervisorTab.node
				},
				onComplete:function(got){
					gotAgents(got);
					dojo.publish("supervisortab/aggregate/agentprofiles", []);
				}
			});
		}
		
		var setHps = function(items){
			info(["setAgateProfileHps fetch done", items]);
			dojo.forEach(items, setHp);
		}
		
		supervisorTab.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHps
		})
	}

	supervisorTab.setGlobalAgentHp = function(){
		var setHp = function(items){
			info(["setGlobalAgentHp fetch done", items]);
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorTab.dataStore.getValue(item, "aggregate"));
			});
			info(["setGlobalAgentHp wants averaged:", hplist]);
			var hp = supervisorTab.averageHp(hplist);
			supervisorTab.agentBubble.setHp(hp);
		}
		supervisorTab.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHp
		});
	}

	supervisorTab.setGlobalQueueHp = function(){
		var setHp = function(items){
			info(["setGlobalQueueHp fetch done", items]);
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorTab.dataStore.getValue(item, "aggregate"));
			});
			debug(["setGlobalQueueHp wants averaged:", hplist]);
			var hp = supervisorTab.averageHp(hplist);
			supervisorTab.queueBubble.setHp(hp);
		}
		supervisorTab.dataStore.fetch({
			query:{"type":"queuegroup"},
			onComplete:setHp
		})
	}

	supervisorTab.setNodeHps = function(){
		var gotNodes = function(items){
			info(["setNodeHps fetch done", items]);
			dojo.forEach(items, function(item){
				var gotNodeItems = function(nitems){
					var hplist = [];
					dojo.forEach(nitems, function(nitem){
						hplist.push(supervisorTab.dataStore.getValue(nitem, "aggregate"));
					});
					var hpobj = supervisorTab.dataStore.getValue(item, "health");
					for(var i in hpobj){
						hplist.push(hpobj[i]);
					}
					debug(["setNodeHps wants averaged", hplist]);
					var hp = supervisorTab.averageHp(hplist);
					supervisorTab.dataStore.setValue(item, "aggregate", hp);
					var rawobj = {
						"id":supervisorTab.dataStore.getValue(item, "id"),
						"display":supervisorTab.dataStore.getValue(item, "display"),
						"aggregate":hp,
						"type":"node",
						"health":{},
						"details":supervisorTab.dataStore.getValue(item, "details")
					};
					supervisorTab.dataStore.save();
					dojo.publish("supervisortab/set/" + rawobj.id, [item, rawobj]);
				}
				supervisorTab.dataStore.save();
				//supervisorTab.refreshSystemStack();
				supervisorTab.dataStore.fetch({
					query:{node:supervisorTab.dataStore.getValue(item, "display")},
					onComplete:gotNodeItems
				});
			});
		}
		
		supervisorTab.dataStore.fetch({
			query:{type:"node"},
			onComplete:gotNodes
		});
	}

	supervisorTab.setSystemHps = function(){
		var gotNodes = function(items){
			info(["setSystemHps fetch done", items]);
			var hps = [];
			dojo.forEach(items, function(item){
				hps.push(supervisorTab.dataStore.getValue(item, "aggregate"));
			});
			var hp = supervisorTab.averageHp(hps);
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
				supervisorTab.dataStore.setValue(items[0], "aggreagate", hp);
				supervisorTab.dataStore.save();
				dojo.publish("supervisortab/set/system-System", [items[0], rawobj]);
			}
			supervisorTab.dataStore.fetch({
				query:{"type":"system"},
				onComplete:gotSystem
			});
		}
		
		supervisorTab.dataStore.fetch({
			query:{"type":"node"},
			onComplete:gotNodes
		});
	}
	
	supervisorTab.setAllHps = function(){
		info(["setAllHps..."]);
		supervisorTab.setMediaHps();
		supervisorTab.setQueueHps();
		supervisorTab.setAgentHps();
		supervisorTab.setQueueGroupHps();
		supervisorTab.setAgentProfileHps();
		supervisorTab.setGlobalAgentHp();
		supervisorTab.setGlobalQueueHp();
		supervisorTab.setNodeHps();
		supervisorTab.setSystemHps();
	}

	supervisorTab.bubbleZoom = function(ev){
		var rect = this.children[0];
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		};
		this.setTransform([dojox.gfx.matrix.scaleAt(1.4, p)]);
	};

	supervisorTab.bubbleOut = function(ev){
		var rect = this.children[0];
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		}
		this.setTransform([dojox.gfx.matrix.scaleAt(1, p)]);
	};

	supervisorTab.setDetails = function(fquery){
		var fetchdone = function(item){
			var obj = supervisorTab.dataStore.getValue(item, "details");
			var out = "";
			if(supervisorTab.dataStore.getValue(item, "node")){
				out += "<p class=\"smaller\"><label class=\"narrow\">Node:</label>" + supervisorTab.dataStore.getValue(item, "node");
			}
			for(var i in obj){
				out += "<p class=\"smaller\"><label class=\"narrow\">" + i + ":</label>" + obj[i].toString() + "</p>";
			}
			out += "<p>Health Report</p>";
			var hps = supervisorTab.dataStore.getValue(item, "health");
			for(var i in hps){
				var sigdigited = Math.floor(supervisorTab.averageHp([hps[i]]) * 100) / 100;
				out += "<p class=\"smaller\"><label class=\"narrow\">" + i + ":</label>" + sigdigited.toString() + "</p>";
			}
			dijit.byId("supervisorDetails").setContent(out);
			dijit.byId("supervisorDetails").setTitle(supervisorTab.dataStore.getValue(item, "type") + ": " + supervisorTab.dataStore.getValue(item, "display"));
		}
		
		supervisorTab.dataStore.fetch({
				query:fquery,
				onItem:fetchdone
		});
	}

	supervisorTab.drawSystemStack = function(opts){
		for(var i =0; i < supervisorTab.systemStack.length; i++){
			supervisorTab.systemStack[i].clear();
		}

		var conf = {
			data: [],
			parent: supervisorTab.surface
		}

		conf = dojo.mixin(conf, opts);
		info(["685", "drawing system stack from data", conf.data]);
		var hps = [];
		for(var i = 0; i < conf.data.length; i++){
			hps.push(conf.data[i].aggregate);
		}
		debug(["drawSystemStack wants averaged", hps]);
		conf.data.unshift({"display":"System", "id":"system-System", "type":"system", "health":supervisorTab.averageHp(hps)});

		var yi = 385;
		out = [];
		var drawIt = function(obj, index, arr){
			var o = supervisorTab.drawBubble({
				scale:.75,
				point: {x:20, y:yi},
				data: obj
			});
			if(supervisorTab.node == "*" && obj.display == "System"){
				o.grow();
			}
			else if(supervisorTab.node == obj.display){
				o.grow();
			}
			
			o.lines = [];
			if(obj.allhealth && obj.allhealth.down){
				o.lines.push(o.createLine({x1:20, x2:60, y1:yi-5, y2:yi+20}).setStroke({width:3, color:[255, 153, 51, 100]}));
				o.lines.push(o.createLine({x1:20, x2:60, y1:yi + 20, y2:yi-5}).setStroke({width:3, color:[255, 153, 51, 100]}));
			}
			else{
				o.connect("onclick", o, function(ev){
					if(this.data.display == "System"){
						supervisorTab.node = "*";
					}
					else{
						supervisorTab.node = this.data.display
					}
					dojo.forEach(supervisorTab.systemStack, function(obj){
						obj.shrink();
					});
					this.grow();
				});
			}
			o.connect("onmouseenter", o, function(ev){				
				supervisorTab.setDetails({display:this.data.display});
				dijit.byId("nodeAction").nodeBubbleHit = this.data.display;
			});
			
			if(obj.display == "System"){
				o.subscriptions.push(dojo.subscribe("supervisortab/set/system-System", function(storeref, rawobj){
					debug(["System set sub", storeref, rawobj]);
					o.setHp(rawobj.aggregate);
				}));
			}
			else{
				o.subscriptions.push(dojo.subscribe("supervisortab/set/node-" + obj.display, function(storeref, rawobj){
					debug(["node set sub", storeref, rawobj]);
					o.setHp(rawobj.aggregate);
					for(var i = 0; i < o.lines.length; i++){
						o.lines[i].clear();
					}
					o.lines = [];								
					if(rawobj.health.down){
						o.lines = [];
						o.lines.push(o.createLine({x1:20, x2:60, y1:yi-5, y2:yi+20}).setStroke({width:3, color:[255, 153, 51, 100]}));
						o.lines.push(o.createLine({x1:20, x2:60, y1:yi + 20, y2:yi-5}).setStroke({width:3, color:[255, 153, 51, 100]}));
					}
				}));
				if(obj.display != "System"){
					dijit.byId("nodeAction").bindDomNode(o.rawNode);
				}
			}
			yi = yi - 40;
			out.push(o);
		}
		dojo.forEach(conf.data, drawIt);
		supervisorTab.systemStack = out;
		return out;
	}

	supervisorTab.drawAgentQueueBubbles = function(agenthp, queuehp){
		var clearStacks = function(){
			var clearables = [
				"queueGroupsStack",
				"queuesStack",
				"agentProfilesStack",
				"agentsStack",
				"callsStack"
			]
			
			for(var i in clearables){
				if(supervisorTab[clearables[i]].clear){
					supervisorTab[clearables[i]].clear();
				}
			}
		}
		
		supervisorTab.agentBubble = new supervisorTab.Bubble({
			point:{x:20, y:20},
			scale: .75,
			data: {"health":agenthp, "display":"Agents"},
			onclick:function(){
				clearStacks();
				supervisorTab.drawAgentProfilesStack();
				this.size(1);
				supervisorTab.queueBubble.size(.75);
			}
		});
		supervisorTab.agentBubble.dragOver = supervisorTab.agentBubble.onclick;
		
		supervisorTab.queueBubble = new supervisorTab.Bubble({
			point:{x:20, y:60},
			scale: .75,
			data:{"health":queuehp, "display":"Queues"},
			onclick: function(){
				clearStacks();
				supervisorTab.drawQueueGroupsStack();
				this.size(1);
				supervisorTab.agentBubble.size(.75);
			}
		});
		
		supervisorTab.queueBubble.dragOver = supervisorTab.queueBubble.onclick;
	}

	supervisorTab.sizeFromDistance = function(distance){
		// y = -.00003x^2 + 1.4 if you quad
		// linear is a bit faster, though.
		// y = -(.65/50)x + 1.4
		if(Math.abs(distance) > 100){
			return .75;
		}
		if(Math.abs(distance) < 10){
			return 1.4;
		}
		
		var scale = -Math.abs(.013 * distance) + 1.4;
		if(scale < .75){
			return .75;
		}
		
		return scale;
	}

	supervisorTab.drawQueueGroupsStack = function(){
		if(supervisorTab.queueGroupsStack && supervisorTab.queuesStack.scrollLocked){
			warning(["queueGroupsstack exists but is scroll-locked"]);
			return false;
		}
		
		if(supervisorTab.agentsStack && supervisorTab.agentsStack.scrollLocked){
			warning(["agent profiles stack exists but is scroll-locked"]);
			return false;
		}
		
		//supervisorTab.queuesStack.clear();
		
		var fetchdone = function(items, request){
			//warning(["fetchdone"]);
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				acc.push({
					data:{
						display:supervisorTab.dataStore.getValue(obj, "display"),
						health:supervisorTab.dataStore.getValue(obj, "aggregate", 50)
					},
					onmouseenter:function(ev){
						supervisorTab.drawQueuesStack(this.data.display, supervisorTab.node, acc.length);
						supervisorTab.setDetails({
							type:supervisorTab.dataStore.getValue(obj, "type"),
							display:supervisorTab.dataStore.getValue(obj, "display")
						});
					}
				});
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			});
			
			supervisorTab.callsStack.clear();
			//warning(["calls stack cleared"]);
			
			/*supervisorTab.queueGroupsStack.clear();
			supervisorTab.queuesStack.clear();
			supervisorTab.agentProfilesStack.clear();*/
			supervisorTab.queueGroupsStack = new supervisorTab.BubbleStack({
				mousept:{
					x:300,
					y:100
				},
				bubbleConfs:acc
			});

			supervisorTab.queueGroupsStack.forEachBubble(function(bubble){
				var chan = "supervisortab/set/queuegroup-" + bubble.data.display;
				bubble.subscriptions.push(dojo.subscribe(chan, function(storeref, rawobj){
					bubble.setHp(rawobj.aggregate);
				}));
			});
			
			supervisorTab.queueGroupsStack._scroll(0);
			supervisorTab.queueGroupsStack.group.moveToBack();
		}
		
		supervisorTab.dataStore.fetch({
			query:{
				type:"queuegroup",
			},
			onComplete:fetchdone
		});
	}

	supervisorTab.drawQueuesStack = function(group, node, scrollIndex){
		if(supervisorTab.queuesStack.scrollLocked){
			warning(["queues stack scroll lacked"]);
			return false;
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate", 50));
				acc.push({
					data:{
						display:supervisorTab.dataStore.getValue(obj, "display"),
						health:supervisorTab.dataStore.getValue(obj, "aggregate", 50)
					},
					onmouseenter:function(ev){
						var queryObj = {
							'queue':this.data.display,
							'node':supervisorTab.node
						};
						supervisorTab.drawCallStack(queryObj, acc.length);
						supervisorTab.setDetails({
							type:"queue",
							display:supervisorTab.dataStore.getValue(obj, "display")
						});
					}
				});
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			});
			
			supervisorTab.callsStack.clear();
			supervisorTab.queuesStack.clear();
			
			supervisorTab.queuesStack = new supervisorTab.BubbleStack({
				mousept:{
					x:540,
					y:100
				},
				bubbleConfs: acc
			});
			
			supervisorTab.queuesStack.forEachBubble(function(bub){
				bub.subscriptions.push(dojo.subscribe("supervisortab/set/queue-" + detailsObj.display, function(storeref, rawobj){
					bub.setHp(rawobj.aggregate);
				}));
				bub.subscriptions.push(dojo.subscribe("supervisortab/drop/queue-" + detailsObj.display, function(storeref, rawobj){
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
			});
			
			supervisorTab.queuesStack._scroll(supervisorTab.queuesStack.indexToY(scrollIndex));
			supervisorTab.queuesStack.group.moveToBack();
		}
		
		var queryo = {
			type:"queue",
			'group':group,
			node:node
		};
		
		supervisorTab.dataStore.fetch({
			query:queryo,
			onComplete:fetchdone
		});
	
		return true;
	}
	
	supervisorTab.drawAgentProfilesStack = function(){
		if(supervisorTab.agentProfilesStack && supervisorTab.agentProfilesStack.scrollLocked){
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
						display:supervisorTab.dataStore.getValue(obj, "display"),
						health:supervisorTab.dataStore.getValue(obj, "aggregate", 50)
					},
					onmouseenter:function(ev){
						supervisorTab.drawAgentsStack(this.data.display, supervisorTab.node, acc.length);
						supervisorTab.setDetails({
							type:supervisorTab.dataStore.getValue(obj, "type"),
							display:supervisorTab.dataStore.getValue(obj, "display")
						});
					}
				});
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			});

			supervisorTab.callsStack.clear();
			//warning(["calls stack cleared"]);
			supervisorTab.queueGroupsStack.clear();
			supervisorTab.queuesStack.clear();
			supervisorTab.agentProfilesStack = new supervisorTab.BubbleStack({
				mousept:{
					x:300,
					y:100
				},
				bubbleConfs:acc
			});
	
			supervisorTab.queueGroupsStack.forEachBubble(function(bubble){
				var chan = "supervisortab/set/agentprofile-" + bubble.data.display;
					bubble.subscriptions.push(dojo.subscribe(chan, function(storeref, rawobj){
					bubble.setHp(rawobj.aggregate);
				}));
			});

			supervisorTab.agentProfilesStack._scroll(0);
			supervisorTab.agentProfilesStack.group.moveToBack();
		}

		supervisorTab.dataStore.fetch({
			query:{
				type:"agentprofile",
			},
			onComplete:fetchdone
		});

		return true;
	}
	
	supervisorTab.drawAgentsStack = function(profile, node, scrollIndex){
		if(supervisorTab.agentsStack.scrollLocked){
			warning(["agents stack scroll lacked"]);
			return false;
		}

		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate", 50));
				acc.push({
					data:{
						display:supervisorTab.dataStore.getValue(obj, "display"),
						health:supervisorTab.dataStore.getValue(obj, "aggregate", 50)
					},
					onmouseenter:function(ev){
						var queryObj = {
							'agent':this.data.display,
							'node':supervisorTab.node
						};
						supervisorTab.drawCallStack(queryObj);
						supervisorTab.setDetails({
							type:"agent",
							display:supervisorTab.dataStore.getValue(obj, "display")
						});
					}
				});
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			});

			supervisorTab.callsStack.clear();
			supervisorTab.agentsStack.clear();
			
			supervisorTab.agentsStack = new supervisorTab.BubbleStack({
				mousept:{
					x:540,
					y:100
				},
				bubbleConfs: acc
			});

			supervisorTab.agentsStack.forEachBubble(function(bub){
				bub.subscriptions.push(dojo.subscribe("supervisortab/set/agent-" + detailsObj.display, function(storeref, rawobj){
					bub.setHp(rawobj.aggregate);
				}));
				bub.subscriptions.push(dojo.subscribe("supervisortab/drop/agent-" + detailsObj.display, function(storeref, rawobj){
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
			
			supervisorTab.agentsStack._scroll(supervisorTab.agentsStack.indexToY(scrollIndex));
		}

		var queryo = {
			type:"agent",
			'profile':profile,
			'node':node
		};

		supervisorTab.dataStore.fetch({
			query:queryo,
			onComplete:fetchdone
		});

		return true;
	}

	supervisorTab.drawCallStack = function(queryObj, scrollIndex){
		if(supervisorTab.callsStack.scrollLocked){
			return false;
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			dojo.forEach(items, function(obj){
				var datas = {
					display:supervisorTab.dataStore.getValue(obj, "display"),
					health:supervisorTab.dataStore.getValue(obj, "aggregate", 50),
					type:"media"
				};
				
				if(supervisorTab.dataStore.getValue(obj, "agent")){
					datas['agent'] = supervisorTab.dataStore.getValue(obj, "agent");
				}
				else{
					datas['queue'] = supervisorTab.dataStore.getValue(obj, "queue");
				}
				
				acc.push({
					data:datas,
					onmouseenter:function(ev){
						supervisorTab.setDetails({
							type:"media",
							display:supervisorTab.dataStore.getValue(obj, "display")
						});
					}
				});
			});
			
			supervisorTab.callsStack.clear();
			supervisorTab.callsStack = new supervisorTab.BubbleStack({
				mousept:{
					x:580 + 240,
					y:100
				},
				bubbleConfs:acc
			});
		
			supervisorTab.callsStack.forEachBubble(function(obj){
				obj.group.connect("onmousedown", obj, function(ev){
					supervisorTab.dndManager.startDrag(obj);
				});
				obj.group.connect("onmouseup", obj, function(ev){
					supervisorTab.dndManager.endDrag()
				});
				
				var nom = obj.data.display;
				obj.subscriptions.push(dojo.subscribe("supervisortab/set/media-" + nom, function(storeref, rawobj){
					obj.setHp(rawobj.aggregate);
				}));
				obj.subscriptions.push(dojo.subscribe("supervisortab/drop/media-" + nom, function(storeref, rawobj){
					obj.clear();
				}));
			});
			
			supervisorTab.callsStack._scroll(supervisorTab.callsStack.indexToY(scrollIndex));
		}

		supervisorTab.dataStore.fetch({
			query:queryObj,
			onComplete:fetchdone
		});
	
		return true;
	}
	
	supervisorTab.drawSystemStack = function(){
		return true;
	}

/*

	supervisorTab.IndividualStackAsAgents = function(items, groupingValue, node){
		var acc = [];
		var hps = [];
		dojo.forEach(items, function(obj){
			hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			
			var detailsObj = {
				display:supervisorTab.dataStore.getValue(obj, "display"),
				type:supervisorTab.dataStore.getValue(obj, "type")
			};
			
			var onEnterf = function(){
				supervisorTab.refreshCallsStack("agent", detailsObj.display, supervisorTab.node);
				supervisorTab.setDetails(detailsObj);
			}
					 
			var bub = supervisorTab.individualsStack.addBubble({
				data:{
					display:supervisorTab.dataStore.getValue(obj, "display"), 
					health:supervisorTab.dataStore.getValue(obj, "aggregate")
				},
				onmouseenter:function(){
					onEnterf();
					dijit.byId("agentAction").agentBubbleHit = detailsObj.display;
				}
			});
			var details = supervisorTab.dataStore.getValue(obj, "details");
			var imgsrc = "images/";
			switch(details.state){
				case "idle":
					imgsrc += "idle.png";
					break;
				
				case "oncall":
					imgsrc += "oncall.png";
					break;
				
				case "wrapup":
					imgsrc += "wrapup.png";
					break;
				
				case "ringing":
					imgsrc += "ringing.png";
					break;
				
				default:
					imgsrc += "released.png"
			}
			
			var icon = bub.createImage({
				src:imgsrc,
				width:14,
				height:14,
				x:bub.bubble.getShape().x,
				y:bub.bubble.getShape().y
			});
			
			bub.icon = icon;
			
			bub.subscriptions.push(dojo.subscribe("supervisortab/set/" + supervisorTab.dataStore.getValue(obj, "id"), function(storeref, dataobj){
				var imgsrc = "images/";
				switch(supervisorTab.dataStore.getValue(storeref, "details").state){
					case "idle":
						imgsrc += "idle.png";
						break;

					case "oncall":
						imgsrc += "oncall.png";
						break;

					case "wrapup":
						imgsrc += "wrapup.png";
						break;

					case "ringing":
						imgsrc += "ringing.png";
						break;

					default:
						imgsrc += "released.png"
				}
				bub.icon.setShape({src:imgsrc});
				bub.setHp(dataobj.aggregate);
			}));
			bub.subscriptions.push(dojo.subscribe("supervisortab/drop/" + supervisorTab.dataStore.getValue(obj, "id"), function(storeref, dataobj){
				bub.clear();
			}));
			var message = "agent " + bub.data.display + " accepted drop, meaning it forwared request to server";
			bub.dropped = function(obj){
				debug("dropped called");
				if(obj.data.type == "media"){
					debug(["bub.dropped, 1133", message, obj.data]);
					if(obj.data.agent){
						var ajaxdone = function(json, args){
							// la la la
							debug("1137, ajax done");
						}
						var geturl = "/supervisor/agent_transfer/" + escape(obj.data.agent) + "/" + escape(bub.data.display);
						dojo.xhrGet({
							url:geturl,
							handleAs:"json",
							load:ajaxdone
						})
					}
					else if(obj.data.queue){
						var ajaxdone = function(json, args){
							debug(["1148, ajax done", json.message]);
						}
						var geturl = "/supervisor/agent_ring/" + escape(obj.data.queue) + "/" + escape(obj.data.display) + "/" + escape(bub.data.display);
						dojo.xhrGet({
							url:geturl,
							handleAs:"json",
							load:ajaxdone
						})
					}
				}
			}
			bub.onEnter = onEnterf;
			
			if(bub.data.display != "All"){
				bub.dragOver = function(){
					bub.onEnter();
					return true;
				};
				dijit.byId("agentAction").bindDomNode(bub.rawNode);
			}				 
		});
		
		dijit.byId("agentAction").onClose = function(ev){
			supervisorTab.individualsStack.scrollLocked = false;
		};
		debug(["Individual Stacks as agents averging", hps]);
		var allbub = supervisorTab.individualsStack.addBubble({
			data:{
				display:"All",
				health:supervisorTab.averageHp(hps)
			},
			onmouseenter:function(ev){
				supervisorTab.refreshCallsStack("agent", "*", supervisorTab.node);
			}
		});
		allbub.subscriptions.push(dojo.subscribe("supervisortab/aggregate/agents", function(){
			var fetched = function(items){
				var hps = [];
				dojo.forEach(items, function(item){
					hps.push(supervisorTab.dataStore.getValue(item, "aggregate"));
				});
				var hp = supervisorTab.averageHp(hps);
				allbub.setHp(hp);
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"agent",
					"node":node,
					"profile":groupingValue
				},
				onComplete:fetched
			});
		}));
	}

	

	supervisorTab.refreshSystemStack = function(){
		debug("refreshing system stack");
		var fetchdone = function(items, request){
			info(["refreshSystemStack fetch done", items]);
			var acc = [];
			dojo.forEach(items, function(item){
				var temphp = supervisorTab.dataStore.getValue(item, "health");
				var hps = [];
				for(var i in temphp){
					hps.push(temphp[i]);
				}
				debug(["refresh system stack averaging", hps]);
				var aggregate = supervisorTab.averageHp(hps);
				var out = {
					id:supervisorTab.dataStore.getValue(item, "id"),
					type:supervisorTab.dataStore.getValue(item, "type"),
					display:supervisorTab.dataStore.getValue(item, "display"),
					"aggregate":aggregate,
					health:aggregate
				}
				acc.push(out);
			});
		
			dojo.forEach(supervisorTab.systemStack, function(obj){
				obj.clear();
			});
			debug(["fetchdone.  Acc:", acc]);
			supervisorTab.systemStack = supervisorTab.drawSystemStack({data:acc});
		}
		
		supervisorTab.dataStore.fetch({
			query:{
				type:"node"
			},
			onComplete:fetchdone
		});
	}*/

	supervisorTab.healthDump = function(){
		var dump = function(items){
			dojo.forEach(items, function(item){
				var out = supervisorTab.dataStore.getValue(item, "aggregate");
				var nom = supervisorTab.dataStore.getValue(item, "display");
				debug([nom, out]);
			})
		}
		
		supervisorTab.dataStore.fetch({
			onComplete:dump
		});
	}

	supervisorTab.reloadDataStore = function(){
		dojo.xhrGet({
			url:"/supervisor/status",
			handleAs:"json",
			error:function(){
				supervisorTab.poller.stop();
			},
			load:function(data){
				if(data.data){
					info(["1400", "store reload ajax completed"]);
					supervisorTab.healthData = data.data;
					supervisorTab.dataStore = new dojo.data.ItemFileWriteStore({
						data: supervisorTab.healthData,
						typeMap:{
							"details":{
								"type":Object,
								"deserialize":function(obj){return obj},
								"serialize":function(obj){return obj}
							}
						}
					});
					supervisorTab.refreshSystemStack();
					supervisorTab.setAllHps();
					if(supervisorTab.suppressPoll){
						return;
					}
				}
				else{
					debug(["1422", "stub for no data.data", data]);
				}
			}
		})
	}

	supervisorTab.blab = function(message, type, target){
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
	
	supervisorTab.setProfile = function(profile, agent){
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
}

supervisorTab.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
dojo.connect(supervisorTab.surface, "ondragstart",   dojo, "stopEvent");
dojo.connect(supervisorTab.surface, "onselectstart", dojo, "stopEvent");

supervisorTab.reloadDataStore();

supervisorTab.drawAgentQueueBubbles(0, 0);

/*supervisorTab.masterSub = dojo.subscribe("agent/supervisortab", function(data){
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
				aggregate = supervisorTab.averageHp(hps);
			}
			savedata.aggregate = aggregate;
			//course, there may be no item yet...
			if(items.length == 0){
				items.push(supervisorTab.dataStore.newItem(savedata));
			}
			for(var i in savedata){
				if(i != "id"){
					supervisorTab.dataStore.setValue(items[0], i, savedata[i]);
				}
			}
			supervisorTab.dataStore.save();
			debug(["1488", "savedata", savedata]);
			dojo.publish("supervisortab/set/" + savedata.id, [items[0], savedata]);
			supervisorTab.setAllHps();
		}
	
		supervisorTab.dataStore.fetch({
			query:{"id":data.id},
			onComplete:fetched
		});
	}
	else if(data.action == "drop"){
		var fetched = function(items){
			supervisorTab.dataStore.deleteItem(items[0]);
			dojo.publish("supervisortab/drop/" + data.id, [data]);
		}
		
		supervisorTab.dataStore.fetch({
			query:{"id":data.id},
			onComplete:fetched
		});
	}
	else{
		// la la la
	}
});

supervisorTab.hpCalcTimer = '';
supervisorTab.hpcalc = function(){
	supervisorTab.hpCalcTimer = setTimeout(function(){
		supervisorTab.setAllHps();
		supervisorTab.hpcalc()}, 5000);
}

supervisorTab.logoutListener = dojo.subscribe("agent/logout", function(data){
	clearTimeout(supervisorTab.hpCalcTimer)
});

window.supervisorTabKillListen = dojo.subscribe("tabPanel-removeChild", function(child){
	if(child.title == "Supervisor"){
		clearTimeout(supervisorTab.hpCalcTimer);
		dojo.unsubscribe(window.supervisorTabKillListen);
		dojo.unsubscribe(supervisorTab.logoutListener);
		dojo.unsubscribe(supervisorTab.masterSub);
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
});

supervisorTab.hpcalc();*/