
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
	supervisorTab.groupsStack = {clear:function(){ return true}};
	supervisorTab.individualsStack = {clear:function(){ return true}};
	supervisorTab.callsStack = {clear:function(){return true}};
	supervisorTab.suppressPoll = false;

	supervisorTab.dndManager = {
		_dragging: null,
		_eventHandle:null,
		_dropCandidate: null,
		checkCollision:function(pt){
			////console.log(pt);
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
				////console.log(ev);
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

	supervisorTab.averageHp = function(hps){
		if(hps.length == 0){
			return 50;
		}
		
		//console.log("averaging hps");
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
			//console.log(["averageing hp obj", obj]);
			if(typeof(obj) == "number"){
				//console.log("number");
				var c = findweight(obj);
				count += c;
				total += c * obj;
			}
			else if(obj.health != undefined){
				//console.log("health");
				var c = findweight(obj.health);
				count += c;
				total += c * obj.health;
			}
			else if(obj.goal != undefined){
				//console.log("goal");
				var value = 0;
				if(obj.time){
					//console.log("time");
					value = Math.floor(new Date().getTime()/1000) - obj.time;
				}
				else{
					//console.log("value");
					value = obj.value;
				}
				//console.log(["value", value]);
				var hp = 50;
				if(obj.goal == value){
					//console.log("goal match");
					var c = findweight(50);
					count += c;
					total += c * 50;
				}
				else if( (obj.min < obj.max) && (obj.max <= value) ){
					//console.log("min < max < value");
					var c = findweight(100);
					count += c;
					total += c * 100;
				}
				else if( ( obj.min < obj.max) && (value <= obj.min) ){
					//console.log("value < min < max");
					var c = findweight(0);
					count += c;
					total += c * 0;
				}
				else if( (obj.max < obj.min) && (value <= obj.max) ){
					//console.log("value < max < min");
					var c = findweight(100);
					count += c;
					total += c * 100;
				}
				else if( (obj.max < obj.min) && (obj.min <= value) ){
					//console.log("max < min < value");
					var c = findweight(0);
					count += c;
					total += c * 0;
				}
				else if( (obj.min < obj.max) && ( value < obj.goal) ){
					//console.log("min < value < goal"); 
					var hp = (50 * value - 50 * obj.min) / (obj.goal - obj.min);
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.min < obj.max) && ( obj.goal < value)){
					//console.log("min < goal < value");
					var hp = (100 * obj.goal - 50 * obj.max - 50 * value) / (obj.goal - obj.max);
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.max < obj.min) && (value < obj.goal) ){
					//console.log("max < value < goal");
					var hp = (50 * (2 * obj.goal - value - obj.max) ) / (obj.goal - obj.max);
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else if( (obj.max < obj.min) && (obj.goal < value) ){
					//console.log("everything else");
					var hp = ( ( 50 * ( obj.goal - value) ) / (obj.min - obj.goal) ) + 50;
					var c = findweight(hp);
					count += c;
					total += c * hp;
				}
				else{
					//console.log("very confused...");
				}				
			}
			else{
				//console.log(["Not number, .health, or .goal", obj]);
			}
		}
		
		dojo.forEach(hps, f);
			
		return Math.round(total / count);
	}

	supervisorTab.setMediaHps = function(){
		var setHps = function(items){
			var setHp = function(obj, ind, arr){
				var healthobj = supervisorTab.dataStore.getValue(obj, "health");
				var hpsvars = [];
				for(var i in healthobj){
					hpsvars.push(healthobj[i]);
				}
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(obj, "aggregate", hp);
			}
			dojo.forEach(items, setHp);
			supervisorTab.dataStore.save();
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
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					type:"media",
					queue:supervisorTab.dataStore.getValue(item, "display"),
					node:supervisorTab.node
				},
				onComplete:gotMedia
			});
		}

		var setHps = function(items){
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
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"media",
					"agent":supervisorTab.dataStore.getValue(item, "display")
				},
				onComplete:gotMedia
			});
		}
		
		var setHps = function(items){
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
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"queue",
					"group":supervisorTab.dataStore.getValue(item, "display"),
					"node":supervisorTab.node
				},
				onComplete:gotQueues
			});
		}

		var setHps = function(items){
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
				var hp = supervisorTab.averageHp(hpsvars);
				supervisorTab.dataStore.setValue(item, "aggregate", hp);
				supervisorTab.dataStore.save();
			}
			
			supervisorTab.dataStore.fetch({
				query:{
					"type":"agent",
					"profile":supervisorTab.dataStore.getValue(item, "display"),
					"node":supervisorTab.node
				},
				onComplete:gotAgents
			});
		}
		
		var setHps = function(items){
			dojo.forEach(items, setHp);
		}
		
		supervisorTab.dataStore.fetch({
			query:{"type":"agentprofile"},
			onComplete:setHps
		})
	}

	supervisorTab.setGlobalAgentHp = function(){
		var setHp = function(items){
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorTab.dataStore.getValue(item, "aggregate"));
			});
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
			var hplist = [];
			dojo.forEach(items, function(item){
				hplist.push(supervisorTab.dataStore.getValue(item, "aggregate"));
			});
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
					var hp = supervisorTab.averageHp(hplist);
					supervisorTab.dataStore.setValue(item, "aggregate", hp);
				}
				supervisorTab.dataStore.save();
				
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

	supervisorTab.setAllHps = function(){
		supervisorTab.setMediaHps();
		supervisorTab.setQueueHps();
		supervisorTab.setAgentHps();
		supervisorTab.setQueueGroupHps();
		supervisorTab.setAgentProfileHps();
		supervisorTab.setGlobalAgentHp();
		supervisorTab.setGlobalQueueHp();
		supervisorTab.setNodeHps();	
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

	supervisorTab.drawBubble = function(opts){
		var conf = {
			scale:1,
			point:{x:100,y:100},
			parent:supervisorTab.surface,
			data:{"display":"bubble", "health":50, "type":"text"},
			onmouseenter:function(){ return false},
			onmouseleave:function(){return false},
			onclick:function(){ return false}
		};
		
		conf = dojo.mixin(conf, opts);
		
		if(conf.scale > 2){
			conf.scale = 2;
		}
		else if(conf.scale < .75){
			conf.scale = .75
		}
		
		var group = conf.parent.createGroup();
			
		var rmod = Math.abs(-(255/50) * conf.data.health + 255);
		var gmod = 255;
		var textcolors = "black";
		if(conf.data.health > 50){
			var gmod = -255/50 * 1.3 * conf.data.health + (255 * 2);
			textcolors = "white";
		}
		var bubblefill = [rmod, gmod, 0, 100];

		group.hpline = group.createLine({
			x1: conf.point.x + (conf.data.health * 2) * conf.scale,
			x2: conf.point.x + (conf.data.health * 2) * conf.scale,
			y1: conf.point.y - 3,
			y2: conf.point.y + 3 + 20 * conf.scale
		}).setStroke({width:7, color:"black", cap:"round"});
		
		group.bubble = group.createRect({
			x: conf.point.x,
			y: conf.point.y,
			width: 200 * conf.scale,
			height: 20 * conf.scale,
			r: 10 * conf.scale
		}).setFill(bubblefill).setStroke("black");
		
		group.data = conf.data;
		
		group.connect("onmouseenter", group, conf.onmouseenter);
		group.connect("onmouseleave", group, conf.onmouseleave);
		group.connect("onclick", group, conf.onclick);
		
		var textdisp = conf.data.display;
		if(textdisp.length > 18){
			textdisp = textdisp.substring(0, 16) + "...";
		}
		
		var text = group.createText({
			x: conf.point.x + (100 * conf.scale),
			y: conf.point.y + 15 * conf.scale,
			align: "middle",
			text: textdisp
		});
		
		text.setStroke(textcolors);
		text.setFill(textcolors);
		
		group.text = text;
		
		group.size = function(scale){
			var rect = group.bubble;
			var p = {
				x: rect.getShape().x,
				y: rect.getShape().y + rect.getShape().height/2
			}
			group.setTransform([dojox.gfx.matrix.scaleAt(scale, p)]);
			
		}
		
		group.shrink = function(){
			group.size(1);
		};

		group.grow = function(){
			group.size(1.4);
		}
		
		group.setHp = function(hp){
			var rmod = Math.abs(-(255/50) * hp + 255);
			var gmod = 255;
			var textcolors = "black";
			if(hp > 50){
				var gmod = -255/50 * 1.3 * hp + (255 * 2);
				textcolors = "white";
			}
			var bubblefill = [rmod, gmod, 0, 100];
			group.text.setStroke(textcolors);
			group.text.setFill(textcolors);
			group.bubble.setFill(bubblefill);
			var bshape = group.bubble.getShape();
			
			var thex = (hp * bshape.width)/100 + bshape.x
			//hp * (200 + bshape.x)/bshape.width + bshape.x;
			group.hpline.setShape({x1: thex, x2:thex});
		}
		
		group.pointCollision = function(point){
			var [topleft, topright, bottomright, bottomleft] = group.bubble.getTransformedBoundingBox();
			return (topleft.x <= point.x && bottomright.x >= point.x) && (topleft.y <= point.y && bottomright.y >= point.y)
		}
		
		group.boxCollision = function(intopleft, inbottomright){
			var [topleft, topright, bottomright, bottomleft] = group.bubble.getTransformedBoundingBox();
			return (!(topleft.x > inbottomright.x || intopleft.x > bottomright.x || topleft.y > inbottomright.y || inbottomright.y > bottomright.y));
		}
		
		return group;
	}

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
		
		conf.data.unshift({"display":"System", "health":supervisorTab.averageHp(conf.data)});

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
			
			if(obj.allhealth && obj.allhealth.down){
				o.createLine({x1:20, x2:60, y1:yi-5, y2:yi+20}).setStroke({width:3, color:[255, 153, 51, 100]});
				o.createLine({x1:20, x2:60, y1:yi + 20, y2:yi-5}).setStroke({width:3, color:[255, 153, 51, 100]});
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
						/*var rect = obj.children[0];
						var p = {
							x: rect.getShape().x,
							y: rect.getShape().y + rect.getShape().height/2
						};
						obj.setTransform([dojox.gfx.matrix.scaleAt(1, p)]);*/
						obj.shrink();
					});
					/*var rect = this.children[0];
					var p = {
						x: rect.getShape().x,
						y: rect.getShape().y + rect.getShape().height/2
					};
					this.setTransform([dojox.gfx.matrix.scaleAt(1.4, p)]);*/
					this.grow();
				});
			}
			o.connect("onmouseenter", o, function(ev){				
				supervisorTab.setDetails({display:this.data.display});
			});
			yi = yi - 40;
			out.push(o);
		}
		dojo.forEach(conf.data, drawIt);
		supervisorTab.systemStack = out;
		return out;
	}

	supervisorTab.drawAgentQueueBubbles = function(agenthp, queuehp){
		supervisorTab.agentBubble = supervisorTab.drawBubble({
			scale:.75, 
			point:{x:20, y:20},
			data: {"health":agenthp, "display":"Agents"}
		});
		supervisorTab.agentBubble.connect("onclick", supervisorTab.agentBubble, function(ev){
			supervisorTab.refreshGroupsStack("agentprofile");
			this.grow();
			supervisorTab.queueBubble.shrink();
		});
		supervisorTab.agentBubble.dragOver = function(obj){
			supervisorTab.refreshGroupsStack("agentprofile");
			this.grow();
			supervisorTab.queueBubble.shrink();
		}
		supervisorTab.queueBubble = supervisorTab.drawBubble({
			scale:.75, 
			point:{x:20, y:60}, 
			data:{"health":queuehp, "display":"Queues"}
		});
		supervisorTab.queueBubble.connect("onclick", supervisorTab.queueBubble, function(ev){
			supervisorTab.refreshGroupsStack("queuegroup");
			this.grow();
			supervisorTab.agentBubble.shrink();
		});
		supervisorTab.queueBubble.dragOver = function(obj){
			supervisorTab.refreshGroupsStack("queuegroup");
			this.grow();
			supervisorTab.agentBubble.shrink();
		}
	}

	supervisorTab.drawBubbleStack = function(props){
		var conf = {
			parent: supervisorTab.surface,
			mousept: {x:100, y:100},
			viewHeight:400,
			bubbleConfs: []		
		}
		
		conf = dojo.mixin(conf, props);

		var groupHeight = conf.bubbleConfs.length * 40;
		var viewHeight = conf.viewHeight;
		
		var ratio = groupHeight / viewHeight;
		
		var parent = conf.parent;
		var group = parent.createGroup();
		
		var yi = 0;
		var pt = {
			x:conf.mousept.x,
			y:conf.mousept.y
		};
		
		group.viewHeight = conf.viewHeight;
		
		group.backing = group.createRect({x:pt.x - 50, y:-100, width:200, height: groupHeight + 200}).setFill([0, 0, 0, 0]).setStroke([0, 0, 0, 0]);
		
		group.bubbles = [];
		var hps = [];
		
		group.addBubble = function(bubbleconf){
			bubbleconf.scale = .75;
			bubbleconf.point = {
				x: conf.mousept.x -50,
				y: (group.bubbles.length + 1) * 40
			};
			bubbleconf.parent = group;

			var bubble = supervisorTab.drawBubble(bubbleconf);
			bubble.connect("onmousedown", group, function(ev){
				group.scrollLocked = true;
			});
			bubble.connect("onmouseup", group, function(ev){
				group.scrollLocked = false;
			});
			group.bubbles.push(bubble);
			group.backing.setShape({height: group.backing.getShape().height + 40});
			return bubble;
		}
			
		dojo.forEach(conf.bubbleConfs, function(obj, ind, arr){
			group.addBubble(obj);
		});
		
		group.scroll = function(y){
			if(group.scrollLocked){
				return false;
			}
			
			var o = [];
			dojo.forEach(group.bubbles, function(obj, index, arr){
				var d = obj.children[0].getTransformedBoundingBox()[0].y - y
				obj.size(supervisorTab.sizeFromDistance(d));
			});
			
			var groupHeight = group.bubbles.length * 40;
			
			var ratio = groupHeight / group.viewHeight;
			
			this.setTransform([dojox.gfx.matrix.translate(0, -y * ratio + 100)]);
		}
		
		group.connect("onmousemove", group, function(ev){
			group.scroll(ev.layerY);
		});
		
		group._clear = group.clear;
		
		group.clear = function(){
			if(this.scrollLocked){
				return;
			}
			
			group._clear();
		};

		group.pointCollision = function(point){
			if (group.backing.getShape().x <= point.x && group.backing.getShape().x + group.backing.getShape().width >= point.x){
				group.scroll(point.y);
				for(var i = 0; i < group.bubbles.length; i++){
					if(group.bubbles[i].pointCollision(point)){
						////console.log(group.bubbles[i]);
						if(group.bubbles[i].onEnter){
							group.bubbles[i].onEnter();
						}
						return group.bubbles[i];
					}
				}
			};
			
			return null;
		};
		
		group.boxCollision = function(intopleft, intopright, inbottomright, inbottomright){
			if(! (group.backing.getShape().x > inbottomright.x || intopleft.x > (group.backing.getShape().x + group.backing.getShape().width))){
				for(var i =0; i < group.bubbles.length; i++){
					if(group.bubbles[i].boxCollision(intopleft, inbottomright)){
						return group.bubbles[i];
					}
				}
			}
			
			return null;
		}
		
		return group;
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

	supervisorTab.simplifyData = function(items){
		var acc = [];
		dojo.forEach(items, function(obj, index, arr){
			acc.push({
				"display":supervisorTab.dataStore.getValue(obj, "display"),
				"health":supervisorTab.dataStore.getValue(obj, "aggregate"),
				"type":supervisorTab.dataStore.getValue(obj, "type"),
				"allhealth":supervisorTab.dataStore.getValue(obj, "health")
			});
		})
		return acc;
	}

	supervisorTab.refreshGroupsStack = function(stackfor){
		if(supervisorTab.groupsStack.scrollLocked){
			return false;
		}
		
		var conf = {
			type:stackfor
		}
		
		var fetchdone = function(items, request){
			var acc = [];
			var hps = [];
			dojo.forEach(items, function(obj){
				acc.push({
					data:{
						display:supervisorTab.dataStore.getValue(obj, "display"),
						health:supervisorTab.dataStore.getValue(obj, "aggregate")
					},
					onmouseenter:function(ev){
						if(conf.type == "agentprofile"){
							supervisorTab.refreshIndividualsStack("agent", "profile", this.data.display, supervisorTab.node);
						}
						else{
							supervisorTab.refreshIndividualsStack("queue", "group", this.data.display, supervisorTab.node);
						};
						supervisorTab.setDetails({
							type:supervisorTab.dataStore.getValue(obj, "type"),
							display:supervisorTab.dataStore.getValue(obj, "display")
						});
					}
				});
				hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			});
		
			supervisorTab.callsStack.clear();
			supervisorTab.individualsStack.clear();
			supervisorTab.groupsStack.clear();
			supervisorTab.groupsStack = supervisorTab.drawBubbleStack({
				mousept:{
					x:300,
					y:100
				},
				bubbleConfs:acc
			});
			
			supervisorTab.groupsStack.addBubble({
				data:{display:"All", health:supervisorTab.averageHp(hps)},
				onmouseenter:function(ev){
					if(conf.type == "agentprofile"){
						supervisorTab.refreshIndividualsStack("agent", "profile", "*", supervisorTab.node);
					}
					else{
						supervisorTab.refreshIndividualsStack("queue", "group", "*", supervisorTab.node);
					}
				}
			});
			
			supervisorTab.groupsStack.moveToBack();
		}
		
		supervisorTab.dataStore.fetch({
			query:{
				type:conf.type,
			},
			onComplete:fetchdone
		});
	};

	supervisorTab.IndividualStackAsAgents = function(items){
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
			
			bub.setSubscription = dojo.subscribe("supervisortab/set/" + supervisorTab.dataStore.getValue(obj, "id"), function(storeref, dataobj){
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
			});
			
			var message = "agent " + bub.data.display + " accepted drop, meaning it forwared request to server";
			bub.dropped = function(obj){
				//console.log("dropped called");
				if(obj.data.type == "media"){
					//console.log(message);
					//console.log(obj.data);
					if(obj.data.agent){
						var ajaxdone = function(json, args){
							//console.log(json.message);
						}
						var geturl = "/supervisor/agent_transfer/" + escape(obj.data.agent) + "/" + escape(bub.data.display);
						//console.log(geturl);
						dojo.xhrGet({
							url:geturl,
							handleAs:"json",
							load:ajaxdone
						})
					}
					else if(obj.data.queue){
						var ajaxdone = function(json, args){
							//console.log(json.message);
						}
						var geturl = "/supervisor/agent_ring/" + escape(obj.data.queue) + "/" + escape(obj.data.display) + "/" + escape(bub.data.display);
						//console.log(geturl);
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
	//			bub.connect("onclick", bub, function(){
	//				dijit.byId("agentAction").agentBubbleHit = bub;
	//			});
				dijit.byId("agentAction").bindDomNode(bub.rawNode);
				/*bub.connect("onclick", bub, function(ev){
					//console.log(ev);
				});*/
			}				 
		});
		
		dijit.byId("agentAction").onClose = function(ev){
			supervisorTab.individualsStack.scrollLocked = false;
		};
		
		supervisorTab.individualsStack.addBubble({
			data:{
				display:"All",
				health:supervisorTab.averageHp(hps)
			},
			onmouseenter:function(ev){
				supervisorTab.refreshCallsStack("agent", "*", supervisorTab.node);
			}
		});
	}

	supervisorTab.IndividualStackAsQueues = function(items){
		var acc = [];
		var hps = [];
		dojo.forEach(items, function(obj){
			hps.push(supervisorTab.dataStore.getValue(obj, "aggregate"));
			
			var detailsObj = {
				display:supervisorTab.dataStore.getValue(obj, "display"),
				type:supervisorTab.dataStore.getValue(obj, "type")
			};
			
			var onEnterf = function(){
				supervisorTab.refreshCallsStack("queue", detailsObj.display, supervisorTab.node);
				supervisorTab.setDetails(detailsObj);
			}

			var bub = supervisorTab.individualsStack.addBubble({
				data:{
					display:supervisorTab.dataStore.getValue(obj, "display"), 
					health:supervisorTab.dataStore.getValue(obj, "aggregate")
				},
				onmouseenter:function(){
					onEnterf();
				}
			});

			var message = "queue " + bub.data.display + " accepted drop, meaning it forwared request to server";
			bub.dropped = function(obj){
				//console.log("dropped something, likely a call");
				if(obj.data.type == "media"){
					//console.log(message);
					//console.log(obj.data);
					if(obj.data.agent){
						var ajaxdone = function(json, args){
							//console.log(json.message);
						}
						var geturl = "/supervisor/requeue/" + escape(obj.data.agent) + "/" + escape(bub.data.display);
						//console.log(geturl);
						dojo.xhrGet({
							url:geturl,
							handleAs:"json",
							load:ajaxdone
						});
					}
				}
			}
			bub.onEnter = onEnterf;
					 
			if(bub.data.display != "All"){
				bub.dragOver = function(){
					bub.onEnter();
					return true;
				}
			}
		});
		
		supervisorTab.individualsStack.addBubble({
			data:{
				display:"All",
				health:supervisorTab.averageHp(hps)
			},
			onmouseenter:function(ev){
				supervisorTab.refreshCallsStack("queue", "*", supervisorTab.node);
			}
		});
	}

	supervisorTab.refreshIndividualsStack = function(seek, dkey, dval, node){
		if(supervisorTab.individualsStack.scrollLocked){
			return false;
		}
			
		var fetchdone = function(items, request){
		
			supervisorTab.callsStack.clear();
			supervisorTab.individualsStack.clear();
			supervisorTab.individualsStack = supervisorTab.drawBubbleStack({
				mousept:{
					x:540,
					y:100
				},
			});
			
			if(seek == "agent"){
				supervisorTab.IndividualStackAsAgents(items);
			}
			else{
				supervisorTab.IndividualStackAsQueues(items);
			}
			
			supervisorTab.individualsStack.moveToBack();
		}
		
		var queryo = {
			type:seek,
			node:node
		};
		queryo[dkey] = dval;
		
		supervisorTab.dataStore.fetch({
			query:queryo,
			onComplete:fetchdone
		});
	}

	supervisorTab.refreshCallsStack = function(dkey, dval, node){
		if(supervisorTab.callsStack.scrollLocked){
			return false;
		}
		var fetchdone = function(items, request){
			var acc = [];
			dojo.forEach(items, function(obj){
				var datas = {
					display:supervisorTab.dataStore.getValue(obj, "display"),
					health:supervisorTab.dataStore.getValue(obj, "aggregate"),
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
				})
			})
		
			supervisorTab.callsStack.clear();
			supervisorTab.callsStack = supervisorTab.drawBubbleStack({
				mousept:{
					x:580 + 240,
					y:100
				},
				bubbleConfs:acc
			});
			
			dojo.forEach(supervisorTab.callsStack.bubbles, function(obj, ind, arr){
				var dnd = new dojox.gfx.Moveable(obj);
				obj.connect("onmousedown", obj, function(ev){
					supervisorTab.dndManager.startDrag(obj);
				});
				obj.connect("onmouseup", obj, function(ev){
					supervisorTab.dndManager.endDrag();
				});
			});
		};
		
		var queryo = {
			node:node,
			type:"media"
		};
		queryo[dkey] = dval;

		supervisorTab.dataStore.fetch({
			query:queryo,
			onComplete:fetchdone
		});
	}

	supervisorTab.refreshSystemStack = function(){
		var fetchdone = function(items, request){
			var acc = supervisorTab.simplifyData(items);
			dojo.forEach(supervisorTab.systemStack, function(obj){
				obj.clear();
			});
			supervisorTab.systemStack = supervisorTab.drawSystemStack({data:acc});
		}
		
		supervisorTab.dataStore.fetch({
			query:{
				type:"node"
			},
			onComplete:fetchdone
		});
	}

	supervisorTab.healthDump = function(){
		var dump = function(items){
			dojo.forEach(items, function(item){
				var out = supervisorTab.dataStore.getValue(item, "aggregate");
				var nom = supervisorTab.dataStore.getValue(item, "display");
				//console.log(nom + ": " + out);
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
					supervisorTab.setAllHps();
					supervisorTab.refreshSystemStack();
					if(supervisorTab.suppressPoll){
						return;
					}
					//window.setTimeout(supervisorTab.reloadDataStore, 5000);
				}
				else{
					//console.log(data);
				}
			}
		})
	}

}

supervisorTab.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
dojo.connect(supervisorTab.surface, "ondragstart",   dojo, "stopEvent");
dojo.connect(supervisorTab.surface, "onselectstart", dojo, "stopEvent");

supervisorTab.drawAgentQueueBubbles(0, 0);

supervisorTab.refreshSystemStack();
supervisorTab.systemStack[0].grow();

supervisorTab.reloadDataStore();

supervisorTab.masterSub = dojo.subscribe("agent/supervisortab", function(data){
	//console.log("master sub!");
	//console.log(data);
	data = data.data;
	if(data.action == "set"){
		var fetched = function(items){
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
				"display":data.display
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
				aggregate = supervisorTab.averageHp(hps);
			}
			savedata.aggregate = aggregate;
			for(var i in savedata){
				if(i != "id"){
					supervisorTab.dataStore.setValue(items[0], i, savedata[i]);
				}
			}
			supervisorTab.dataStore.save();
			//console.log("savedata");
			//console.log(savedata);
			dojo.publish("supervisortab/set/" + savedata.id, [items[0], savedata]);
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

supervisorTab.testSub = dojo.subscribe("supervisortab/set/agent-administrator", function(a, b){
	//console.log(["tiny update", a, b]);
});
