
supervisorTab = function(){
	{};
};

supervisorTab.healthData = {identifier:"id",
label:"display",
items:[{id:"1",
	   display:"System",
	   type:"system",
	   health:50,
	   details:{
		_type:"details",
		_value:{
			uptime:99
		}}},
	   {id:"2",
	   display:"node1@example",
	   type:"node",
	   health:50,
	   details:{
		_type:"details",
		_value:{
			uptime:100,
			location:"here"
		}
	   }
	   },
	   {id:"3",
	   display:"node2@example",
	   type:"node",
	   health:50,
	   uptime:100,
	   details:{
		_type:"details",
		_value:{
			uptime:100,
			location:"there"
		}
	   }},
	   {id:"4",
	   display:"queuegroup1",
	   type:"queuegroup",
	   health:50,
	   details:{
		_type:"details",
		_value:{}
	   }},
	   {id:"5",
	   display:"queuegroup2",
	   type:"queuegroup",
	   health:50},
	   {id:"6",
	   display:"queue1",
	   type:"queue",
	   group:"queuegroup1",
	   node:"node1@example",
	   health:50},
	   {id:"7",
	   display:"queue2",
	   type:"queue",
	   group:"queuegroup2",
	   node:"node1@example",
	   health:50},
	   {id:"8",
	   display:"queue3",
	   type:"queue",
	   group:"queuegroup1",
	   node:"node2@example",
	   health:50},
	   {id:"9",
	   display:"queue4",
	   type:"queue",
	   group:"queuegroup2",
	   node:"node2@example",
	   health:50},
	   {id:"10",
	   display:"media1",
	   type:"media",
	   node:"node1@example",
	   queue:"queue1",
	   health:50},
	   {id:"11",
	   display:"media2",
	   type:"media",
	   node:"node2@example",
	   queue:"queue1",
	   health:50},
	   {id:"12",
	   display:"media3",
	   type:"media",
	   node:"node1@example",
	   queue:"queue2",
	   health:50},
	   {id:"13",
	   display:"media4",
	   node:"node2@example",
	   queue:"queue2",
	   health:50},
	   {id:"14",
	   display:"agentprofile1",
	   type:"agentprofile",
	   health:50},
	   {id:"15",
	   display:"agentprofile2",
	   type:"agentprofile",
	   health:50},
	   {id:"16",
	   display:"agent1",
	   type:"agent",
	   profile:"agentprofile1",
	   node:"node1@example",
	   health:50},
	   {id:"17",
	   display:"agent2",
	   type:"agent",
	   profile:"agentprofile2",
	   node:"node1@example",
	   health:50},
	   {id:"18",
	   display:"agent3",
	   type:"agent",
	   profile:"agentprofile1",
	   node:"node2@example",
	   health:50},
	   {id:"19",
	   display:"agent4",
	   profile:"agentprofile2",
	   node:"node2@example",
	   health:50},
	   {id:"20",
	   display:"media5",
	   type:"media",
	   agent:"agent1",
	   node:"node1@example",
	   health:50},
	   {id:"21",
	   display:"media6",
	   type:"media",
	   agent:"agent2",
	   node:"node2@example",
	   health:50}]
};

supervisorTab.dataStore = new dojo.data.ItemFileReadStore({
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

supervisorTab.dndManager = {
	_dragging: null,
	_eventHandle:null,
	_dropCandidate: null,
	checkCollision:function(pt){
		//console.log(pt);
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
			//console.log(ev);
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
		supervisorTab.dndManager._dragging = obj;
	},
	endDrag:function(){
		supervisorTab.dndManager._dragging.disconnect(supervisorTab.dndManager._eventHandle);
		if(supervisorTab.dndManager._dropCandidate){
			supervisorTab.dndManager._dropCandidate.dropped(supervisorTab.dndManager._dragging);
		}
	}
};

supervisorTab.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
dojo.connect(supervisorTab.surface, "ondragstart",   dojo, "stopEvent");
dojo.connect(supervisorTab.surface, "onselectstart", dojo, "stopEvent");

supervisorTab.averageHp = function(hps){
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
		if(typeof(obj) == "number"){
			var c = findweight(obj);
			count += c;
			total += c * obj;
		}
		else{
			var c = findweight(obj.health);
			count += c;
			total += c * obj.health;
		}
	}
	
	dojo.forEach(hps, f);
		
	return Math.round(total / count);
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
	
	var text = group.createText({
		x: conf.point.x + (100 * conf.scale),
		y: conf.point.y + 15 * conf.scale,
		align: "middle",
		text: conf.data.display
	});
	
	text.setStroke(textcolors);
	text.setFill(textcolors);
		
	group.size = function(scale){
		var rect = group.children[0];
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
		for(var i in obj){
			out += "<p><label class=\"narrow\">" + i + ":</label>" + obj[i].toString() + "</p>";
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
		o.connect("onclick", o, function(ev){
			if(this.data.display == "System"){
				supervisorTab.node = "*";
			}
			else{
				supervisorTab.node = this.data.display
			}
			dojo.forEach(supervisorTab.systemStack, function(obj){
				var rect = obj.children[0];
				var p = {
					x: rect.getShape().x,
					y: rect.getShape().y + rect.getShape().height/2
				};
				obj.setTransform([dojox.gfx.matrix.scaleAt(1, p)]);
			});
			var rect = this.children[0];
			var p = {
				x: rect.getShape().x,
				y: rect.getShape().y + rect.getShape().height/2
			};
			this.setTransform([dojox.gfx.matrix.scaleAt(1.4, p)]);
		});
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
	
	group.backing = group.createRect({x:pt.x - 50, y:-100, width:200, height: groupHeight + 200}).setFill([0, 0, 0, 100]).setStroke([0, 0, 0, 0]);
	
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
					console.log(group.bubbles[i]);
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
			"health":supervisorTab.dataStore.getValue(obj, "health"),
			"type":supervisorTab.dataStore.getValue(obj, "type")
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
					health:supervisorTab.dataStore.getValue(obj, "health")
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
			hps.push(supervisorTab.dataStore.getValue(obj, "health"));
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
		
		dojo.forEach(supervisorTab.groupsStack.bubbles, function(obj, ind, arr){
			
		});
	}
	
	supervisorTab.dataStore.fetch({
		query:{
			type:conf.type,
		},
		onComplete:fetchdone
	});
};

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
		
		var acc = [];
		var hps = [];
		dojo.forEach(items, function(obj){
			hps.push(supervisorTab.dataStore.getValue(obj, "health"));
			
			var detailsObj = {
				display:supervisorTab.dataStore.getValue(obj, "display"),
				type:supervisorTab.dataStore.getValue(obj, "type")
			};
			
			var onEnterf = function(){
				if(seek == "agent"){
					supervisorTab.refreshCallsStack("agent", detailsObj.display, supervisorTab.node);
				}
				else{
					supervisorTab.refreshCallsStack("queue", detailsObj.display, supervisorTab.node);
				};
				supervisorTab.setDetails(detailsObj);
			}
			
			var bub = supervisorTab.individualsStack.addBubble({
				data:{
					display:supervisorTab.dataStore.getValue(obj, "display"), 
					health:supervisorTab.dataStore.getValue(obj, "health")
				},
				onmouseenter:function(){
					onEnterf();
				}
			});

			if(seek == "agent"){
				var message = "agent " + bub.data.display + " accepted drop, meaning it forwared request to server";
			}
			else{
				var message = "queue " + bub.data.display + " accepted drop, meaning it forwared request to server";
			}
			bub.onEnter = onEnterf;
			bub.dropped = function(obj){
				if(obj.data.type == "media"){
					console.log(message);
				}
			}
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
				if(seek == "agent"){
					supervisorTab.refreshCallsStack("agent", "*", supervisorTab.node);
				}
				else{
					supervisorTab.refreshCallsStack("queue", "*", supervisorTab.node);
				}
			}
		});
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
			acc.push({
				data:{
					display:supervisorTab.dataStore.getValue(obj, "display"),
					health:supervisorTab.dataStore.getValue(obj, "health"),
					type:"media"
				},
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

supervisorTab.drawAgentQueueBubbles(0, 0);
supervisorTab.refreshSystemStack();
supervisorTab.systemStack[0].grow();

