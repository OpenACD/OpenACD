dojo.require("dojo.dnd.Source");
dojo.require("dojo.dnd.Selector");
dojo.require("dojo.dnd.Container");

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
	
	var bubble = group.createRect({
		x: conf.point.x,
		y: conf.point.y,
		width: 200 * conf.scale,
		height: 20 * conf.scale,
		r: 10 * conf.scale
	});
	
	var rmod = Math.abs(-(255/50) * conf.data.health + 255);
	var gmod = 255;
	var textcolors = "black";
	if(conf.data.health > 50){
		var gmod = -255/50 * 1.3 * conf.data.health + (255 * 2);
		textcolors = "white";
	}
	var bubblefill = [rmod, gmod, 0, 100];
	
	bubble.setFill(bubblefill);
	bubble.setStroke("black");
	
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
	
	var dnder = new dojox.gfx.Moveable(group);
	console.log(dnder);
	
//	dnder.connect("onFirstMove", dnder, function(){
//		console.log("arrrrrrgggggg!");
//	});
	
	group.size = function(scale){
		var rect = group.children[0];
		var p = {
			x: rect.getShape().x,
			y: rect.getShape().y + rect.getShape().height/2
		}
		group.setTransform([dojox.gfx.matrix.scaleAt(scale, p)]);
		
	}
	
//	group.connect("onDndSourceOver", group, function(){
//		console.log("goober pants!");
//		console.log(arguments);
//	})

	group.shrink = function(){
		group.size(1);
	};

	group.grow = function(){
		group.size(1.4);
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
			data: obj,
			onclick: function(ev){
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
			},
			onmouseenter:function(ev){				
				supervisorTab.setDetails({display:this.data.display});
			}
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
		data: {"health":agenthp, "display":"Agents"},
		onclick: function(){
			supervisorTab.refreshGroupsStack("agentprofile");
			this.grow();
			supervisorTab.queueBubble.shrink();
		}
	});
	supervisorTab.queueBubble = supervisorTab.drawBubble({
		scale:.75, 
		point:{x:20, y:60}, 
		data:{"health":queuehp, "display":"Queues"},
		onclick: function(){
			supervisorTab.refreshGroupsStack("queuegroup");
			this.grow();
			supervisorTab.agentBubble.shrink();
		}
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
	
	group.createRect({x:pt.x - 50, y:-100, width:100, height: groupHeight + 200}).setFill([0, 0, 0, 0]).setStroke([0, 0, 0, 0]);
	
	var stack = [];
	var hps = [];
	
	var drawIt = function(obj){//, index, arr){
		var bubbleconf = {
			scale:.75,
			point:{x:pt.x - 50, y:yi},
			data:{},
			parent:group
		};
		
		var o = supervisorTab.drawBubble(dojo.mixin(bubbleconf, obj));
		o.connect("onmousedown", group, function(ev){
			group.scrollLocked = true;
		});
		
		o.connect("onmouseup", group, function(ev){
			group.scrollLocked = false;
		});
		yi = yi + 30;
		stack.push(o);
		hps.push(obj.health);
		return {
			node: o.getNode(),
			data: obj,
			type: ["agent", "queue", "media"]
		}
	}
	
	group.connect("onmousemove", group, function(ev){
		if(this.scrollLocked){
			return false;
		}
		
		var o = [];
		dojo.forEach(group.bubbles, function(obj, index, arr){
			var d = obj.children[0].getTransformedBoundingBox()[0].y - ev.layerY
			obj.size(supervisorTab.sizeFromDistance(d));
		})
		
		this.setTransform([dojox.gfx.matrix.translate(0, -ev.layerY * ratio + 100)]);
	});
		
	var dnder = new dojo.dnd.Source(group.getNode(), {
		creator:function(item, hint){
			var out = drawIt(item);
			return out;
		},
		accept:["agent", "queue", "media"],
		singular:true		
	});
	
	dnder.insertNodes(false, conf.bubbleConfs, true, group.children[0]);
	
	group.connect("onDraggingOver", group, function(ev){
		console.log("goober");
		this.onmousemove(ev);
	});
	console.log(group);
//	dnder.connect("onDraggingOver", group, function(ev){
//		console.log("goober");
//		this.onmousemove(ev);
//	});
	
	group.bubbles = stack;
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
			"health":supervisorTab.dataStore.getValue(obj, "health")
		});
	})
	return acc;
}

supervisorTab.refreshGroupsStack = function(stackfor){
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
	
		acc.push({
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
	}
	
	supervisorTab.dataStore.fetch({
		query:{
			type:conf.type,
		},
		onComplete:fetchdone
	});
};

supervisorTab.refreshIndividualsStack = function(seek, dkey, dval, node){
	var fetchdone = function(items, request){
		var acc = [];
		var hps = [];
		dojo.forEach(items, function(obj){
			hps.push(supervisorTab.dataStore.getValue(obj, "health"));
			acc.push({
				data:{
					display:supervisorTab.dataStore.getValue(obj, "display"), 
					health:supervisorTab.dataStore.getValue(obj, "health")
				},
				onmouseenter:function(ev){
					if(seek == "agent"){
						supervisorTab.refreshCallsStack("agent", this.data.display, supervisorTab.node);
					}
					else{
						supervisorTab.refreshCallsStack("queue", this.data.display, supervisorTab.node);
					};
					supervisorTab.setDetails({
						display:supervisorTab.dataStore.getValue(obj, "display"),
						type:supervisorTab.dataStore.getValue(obj, "type")
					});
				}
			})
		});
	
		acc.push({
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
		})
	
		supervisorTab.callsStack.clear();
		supervisorTab.individualsStack.clear();
		supervisorTab.individualsStack = supervisorTab.drawBubbleStack({
			mousept:{
				x:540,
				y:100
			},
			bubbleConfs:acc
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
	var fetchdone = function(items, request){
		var acc = [];
		dojo.forEach(items, function(obj){
			acc.push({
				data:{
					display:supervisorTab.dataStore.getValue(obj, "display"),
					health:supervisorTab.dataStore.getValue(obj, "health")
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
	}
	
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
