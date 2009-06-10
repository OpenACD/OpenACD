supervisorTab = function(){
	{};
};

supervisorTab.healthData = {
	identifier:"id",
	label:"display",
	items:[
		{
			id:"1",
			display:"system",
			type:"system",
			health:50,
			uptime:100
		},
		{
			id:"2",
			display:"node1@example",
			type:"node",
			health:50,
			uptime:100
		},
		{
			id:"3",
			display:"queuegruop1",
			type:"queuegroup",
			health:50
		},
		{
			id:"4",
			display:"queue1",
			type:"queue",
			group:"queuegroup1",
			node:"node1@example",
			health:50
		},
		{
			id:"5",
			display:"queue2",
			type:"queue",
			group:"queuegroup1",
			node:"node1@example",
			health:35
		},
		{
			id:"6",
			display:"agentprofile1",
			type:"agentprofile",
			health:50
		},
		{
			id:"7",
			display:"agent1",
			type:"agent",
			health:50,
			profile:"agentprofile1",
			node:"node1@example"
		},
		{
		   id:"8",
		   display:"call1",
		   type:"media",
		   health:50,
		   queue:"queue1",
		   node:"node1@example"
		}
	]
};

supervisorTab.dataStore = new dojo.data.ItemFileReadStore({data: supervisorTab.healthData});

supervisorTab.systemStack = [];

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
		var c = findweight(obj.health);
		count += c;
		total += c * obj.health;
	}
	
	dojo.forEach(hps, f);
		
	return Math.round(total / count);
}

supervisorTab.bubbleZoom = function(ev){
	var rect = this.children[0];
	var p = {
		x: rect.getShape().x,// + rect.getShape().width/2,
		y: rect.getShape().y + rect.getShape().height/2
	};
	this.setTransform([dojox.gfx.matrix.scaleAt(1.4, p)]);
};

supervisorTab.bubbleOut = function(ev){
	var rect = this.children[0];
	var p = {
		x: rect.getShape().x,// + rect.getShape().width/2,
		y: rect.getShape().y + rect.getShape().height/2
	}
	this.setTransform([dojox.gfx.matrix.scaleAt(1, p)]);
};

supervisorTab.drawBubble = function(opts){
	var conf = {
		scale:1,
		point:{x:100,y:100},
		parent:supervisorTab.surface,
		data:{"display":"bubble", "health":50},
		onmouseenter:function(){return true},
		onmouseleave:function(){return false},
		onclick:function(){return false}
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
	
	new dojox.gfx.Moveable(group);
	
	return group;
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
			onmouseenter: supervisorTab.bubbleZoom,
			onmouseleave: supervisorTab.bubbleOut
		});
		yi = yi - 40;
		out.push(o);
	}
	dojo.forEach(conf.data, drawIt);
	return out;
}

supervisorTab.drawSystemStackTest = function(count){
	var data = [];
	for(var i = 1; i <= count; i++){
		var hp = Math.round(100 * (i / count));
		var o = {
			"display":"node" + i.toString() + "@example",
			"health":hp
		};
		data.push(o);
	};

	supervisorTab.systemStack = supervisorTab.drawSystemStack({"data":data});
	return supervisorTab.systemStack;
}

supervisorTab.drawAgentQueueBubbles = function(agenthp, queuehp){
	supervisorTab.agentBubble = supervisorTab.drawBubble({
		scale:.75, 
		point:{x:20, y:20},
		data: {"health":agenthp, "display":"Agents"},
		onclick: function(){
			supervisorTab.refreshGroupsStack({type:"agentprofile"});
		}
	});
	supervisorTab.queueBubble = supervisorTab.drawBubble({
		scale:.75, 
		point:{x:20, y:60}, 
		data:{"health":queuehp, "display":"Queues"},
		onclick: function(){
			supervisorTab.refreshGroupsStack({type:"queuegroup"});
		}
	});
}

supervisorTab.drawBubbleStack = function(props){
	var conf = {
		parent: supervisorTab.surface,
		mousept: {x:100, y:100},
		data: [],
		viewHeight: 400,
		bubbleenter: function(){return true},
		bubbleleave: function(){return true},
		bubbleclick: function(){return true}
	}
	
	conf = dojo.mixin(conf, props);
	
	var groupHeight = conf.data.length * 40;
	var viewHeight = conf.viewHeight;
	
	var ratio = groupHeight / viewHeight;
	
	var parent = conf.parent;
	var group = parent.createGroup();
	
	var yi = 0;
	var pt = {
		x:conf.mousept.x,
		y:conf.mousept.y
	};
	
	group.createRect({x:pt.x - 50, y:-100, width:100, height: groupHeight + 200}).setFill("white").setStroke("white");
	
	var stack = [];
	var drawIt = function(obj, index, arr){
		var o = supervisorTab.drawBubble({
			scale:.75, 
			point: {x:pt.x - 50, y:yi}, 
			data: obj, 
			parent: group,
			onmouseenter: conf.bubbleenter,
			onmouseleave: conf.bubbleleave,
			onclick: conf.bubbleclick
		});
		yi = yi + 40;
		stack.push(o);
	}
	
	dojo.forEach(conf.data, drawIt);
	
	group.connect("onmousemove", group, function(ev){
		this.setTransform([dojox.gfx.matrix.translate(0, -ev.layerY * ratio + 100)]);
	});
	
	group.bubbles = stack;
	return group;
}

supervisorTab.sizeFromDistance = function(distance){
	// y = -.000004x^2 + 1
	// Don't want it vanishing totally, so there's a minimum size.
	var scale = -.000004 * Math.pow(250, 2) + 1;
	if(scale < .75){
		return .75;
	}
	
	return scale;
}

supervisorTab.drawBubbleStackTest = function(mousept, count){
	var datas = [];
	
	for(var i = 0; i < count; i++){
		var hp = Math.round(100 * (i / count));
		var o = {
			"display":i.toString() + "(" + hp + ")",
			"health":hp
		};
		datas.push(o);
	}
	
	supervisorTab.drawBubbleStack({
		"mousept":mousept,
		"data": datas
	});
};

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

supervisorTab.refreshGroupsStack = function(opts){
	var conf = {
		type:"agentprofile"
	}
	
	conf = dojo.mixin(conf, opts);
	
	var fetchdone = function(items, request){
		var acc = supervisorTab.simplifyData(items)
		console.log(acc);
		supervisorTab.groupsStack.clear();
		supervisorTab.groupsStack = supervisorTab.drawBubbleStack({
			mousept:{
				x:300,
				y:100
			},
			data:acc
		});
	}
	
	supervisorTab.dataStore.fetch({
		query:{
			type:conf.type,
		},
		onComplete:fetchdone
	});
};

supervisorTab.refreshIndividualsStack = function(opts){
	var conf = {
		type:"agent",
		node:"*"
	}
	
	conf = dojo.mixin(conf, opts);
	
	var fetchdone = function(items, request){
		var acc = supervisorTab.simplifyData(items);
		supervisorTab.individualsStack.clear();
		supervisorTab.individualsStack = supervisorTab.drawBubbleStack({
			mousept:{
				x:540,
				y:100
			},
			data:acc
		});
	}
	
	supervisorTab.dataStore.fetch({
		query:{
			type:conf.type,
			node:conf.node
		},
		onComplete:fetchdone
	});
}

supervisorTab.refreshCallsStack = function(opts){
	var conf = {
		node:"*",
		queue:"*"
	}
	
	conf = dojo.mixin(conf, opts);
	
	var fetchdone = function(items, request){
		var acc = supervisorTab.simplifyData(items);
		supervisorTab.callsStack.clear();
		supervisorTab.callsStack = supervisorTab.drawBubbleStack({
			mousept:{
				x:580 + 240,
				y:100
			},
			data:acc
		});
	}
	
	supervisorTab.dataStore.fetch({
		query:{
			node:conf.node,
			queue:conf.queue,
			type:"media"
		},
		onComplete:fetchdone
	});
}

supervisorTab.drawSystemStack({});
supervisorTab.drawAgentQueueBubbles(0, 0);

//supervisorTab.drawBubbleStackTest({x:300, y:100}, 10);
//supervisorTab.drawBubbleStackTest({x:540, y:100}, 20);
//supervisorTab.drawBubbleStackTest({x:580 + 240, y:100}, 100);