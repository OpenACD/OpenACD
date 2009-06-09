supervisorTab = function(){
	{};
};

supervisorTab.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), "99%", 400);
dojo.connect(supervisorTab.surface, "ondragstart",   dojo, "stopEvent");
dojo.connect(supervisorTab.surface, "onselectstart", dojo, "stopEvent");

supervisorTab.testDraw = function(){
//	var rect = supervisorTab.surface.createRect({
//		y: 50,
//		x: 100,
//		width: 400,
//		height: 300,
//		r: 50
//	});
//	rect.setStroke("black");
//	supervisorTab.surface.connect("onmouseover", rect, function(){ console.log("gooberpants!") });
//	supervisorTab.surface.add(rect);

	var rect2 = supervisorTab.surface.createRect({
		y: 50,
		x:  500,
		width: 50,
		height: 50,
		r: 10
	});
	rect2.setFill([255, 255, 255, 100]);
	rect2.setStroke("red");
	rect2.connect("onmouseenter", rect2, function(ev){
		supervisorTab.thiis = this;
		var p = {
			x: this.getShape().x + 25,
			y: this.getShape().y + 25
		};
		this.setTransform([dojox.gfx.matrix.scaleAt(1.1, p)]);
	});
	rect2.connect("onmouseleave", rect2, function(ev){
		supervisorTab.thiis = this;
		var p = {
			x: this.getShape().x + (this.getShape().width/2),
			y: this.getShape().y + (this.getShape().width/2)
		};
		this.setTransform([dojox.gfx.matrix.scaleAt(.9, p)]);
	});
	
	//dojo.connect(rect2, "onclick", dojo, function(){ console.log("i'm hit!") });
	//supervisorTab.surface.connect("onclick", rect, function(){ console.log("niftytoes!")});
	//supervisorTab.surface.add(rect2);
	new dojox.gfx.Moveable(rect2);
	

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

supervisorTab.drawBubble = function(scale, point, data){
	if(arguments[3]){
		var parent = arguments[3];
	}
	else{
		var parent = supervisorTab.surface;
	}

	if(scale > 2){
		scale = 2;
	}
	else if(scale < .75){
		scale = .75
	}
	
	var group = parent.createGroup();
	
	var bubble = group.createRect({
		x: point.x,
		y: point.y,
		width: 200 * scale,
		height: 20 * scale,
		r: 10 * scale
	});
	
	var rmod = Math.abs(-(255/50) * data.health + 255);
	var gmod = 255;
	var textcolors = "black";
	if(data.health > 50){
		var gmod = -255/50 * 1.3 * data.health + (255 * 2);
		textcolors = "white";
	}
	var bubblefill = [rmod, gmod, 0, 100];
	
	bubble.setFill(bubblefill);
	bubble.setStroke("black");
	group.connect("onmouseenter", group, supervisorTab.bubbleZoom);
	group.connect("onmouseleave", group, supervisorTab.bubbleOut);
	
	var text = group.createText({
		x: point.x + (100 * scale),
		y: point.y + 15 *scale,
		align: "middle",
		text: data.display
	});
	
	text.setStroke(textcolors);
	text.setFill(textcolors);
	
	//console.log(group);
	
	new dojox.gfx.Moveable(group);
}

supervisorTab.drawSystemStack = function(datas){
	console.log(datas);
	if(arguments[1]){
		var parent = arguments[1];
	}
	else{
		var parent = supervisorTab.surface;
	}
	
	if(datas.length > 0){
		var culm = 0;
		for(var i = 0; i < datas.length; i++){
			culm += datas[i].health;
		}
		var averageHp = culm/datas.length;
	}
	else{
		var averageHp = 0;
	}
	console.log(datas);
	datas.unshift({"display":"System", "health":averageHp});
	console.log(datas);
	var yi = 385;
	var drawIt = function(obj, index, arr){
		console.log(obj);
		supervisorTab.drawBubble(.75, {x:20, y:yi}, obj);
		yi = yi - 40;
	}
	dojo.forEach(datas, drawIt);
}

supervisorTab.drawSystemStackTest = function(count){
	var data = [];
	console.log(count);
	for(var i = 1; i <= count; i++){
		var hp = Math.round(100 * (i / count));
		var o = {
			"display":"node" + i.toString() + "@example",
			"health":hp
		};
		data.push(o);
	};
	console.log(data);
	supervisorTab.drawSystemStack(data);
}

supervisorTab.drawAgentQueueBubbles = function(agenthp, queuehp){
	supervisorTab.drawBubble(.75, {x:20, y:20}, {"health":agenthp, "display":"Agents"});
	supervisorTab.drawBubble(.75, {x:20, y:60}, {"health":queuehp, "display":"Queues"});
}

supervisorTab.drawBubbleStack = function(props){
	var conf = {
		parent: supervisorTab.surface,
		mousept: {x:100, y:100},
		data: [],
		viewHeight: 400
	}
	
	conf = dojo.mixin(conf, props);
	
	var groupHeight = conf.data.length * 40;
	var viewHeight = conf.viewHeight;//supervisorTab.surface.rawNode.height;
	
	var ratio = groupHeight / viewHeight;
	
	var parent = conf.parent;
	var group = parent.createGroup();
	
	var yi = 0;
	var pt = {
		x:conf.mousept.x,
		y:conf.mousept.y
	};
	
	group.createRect({x:pt.x - 50, y:-100, width:100, height: groupHeight + 200}).setFill("white").setStroke("white");
	
	var drawIt = function(obj, index, arr){
		supervisorTab.drawBubble(.75, {x:pt.x - 50, y:yi}, obj, group);
		yi = yi + 40;
	}
	
	dojo.forEach(conf.data, drawIt);
	
	group.connect("onmousemove", group, function(ev){
		//console.log(ev);
		this.setTransform([dojox.gfx.matrix.translate(0, -ev.layerY * ratio + 100)]);
	});
}

supervisorTab.drawBubbleStackTest = function(mousept, count){
	var datas = [];
	
	for(var i = 0; i < count; i++){
		//var hp = Math.round(Math.random() * 100);
		var hp = Math.round(100 * (i / count));
		var o = {
			"display":i.toString() + "(" + hp + ")",
			"health":hp
		};
		datas.push(o);
	}
	
	//supervisorTab.surface.clear();
	supervisorTab.drawBubbleStack({
		"mousept":mousept,
		"data": datas
	});
};

supervisorTab.drawSystemStackTest(5);
supervisorTab.drawAgentQueueBubbles(20, 80);
supervisorTab.drawBubbleStackTest({x:300, y:100}, 10);
supervisorTab.drawBubbleStackTest({x:500, y:100}, 20);
supervisorTab.drawBubbleStackTest({x:700, y:100}, 100);