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
	this.setTransform([dojox.gfx.matrix.scaleAt(2, p)]);
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
		width: 100 * scale,
		height: 20 * scale,
		r: 10 * scale
	});
	
	var fill;
	var textcolors = "black";
	if(data.health == 50){
		bubblefill = [0, 255, 0, 100];
	}
	else if(data.health < 50){
		var ymod = -(255/50) * data.health + 255;
		bubblefill = [ymod, 255, 0, 100];
	}
	else{
		var rmod = 255/50 * data.health - 255;
		var gmod = -255/50 * 1.3 * data.health + (255 * 2);
		bubblefill = [rmod, gmod, 0, 100];
		textcolors = "white";
	}
	
	bubble.setFill(bubblefill);
	bubble.setStroke("black");
	group.connect("onmouseenter", group, supervisorTab.bubbleZoom);
	group.connect("onmouseleave", group, supervisorTab.bubbleOut);
	
	var text = group.createText({
		x: point.x + (50 * scale),
		y: point.y + 15 *scale,
		align: "middle",
		text: data.display
	});
	
	text.setStroke(textcolors);
	text.setFill(textcolors);
	
	//console.log(group);
	
	new dojox.gfx.Moveable(group);
}

supervisorTab.drawBubbleStack = function(mousept, datas){
	if(arguments[2]){
		var parent = arguments[2];
	}
	else{
		var parent = supervisorTab.surface;
	}
	
	var groupHeight = datas.length * 40;
	var viewHeight = 400;//supervisorTab.surface.rawNode.height;
	
	var ratio = groupHeight / viewHeight;
		
	var group = parent.createGroup();
	
	var yi = 0;
	var pt = {
		x:100,
		y:mousept.y
	};
	
	group.createRect({x:pt.x - 50, y:0, width:100, height: groupHeight}).setFill("white").setStroke("white");
	
	var drawIt = function(obj, index, arr){
		supervisorTab.drawBubble(.75, {x:pt.x - 50, y:yi}, obj, group);
		yi = yi + 40;
	}
	
	dojo.forEach(datas, drawIt);
	
	group.connect("onmousemove", group, function(ev){
		//console.log(ev);
		this.setTransform([dojox.gfx.matrix.translate(0, -ev.layerY * ratio)]);
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
	
	supervisorTab.surface.clear();
	supervisorTab.drawBubbleStack(mousept, datas);
};

supervisorTab.drawBubbleStackTest({x:100, y:100}, 100);