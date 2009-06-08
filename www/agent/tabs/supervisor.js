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
		x: rect.getShape().x + rect.getShape().width/2,
		y: rect.getShape().y + rect.getShape().height/2
	};
	this.setTransform([dojox.gfx.matrix.scaleAt(2, p)]);
};

supervisorTab.bubbleOut = function(ev){
	var rect = this.children[0];
	var p = {
		x: rect.getShape().x + rect.getShape().width/2,
		y: rect.getShape().y + rect.getShape().height/2
	}
	this.setTransform([dojox.gfx.matrix.scaleAt(1, p)]);
};

supervisorTab.drawBubble = function(scale, point, data){
	if(scale > 2){
		scale = 2;
	}
	else if(scale < .75){
		scale = .75
	}
	
	var group = supervisorTab.surface.createGroup();
	
	var bubble = group.createRect({
		x: point.x,
		y: point.y,
		width: 100 * scale,
		height: 20 * scale,
		r: 10 * scale
	});
	bubble.setFill([255, 255, 255, 100]);
	bubble.setStroke("black");
	group.connect("onmouseenter", group, supervisorTab.bubbleZoom);
	group.connect("onmouseleave", group, supervisorTab.bubbleOut);
	
	var text = group.createText({
		x: point.x + (50 * scale),
		y: point.y + 15 *scale,
		align: "middle",
		text: data.display
	});
	
	text.setStroke("black");
	
	console.log(group);
	
	new dojox.gfx.Moveable(group);
}

supervisorTab.drawBubbleStack = function(mousept, datas){
	var count = datas.length;
	var height = supervisorTab.surface.rawNode.height;
	var y = mousept.y;
	
	var actualPercentile = y/height;
	
	var totalHeight = datas.length * 20;
	
	var stackPercentile = datas.length * y/height;
	
	var didBig = false;
	var yi = 0;
	
	var drawIt = function(obj, index, arr){
		if(index + 1 > stackPercentile){
			if( ! didBig){
				didBig = true;
				supervisorTab.drawBubble(2, {x:mousept.x, y:yi}, obj);
				yi = yi + 40;
				return;
			}
		}
		
		supervisorTab.drawBubble(2, {x:mousept.x - 50, y:yi}, obj);
		yi = yi + 40;
	}
	
	dojo.forEach(datas, drawIt);
}

supervisorTab.drawBubbleStackTest = function(mousept, count){
	var datas = [];
	
	for(var i = 0; i < count; i++){
		var o = {
			"display":i.toString()
		};
		datas.push(o);
	}
	
	supervisorTab.surface.clear();
	supervisorTab.drawBubbleStack(mousept, datas);
};

supervisorTab.surface.connect("onmousemove", supervisorTab.surface, function(ev){
	supervisorTab.drawBubbleStackTest({x:ev.clientX, y:ev.clientY}, 10);
});