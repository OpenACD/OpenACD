supervisorTab = function(){
	{};
};

supervisorTab.surface = dojox.gfx.createSurface(dojo.byId("supervisorMonitor"), 600, 400);
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