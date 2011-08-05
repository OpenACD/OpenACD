//>>built
define("dojox/gfx/canvas_attach", ["dojo/main", "dojox/gfx/canvas"], function(dojo){
	dojo.getObject("dojox.gfx.canvas_attach", true);
	dojo.experimental("dojox.gfx.canvas_attach");

	// not implemented
	dojox.gfx.canvas.attachSurface = dojox.gfx.canvas.attachNode = function(){
		return null;	// for now
	};

	return dojox.gfx.canvas;
});
