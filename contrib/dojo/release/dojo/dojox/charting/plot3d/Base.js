//>>built
define("dojox/charting/plot3d/Base", ["dojo/_base/kernel", "dojo/_base/declare"], 
  function(lang, declare) {

	return declare("dojox.charting.plot3d.Base", null, {
		constructor: function(width, height, kwArgs){
			this.width  = width;
			this.height = height;
		},
		setData: function(data){
			this.data = data ? data : [];
			return this;
		},
		getDepth: function(){
			return this.depth;
		},
		generate: function(chart, creator){
		}
	});
});

