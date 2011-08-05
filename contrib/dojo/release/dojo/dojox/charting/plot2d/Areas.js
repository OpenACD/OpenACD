//>>built
define("dojox/charting/plot2d/Areas", ["dojo/_base/declare", "./Default"], 
  function(declare, Default){
	return declare("dojox.charting.plot2d.Areas", dojox.charting.plot2d.Default, {
		//	summary:
		//		Represents an area chart.  See dojox.charting.plot2d.Default for details.
		constructor: function(){
			this.opt.lines = true;
			this.opt.areas = true;
		}
	});
});
