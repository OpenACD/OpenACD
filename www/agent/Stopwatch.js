function Stopwatch(){
	this.t = new dojox.timing.Timer(1000);
	this.elapsed = 0;
	this.onTick = function(){};
	
	var stopwatch = this;
	
	this.t.onTick = function(){
		stopwatch.elapsed += 1;
		stopwatch.onTick();
	}
	
	this.time = function(){
		return this.elapsed
	};
	
	this.reset = function(){
		this.elapsed = 0;
	}
	
	this.start = function(){
		this.t.start();
	}
	
	this.stop = function(){
		this.t.stop();
	}
}
