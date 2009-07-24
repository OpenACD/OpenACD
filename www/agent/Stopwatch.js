function Stopwatch(){
	this.tref = null;
	this.elapsed = 0;
	this.onTick = function(){};
	
	var stopwatch = this;
	
	this.onTickInternal = function(){
		stopwatch.onTick();
		function tick() {
			stopwatch.elapsed +=1;
			stopwatch.onTickInternal();
		}
		stopwatch.tref = setTimeout(tick, 1000);
	}
	
	this.time = function(){
		return this.elapsed
	};
	
	this.reset = function(){
		this.elapsed = 0;
	}
	
	this.start = function(){
		if (stopwatch.tref) {
			stopwatch.stop();
		}
		function tick() {
			stopwatch.elapsed +=1;
			stopwatch.onTickInternal();
		}
		stopwatch.tref = setTimeout(tick, 1000);
	}
	
	this.stop = function(){
		if (stopwatch.tref) {
			clearTimeout(stopwatch.tref);
			stopwatch.tref = null;
		}
	}
}
