if(typeof(queueDashbaord) == "undefined"){
	
	queueDashbaord = function(){
		return {};
	}
	
	queueDashbaord.dataStore = {
		queues:[]
	};
	
	// =====
	// helper class
	// =====
	queueDashbaord.queue = function(conf){
		var defaultConf = {
			name: 'default_queue',
			medias: []
		}
	}
	
	// =====
	// Helpful functions
	// =====
	
	queueDashbaord.filterSupevent = function(supevent){
		return supevent.type != 'agent';
	}
}

dojo.xhrGet({
	url:'/dynamic/all.json',
	handleAs: 'json',
	load: function(res){
		var raws = res.rawdata;
		console.log(res);
		dojo.xhrGet({
			url:'/supervisor/startmonitor',
			handleAs: 'json',
			load: function(res){
				if(res.success){
					return true;
				}
				
				return false;
			},
			error: function(res){
				console.log(['error', res]);
			}
		});
	},
	error: function(res){
		console.log(['error', res]);
	}
});

queueDashbaord.masterSub = dojo.subscribe("agent/supervisortab", queueDashbaord, function(supevent){
	if(! this.filterSupevent(supevent.data)){
		return false;
	}
	
	console.log(supevent);
});
