if(typeof(dashboard) == 'undefined'){
	var link = document.createElement('link');
	var head = dojo.query('head')[0];
	head.appendChild(link);
	link.rel = 'stylesheet';
	link.href='/tabs/dashboard.css';
	link.type = 'text/css';
	
	dashboard = function(){
		throw 'NameSpace, not to be instanciated';
	}
	
	dashboard.medias = {};
	
	/*dashboard.filterSupevent = function(supevent){
		debug(["the subevent", supevent]);
		if (supevent.type == 'media' || supevent.type == 'queue'){
			return true;
		}
		
		if( (supevent.action == 'drop') && supevent.id.match(/^media-/) ){
			return true;
		}
		
		return false;
	}*/
	
	// =====
	// Media is used for both agents and queues
	// =====
	
	dashboard.Media = function(initEvent){
		this.initialEvent = initEvent;
		this.created = Math.floor(new Date().getTime() / 1000);
		if(initEvent.details.queued_at){
			this.created = initEvent.details.queued_at.timestamp;
		}
		this.type = initEvent.details.type;
		this.client = initEvent.details.client;
		this.status = 'limbo';
	}
	
	dashboard.Media.prototype.end = function(cause){
		this.status = cause;
		this.ended = Math.floor(new Date().getTime() / 1000);
	}
	
	dashboard.getStatus = function(){
		dojo.xhrGet({
			url:'/supervisor/status',
			handleAs:'json',
			load:function(res){
				debug(res);
				var real = [];
				var items = res.data.items;
				for(var i = 0; i < items.length; i++){
					if(items[i].type == 'media' || items[i].type == 'agent'){
						var fixedItem = {
							action: 'set',
							details: items[i].details._value,
							display: items[i].display,
							id: items[i].id,
							type: items[i].type,
							name: items[i].display
						}
						real.push(fixedItem);
					}
				}
				for(i = 0; i < real.length; i++){
					debug(["status fixed", real[i]]);
					if(real[i].type == 'media'){
						dashboard.medias[real[i].name] = real[i];
					}
					dojo.publish("dashboard/supevent/" + real[i].type, [real[i]]);
				}
			},
			error:function(res){
				errMessage(["getting initial status errored", res]);
			}
		});	
	}
}

dashboard.masterSub = dojo.subscribe("agent/supervisortab", dashboard, function(supevent){
	if(supevent.data.type == 'media' && supevent.data.action == 'drop'){
		delete this.medias[supevent.data.name];
	} else if(supevent.data.type == 'media'){
		if(this.medias[supevent.data.name]){
			this.medias[supevent.data.name] = supevent.data;
		} else {
			this.medias[supevent.data.name] = supevent.data;
		}
	}
	
	dojo.publish('dashboard/supevent/' + supevent.data.type, [supevent.data]);
});