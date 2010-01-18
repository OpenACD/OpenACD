if(typeof(queueDashbaord) == "undefined"){
	
	var link = document.createElement('link');
	var head = dojo.query('head')[0];
	head.appendChild(link);
	link.rel = 'stylesheet';
	link.href='/tabs/queue_dashboard.css';
	link.type = 'text/css';
	
	queueDashbaord = function(){
		return {};
	}
	
	queueDashbaord.dataStore = {
		queues:{}
	};
	
	// =====
	// helper class queue
	// =====
	queueDashbaord.Queue = function(initalEvent){
		this.name = initalEvent.display;
		this.medias = {};
		this.consumeEvent(initalEvent);
	}
	
	queueDashbaord.Queue.prototype.consumeEvent = function(data){
		if(data.type == 'media'){
			if(this.medias[data.id] == undefined && data.details.queue != this.name){
				return false;
			} else if(this.medias[data.id] == undefined){
				this.medias[data.id] = new queueDashbaord.Media(data);
			} else {
				this.medias[data.id].consumeEvent(data);
			}
			return true;
		}
	}
	
	queueDashbaord.Queue.prototype.recalc = function(){
		var now = new Date();
		now = Math.floor(now.getTime() / 1000);
		
		var inqueue = 0;
		var abandoned = 0;
		var handled = 0;
		var age = 0;
		var oldest = now;
		
		for(var i in this.medias){
			if(now - 86400 > this.medias[i].time){
				delete this.medias[i];
				continue;
			}
			
			if(this.medias[i].time < oldest && this.medias[i].status == 'queued'){
				oldest = this.medias[i].time;
			}
			
			if(this.medias[i].status == 'queued'){
				age += (now - this.medias[i].queued); 
			}
			
			switch(this.medias[i].status){
				case 'queued':
					inqueue++;
					break;
				case 'abandoned':
					abandoned++;
					break;
				case 'handled':
					handled++;
					break;
			}
		}
		
		this.inqueue = inqueue;
		this.abandoned = abandoned;
		this.averageage = Math.floor(age / inqueue);
		
		dojo.publish("queueDashboard/queueUpdate", [this]);
	}
	
	// =====
	// helper class media
	// ====
	
	queueDashbaord.Media = function(initalEvent){
		this.status = 'limbo';
		this.id = initalEvent.id.substr(6);
		this.display = initalEvent.display;
		this.type = initalEvent.details.type;
		this.brand = initalEvent.details.brand;
		this.consumeEvent(initalEvent);
	}
	
	queueDashbaord.Media.prototype.getHistory  = function(){
		return this._history;
	}
	
	queueDashbaord.Media.prototype.consumeEvent = function(data){
		if(data.type != media || data.id != this.id){
			return false
		}
		
		if(data.action == 'drop'){
			var historyEvent = {
				timestamp: new Date(),
				event: 'ended'
			};
			
			var lastEvent = this._history.length - 1;
			switch(this._history[lastEvent].event){
				case 'inivr':
					this.status = 'ivrAbandon';
					break;
				case 'inqueue':
					this.status = 'queueAbandon';
					break;
				case 'handled':
					this.status = 'completed';
					break;
			}
			this._history.push(historyEvent);
			return this.status;
		}
		
		if(data.details.queue){
			if(this.status != 'queued'){
				var historyEvent = {
					timestamp: new Date(),
					event: 'queued'
				};
				
				this._history.push(historyEvent);
				this.status = 'queued';
				return this.status;
			}
			
			if(this.queue != data.details.queue){
				var historyEvent = {
					timestamp: new Date(),
					event: 'queued'
				};
				
				this._history.push(historyEvent);
				this.queue = data.details.queue;
				return this.status;
			}
		}
	}
	
	// =====
	// Helpful functions
	// =====
	
	queueDashbaord.filterSupevent = function(supevent){
		return (supevent.type == 'media' || supevent.type == 'queue');
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
	
	if(supevent.data.type == 'queue'){
		if(queueDashbaord.dataStore[supevent.data.display]){
			queueDashbaord.dataStore[supevent.data.display].consumeEvent(supevent.data);
		} else {
			var queue = new queueDashbaord.Queue(supevent.data);
			queueDashbaord.dataStore[supevent.data.display] = queue;
		}
		return true;
	}
	
	if(supevent.data.type == 'media'){
		supevent.data.id = supevent.data.id.substr(6);
		var queue = supevent.data.details
	}
	
	console.log(supevent);
});

dojo.byId('queueDashboardTable').updateSub = dojo.subscribe('queueDashboard/queueUpdate', dojo.byId('queueDashboardTable'), function(data){
	if(! dojo.byId('queueDashboardRow-' + data.name)){
		var queueRow = dojo.create('tr', {id: "queueDashboardRow-" + data.name}, this, 'last');
		var mediasRow = dojo.create('tr', {id: "queueDashboardMedias-" + data.name}, this, 'last');
	}
});
