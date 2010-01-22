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
	
	queueDashbaord.getStatus = function(){
		dojo.xhrGet({
			url:'/supervisor/status',
			handleAs:'json',
			load:function(res){
				console.log(res);
				// nab only the medias
				var real = [];
				var items = res.data.items;
				for(var i = 0; i < items.length; i++){
					if(items[i].type == 'media'){
						items[i].details = items[i].details._value;
						items[i].health = items[i].health._value;
						real.push(items[i]);
					}
				}
				
				for(i = 0; i < real.length; i++){
					if(real[i].queue){
						var queuenom = real[i].queue
						queueDashbaord.dataStore.queues[queuenom].medias[real[i].display] = real[i];
						/*if(dojo.query('#queueDashboardTable > tr[queue="' + queuenom + '"]').length == 0){
							var queueTr = document.createElement('tr');
							queueTr.queue = queuenom;
							queueTr.purpose = 'queue_display';
							queueTr.innerHTML = '<tr>
							var queueMediasTr = document.createElement('tr');
							queueMediasTr.queue = queuenom;
							queueMediasTr.purpose = 'call_display';
							dojo.byId('queueDashboardTable').appendChild(
						}*/
					}
				}
			},
			error:function(res){
				errMessage(["getting initial status errored", res]);
			}
		});
	}
	
	queueDashbaord.updateTable = function(){
		var nodes = dojo.query('#queueDashboardTable *[queue][purpose="queueDisplay]');
		for(var i = 0; i < nodes.length; i++){
			if(! queueDashbaord.dataStore.queues[nodes[i].queue]){
				var queueNom = nodes[i].queue;
				var subnodes = dojo.query('#queueDashboardTable *[queue="' + queueNom + '"]');
				for(var j = 0; j < subnodes.length; j++){
					dojo.byId('queueDashboardTable').removeChild(subnodes[j]);
				}
			}
		}
		
		for(i in queueDashbaord.dataStore.queues){
			nodes = dojo.query('#queueDashboardTable *[queue="' + i + '"]');
			if(nodes.length == 0){
				var queueTr = document.createElement('tr');
				queueTr.setAttribute('queue', i);
				queueTr.setAttribute('purpose', 'queueDisplay');
				queueTr.innerHTML = '<td purpose="name">' + i + 
				'</td><td purpose="callCount"></td>' + 
				'<td purpose="completeCount">0</td>' + 
				'<td purpose="abandonCount">0</td>' + 
				'<td purpose="avergageHold">0</td>' + 
				'<td purpose="oldestHold">0</td>';
				var queueMediasTr = document.createElement('tr');
				queueMediasTr.setAttribute('queue', i);
				queueMediasTr.setAttribute('purpose', 'callDisplay');
				queueMediasTr.innerHTML = '<td></td>' + 
				'<td colspan=4>' + 
				'<table>' + 
				'<tr>' + 
				'<th>Callid</th>' + 
				'<th>Type</th>' + 
				'<th>Hold Time</th>' + 
				'<th>Brand</th>' + 
				'</tr>' + 
				'</table>' + 
				'</td>';
				dojo.byId('queueDashboardTable').appendChild(queueTr);
				dojo.byId('queueDashboardTable').appendChild(queueMediasTr);
			}
		}
	}
}

dojo.xhrGet({
	url:'/queuelist',
	handleAs:'json',
	load:function(res){
		if(res.success){
			for(var i = 0; i < res.queues.length; i++){
				queueDashbaord.dataStore.queues[res.queues[i].name] = {medias:{}, completed:0, abandoned:0, averageage: 0, maxage: 0};
			}
			queueDashbaord.getStatus();
		} else {
			errMessage(["getting queues failed", res.message]);
		}
	},
	error:function(res){
		errMessage(["getting queues errored", res]);
	}
});





//
//dojo.xhrGet({
//	url:'/dynamic/all.json',
//	handleAs: 'json',
//	load: function(res){
//		var raws = res.rawdata;
//		console.log(res);
//		dojo.xhrGet({
//			url:'/supervisor/startmonitor',
//			handleAs: 'json',
//			load: function(res){
//				if(res.success){
//					return true;
//				}
//				
//				return false;
//			},
//			error: function(res){
//				console.log(['error', res]);
//			}
//		});
//		var now = Math.floor(new Date().getTime() / 1000);
//		var hourago = now - (60 * 60 * 1000);
//		for(var i = 0; i < raws.length; i++){
//			if(raws[i].ended && raws[i].ended + hourago < now){
//				
//			}
//		}
//	},
//	error: function(res){
//		console.log(['error', res]);
//	}
//});

queueDashbaord.masterSub = dojo.subscribe("agent/supervisortab", queueDashbaord, function(supevent){
	if(! this.filterSupevent(supevent.data)){
		return false;
	}
	
	/*if(supevent.data.type == 'queue'){
		if(queueDashbaord.dataStore[supevent.data.display]){
			queueDashbaord.dataStore[supevent.data.display].consumeEvent(supevent.data);
		} else {
			var queue = new queueDashbaord.Queue(supevent.data);
			queueDashbaord.dataStore[supevent.data.display] = queue;
		}
		//return true;
	}*/
	
	
	
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
