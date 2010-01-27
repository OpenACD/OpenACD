if(typeof(queueDashboard) == "undefined"){
	
	var link = document.createElement('link');
	var head = dojo.query('head')[0];
	head.appendChild(link);
	link.rel = 'stylesheet';
	link.href='/tabs/queue_dashboard.css';
	link.type = 'text/css';
	
	queueDashboard = function(){
		return {};
	}
	
	queueDashboard.dataStore = {
		queues:{}
	};
	
	// =====
	// helper class queue
	// =====
	queueDashboard.Queue = function(display){
		this.name = display;
		this.medias = {};
		this._history = {};
		this._limboMedias = {};
		this.completed = 0;
		this.calls = 0;
		this.abandoned = 0;
		this.avgHold = 0;
		this.maxHold = 0;
		this._masterSubscription = dojo.subscribe("queueDashboard/supevent", this, function(event){
			this.consumeEvent(event.data);
		});
	}
	
	queueDashboard.Queue.prototype.consumeEvent = function(data){
		debug(["nom nom'ing", data]);
		if(data.type == 'media'){
			// current, history, or new?
			if(this.medias[data.id]){
				this.updateLiveMedia(data);
			} else if(this._history[data.id]){
				this.updateHitoricalMedia(data);
			} else if(this._limboMedias[data.id]){
				debug(['updating limbo', data]);
				if(data.details.queue && data.details.queue == this.name){
					this.medias[data.id] = this._limboMedias[data.id];
					this.medias[data.id].status = 'queued';
					this.calls++;
				} else if(data.action == 'drop' || data.details.agent){
					delete this._limboMedias[data.id];
				}
			} else if(data.details.queue == this.name) {
				debug(['updating medias list', data]);
				this.calls++;
				this.medias[data.id] = new queueDashboard.Media(data);
			} else {
				debug(['appending to limbo', data]);
				this._limboMedias[data.id] = new queueDashboard.Media(data);
			}
			return true;
		}
		
		debug(['apparently data.type was not media', data.type]);
		return false;
	}
	
	queueDashboard.Queue.prototype.updateLiveMedia = function(data){
		debug(['updating live', data]);
		if(data.action == 'drop'){
			this.calls--;
			this.abandoned++;
			this._history[data.id] = this.medias[data.id];
			this._history[data.id].status = 'abandoned';
			delete this.medias[data.id];
		} else if(data.details.queue != this.name){
			// moved queue, keep track of it in history.
			this.calls--;
			this._history[data.id] = this.medias[data.id];
			delete this.medias[data.id];
		} else if(data.details.agent){
			this.calls--;
			this.completed++;
			this._history[data.id] == this.medias[data.id];
			this._history[data.id].status = 'completed';
			delete this.medias[data.id];
		}
	}
	
	queueDashboard.Queue.prototype.updateHistoricalMedia = function(data){
		debug(['updating history', data]);
		if(data.action == 'drop'){
			if(this._history[data.id].status != 'completed'){
				this.abandoned++;
				this._history[data.id].end('abandoned');
			}
		} else if(data.details.agent){
			if(this._history[data.id].status != 'completed'){
				this.completed++;
				this._history[data.id].end('completed');
			}
		}
	}
		
	queueDashboard.Queue.prototype.recalc = function(){
		var now = new Date();
		now = Math.floor(now.getTime() / 1000);
		
		//var inqueue = 0;
		//var abandoned = 0;
		//var handled = 0;
		//var age = 0;
		//var oldest = now;
		
		for(var i in this._history){
			if(now - 86400 < this._history[i].ended){
				if(this._history[i].status == 'abandoned'){
					this.abandoned--;
				} else {
					this.completed--;
				}
				delete this._history[i];
			}
		}
		
		var totalAge = 0;
		var counted = 0;
		var oldest = 0;
		
		for(i in this.medias){
			counted++;
			var age = now - this.medias[i].created;
			totalAge += age;
			if(oldest < age){
				oldest = age;
			}
		}
		
		for(i in this._history){
			counted++;
			var age = this.medias[i].ended - this.medias[i].created;
			totalAge += age;
			if(oldest < age){
				oldest = age;
			}
		}
		
		this.maxHold = oldest;
		this.avgHold = Math.floor(totalAge / counted);
		
		dojo.publish("queueDashboard/queueUpdate", [this]);
	}
	
	// =====
	// helper class media
	// ====
	
	queueDashboard.Media = function(initalEvent){
		this.initalEvent = initalEvent;
		this.created = Math.floor(new Date().getTime() / 1000);
		if(initalEvent.details.queued_at){
			this.created = initalEvent.details.queued_at.timestamp;
		}
		this.type = initalEvent.details.type;
		this.client = initalEvent.details.client;
		this.status = 'limbo';
	}
	
	queueDashboard.Media.prototype.end = function(cause){
		this.status = cause;
		this.ended = Math.floor(new Date().getTime() / 1000);
	}
	
	// =====
	// Helpful functions
	// =====
	
	queueDashboard.filterSupevent = function(supevent){
		return (supevent.type == 'media' || supevent.type == 'queue');
	}
	
	queueDashboard.getStatus = function(){
		dojo.xhrGet({
			url:'/supervisor/status',
			handleAs:'json',
			load:function(res){
				debug(res);
				// nab only the medias
				var real = [];
				var items = res.data.items;
				for(var i = 0; i < items.length; i++){
					if(items[i].type == 'media'){
						var fixedItem = {
							action: 'set',
							details: items[i].details._value,
							display: items[i].display,
							id: items[i].id.substr(6),
							type: 'media'
						}
						real.push(fixedItem);
					}
				}
				
				for(i = 0; i < real.length; i++){
					debug(["status fixed", real[i]]);
					dojo.publish("queueDashboard/supevent", [{data: real[i]}]);
				}
				
				if(! queueDashboard.recalcTimer){
					queueDashboard.recalcTimerFunc();
				}
			},
			error:function(res){
				errMessage(["getting initial status errored", res]);
			}
		});
	}
	
	queueDashboard.drawQueueTable = function(){
		// Should only need to be called once, after the queue list is gotten.
		for(i in queueDashboard.dataStore.queues){
			nodes = dojo.query('#queueDashboardTable *[queue="' + i + '"]');
			if(nodes.length == 0){
				var queueTr = document.createElement('tr');
				queueTr.setAttribute('queue', i);
				queueTr.setAttribute('purpose', 'queueDisplay');
				queueTr.innerHTML = '<td purpose="name">' + i + 
				'</td><td purpose="callCount"></td>' + 
				'<td purpose="completeCount">0</td>' + 
				'<td purpose="abandonCount">0</td>' + 
				'<td purpose="averageHold">0</td>' + 
				'<td purpose="oldestHold">0</td>';
				queueTr.onclick = function(){
					var callDisps = dojo.query('#queueDashboardTable *[queue="' + i + '"][purpose="callDisplay"]');
					if(callDisps.length == 0){
						queueDashboard.drawCallTable(i);
					} else {
						dojo.byId('queueDashboardTable').removeChild(callDisps[0]);
					}
				}
				queueTr.queueSubscription = dojo.subscribe("queueDashboard/queueUpdate", function(queue){
					if(queue.name != i){
						return false;
					}
					
					var tds = dojo.query('#queueDashboardTable *[queue="' + i + '"][purpose="queueDisplay"] td');
					for(var j = 0; j < tds.length; j++){
						switch(tds[j].getAttribute('purpose')){
							case 'callCount':
								tds[j].innerHTML = queue.calls;
								break;
							case 'completeCount':
								tds[j].innerHTML = queue.completed;
								break;
							case 'abandonCount':
								tds[j].innerHTML = queue.abandoned;
								break;
							case 'averageHold':
								tds[j].innerHTML = queue.avgHold;
								break;
							case 'oldestHold':
								tds[j].innerHTML = queue.maxHold;
								break;
							default:
								// do nothing, nothing!
						}
					}
				});
				dojo.byId('queueDashboardTable').appendChild(queueTr);
			}
		}
	}
	
	queueDashboard.drawCallTable = function(queuename){
		var queueMediasTr = document.createElement('tr');
		queueMediasTr.setAttribute('queue', queuename);
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
		
		dojo.place(queueMediasTr, dojo.query('#queueDashboardTable *[queue="' + queuename + '"][purpose="queueDisplay"]')[0], 'after');
		
		var tbody = dojo.query('#queueDashboardTable *[queue="default_queue"][purpose="callDisplay"] tbody')[0];
		
		for(var i in queueDashboard.dataStore.queues[queuename].medias){
			queueDashboard.drawCallTableRow(queuename, i, tbody);
		}
	}
	
	queueDashboard.drawCallTableRow = function(queuename, mediaid, tbody){
		var tr = document.createElement('tr');
		var now = Math.floor(new Date().getTime() / 1000);
		var age = now - queueDashboard.dataStore.queues[queuename].medias[mediaid].created;
		tr.setAttribute('callid', mediaid);
		tr.innerHTML = '<td>' + mediaid + '</td>' +
		'<td>' + queueDashboard.dataStore.queues[queuename].medias[mediaid].type + '</td>' +
		'<td>' + age + '</td>' +
		'<td>' + queueDashboard.dataStore.queues[queuename].medias[mediaid].client + '</td>';
		dojo.place(tr, tbody, 'last');
	}
	
	queueDashboard.recalcTimerFunc = function(){
		if(dojo.byId('queueDashboardTable')){
			for(var i in queueDashboard.dataStore.queues){
				queueDashboard.dataStore.queues[i].recalc();
			}
			queueDashboard.recalcTimer = setTimeout(queueDashboard.recalcTimerFunc, 5000);
		}
	}
}

dojo.xhrGet({
	url:'/queuelist',
	handleAs:'json',
	load:function(res){
		if(res.success){
			for(var i = 0; i < res.queues.length; i++){
				queueDashboard.dataStore.queues[res.queues[i].name] = new queueDashboard.Queue(res.queues[i].name);
			}
			queueDashboard.drawQueueTable();
			queueDashboard.getStatus();
		} else {
			errMessage(["getting queues failed", res.message]);
		}
	},
	error:function(res){
		errMessage(["getting queues errored", res]);
	}
});

queueDashboard.masterSub = dojo.subscribe("agent/supervisortab", queueDashboard, function(supevent){
	if(! this.filterSupevent(supevent.data)){
		return false;
	}
	
	supevent.data.id = supevent.data.id.substr(6);

	debug(["queueDashboard forwarding", supevent]);
	dojo.publish("queueDashboard/supevent", [supevent]);
});

/*dojo.byId('queueDashboardTable').updateSub = dojo.subscribe('queueDashboard/queueUpdate', dojo.byId('queueDashboardTable'), function(data){
	if(! dojo.byId('queueDashboardRow-' + data.name)){
		var queueRow = dojo.create('tr', {id: "queueDashboardRow-" + data.name}, this, 'last');
		var mediasRow = dojo.create('tr', {id: "queueDashboardMedias-" + data.name}, this, 'last');
	}
});*/

