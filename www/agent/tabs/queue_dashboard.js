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
		if(this.medias[data.id]){
			this.updateLiveMedia(data);
		} else if(this._history[data.id]){
			this.updateHistoricalMedia(data);
		} else if(this._limboMedias[data.id]){
			debug(['updating limbo', data]);
			if(data.action == 'drop'){
				delete this._limboMedias[data.id];
			} else if(data.details.agent){
				delete this._limboMedias[data.id];
			} else if (data.details.queue && data.details.queue == this.name){
				this.medias[data.id] = this._limboMedias[data.id];
				this.medias[data.id].status = 'queued';
				this.calls++;
			} else if(data.details.queue){
				delete this._limboMedias[data.id];
			}
		} else {
			if(data.action == 'drop'){
				return false;
			} else if(data.details.queue == this.name){
				this.medias[data.id] = new queueDashboard.Media(data);
				this.calls++;
			} else {
				this._limboMedias[data.id] = new queueDashboard.Media(data);
			}
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
			this._history[data.id].end('abandoned');
			delete this.medias[data.id];
		} else if(data.details.queue != this.name && ! data.details.agent){
			// moved queue, keep track of it in history.
			this.calls--;
			this._history[data.id] = this.medias[data.id];
			delete this.medias[data.id];
		} else if(data.details.agent && ! data.details.queue){
			this.calls--;
			this.completed++;
			this._history[data.id] = this.medias[data.id];
			this._history[data.id].end('completed');
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
				
		for(var i in this._history){
			if(now - 86400 > this._history[i].ended){
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
			var age = this._history[i].ended - this._history[i].created;
			totalAge += age;
			if(oldest < age){
				oldest = age;
			}
		}
		
		//console.log(["recalc data", totalAge, counted]);
		this.maxHold = oldest;
		if(counted > 0){
			this.avgHold = Math.floor(totalAge / counted);
		} else {
			this.avgHold = 0;
		}
		
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
		debug(["the subevent", supevent]);
		if (supevent.type == 'media' || supevent.type == 'queue'){
			return true;
		}
		
		if( (supevent.action == 'drop') && supevent.id.match(/^media-/) ){
			return true;
		}
		
		return false;
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
							id: items[i].id,
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
		for(var i in queueDashboard.dataStore.queues){
			var testnom = i;
			nodes = dojo.query('#queueDashboardTable *[queue="' + testnom + '"]');
			if(nodes.length == 0){
				var queueTr = document.createElement('tr');
				queueTr.setAttribute('queue', testnom);
				queueTr.setAttribute('purpose', 'queueDisplay');
				dojo.create('td', {purpose: 'name', innerHTML: testnom}, queueTr);
				dojo.create('td', {purpose: 'callCount'}, queueTr);
				dojo.create('td', {purpose: 'completeCount'}, queueTr);
				dojo.create('td', {purpose: 'abandonCount'}, queueTr);
				dojo.create('td', {purpose: 'averageHold'}, queueTr);
				dojo.create('td', {purpose: 'oldestHold'}, queueTr);
				queueTr.onclick = function(){
					var queuenom = this.getAttribute('queue');
					//console.log(["the queue", testnom, this]);
					var callDisps = dojo.query('#queueDashboardTable *[queue="' + queuenom + '"][purpose="callDisplay"]');
					if(callDisps.length == 0){
						queueDashboard.drawCallTable(queuenom);
					} else {
						dojo.byId('queueDashboardTable').removeChild(callDisps[0]);
					}
				}
				queueTr.queueSubscription = dojo.subscribe("queueDashboard/queueUpdate", queueTr, function(queue){
					if(queue.name != this.getAttribute('queue')){
						return false;
					}
					var queuenom = this.getAttribute('queue');
					var tds = dojo.query('#queueDashboardTable *[queue="' + queuenom + '"][purpose="queueDisplay"] td');
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
								tds[j].innerHTML = formatseconds(queue.avgHold);
								break;
							case 'oldestHold':
								tds[j].innerHTML = formatseconds(queue.maxHold);
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
		dojo.place(queueMediasTr, dojo.query('#queueDashboardTable *[queue="' + queuename + '"][purpose="queueDisplay"]')[0], 'after');
		dojo.create('td', null, queueMediasTr);
		var widetd = dojo.create('td', {colspan: 5}, queueMediasTr);
		var table = dojo.create('table', null, widetd);
		table.innerHTML = '<tr>' +
		'<th>Caller ID</th>' +
		'<th>Type</th>' +
		'<th>Hold Time</th>' +
		'<th>Brand</th>' +
		'</tr>';
		var tbody = dojo.query('#queueDashboardTable *[queue="' + queuename + '"][purpose="callDisplay"] table')[0];
		
		for(var i in queueDashboard.dataStore.queues[queuename].medias){
			queueDashboard.drawCallTableRow(queuename, i, tbody);
		}
	}
	
	queueDashboard.drawCallTableRow = function(queuename, mediaid, tbody){
		var tr = document.createElement('tr');
		var media = queueDashboard.dataStore.queues[queuename].medias[mediaid];
		var now = Math.floor(new Date().getTime() / 1000);
		var age = now - media.created;
		tr.setAttribute('callid', mediaid);
		dojo.create('td', {innerHTML: media.initalEvent.details.callid_name + " " + media.initalEvent.details.callid_data}, tr);
		dojo.create('td', {innerHTML: media.type}, tr);
		dojo.create('td', {innerHTML: formatseconds(age)}, tr);
		dojo.create('td', {innerHTML: media.client}, tr);
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
	var supcp = supevent;
	//supcp.data.id = supcp.data.id.substr(6);

	debug(["queueDashboard forwarding", supcp]);

	dojo.publish("queueDashboard/supevent", [supcp]);
});
