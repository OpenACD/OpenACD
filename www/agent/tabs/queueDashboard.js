if(typeof(queueDashboard) == "undefined"){
		
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
		this._masterSubscription = dojo.subscribe("queueDashboard/supevent", this, function(event){
			this.consumeEvent(event);
		});
	}
	
	queueDashboard.Queue.prototype.now = function(){
		return Math.floor(new Date().getTime() / 1000);
	}
	
	queueDashboard.Queue.prototype.consumeEvent = function(data){
		//console.log(["nom nom'ing", data]);
		if(data.type != 'media'){
			return false;
		}
		
		if(data.action == 'drop'){
			delete this.medias[data.name];
			var nodes = dojo.query('#queueDashboardTable tr[callid="' + data.name + '"]');
			while(nodes.length > 0){
				var n = nodes.pop();
				n.parentNode.removeChild(n);
			}
			return true;
		}
		
		if(data.details.queue != this.name && this.medias[data.name]){
			delete this.medias[data.name];
			var nodes = dojo.query('#queueDashboardTable tr[callid="' + data.name + '"]');
			while(nodes.length > 0){
				var n = nodes.pop();
				n.parentNode.removeChild(n);
			}			
			dojo.publish('queueDashboard/updateQueue/' + data.name, [{action:'drop'}]);
		}
		
		if(data.details.queue == this.name){
			this.medias[data.name] = data;
			dojo.publish('queueDashboard/updateQueue/' + data.name, [{action:'set'}]);
			var nodes = dojo.query('#queueDashboardTable tr[callid="' + data.name = '"]');
			if(nodes.length == 0){
			
			} else {
			
			}
		}
		
		return true;
	}
	/*
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
			if(now - 3600 > this._history[i].ended){
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
		
		this.maxHold = oldest;
		if(counted > 0){
			this.avgHold = Math.floor(totalAge / counted);
		} else {
			this.avgHold = 0;
		}
		
		dojo.publish("queueDashboard/queueUpdate", [this]);
	}
	
	queueDashboard.Queue.prototype.awareOfMedia = function(id){
		if(! id.match(/^media-/)){
			id = 'media-' + id;
		}
		
		if(this.medias[id]){
			return true;
		}
		
		if(this._history[id]){
			return true;
		}
		
		if(this._limboMedias[id]){
			return true;
		}
		
		return false;
	}
	*/
	// =====
	// Helpful functions
	// =====
	
	/*queueDashboard.filterSupevent = function(supevent){
		debug(["the subevent", supevent]);
		if (supevent.type == 'media' || supevent.type == 'queue'){
			return true;
		}
		
		if( (supevent.action == 'drop') && supevent.id.match(/^media-/) ){
			return true;
		}
		
		return false;
	}*/
	
	/*queueDashboard.getStatus = function(){
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
				
				queueDashboard.getHistory();
			},
			error:function(res){
				errMessage(["getting initial status errored", res]);
			}
		});
	}*/
	/*
	queueDashboard.getHistory = function(file){
		if(! file){
			file = "/dynamic/all.json";
		}
		var now = new Date();
		now = Math.floor(now.getTime() / 1000);
		dojo.xhrGet({
			url:file,
			handleAs:'json',
			load:function(res){
				for(var j = 0; j < res.queues.length; j++){
					var queue = res.queues[j].name;
					var medias = res.queues[j].medias;
					var seedAbn = res.queues[j].totalAbandoned;
					var seedComplete = res.queues[j].totalInbound - seedAbn;
					for(var k = 0; k < medias.length; k++){
						var media = medias[k];
						if(media.ended && (now - media.ended > 3600)){
							console.log(["too old", media]);
							continue;
						}
						
						if(queueDashboard.dataStore.queues[queue].awareOfMedia(media.id)){
							console.log(["already knew", media]);
							continue;
						}
						
						media.id = 'media-' + media.id;
						var initEvent = {
							details:{
								queued_at: {
									timestamp: media.queued
								},
								type: media.type,
								client: media.brand
							}
						};
						var usableMedia = new dashboard.Media(initEvent);
						
						usableMedia.note = 'transformed from historical data';
						if(media.ended){
							if(media.didAbandon){
								usableMedia.status = 'abandoned';
								usableMedia.ended = media.ended;
								queueDashboard.dataStore.queues[queue].abandoned++;
								console.log(['abandoned', media]);
							} else {
								usableMedia.status = 'completed';
								usableMedia.ended = media.ended;
								queueDashboard.dataStore.queues[queue].completed++;
								console.log(['completed', media]);
							}
							queueDashboard.dataStore.queues[queue]._history[media.id] = usableMedia;
						} else {
							usableMedia.status = 'queued';
							queueDashboard.dataStore.queues[queue].medias[media.id] = usableMedia;
							queueDashboard.dataStore.queues[queue].calls++;
						}
					}
				}
			},
			error:function(res){
				var m = ["getting historical data errored", res];
				warning(m);
				errMessage(m);
			}
		});
	}
	*/
	queueDashboard.drawQueueTable = function(){
		// Should only need to be called once, after the queue list is gotten.
		for(var i in queueDashboard.dataStore.queues){
			var testnom = i;
			nodes = dojo.query('#queueDashboardTable *[queue="' + testnom + '"]');
			if(nodes.length == 0){
				var queueTr = document.createElement('tr');
				queueTr.setAttribute('queue', testnom);
				queueTr.setAttribute('purpose', 'queueDisplay');
				queueTr.setAttribute('expanded', false);
				dojo.create('td', {purpose: 'name', innerHTML: testnom}, queueTr);
				dojo.create('td', {purpose: 'callCount'}, queueTr);
				dojo.create('td', {purpose: 'completeCount'}, queueTr);
				dojo.create('td', {purpose: 'abandonCount'}, queueTr);
				dojo.create('td', {purpose: 'averageHold'}, queueTr);
				dojo.create('td', {purpose: 'oldestHold'}, queueTr);
				queueTr.onclick = function(){
					console.log(this);
					var ex = this.getAttribute('expanded');
					if(ex == 'false'){
						ex = false;
					} else {
					 ex = true
					}
					console.log(['got ex', ex]);
					var queuenom = this.getAttribute('queue');
					console.log('got queuenom');
					if(ex){
						console.log('should try to collapse');
						var callDisps = dojo.query('#queueDashboardTable *[queue="' + queuenom + '"][purpose="callDisplay"]');
						dojo.byId('queueDashboardTable').removeChild(callDisps[0]);
					} else {
						console.log('should try to expand');
						queueDashboard.drawCallTable(queuenom);
					}
					console.log('toggling state');
					this.setAttribute('expanded', ! ex);
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
		var age = now - media.details.queued_at.timestamp;
		tr.setAttribute('callid', mediaid);
		dojo.create('td', {purpose:'callerid', innerHTML: media.details.callid_name + " " + media.details.callid_data}, tr);
		dojo.create('td', {purpose: 'mediaType', innerHTML: '<img src="/images/' + media.details.type + '.png" />'}, tr);
		dojo.create('td', {purpose: 'age', innerHTML: formatseconds(age)}, tr);
		dojo.create('td', {purpose: 'client', innerHTML: media.client}, tr);
		dojo.place(tr, tbody, 'last');
	}
	/*
	queueDashboard.recalcTimerFunc = function(){
		if(dojo.byId('queueDashboardTable')){
			for(var i in queueDashboard.dataStore.queues){
				queueDashboard.dataStore.queues[i].recalc();
			}
			queueDashboard.recalcTimer = setTimeout(queueDashboard.recalcTimerFunc, 5000);
		}
	}*/
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
			//queueDashboard.getHistory();
			//queueDashboard.getStatus();
		} else {
			errMessage(["getting queues failed", res.message]);
		}
	},
	error:function(res){
		errMessage(["getting queues errored", res]);
	}
});

/*queueDashboard.queueSub = dojo.subscribe('dashboard/supevent/queue', queueDashboard, function(action, supevent){
	console.log(['wow, queue sup event', supevent]);
});*/

queueDashboard.mediaSub = dojo.subscribe('dashboard/supevent/media', queueDashboard, function(supevent){
	var supcp = supevent;
	debug(["queueDashboard forwarding", supcp]);
	dojo.publish("queueDashboard/supevent", [supcp]);
});
