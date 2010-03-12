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
		/*console.log(data);*/
		if(data.type != 'media'){
			return false;
		}

		if(data.action == 'drop'){
			if(! this.medias[data.name]){
				return true;
			}
			delete this.medias[data.name];
			var nodes = dojo.query('table[purpose="callDisplay"][queue="' + this.name + '"] tr[callid="' + data.name + '"]');
			while(nodes.length > 0){
				var n = nodes.pop();
				var thistable = n.parentNode;
				var queuerow = dojo.query('#queueDashboardTable tr[purpose="queueDisplay"][queue="' + this.name + '"]')[0];
				queuerow.cells[1].innerHTML = parseInt(queuerow.cells[1].innerHTML) - 1; /* callcount */
				queuerow.cells[3].innerHTML = parseInt(queuerow.cells[3].innerHTML) + 1; /* abandoned */
				n.parentNode.removeChild(n);
				var longest = 0;
				for(var i = 0; i < thistable.rows.length - 1; i++) {
					var realvalue = queuerow.cells[5].getAttribute("realvalue");
					if (longest == 0 || longest > realvalue) {
						longest = realvalue;
					}
				}
				queuerow.cells[5].setAttribute("realvalue", longest);
				if (longest > 0) {
					var now = Math.floor(new Date().getTime() / 1000);
					var age = now - longest;
					queuerow.cells[5].innerHTML = formatseconds(age);
				} else {
					queuerow.cells[5].innerHTML = "0:00";
				}
			}
			return true;
		}

		/* wtf? */
		if(data.details.queue != this.name && this.medias[data.name]){
			delete this.medias[data.name];
			var nodes = dojo.query('table[purpose="callDisplay"][queue="' + this.name + '"] tr[callid="' + data.name + '"]');
			while(nodes.length > 0){
				var n = nodes.pop();
				var thistable = n.parentNode;
				var queuerow = dojo.query('#queueDashboardTable tr[purpose="queueDisplay"][queue="' + this.name + '"]')[0];
				queuerow.cells[1].innerHTML = parseInt(queuerow.cells[1].innerHTML) - 1; /* callcount */
				queuerow.cells[2].innerHTML = parseInt(queuerow.cells[2].innerHTML) + 1; /* completed */
				n.parentNode.removeChild(n);
				var longest = 0;
				for(var i = 0; i < thistable.rows.length - 1; i++) {
					var realvalue = queuerow.cells[5].getAttribute("realvalue");
					if (longest == 0 || longest > realvalue) {
						longest = realvalue;
					}
				}
				queuerow.cells[5].setAttribute("realvalue", longest);
				if (longest > 0) {
					var now = Math.floor(new Date().getTime() / 1000);
					var age = now - longest;
					queuerow.cells[5].innerHTML = formatseconds(age);
				} else {
					queuerow.cells[5].innerHTML = "0:00";
				}
			}
			dojo.publish('queueDashboard/updateQueue/' + data.name, [{action:'drop'}]);
		} else if(data.details.queue == this.name){
			this.medias[data.name] = data;
			dojo.publish('queueDashboard/updateQueue/' + data.name, [{action:'set'}]);
			var nodes = dojo.query('table[purpose="callDisplay"][queue="' + this.name + '"] tr[callid="' + data.name + '"]');
			if(nodes.length == 0){
				/* add row */
				var rows = dojo.query('#queueDashboardTable tr[queue="' + data.details.queue + '"][purpose="queueDisplay"]');
				if(rows.length == 1) {
					rows[0].cells[1].innerHTML = parseInt(rows[0].cells[1].innerHTML) + 1;
					var queuerow = rows[0];
					var realvalue = parseInt(queuerow.cells[5].getAttribute("realvalue"));
					if (realvalue < 1 || realvalue > data.details.queued_at) {
						var now = Math.floor(new Date().getTime() / 1000);
						var age = now - data.details.queued_at.timestamp;
						queuerow.cells[5].setAttribute("realvalue", data.details.queued_at);
						queuerow.cells[5].innerHTML = formatseconds(age);
					}
					var tbody = dojo.query('table[purpose="callDisplay"][queue="' + this.name + '"]')[0];
					queueDashboard.drawCallTableRow(data.details.queue, data.name, tbody);
				}
			} else {
				/* update row */
				console.log("update row NYI");
			}
		}

		return true;
	}

	// =====
	// Helpful functions
	// =====

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
				dojo.create('td', {purpose: 'callCount', innerHTML: "0"}, queueTr);
				dojo.create('td', {purpose: 'completeCount', innerHTML: "0"}, queueTr);
				dojo.create('td', {purpose: 'abandonCount', innerHTML: "0"}, queueTr);
				dojo.create('td', {purpose: 'averageHold', realvalue: 0, innerHTML: "0:00"}, queueTr);
				dojo.create('td', {purpose: 'oldestHold', realvalue: 0, innerHTML: "0:00"}, queueTr);
				queueTr.onclick = function(){
					this.setAttribute('expanded', 'true');
					var queuenom = this.getAttribute('queue');
					var tables = dojo.query('table[purpose="callDisplay"]');
					for(var i = 0; i < tables.length; i++){
						if(tables[i].getAttribute('queue') == queuenom){
							tables[i].style.display = '';
						} else {
							tables[i].style.display = 'none';
						}
					}
					var rows = dojo.byId('queueDashboardTable').rows;
					for(i = 1; i < rows.length; i++){
						if(rows[i].getAttribute('queue') != queuenom){
							rows[i].removeAttribute('expanded');
						}
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

				queueDashboard.drawCallTable(testnom);
			}
		}
	}

	queueDashboard.drawCallTable = function(queuename){
		var table = dojo.create('table', {width: "100%", 'class':'dashboard'}, dojo.create('div', {'class':'subData'}));
		table.innerHTML = '<tr>' +
		'<th>Caller ID</th>' +
		'<th>Type</th>' +
		'<th>Time</th>' +
		'<th>Brand</th>' +
		'</tr>';

		table.style.display = 'none';
		table.setAttribute('queue', queuename);
		table.setAttribute('purpose', 'callDisplay');
		dojo.place(table, 'dashboardQueueCallsPane', 'last');
	}

	queueDashboard.drawCallTableRow = function(queuename, mediaid, tbody){
		var tr = document.createElement('tr');
		var media = queueDashboard.dataStore.queues[queuename].medias[mediaid];
		if(!media) {
			console.log("Can't find "+mediaid);
			return; /* TODO - why does this occur? */
		}
		var now = Math.floor(new Date().getTime() / 1000);
		var age = now - media.details.queued_at.timestamp;
		tr.setAttribute('callid', mediaid);
		dojo.create('td', {purpose:'callerid', innerHTML: media.details.callid_name + " " + media.details.callid_data}, tr);
		dojo.create('td', {purpose: 'mediaType', innerHTML: '<img src="/images/' + media.details.type + '.png" />'}, tr);
		dojo.create('td', {purpose: 'age', realvalue: media.details.queued_at, innerHTML: formatseconds(age)}, tr);
		dojo.create('td', {purpose: 'client', innerHTML: media.details.client}, tr);
		dojo.place(tr, tbody, 'last');
		var menu = new dijit.Menu({});
		menu.addChild(new dijit.MenuItem({
			label: "Send To Agent...",
			onClick: function() { queueDashboard.sendToAgentDialog(mediaid, queuename);}
		}));
		if (media.details.type == "email") {
			menu.addChild(new dijit.MenuItem({
				label: "Peek",
				onClick: function() {queueDashboard.mediaPeek(mediaid, queuename)}
			}));
			menu.addChild(new dijit.MenuItem({
				label: "Remove",
				onClick: function() {queueDashboard.removeFromQueue(mediaid, queuename)}
			}));
		} else if (media.details.type == "voice") {
			menu.addChild(new dijit.MenuItem({
				label: "Send to voicemail",
				onClick: function(){
					confirmDialog({
						'title':'Send to Voicemail',
						'question':'Are you sure you want to send this call to voicemail?',
						'yesLabel':'Voicemail',
						'noLabel':'Cancel',
						'yesAction':function(){queueDashboard.sendToVoicemail(mediaid, queuename);}
					});
				}
			}));
		}
		tr.boundMenu = menu;
		menu.bindDomNode(tr);
	}

	queueDashboard.sendToAgentDialog = function(mediaID, queue){
		console.log(["mediaID", mediaID]);
		dojo.xhrGet({
			url:'/get_avail_agents',
			handleAs:'json',
			load: function(res){
				if(res.success){
					console.log(res.agents);
					var selectContent = '';
					if(res.agents.length == 0){
						errMessage('No agents available!');
						return false;
					}
					for(var i = 0; i < res.agents.length; i++){
						selectContent += '<option value="' + res.agents[i].name + '">' + res.agents[i].name + ' (' + res.agents[i].profile + ')</option>';
					}
					var content = '<p><label>Agent:</label><select name="agent" id="supSelectAgent">' + selectContent + '</select></p><p><label>&nbsp;</label><input type="submit" dojoType="dijit.form.Button" label="Submit" /></p>';
					var dialog = new dijit.Dialog({
						title:'Select Agent',
						content: content
					});
					dialog.attr('execute', function(){
						var agentName = dojo.byId('supSelectAgent').value;
						dialog.destroy();
						console.log([agentName, arguments]);

						var simpleObj = {
							id: mediaID,
							queue: queue
						};

						queueDashboard.sendMediaToAgent(simpleObj, agentName);
					});
					dialog.show();
					return true;
				}
				errMessage(['getting available agents failed', res.message]);
			},
			error: function(res){
				errMessage(['getting available agents errored', res]);
			}
		});
	};

	queueDashboard.sendMediaToAgent = function(media, agent){
		if(! agent){
			return false;
		}

		if(media.queue){
			var queue = media.queue;
			var id = media.id;
			return dojo.xhrGet({
				handleAs:"json",
				url:"/supervisor/agent_ring/" + escape(queue) + "/" + escape(id) + "/" + escape(agent),
				load:function(res){
					if(res.success){
						//kewl
						return true;
					}
					else{
						errMessage(["agent ring failed", res.message]);
					}
				},
				error:function(res){
					errMessage(["agent ring errored", res]);
				}
			});
		}
	};

	queueDashboard.mediaPeek = function(mediaid, queue){
		if(queue){
			queue = escape(queue);
		} else {
			return false;
		}

		id = escape(mediaid);
		dojo.xhrGet({
			url: '/supervisor/peek/' + queue + '/' + id,
			handleAs: 'json',
			load: function(res){
				if(res.success){
					return true;
				}

				errMessage(["peeking at media failed", res.message]);
			},
			error: function(res){
				errMessage(["peeking at media failed", res]);
			}
		});
	}

	queueDashboard.removeFromQueue = function(mediaid, queue){
		if(! queue){
			return false;
		}

		queue = escape(queue);
		var id = mediaid;
		dojo.xhrGet({
			url:'/supervisor/drop_call/' + queue + '/' + id,
			handleAs: 'json',
			load: function(res){
				if(res.success){
					return true;
				}

				errMessage(["drop call failed", res.message]);
			},
			error: function(res){
				errMessage(["drop call errored", res]);
			}
		});
	}

	queueDashboard.sendToVoicemail = function(mediaid, queue){
		dojo.xhrPost({
			url:'/supervisor/voicemail/' + escape(queue) + '/' + escape(mediaid),
			handleAs:'json',
			load:function(res){
				if(res.success){
					return true;
				}
				else{
					errMessage(["sending to voicemail failed", res.message]);
				}
			},
			error:function(res){
				errMessage(["sending to voicemail errored", res]);
			}
		});
	};

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
		} else {
			errMessage(["getting queues failed", res.message]);
		}
	},
	error:function(res){
		errMessage(["getting queues errored", res]);
	}
});

queueDashboard.mediaSub = dojo.subscribe('dashboard/supevent/media', queueDashboard, function(supevent){
	var supcp = supevent;
	debug(["queueDashboard forwarding", supcp]);
	dojo.publish("queueDashboard/supevent", [supcp]);
});
