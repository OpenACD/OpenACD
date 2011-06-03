if(typeof(agentDashboard) == 'undefined'){

	agentDashboard = function(){
		throw 'NameSpace, not to be instanciated';
	}
	
	agentDashboard.profiles = [];
	
	agentDashboard.makeMenu = function(profileNom, agentNom){
		var menu = new dijit.Menu({
			onOpen:function(){
				this.profile = {};
				for(var i = 0; i < agentDashboard.profiles.length; i++){
					if(agentDashboard.profiles[i].name == profileNom){
						this.profile = agentDashboard.profiles[i];
						break;
					}
				}
				this.agent = this.profile.agents[agentNom];
				var kids = this.getChildren();
				for(var i = 0; i < kids.length; i++){
					kids[i].attr('disabled', false);;
				}
				switch(this.agent.state){
					case 'released':
					case 'wrapup':
						kids[1].attr('disabled', true);
						kids[2].attr('disabled', true);
						break;
					case 'idle':
					case 'ringing':
						kids[0].attr('disabled', true);
						kids[2].attr('disabled', true);
						break;
					case 'oncall':
						kids[0].attr('disabled', true);
						kids[1].attr('disabled', true);
						break;
					default:
						kids[0].attr('disabled', true);
						kids[1].attr('disabled', true);
						kids[2].attr('disabled', true);
				}
			}
		});
		// TODO i18n these.
		menu.addChild(new dijit.MenuItem({
			label:'Idle',
			onClick:function(){
				this.getParent().agent.setState('idle');
			}
		}));
		menu.addChild(new dijit.MenuItem({
			label:'Released',
			onClick:function(){
				this.getParent().agent.setState('released', 'default');
			}
		}));
		menu.addChild(new dijit.MenuItem({
			label:'Spy',
			onClick:function(){
				this.getParent().agent.spy();
			}
		}));
		menu.addChild(new dijit.MenuSeparator());
		menu.addChild(new dijit.MenuItem({
			label:'Blab...',
			onClick:function(){ 
				agentDashboard.showBlabDialog('agent', this.getParent().agent.name);
			}
		}));
		menu.addChild(new dijit.MenuItem({
			label:'Set Profile...',
			onClick:function(){
				var thisagent = this;
				var dialog = dijit.byId("profileSwapDialog");
				var submitSetProf = function(){
					var data = dialog.attr('value');
					/*console.log(['das data', data]);*/
					thisagent.getParent().agent.setProfile(data.profile, data.makePermanent[0]);
				}
				dialog.attr('execute', submitSetProf);
				window.agentConnection.webApi('supervisor', 'get_profiles', {
					failure:function(r){
						warning(["get_profiles failure", r.message]);
					},
					success:function(r){
						var span = dojo.byId('profileSwapDialogParent');
						while(span.hasChildNodes()){
							span.removeChild(span.firstChild);
						}
						var html = '<select name="profile">';
						dojo.forEach(r.profiles, function(item){
							html += '<option>' + item + '</option>';
						});
						html += '</select>';
						span.innerHTML = html;
						var modenode = span.firstChild;
						new dijit.form.ComboBox({
							name:'profile'
						}, modenode);
						dialog.show();
					},
					error:function(r){
						warning(["get_profiles errored", r])
					}
				});
			}
		}));
		menu.addChild(new dijit.MenuSeparator());
		menu.addChild(new dijit.MenuItem({
			label: 'Kick',
			onClick:function(){
				this.getParent().agent.kick();
			}
		}));
		return menu;
	}
	
	// ======
	// Helper class Profile
	// ======
	
	agentDashboard.Profile = function(display){
		this.name = display;
		this.agents = {};
		this.agentsCount = 0;
		this.avail = 0;
		this.idle = 0;
		this.oncall = 0;
		this.released = 0;
		this.wrapup = 0;
		this._masterSubscription = dojo.subscribe('dashboard/supevent/agent', this, function(event){
			this.consumeEvent(event);
		});
	}
	
	agentDashboard.Profile.prototype._decState = function(state){
		if(! this[state]){
			this[state] = 0;
			return true;
		}
		
		this[state]--;
	}
	
	agentDashboard.Profile.prototype._incState = function(state){
		if(! this[state]){
			this[state] = 1;
			return true;
		}
		
		this[state]++;
	}
		
	agentDashboard.Profile.prototype.consumeEvent = function(event){
		if(event.action == 'drop'){
			if(this.agents[event.name]){
				var state = this.agents[event.name].state;
				this._decState(state);
				this.agentsCount--;
				if (this.agentsCount == 0) {
					nodes = dojo.query('tr[profile="' + this.name + '"]');
					for(var i = 0; i < nodes.length; i++) {
						nodes[i].style.display = 'none';
					}
				}
				delete this.agents[event.name];
				this._destroyAgentRow(event.name);
				dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
			}
			return true;
		}
		
		if( (event.details.profile == this.name) && ! this.agents[event.name]){
			var agent = new agentDashboard.Agent(event);
			this.agents[event.name] = agent;
			if (this.agentsCount == 0) {
				nodes = dojo.query('tr[profile="' + this.name + '"][purpose="profileDisplay"]');
				for(var i = 0; i < nodes.length; i++) {
					nodes[i].style.display = '';
				}
			}
			this.agentsCount++;
			this._incState(agent.state);
			agentDashboard.drawAgentTableRow(this, agent);
			dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
			return true;
		}
		
		if( (event.details.profile != this.name) && this.agents[event.name]){
			var agent = this.agents[event.name]
			this.agentsCount--;
			if (this.agentsCount == 0) {
				nodes = dojo.query('tr[profile="' + this.name + '"]');
				for(var i = 0; i < nodes.length; i++) {
					nodes[i].style.display = 'none';
				}
			}
			this._decState(agent.state);
			delete this.agents[event.name];
			this._destroyAgentRow(event.name);
			dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
			return true;
		}
		
		if(this.agents[event.name]){
			var change = this.agents[event.name].consumeEvent(event);
			this._decState(change.oldState);
			this._incState(change.newState);
			dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
		}
		
		return true;
	};
	
	agentDashboard.Profile.prototype._destroyAgentRow = function(agentname){
		var tbody = dojo.query('#agentDashboardTable *[profile="' + this.name + '"][purpose="agentDisplay"] table')[0];
		var rows = dojo.query('tr[agent="' + escape(agentname) + '"]', tbody);
		for(var i = 0; i < rows.length; i++){
			dojo.forEach(rows[i].subs, function(obj){
				dojo.unsubscribe(obj);
			});
			dojo.destroy(rows[i]);
		}
	}
	
	// ======
	// Helper class Agent
	// ======
	
	agentDashboard.Agent = function(initEvent){
		this.name = initEvent.details.login;
		this.id = initEvent.name;
		this.start = Math.floor(new Date().getTime() / 1000);
		this.state = initEvent.details.state;
		this._setStateData(initEvent.details);
		//this.statedata = initEvent.details.statedata;
		this.setWorking(initEvent);
		var now = Math.floor(new Date().getTime() / 1000);
		if(this._isWorking){
			this._working = now - initEvent.details.lastchange.timestamp;
			this._idleing = 0;
		} else {
			this._working = 0;
			this._idleing = now - initEvent.details.lastchange.timestamp;
		}
		//this._isWorking = false;
		this.lastchange = initEvent.details.lastchange.timestamp;
	}
	
	agentDashboard.Agent.prototype.setWorking = function(event){
		switch(event.details.state){
			case this.state:
				// can't really go from one state to same
				// and released doesn't really change anything important here.
				break;
			case 'idle':
			case 'released':
				this._isWorking = false;
				break;
			default:
				this._isWorking = true;
		}
	}
	
	agentDashboard.Agent.prototype.consumeEvent = function(event){
		//var oldWorking = this._working;
		var now = Math.floor(new Date().getTime() / 1000);
		if(this._isWorking){
			this._working += (now - this.lastchange);
		} else {
			this._idleing += (now - this.lastchange);
		}
		this.setWorking(event);
		var out = {};
		out.oldState = this.state;
		out.newState = event.details.state;
		this.state = event.details.state;
		this._setStateData(event.details);
		//this.statedata = event.details.statedata;
		this.lastchange = event.details.lastchange.timestamp;
		dojo.publish('agentDashboard/agent/' + this.id + '/update', [this]);
		return out;
	}
	
	agentDashboard.Agent.prototype._setStateData = function(details){
		switch(this.state){
			case 'released':
				console.log(['went released', details]);
				if(details.reason){
					this.statedata = details.reason;
				} else {
					this.statedata = 'Default'
				}
				break;
			case 'idle':
				this.statedata = '';
				break;
			case 'oncall':
			case 'wrapup':
			case 'ringing':
				this.statedata = details.statedata;
				break;
			case 'warmtransfer':
				this.statedata = details.statedata;
				break;
			default:
				console.log(['state data not properly determined', this.state, details]);
				this.statedata = details;
		}
	}
	
	agentDashboard.Agent.prototype.calcUtilPercent = function(){
		// TODO This is going to be horribly wrong since we have no history.
		var idle = this._idleing;
		var working = this._working;
		var now = Math.floor(new Date().getTime() / 1000);
		if(this._isWorking){
			working += (now - this.lastchange);
		} else {
			idle += (now - this.lastchange);
		}
		var total = idle + working;
		return (working / total) * 100;
	}
	
	agentDashboard.Agent.prototype.statedataDisplay = function(){
		switch(this.state){
			case 'released':
				return this.statedata;
				break;
			case 'oncall':
			case 'wrapup':
			case 'ringing':
			case 'outgoing':
			case 'precall':
			case 'warmtransfer':
				return '<img src="/images/' + this.statedata.type + '.png" />' + this.statedata.client
			default:
				//console.log(['dinna parse', this.statedata]);
				return '';
		}
	}
	
	agentDashboard.Agent.prototype.spy = function(){
		window.agentConnection.webApi('supervisor', 'spy', {
			failure:function(res){
				errMessage(['Counldn\'t spy', res.message]);
			},
			error:function(res){
				errMessage(['error spying', res]);
			}
		}, this.name);
	}

	agentDashboard.Agent.prototype.setState = function(stateName, stateData){
		var callbacks = {
			failure: function(resp, message){
				errMessage(["setting state to idle failed", message]);
			},
			error: function(res){
				errMessage(["setting state to idle error'ed", res]);
			}
		};

		if(stateData){
			window.agentConnection.webApi('supervisor', 'agent_state', callbacks, this.name, stateName, stateData)
		} else {
			window.agentConnection.webApi('supervisor', 'agent_state', callbacks, this.name, stateName);
		}
	}
	
	agentDashboard.Agent.prototype.setProfile = function(newProf, makePerm){
		// letting the subscriptions that happen on agent changes deal w/ the repercussions.
		/*console.log(['das smack', newProf, makePerm]);*/
		var callbacks = {
			failure:function(res){
				errMessage(["set profile failed", res.message]);
			},
			error:function(res){
				errMessage(["set profile errored", res]);
			}
		};
		if(makePerm){
			window.agentConnection.webApi('supervisor', 'set_profile', callbacks, this.id, newProf, true);
		} else {
			window.agentConnection.webApi('supervisor', 'set_profile', callbacks, this.id, newProf);
		}
	}
	
	agentDashboard.Agent.prototype.kick = function(){
		window.agentConnection.webApi('supervisor', 'kick_agent', {
			failure:function(resp){
				errMessage(["kicking agent failed", resp.message]);
			},
			error:function(res){
				errMessage(["Kicking agent errored", res]);
			}
		}, this.name);
	}
	
	// =====
	// drawing functions
	// =====
	
	agentDashboard.drawProfileTable = function(){
		// Call after fetching the profiles.  Should only need to be done once.
		for(var i = 0; i < agentDashboard.profiles.length; i++){
			var testnom = agentDashboard.profiles[i].name;
			nodes = dojo.query('#agentDashboard *[profile="' + testnom + '"]');
			if(nodes.length == 0){
				var profCache = agentDashboard.profiles[i];
				var profileTr = dojo.create('tr', {profile: testnom, purpose: 'profileDisplay'}, 'agentDashboardTable');
				dojo.create('td', {purpose: 'name', innerHTML: testnom}, profileTr);
				dojo.create('td', {purpose: 'agentCount', innerHTML: profCache.agentsCount}, profileTr);
				dojo.create('td', {purpose: 'idle', innerHTML: profCache.idle}, profileTr);
				dojo.create('td', {purpose: 'incall', innerHTML: profCache.oncall}, profileTr);
				dojo.create('td', {purpose: 'released', innerHTML: profCache.released}, profileTr);
				dojo.create('td', {purpose: 'wrapup', innerHTML: profCache.wrapup}, profileTr);
				profileTr.sub = dojo.subscribe('agentDashboard/profile/' + testnom + '/update', profileTr, function(inProf){
					this.cells[1].innerHTML = inProf.agentsCount;
					this.cells[2].innerHTML = inProf.idle;
					this.cells[3].innerHTML = inProf.oncall;
					this.cells[4].innerHTML = inProf.released;
					this.cells[5].innerHTML = inProf.wrapup;
				});
				profileTr.onclick = function(){
					var profileNom = this.getAttribute('profile');
					var profile = {};
					for(var i = 0; i < agentDashboard.profiles.length; i++){
						if(agentDashboard.profiles[i].name == profileNom){
							profile = agentDashboard.profiles[i];
							break;
						}
					}
					var agentDisps = dojo.query('#agentDashboardTable *[profile="' + profileNom + '"][purpose="agentDisplay"]');
					if(agentDisps[0].style.display == 'none'){
						agentDisps[0].style.display = '';
					} else {
						agentDisps[0].style.display = 'none';
					}
				}
				dojo.byId('agentDashboardTable').appendChild(profileTr);
				var menu = new dijit.Menu({});
				menu.addChild(new dijit.MenuItem({
					label:'Blab...',
					profile: testnom,
					onClick: function(){
						agentDashboard.showBlabDialog('profile', this.profile);
					}
				}));
				menu.bindDomNode(profileTr);
				agentDashboard.drawAgentTable(profCache);
				if (profCache.agentsCount == 0) {
					profileTr.style.display = 'none';
				}
			}
		}
	}
	
	agentDashboard.drawAgentTable = function(profCache){
		var profile = profCache.name;
		var profileAgentsTr = dojo.create('tr', {'profile': profile, purpose: 'agentDisplay', style:'display:none'});
		dojo.place(profileAgentsTr, dojo.query('#agentDashboardTable *[profile="' + profile + '"][purpose="profileDisplay"]')[0], 'after');
		// make the odd even stripping consistant w/ queueDashboard
		dojo.create('tr', {style:'display:none'}, profileAgentsTr, 'before');

		var widetd = dojo.create('td', {colspan: 6}, profileAgentsTr);
		var table = dojo.create('table', {'width':'100%'}, dojo.create('div', {'class':'subData'}, widetd));
		table.innerHTML = '<tr>' + 
		'<th>name</th>' +
		'<th>state</th>' +
		'<th>time</th>' +
		'<th>util</th>' +
		'<th>Details</th>';
		var tbody = dojo.query('#agentDashboardTable *[profile="' + profile + '"][purpose="agentDisplay"] table')[0];
		for(var i in profCache.agents){
			agentDashboard.drawAgentTableRow(profCache, profCache.agents[i], tbody);
		}
		if (profCache.agentsCount == 0) {
			profileAgentsTr.style.display = 'none';
		}
	}
	
	agentDashboard.drawAgentTableRow = function(profile, agent){
		var tr = dojo.create('tr', {'agent':escape(agent.id)});//, tbody, 'last');
		var now = Math.floor(new Date().getTime() / 1000);
		dojo.create('td', {'agent':escape(agent.id), purpose:'name', innerHTML:agent.name}, tr);
		dojo.create('td', {'agent':escape(agent.id), purpose:'state', style: 'background-image:url("/images/' + agent.state + '.png")'}, tr);
		dojo.create('td', {'agent':escape(agent.id), purpose:'time', innerHTML: formatseconds(now - agent.lastchange)}, tr);
		dojo.create('td', {'agent':escape(agent.id), purpose:'util', innerHTML: Math.floor(agent.calcUtilPercent()) + '%'}, tr);
		dojo.create('td', {'agent':escape(agent.id), purpose:'details', innerHTML:agent.statedataDisplay()}, tr);
		//name, state, time, util, details
		var tbody = dojo.query('#agentDashboardTable *[profile="' + profile.name + '"][purpose="agentDisplay"] table')[0];
		var agentRows = dojo.query('tr[agent]', tbody);
		var i = 0;
		for(i; i < agentRows.length; i++){
			var agentId = agentRows[i].getAttribute('agent');
			var compName = profile.agents[unescape(agentId)].name;
			if(compName > agent.name){
				break;
			}
		}
		dojo.place(tr, tbody, i + 1);
		tr.subs = [];
		tr.subs.push(dojo.subscribe('agentDashboard/agent/' + agent.id + '/update', tr, function(inAgent){
			var nowTime = Math.floor(new Date().getTime() / 1000);
			tr.cells[1].style.backgroundImage = 'url("/images/' + inAgent.state + '.png")';
			tr.cells[2].innerHTML = formatseconds(nowTime - inAgent.lastchange);
			tr.cells[3].innerHTML = Math.floor(inAgent.calcUtilPercent()) + '%';
			tr.cells[4].innerHTML = inAgent.statedataDisplay();
		}));
		var menu = agentDashboard.makeMenu(profile.name, agent.id);
		menu.bindDomNode(tr);
	}
	
	// =====
	// Action functions (usual requires server communication)
	// =====
	
	agentDashboard.spy = function(agent){
		window.agentConnection.webApi('supervisor', 'spy', {
			failure:function(res){
				errMessage(['Counldn\'t spy', res.message]);
			},
			error:function(res){
				errMessage(['error spying', res]);
			}
		}, agent);
	}
	
	agentDashboard.showBlabDialog = function(type, target){
		var dialog = dijit.byId('blabDialog');
		dialog.attr('title', 'Blab');
		dialog.attr('value', {'message':'Type your message here.  Url\'s get automatically interpreted'});
		var submitblab = function(){
			var data = dialog.attr('value');
			agentDashboard.blab(data.message, type, target);
		}
		dialog.attr('execute', submitblab);
		dialog.show();
	}
	
	agentDashboard.blab = function(message, type, target){
		window.agentConnection.webApi('supervisor', 'blab', {
			success:function(res){
				debug(["blab worked", res]);
			},
			error:function(res){
				errMessage(["blab failed", res]);
			}
		}, replaceUrls(message), type, target);
	}
}

var menu = new dijit.Menu({});
menu.addChild(new dijit.MenuItem({
	label:'Blab...',
	onClick:function(){
		agentDashboard.showBlabDialog('all', 'all');
	}
}));
menu.bindDomNode(dojo.byId('agentDashboardTable').rows[0]);

window.agentConnection.webApi('supervisor', 'get_profiles', {
	success:function(res){
		for(var i = 0; i < res.length; i++){
			agentDashboard.profiles.push(new agentDashboard.Profile(res[i]));
		}
		agentDashboard.drawProfileTable();
	},
	failure:function(res){
		console.warn('getting profiles failed', res);
		errMessage(['Getting profiles failed', res.message]);
	},
	error:function(res){
		console.error('Getting profiles erred', res);
		errMessage(['Getting profiles errored', res]);
	}
});

agentDashboard.unloadSub = dojo.subscribe('tabPanel-removeChild', function(child){
	if(child.title == 'Dashboard'){
		dojo.unsubscribe(agentDashboard.unloadSub);
		dojo.unsubscribe(agentDashboard.globalTickSub);
		agentDashboard.profiles = [];
	}
});

agentDashboard.refreshRate = 1;
agentDashboard.tickCount = 0;

agentDashboard.globalTickSub = dojo.subscribe('globaltick', agentDashboard, function(){
	if(this.doingTick){
		return;
	}
	
	this.tickCount++;
	if(this.tickCount % this.refreshRate == 0){
		this.doingTick = true;
		var now = Math.floor(new Date().getTime() / 1000);
		for(var i = 0; i < this.profiles.length; i++){
			for(var agentNom in this.profiles[i].agents){
				var agent = this.profiles[i].agents[agentNom];
				var nodes = dojo.query('tr[agent="' + escape(agentNom) + '"] td[purpose="time"]');
				if(nodes.length > 0){
					nodes[0].innerHTML = formatseconds(now - agent.lastchange);
				}
			}
		}
		delete this.doingTick;
		this.tickCount = 0;
	}
});
