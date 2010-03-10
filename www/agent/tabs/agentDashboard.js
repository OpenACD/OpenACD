if(typeof(agentDashboard) == 'undefined'){

	agentDashboard = function(){
		throw 'NameSpace, not to be instanciated';
	}
	
	agentDashboard.profiles = [];
	
	// ======
	// Helper class Profile
	// ======
	
	agentDashboard.Profile = function(display){
		this.name = display;
		this.agents = {};
		this.agentsCount = 0;
		this.avail = 0;
		this.idle = 0;
		this.incall = 0;
		this.released = 0;
		this.wrapup = 0;
		this._masterSubscription = dojo.subscribe('dashboard/supevent/agent', this, function(event){
			//console.log(["consuming profile event thing", event]);
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
				delete this.agents[event.name];
				this._destoryAgentRow(event.name);
				dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
			}
			return true;
		}
		
		if( (event.details.profile == this.name) && ! this.agents[event.name]){
			var agent = new agentDashboard.Agent(event);
			this.agents[event.name] = agent;
			this.agentsCount++;
			this._incState(agent.state);
			agentDashboard.drawAgentTableRow(this.name, agent);
			dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
			return true;
		}
		
		if( (event.details.profile != this.name) && this.agents[event.name]){
			var agent = this.agents[event.name]
			this.agentsCount--;
			this._decState(agent.state);
			delete this.agents[event.name];
			this._destoryAgentRow(event.name);
			dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
			return true;
		}
		
		if(this.agents[event.name]){
			var change = this.agents[event.name].consumeEvent(event);
			this._decState(change.oldState);
			this._incState(change.newState);
			console.log(['doing publish', this.name, this]);
			dojo.publish('agentDashboard/profile/' + this.name + '/update', [this]);
		}
		
		return true;
	};
	
	agentDashboard.Profile.prototype._destoryAgentRow = function(agentname){
		var tbody = dojo.query('#agentDashboardTable *[profile="' + this.name + '"][purpose="agentDisplay"] table')[0];
		var rows = dojo.query('tr[agent="' + agentname + '"]', tbody);
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
				this.statedata = details.reason;
				break;
			case 'idle':
				this.statedata = '';
				break;
			case 'oncall':
			case 'wrapup':
			case 'ringing':
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
		return (idle / total) * 100;
	}
	
	agentDashboard.Agent.prototype.statedataDisplay = function(){
		switch(this.state){
			case 'released':
				return this.statedata.reason;
				break;
			case 'oncall':
			case 'wrapup':
			case 'ringing':
			case 'outgoing':
				d = agent.statedata;
				return '<img src="/images/' + this.statedata.type + '.png" />' + this.statedata.client
			default:
				console.log(['dinna parse', this.statedata]);
				return '';
		}
	}
	
	// =====
	// Other Helper functions
	// =====
	
	agentDashboard.drawProfileTable = function(){
		// Call after fetching the profiles.  Should only need to be done once.
		console.log('drawing profile table');
		for(var i = 0; i < agentDashboard.profiles.length; i++){
			var testnom = agentDashboard.profiles[i].name;
			nodes = dojo.query('#agentDashboard *[profile="' + testnom + '"]');
			if(nodes.length == 0){
				var profCache = agentDashboard.profiles[i];
				var profileTr = dojo.create('tr', {profile: testnom, purpose: 'profileDisplay'}, 'agentDashboardTable');
				dojo.create('td', {purpose: 'name', innerHTML: testnom}, profileTr);
				dojo.create('td', {purpose: 'agentCount', innerHTML: profCache.agentsCount}, profileTr);
				dojo.create('td', {purpose: 'idle', innerHTML: profCache.idle}, profileTr);
				dojo.create('td', {purpose: 'incall', innerHTML: profCache.incall}, profileTr);
				dojo.create('td', {purpose: 'released', innerHTML: profCache.released}, profileTr);
				dojo.create('td', {purpose: 'wrapup', innerHTML: profCache.wrapup}, profileTr);
				profileTr.sub = dojo.subscribe('agentDashboard/profile/' + testnom + '/update', profileTr, function(inProf){
					console.log('updating profile');
					this.cells[1].innerHTML = inProf.agentsCount;
					this.cells[2].innerHTML = inProf.idle;
					this.cells[3].innerHTML = inProf.incall;
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
					/*if(agentDisps.length == 0){
						agentDashboard.drawAgentTable(profile);
					} else {
						dojo.byId('agentDashboardTable').removeChild(agentDisps[0]);
					}*/
					if(agentDisps[0].style.display == 'none'){
						agentDisps[0].style.display = '';
					} else {
						agentDisps[0].style.display = 'none';
					}
				}
				// TODO put the subscription here.
				dojo.byId('agentDashboardTable').appendChild(profileTr);
				agentDashboard.drawAgentTable(profCache);
			}
		}
	}
	
	agentDashboard.drawAgentTable = function(profCache){
		var profile = profCache.name;
		//console.log('drawAgentTable');
		var profileAgentsTr = dojo.create('tr', {'profile': profile, purpose: 'agentDisplay', style:'display:none'});
		dojo.place(profileAgentsTr, dojo.query('#agentDashboardTable *[profile="' + profile + '"][purpose="profileDisplay"]')[0], 'after');
		// make the odd even stripping consistant w/ queueDashboard
		dojo.create('tr', {style:'display:none'}, profileAgentsTr, 'before');

		var widetd = dojo.create('td', {colspan: 6}, profileAgentsTr);
		var table = dojo.create('table', null, widetd);
		table.innerHTML = '<tr>' + 
		'<th>name</th>' +
		'<th>state</th>' +
		'<th>time</th>' +
		'<th>util</th>' +
		'<th>Details</th>';
		var tbody = dojo.query('#agentDashboardTable *[profile="' + profile + '"][purpose="agentDisplay"] table')[0];
		for(var i in profCache.agents){
			agentDashboard.drawAgentTableRow(profile, profCache.agents[i], tbody);
		}
	}
	
	agentDashboard.drawAgentTableRow = function(profile, agent){
		//console.log(['draing agent row', agent]);
		var tr = dojo.create('tr', {'agent':agent.id});//, tbody, 'last');
		var now = Math.floor(new Date().getTime() / 1000);
		dojo.create('td', {'agent':agent.id, purpose:'name', innerHTML:agent.name}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'state', style: 'background-image:url("/images/' + agent.state + '.png")'}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'time', innerHTML: formatseconds(now - agent.lastchange)}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'util', innerHTML: Math.floor(agent.calcUtilPercent()) + '%'}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'details', innerHTML:agent.statedataDisplay()}, tr);
		//name, state, time, util, details
		var tbody = dojo.query('#agentDashboardTable *[profile="' + profile + '"][purpose="agentDisplay"] table')[0];
		//console.log(['drawAgentTableRow', profile, agent, tbody]);
		var agentRows = dojo.query('[agent]', tbody);
		var i = 1;
		for(i; i < agentRows.length; i++){
			if(agentRows[i].getAttribute('agent') > agent.ane){
				break;
			}
		}
		dojo.place(tr, tbody, i);
		tr.subs = [];
		tr.subs.push(dojo.subscribe('agentDashboard/agent/' + agent.id + '/update', tr, function(inAgent){
			//console.log(['tr sub hit', agent.name, agent.id]);
			var nowTime = Math.floor(new Date().getTime() / 1000);
			tr.cells[1].style.backgroundImage = 'url("/images/' + inAgent.state + '.png")';
			tr.cells[2].innerHTML = formatseconds(nowTime - inAgent.lastchange);
			tr.cells[3].innerHTML = Math.floor(inAgent.calcUtilPercent()) + '%';
			tr.cells[4].innerHTML = inAgent.statedataDisplay();
		}));
	}
}

dojo.xhrGet({
	url:'/profilelist',
	handleAs:'json',
	load:function(res){
		if(res.success){
			for(var i = 0; i < res.profiles.length; i++){
				agentDashboard.profiles.push(new agentDashboard.Profile(res.profiles[i].name));
			}
			agentDashboard.drawProfileTable();
		} else {
			errMessage(['Getting profiles failed', res.message]);
		}
	},
	error:function(res){
		errMessage(['Getting profiles errored', res]);
	}
});