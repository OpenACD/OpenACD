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
		this.incall = 0;
		this.released = 0;
		this.wrapup = 0;
		this._masterSubscription = dojo.subscribe('dashboard/supevent/agent', this, function(event){
			//console.log(["consuming profile event thing", event]);
			this.consumeEvent(event);
		});
	}
	
	agentDashboard.Profile.prototype.consumeEvent = function(event){
		if(event.action == 'drop'){
			delete this.agents[event.name];
			return true;
		}
		
		if(event.details.profile == this.name){
			this.agents[event.name] = event;
		}
	};
	
	// ======
	// Helper class Agent
	// ======
	
	agentDashboard.Agent = function(initEvent){
		this.name = initEvent.details.login;
		this.start = Math.floor(new Date().getTime() / 1000);
		this._idleing = 0;
		this._working = 0;
		this._isWorking = false;
		this._lastChangeTime = Math.floor(new Date().getTime() / 1000);
	}
	
	agentDashboard.Agent.prototype.calcUtilPercent = function(){
		var idle = this._idleing;
		var working = this._working;
		var now = Math.floor(new Date().getTime() / 1000);
		if(this._isWorking){
			working += (now - this._lastChangeTime);
		} else {
			idle += (now - this._lastChangeTime);
		}
		var total = idle + working;
		return (idle / total) * 100;
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
				dojo.create('td', {purpose: 'avail', innerHTML: profCache.avail}, profileTr);
				dojo.create('td', {purpose: 'incall', innerHTML: profCache.incall}, profileTr);
				dojo.create('td', {purpose: 'released', innerHTML: profCache.released}, profileTr);
				dojo.create('td', {purpose: 'wrapup', innerHTML: profCache.wrapup}, profileTr);
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
					if(agentDisps.length == 0){
						agentDashboard.drawAgentTable(profile);
					} else {
						dojo.byId('agentDashboardTable').removeChild(agentDisps[0]);
					}
				}
				// TODO put the subscription here.
				dojo.byId('agentDashboardTable').appendChild(profileTr);
			}
		}
	}
	
	agentDashboard.drawAgentTable = function(profCache){
		var profile = profCache.name;
		//console.log('drawAgentTable');
		var profileAgentsTr = dojo.create('tr', {'profile': profile, purpose: 'agentDisplay'});
		dojo.place(profileAgentsTr, dojo.query('#agentDashboardTable *[profile="' + profile + '"][purpose="profileDisplay"]')[0], 'after');
		dojo.create('td', null, profileAgentsTr);
		var widetd = dojo.create('td', {colspan: 5}, profileAgentsTr);
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
	
	agentDashboard.drawAgentTableRow = function(profile, agent, tbody){
		//console.log(['drawAgentTableRow', profile, agent, tbody]);
		var tr = dojo.create('tr', {'agent':agent.name}, tbody, 'last');
		var now = Math.floor(new Date().getTime() / 1000);
		dojo.create('td', {'agent':agent.name, purpose:'name', innerHTML:agent.name}, tr);
		dojo.create('td', {'agent':agent.name, purpose:'state', innerHTML:'<img src="/images/' + agent.details.state + '.png" />'}, tr);
		dojo.create('td', {'agent':agent.name, purpose:'time', innerHTML: formatseconds(now - agent.details.lastchange.timestamp)}, tr);
		dojo.create('td', {'agent':agent.name, purpose:'util', innerHTML:agent.util}, tr);
		var details = '';
		switch(agent.details.state){
			case 'released':
				details = agent.details.reason;
				break;
			case 'oncall':
			case 'wrapup':
			case 'ringing':
			case 'outgoing':
				d = agent.details.statedata;
				details = '<img src="/images/' + d.type + '.png" />' + d.client;
				break;
			default:
				console.log(['dinna parse', agent]);
		}
		dojo.create('td', {'agent':agent.name, purpose:'details', innerHTML:details}, tr);
		//name, state, time, util, details
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