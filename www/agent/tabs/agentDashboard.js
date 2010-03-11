if(typeof(agentDashboard) == 'undefined'){

	agentDashboard = function(){
		throw 'NameSpace, not to be instanciated';
	}
	
	agentDashboard.profiles = [];
	
	agentDashboard.makeMenu = function(profileNom, agentNom){
		var menu = new dijit.Menu({
			onOpen:function(){
				var profile = {};
				for(var i = 0; i < agentDashboard.profiles.length; i++){
					if(agentDashboard.profiles[i].name == profileNom){
						profile = agentDashboard.profiles[i];
						break;
					}
				}
				var agent = profile.agents[agentNom];
				switch(agent.state){
					case 'released':
					case 'wrapup':
						this.addChild(new dijit.MenuItem({
							label:'Idle',
							onClick:function(){
								agent.setState('idle');
							}
						}));
						break;
					case 'idle':
						this.addChild(new dijit.MenuItem({
							label:'Released',
							onClick:function(){
								agent.setState('released', 'default');
							}
						}));
						break;
					case 'oncall':
						this.addChild(new dijit.MenuItem({
							label:'Spy',
							onClick:function(){
								agent.spy();
							}
						}));
						break;
					default:
						// noop
				}
				this.addChild(new dijit.MenuSeparator());
				this.addChild(new dijit.MenuItem({
					label:'Blab...',
					onClick:function(){ 
						agentDashboard.showBlabDialog('agent', agent.name);
					}
				}));
				this.addChild(new dijit.MenuItem({
					label:'Set Profile...',
					onClick:function(){
						var dialog = dijit.byId("profileSwapDialog");
						var submitSetProf = function(){
							var data = dialog.attr('value');
							agent.setProfile(data.profile);
						}
						dialog.attr('execute', submitSetProf);
						dojo.xhrGet({
							url:"/supervisor/get_profiles",
							handleAs:"json",
							load:function(r){
								if(r.success){
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
								} else {
									warning(["get_profiles failure", r.message]);
								}
							},
							error:function(r){
								warning(["get_profiles errored", r])
							}
						});
					}
				}));
				this.addChild(new dijit.MenuSeparator());
				this.addChild(new dijit.MenuItem({
					label: 'Kick',
					onClick:function(){
						agent.kick();
					}
				}));
			},
			onClose:function(){
				this.destroyDescendants();
			}
		});
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
			agentDashboard.drawAgentTableRow(this, agent);
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
				return this.statedata.reason;
				break;
			case 'oncall':
			case 'wrapup':
			case 'ringing':
			case 'outgoing':
				d = agent.statedata;
				return '<img src="/images/' + this.statedata.type + '.png" />' + this.statedata.client
			default:
				//console.log(['dinna parse', this.statedata]);
				return '';
		}
	}
	
	agentDashboard.Agent.prototype.spy = function(){
		dojo.xhrGet({
			url:'/supervisor/spy/' + this.name,
			handleAs:'json',
			load:function(res){
				if(res.success){
					// cool
				} else {
					errMessage(['Counldn\'t spy', res.message]);
				}
			},
			error:function(res){
				errMessage(['error spying', res]);
			}
		});
	}

	agentDashboard.Agent.prototype.setState = function(stateName, stateData){
		if(! stateData){
			stateData = '';
		} else {
			stateData = '/' + escape(stateData);
		}
		
		var geturl = "/supervisor/agentstate/" + escape(this.name) + "/" + stateName + stateData;
		dojo.xhrGet({
			url:geturl,
			handleAs: "json",
			load: function(resp){
				if(resp.success){
					return true;
				}
				errMessage(["setting state to idle failed", resp.message]);
			},
			errror: function(res){
				errMessage(["setting state to idle error'ed", res]);
			}
		});
	}
	
	agentDashboard.Agent.prototype.setProfile = function(newProf){
		// letting the subscriptions that happen on agent changes deal w/ the repercussions.
		dojo.xhrGet({
			handleAs:"json",
			url:"/supervisor/set_profile/" + escape(this.name) + "/" + escape(newProf),
			load:function(res){
				if(res.success){
					//kewl
					return true;
				} else{
					errMessage(["set profile failed", res.message]);
				}
			},
			error:function(res){
				errMessage(["set profile errored", res]);
			}
		});
	}
	
	agentDashboard.Agent.prototype.kick = function(){
		var geturl = "/supervisor/kick_agent/" + escape(this.name);
		dojo.xhrGet({
			url:geturl,
			handleAs: "json",
			load:function(resp){
				if(resp.success){
					return true;
				}
				errMessage(["kicking agent failed", resp.message]);
			},
			error:function(res){
				errMessage(["Kicking agent errored", res]);
			}
		});
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
	}
	
	agentDashboard.drawAgentTableRow = function(profile, agent){
		var tr = dojo.create('tr', {'agent':agent.id});//, tbody, 'last');
		var now = Math.floor(new Date().getTime() / 1000);
		dojo.create('td', {'agent':agent.id, purpose:'name', innerHTML:agent.name}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'state', style: 'background-image:url("/images/' + agent.state + '.png")'}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'time', innerHTML: formatseconds(now - agent.lastchange)}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'util', innerHTML: Math.floor(agent.calcUtilPercent()) + '%'}, tr);
		dojo.create('td', {'agent':agent.id, purpose:'details', innerHTML:agent.statedataDisplay()}, tr);
		//name, state, time, util, details
		var tbody = dojo.query('#agentDashboardTable *[profile="' + profile.name + '"][purpose="agentDisplay"] table')[0];
		var agentRows = dojo.query('tr[agent]', tbody);
		var i = 1;
		for(i; i < agentRows.length; i++){
			var agentId = agentRows[i].getAttribute('agent');
			var compName = profile.agents[agentId].name;
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
		dojo.xhrGet({
			url:'/supervisor/spy/' + agent,
			load:function(res){
				if(res.success){
					// cool
				} else {
					errMessage(['Counldn\'t spy', res.message]);
				}
			},
			error:function(res){
				errMessage(['error spying', res]);
			}
		});
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
		dojo.xhrPost({
			handleAs:"json",
			url:"/supervisor/blab",
			content:{
				message:replaceUrls(message),
				type: type,
				value: target
			},
			load:function(res){
				debug(["blab worked", res]);
			},
			error:function(res){
				errMessage(["blab failed", res]);
			}
		});
	};	
}

var menu = new dijit.Menu({});
menu.addChild(new dijit.MenuItem({
	label:'Blab...',
	onClick:function(){
		agentDashboard.showBlabDialog('all', 'all');
	}
}));
menu.bindDomNode(dojo.byId('agentDashboardTable').rows[0]);

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
				var nodes = dojo.query('tr[agent="' + agentNom + '"] td[purpose="time"]');
				if(nodes.length > 0){
					nodes[0].innerHTML = formatseconds(now - agent.lastchange);
				}
			}
		}
		delete this.doingTick;
		this.tickCount = 0;
	}
});
