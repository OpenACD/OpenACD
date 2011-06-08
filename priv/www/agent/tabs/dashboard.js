if(typeof(dashboard) == 'undefined'){
	var link = document.createElement('link');
	var head = dojo.query('head')[0];
	head.appendChild(link);
	link.rel = 'stylesheet';
	link.href='/tabs/dashboard.css';
	link.type = 'text/css';
	
	dashboard = function(){
		throw 'NameSpace, not to be instanciated';
	}
	
	dashboard.medias = {};
	
	/*dashboard.filterSupevent = function(supevent){
		debug(["the subevent", supevent]);
		if (supevent.type == 'media' || supevent.type == 'queue'){
			return true;
		}
		
		if( (supevent.action == 'drop') && supevent.id.match(/^media-/) ){
			return true;
		}
		
		return false;
	}*/
	
	// =====
	// Media is used for both agents and queues
	// =====
	
	dashboard.Media = function(initEvent){
		this.initialEvent = initEvent;
		this.created = Math.floor(new Date().getTime() / 1000);
		if(initEvent.details.queued_at){
			this.created = initEvent.details.queued_at.timestamp;
		}
		this.type = initEvent.details.type;
		this.client = initEvent.details.client;
		this.status = 'limbo';
	}
	
	dashboard.Media.prototype.end = function(cause){
		this.status = cause;
		this.ended = Math.floor(new Date().getTime() / 1000);
	}
	
	dashboard.getStatus = function(){
		window.agentConnection.webApi('supervisor', 'status', {
			success:function(res){
				//console.log('got status', arguments);
				var real = [];
				var items = res.items;
				for(var i = 0; i < items.length; i++){
					if(items[i].type == 'media' || items[i].type == 'agent'){
						var fixedItem = {
							action: 'set',
							details: items[i].details._value,
							display: items[i].display,
							id: items[i].id,
							type: items[i].type,
							name: items[i].display
						}
						if(items[i].type == 'agent'){
							fixedItem.name = fixedItem.id.substr(6);
						}
						real.push(fixedItem);
					}
				}
				for(i = 0; i < real.length; i++){
					debug(["status fixed", real[i]]);
					if(real[i].type == 'media'){
						dashboard.medias[real[i].name] = real[i];
					}
					dojo.publish("dashboard/supevent/" + real[i].type, [real[i]]);
				}
			},
			error:function(res){
				errMessage(["getting initial status errored", res]);
			}
		});	
	}
	
	// =====
	// Menu Item actions
	// =====
	
	dashboard.showMotdDialog = function(nodename){
		window.agentConnection.webApi('supervisor', 'get_motd', {
			failure:function(res){
				console.warn('Failed getting motd', res);
				errMessage(["Failed getting motd", res.message]);
			},
			success:function(res){
				var dialog = dijit.byId("blabDialog");
				dialog.attr('title', 'MotD');
				if(res){
					dialog.attr('value', {'message':res});
				} else {
					dialog.attr('value', {'message':'Type the Message of the Day here.  Leave blank to unset.'});
				}
				var submitblab = function(){
					var data = dialog.attr('value').message;
					window.agentConnection.webApi('supervisor', 'set_motd', {
						failure:function(res, msg){
							console.warn("setting motd failed", res);
							errMessage(["setting motd failed", msg]);
						},
						error:function(res){
							console.error('setting motd erred', res);
							errMessage(["setting motd errored", res]);
						}
					}, data, nodename);
				}
				dialog.attr('execute', submitblab);
				dialog.show();
			},
			error: function(res){
				console.error('Geting motd erred', res);
				errMessage(["Errored getting motd", res]);
			}
		});
	}
	
	dashboard.showProblemRecordingDialog = function(){
		window.agentConnection.webApi('api', 'get_brand_list', {
			failure:function(res){
				errMessage(["failed loading brands", res.message]);
			},
			success:function(res){
				var sel = dojo.byId('supervisorClientSelect');
				for(var i = 0; i < res.length; i++){
					var optionnode = document.createElement('option');
					optionnode.value = res[i].id;
					optionnode.innerHTML = res[i].label;
					sel.appendChild(optionnode);
				}

				var dialog = dijit.byId('setProblemRecording');
				dialog.attr('execute', function(){
					var clientId = dojo.byId('supervisorClientSelect').value;
					if(dialog.attr('value').set.length < 1){
						window.agentConnection.webApi('supervisor', 'remove_problem_recording', {
							failure:function(res, message){
								console.warn("removing problem recording failed", res, message);
								errMessage(['removing problem recording failed', message]);
							},
							error:function(res){
								console.error("removing problem recording erred", res);
								errMessage(['error removing problem recording', res]);
							}
						}, clientId);
					} else {
						dashboard.startProblemRecording(clientId);
					}
				});
				dialog.show();
			},
			error: function(res){
				errMessage(["error loading brands", res]);
			}
		});	
	}
	
	dashboard.startProblemRecording = function(clientId){
		window.agentConnection.webApi('supervisor', 'start_problem_recording', {
			failure: function(res, msg){
				console.warn('Starting problem recording failed', res, msg);
				errMessage(['Starting problem recording failed', msg]);
			},
			error: function(res){
				console.error('Starting problem recording erred', res);
				errMessage(['Starting problem recofing errored', res]);
			}
		}, clientId);
	}
	
	dashboard.now = function(){
		return Math.floor(new Date().getTime() / 1000);
	}
}

dashboard.masterSetSub = dojo.subscribe("OpenACD/Agent/supervisorSet", dashboard, function(supevent){
	if(supevent.data.type){
		delete this.medias[supevent.data.name];
	}
	supevent.data.action = 'set';
	dojo.publish('dashboard/supevent/' + supevent.data.type, [supevent.data]);
});

dashboard.masterDropSub = dojo.subscribe("OpenACD/Agent/supervisorDrop", dashboard, function(supevent){
	if(this.medias[supevent.data.name]){
		this.medias[supevent.data.name] = supevent.data;
	} else {
		this.medias[supevent.data.name] = supevent.data;
	}
	supevent.data.action = 'drop';
	dojo.publish('dashboard/supevent/' + supevent.data.type, [supevent.data]);
});

dashboard.unloadSub = dojo.subscribe('tabPanel-removeChild', function(child){
	if(child.title == 'Dashboard'){
		dojo.unsubscribe(dashboard.unloadSub);
		var menu = dijit.byId('dashboardMenu');
		if(menu){
			menu.destroy();
		}
	}
});

var menu = dijit.byId('dashboardMenu');
if(menu){
	menu.destroy();
}

var menu = new dijit.Menu({
	style:'display:none'
});

menu.addChild(new dijit.MenuItem({
	label:'Set Motd...',
	onClick:function(){
		dashboard.showMotdDialog('system');
		//errMessage('motd nyi');
	}
}));

menu.addChild(new dijit.MenuItem({
	label:'Record Problem...',
	onClick:function(){
		dashboard.showProblemRecordingDialog();
	}
}));

var button = new dijit.form.DropDownButton({
	label: 'Dashboard',
	name: 'dashboardMenu',
	dropDown: menu,
	id: 'dashboardMenu'
});

dojo.byId("menubar").appendChild(button.domNode);
