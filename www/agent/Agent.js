function Agent(username, statetime, timestamp){
	this.login = username;
	this.securitylevel = "";
	this.profile = "";
	this.skills = [];
	this.state = "";
	this.statedata = "";
	this.pollfailures = 0;
	this.skew = 0;
	this._nags = {};
	
	var agentref = this;

	
	this.setSkew = function(timestamp){
		var now = new Date();
		now = Math.floor(now.getTime() / 1000);
		agentref.skew = now - timestamp;
	};
	
	if(timestamp){
		agentref.setSkew(timestamp);
	}
	
	this.stopwatch = new Stopwatch(statetime + this.skew);
	this.stopwatch.onTick = function(){};
	
	this.handleData = function(datalist){
		for(var i = 0; i < datalist.length; i++){
			 switch (datalist[i].command){
				case "pong":
					agentref.setSkew(datalist[i].timestamp);
					// la la la
					break;
				
				case "astate":
					agentref.state = datalist[i].state;
					agentref.statedata = datalist[i].statedata;
					dojo.publish("agent/state", [datalist[i]]);
					agentref.stopwatch.reset();
					if(datalist[i].state == 'wrapup'){
						this._wrapupNag = this.setNag("You have been in wrapup for more than 3 minutes.  Perhaps you forgot?", 1000 * 60 * 3);
					} else if(this._wrapupNag){
						this.clearNag(this._wrapupNag);
						delete(this._wrapupNag);
					}
					break;

				case "aprofile":
					agentref.state = datalist[i].profile;
					dojo.publish("agent/profile", [datalist[i]]);
					break;

				case "urlpop":
					dojo.publish("agent/urlpop", [datalist[i]]);
					break;
				
				case "blab":
					dojo.publish("agent/blab", [datalist[i].text]);
					break;
				
				case "mediaload":
					dojo.publish("agent/mediaload", [datalist[i]]);
					break;
				
				case "mediaevent":
					dojo.publish("agent/mediaevent/" + datalist[i].media, [datalist[i]]);
					break;
				
				default:
					dojo.publish("agent/" + datalist[i].command, [datalist[i]]);
					info(["non-local command", datalist[i].command]);
			 }
		}
	};
	
	this.poll = function(){
		this._pollHandle = dojo.xhrGet({
			url:"/poll",
			handleAs:"json",
			error:function(response, ioargs){
				if(ioargs.xhr.status === 0){
					warning(["status 0, prolly due to logout", response]);
					dojo.publish("agent/logout", [response.responseText]);
					return;
				}
				warning(["poll errored", response, ioargs.xhr.status]);
				EventLog.log("Poll failed:  " + response.responseText);
				agentref.pollfailures += 1;
				if (agentref.pollfailures >= 5 || ioargs.xhr.status == 403) {
					//agentref.stopwatch.stop();
					//agentref.stopwatch.reset();
					//agentref.poller.stop();
					dojo.publish("agent/logout", [response.responseText]);
				}
				else if (ioargs.xhr.status != 200) {
					agentref.poll();
				} else {
					warning("NOT re-polling");
				}
			},
			load:function(response, ioargs){
				debug([response]);
				//EventLog.log("Poll success, handling data");
				if(response.success === false){
					errMessage(["poll failed", response.message]);
					//agentref.poller.stop();
					dojo.publish("agent/logout", [response.message]);
					//agentref.poll();
				}
				else{
					agentref.pollfailures = 0;
					agentref.poll();
					agentref.handleData(response.data);
				}
			}
		});
	};
	
	this.poll();
}

Agent.states = ["idle", "ringing", "precall", "oncall", "outgoing", "released", "warmtransfer", "wrapup"];

Agent.prototype.setState = function(state){
	//build the request
	var statedata = arguments[1];
	
	var requesturl = '';
	if(statedata){
		requesturl = "/state/" + state + "/" + arguments[1];
	} else {
		requesturl = "/state/" + state;
	}
	
	var agentref = this;
	
	dojo.xhrGet({
		url:requesturl,
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error for set data", response]);
			//EventLog.log("state change failed:  " + response.responseText);
			//dojo.publish("agent/state", [{"success":false, "state":state, "statedata":statedata, "message":responseText}]);
		},
		load:function(response, ioargs){
			EventLog.log("state change success:  " + state);
			agentref.state = state;
			agentref.statedata = statedata;
			agentref.stopwatch.reset();
			agentref.stopwatch.start();
			//dojo.publish("agent/state", [{"success":true, "state":state, "statedata":statedata}]);
		}
	});
};

Agent.prototype.initOutbound = function(Client, Type) {
	dojo.xhrGet({
		url: "/init_outbound/" + Client + '/' + Type,
		handleAs: 'json',
		error: function(response, ioargs){
			errMessage(["error for init outbound", response]);
		},
		load: function(response, ioargs){
			if (response.success){
				EventLog.log("init outbound success");
			} else {
				errMessage(response.message);
			}
		}
	});
};

Agent.prototype.logout = function(/*callback*/){
	var agentref = this;
	dojo.xhrGet({
		url:"/logout",
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error logging out", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/logout", [true]);
				agentref._pollHandle.cancel();
			}			
		}
	});
};

Agent.prototype.dial = function() {
	if (dijit.byId("dialbox").isValid()) {
		EventLog.log("dialing outbound to "+dijit.byId("dialbox").getValue());
		dojo.xhrGet({
			url:"/dial/"+dijit.byId("dialbox").getValue(),
			handleAs:"json",
			error:function(response, ioargs){
				errMessage(["error for dial", response]);
			},
			load:function(response, ioargs){
				if (response.success) {
					debug(["success for dial", response]);
				} else {
					errMessage(["failure for dial", response.message]);
				}
			}
		});
	}
};

Agent.prototype.mediaPush = function(data){
	if(this.state == "oncall"){
		dojo.xhrPost({
			url:"/mediapush",
			handleAs:"json",
			error:function(response, ioargs){
				errMessage(["media push request failure", response]);
			},
			load:function(response, ioargs){
				if(response.success){
					info(["media push success", response]);
				} else{
					errMessage(["media push failed", response.message]);
				}
			},
			content:{
				"data":data
			}
		});
		return true;
	}
	
	return false;
};

Agent.prototype.setNag = function(message, time){
	var ref = this;
	var nag = setTimeout(function(){
		dojo.publish("agent/blab", [message]);
		delete(ref._nags[nag]);
	}, time);
	this._nags[nag] = true;
	return nag;
}

Agent.prototype.clearNag = function(nagref){
	if(this._nags[nagref]){
		clearTimeout(nagref);
	}
	delete this._nags[nagref];
}

Agent.transfer = function(aname) {
	var dialog = dijit.byId("getCaseIDDialog");
	var caseid = dijit.byId('caseid');
	caseid.setAttribute("value", "");
	dialog.attr('execute', function(){
			if (caseid.isValid()) {
				dialog.hide();
				var url = "/agent_transfer/"+aname;
				if (caseid.attr("value") != "") {
					url += "/" + caseid.attr("value");
				}
				dojo.xhrGet({
					url: url,
					handleAs:"json",
					error:function(response, ioargs){
						errMessage(["error on transfer", response]);
					},
					load:function(response, ioargs){
						if(response.success){
							dojo.publish("agent/transfer", [response.success]);
						}
						else{
							errMessage(["failed to ring to 2nd agent", response.message]);
						}
					}
				});
			} else {
				dialog.show();
			}
	});
	dialog.show();
};

Agent.warmtransfer = function(num) {
	dojo.xhrGet({
		url:"/warm_transfer/" + num,
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error on transfer", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/warmtransfer", [response.success]);
			}
			else{
				errMessage(["failed to initiate warm transfer", response.message]);
			}
		}
	});
};

Agent.warmtransfercancel = function() {
	dojo.xhrGet({
		url:"/warm_transfer_cancel",
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error cancelling transfer", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/warmtransfer_cancel", [response.success]);
			}
			else{
				errMessage(["failed to cancel warm transfer", response.message]);
			}
		}
	});
};

Agent.warmtransfercomplete = function() {
	dojo.xhrGet({
		url:"/warm_transfer_complete",
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error completing transfer", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/warmtransfer_complete", [response.success]);
			}
			else{
				errMessage(["failed to complete warm transfer", response.message]);
			}
		}
	});
};

Agent.queuetransfer = function(queue) {
	var dialog = dijit.byId("getCaseIDDialog");
	var caseid = dijit.byId('caseid');
	caseid.setAttribute("value", "");
	dialog.attr('execute', function(){
			if (caseid.isValid()) {
				dialog.hide();
				var url = "/queue_transfer/"+queue;
				if (caseid.attr("value") != "") {
					url += "/" + caseid.attr("value");
				}
				dojo.xhrGet({
					url: url,
					handleAs:"json",
					error:function(response, ioargs){
						errMessage(["error on transfer", response]);
					},
					load:function(response, ioargs){
						if(response.success){
							dojo.publish("agent/queuetransfer", [response.success]);
						}
						else{
							errMessage(["failed to initiate queue transfer", response.message]);
						}
					}
				});
			} else {
				dialog.show();
			}
	});
	dialog.show();
};


Agent.getAvailAgents = function() {
	dojo.xhrGet({
		url:"/get_avail_agents",
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error getting available agents", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/available", [response.agents]);
			}
			else{
				errMessage(["Failed to get agents due to", response]);
			}
		}
	});
};
