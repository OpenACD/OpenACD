function Agent(username, statetime, timestamp){
	this.login = username;
	this.securitylevel = "";
	this.profile = "";
	this.skills = [];
	this.state = "";
	this.statedata = "";
	this.pollfailures = 0;
	this.skew = 0;
	
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
		dojo.xhrGet({
			url:"/poll",
			handleAs:"json",
			error:function(response, ioargs){
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
		url: "init_outbound/" + Client + '/' + Type,
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
			}			
		}
	});
};

Agent.prototype.dial = function() {
	if (dijit.byId("dialbox").isValid()) {
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
					errMessage(["failure for dial", response]);
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
					errMessage(["media push failed", response]);
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

Agent.transfer = function(aname) {
	dojo.xhrGet({
		url:"/agent_transfer/" + aname,
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error on transfer", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/transfer", [response.success]);
			}
			else{
				errMessage(["failed to ring to 2nd agent", response]);
			}
		}
	});
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
				errMessage(["failed to initiate warm transfer", response]);
			}
		}
	});
};

Agent.queuetransfer = function(queue) {
	dojo.xhrGet({
		url:"/queue_transfer/" + queue,
		handleAs:"json",
		error:function(response, ioargs){
			errMessage(["error on transfer", response]);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.publish("agent/queuetransfer", [response.success]);
			}
			else{
				errMessage(["failed to initiate queue transfer", response]);
			}
		}
	});
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
