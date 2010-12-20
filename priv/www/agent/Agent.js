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
		var options = {
			timeout: 30000,
			error:function(response, ioargs){
				if (response.dojoType == "timeout") {
					/*console.log("repolling");*/
					agentref.poll();
					return;
				}

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
			success:function(response){
				debug([response]);
				//EventLog.log("Poll success, handling data");
				agentref.pollfailures = 0;
				agentref.poll();
				agentref.handleData(response);
			},
			failure:function(errcode, msg){
				errMessage(["poll failed", msg]);
				//agentref.poller.stop();
				dojo.publish("agent/logout", [msg]);
				//agentref.poll();
			}
		};
		this._pollHandle = Agent.webApi("poll", options);
	};
	
	this.poll();
}

Agent.states = ["idle", "ringing", "precall", "oncall", "outgoing", "released", "warmtransfer", "wrapup"];

Agent.webApi = function(func, opts){
	var defaultOpts = {
		"success":function(){ return true; },
		"failure":function(errcode, msg){
			console.warn("failure for " + func, errcode, msg);
		},
		"error":function(res){
			console.error("error for ", func, res);
		}
	}
	var trueOpts = dojo.mixin(defaultOpts, opts);
	var loadFunc = function(res){
		if(res.success === true){
			return trueOpts.success(res.result);
		}
		return trueOpts.failure(res.errcode, res.message);
	};
	var args = [];
	for(var i = 2; i < arguments.length; i++){
		args.push(arguments[i]);
	}
	var xhrOverrides = {
		url:"/api",
		content:{
			request:dojo.toJson({
				"function":func,
				"args":args
			})
		},
		handleAs:"json",
		load:loadFunc
	};
	var xhrOpts = dojo.mixin(trueOpts, xhrOverrides);
	
	return dojo.xhrPost(xhrOpts);
};

Agent.prototype.setState = function(state){
	//build the request
	var statedata = arguments[1];

	var agentref = this;

	var options = {
		success:function(resut){
			EventLog.log("state change success:  " + state);
			agentref.state = state;
			agentref.statedata = statedata;
			agentref.stopwatch.reset();
			agentref.stopwatch.start();
		}
	};

	if(statedata){
		Agent.webApi("set_state", options, state, statedata);
	} else {
		Agent.webApi("set_state", options, state);
	}
};

Agent.prototype.initOutbound = function(Client, Type) {
	var options = {
		error: function(response, ioargs){
			errMessage(["error for init outbound", response]);
		},
		success: function(){
			EventLog.log("init outbound success");
		}
	};
	Agent.webApi("init_outbound", Options, Client, Type);
};

Agent.prototype.logout = function(/*callback*/){
	// cancel this up front so
	this._pollHandle.cancel();
	// this should always succeed, so send the agent/logout in all cases
	var options = {
		error:function(response, ioargs){
			errMessage(["error logging out", response]);
			dojo.publish("agent/logout", [true]);
		},
		success:function(){
			dojo.publish("agent/logout", [true]);
		}
	};
	Agent.webApi("logout", options);
};

Agent.prototype.dial = function() {
	if (dijit.byId("dialbox").isValid()) {
		EventLog.log("dialing outbound to "+dijit.byId("dialbox").getValue());
		var options = {
			error:function(response, ioargs){
				errMessage(["error for dial", response]);
			},
			success:function(response, ioargs){
				debug(["success for dial", response]);
			},
			failure:function(code, message){
				errMessage(["failure for dial", message]);
			}
		};
		Agent.webApi("dial", options, dijit.byId("dialbox").getValue());
	}
};

Agent.prototype.mediaPush = function(data){
	if(this.state == "oncall"){
		var options = {
			error:function(response, ioargs){
				errMessage(["media push request failure", response]);
			},
			success:function(response, ioargs){
				info(["media push success", response]);
			},
			failure:function(errcode, message){
				errMessage(["media push failed", message]);
			}
		};
		Agent.webApi("mediapush", options, data);
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
			var options = {
				error:function(response){
					errMessage(["error on transfer", response]);
				},
				success:function(result){
					dojo.publish("agent/transfer", [true]);
				},
				failure:function(errcode, message){
					errMessage(["failed to ring to 2nd agent", message]);
				}
			};
			Agent.webApi("agent_transfer", options, aname, caseid.attr("value"));
		} else {
			dialog.show();
		}
	});
	dialog.show();
};

Agent.warmtransfer = function(num) {
	var options = {
		error:function(response, ioargs){
			errMessage(["error on transfer", response]);
		},
		success:function(){
			dojo.publish("agent/warmtransfer", [response.success]);
		},
		failure:function(errode, message){
			errMessage(["failed to initiate warm transfer", message]);
		}
	};
	Agent.webApi("warm_transfer", options, num);
};

Agent.warmtransfercancel = function() {
	var options = {
		error:function(response, ioargs){
			errMessage(["error cancelling transfer", response]);
		},
		success:function(){
			dojo.publish("agent/warmtransfer_cancel", [true]);
		},
		failure:function(errcode, message){
			errMessage(["failed to cancel warm transfer", message]);
		}
	};
	Agent.webApi("warm_transfer_cancel", options);
};

Agent.warmtransfercomplete = function() {
	var options = {
		error:function(response, ioargs){
			errMessage(["error completing transfer", response]);
		},
		success:function(){
			dojo.publish("agent/warmtransfer_complete", [true]);
		},
		failure:function(errcode, message){
			errMessage(["failed to complete warm transfer", message]);
		}
	};
	Agent.webApi("warm_transfer_complete", options);
};

Agent.queuetransfer = function(queue, skills, urlopts) {
	urlopts.skills = skills;
	var options = {
		error:function(response, ioargs){
			errMessage(["error on transfer", response]);
		},
		success:function(){
			dojo.publish("agent/queuetransfer", [true]);
		},
		failure:function(errcode, message){
			errMessage(["failed to initiate queue transfer", message]);
		}
	};
	Agent.webApi("queue_transfer", options, queue, urlopts);
};


Agent.getAvailAgents = function() {
	var options = {
		error:function(response, ioargs){
			errMessage(["error getting available agents", response]);
		},
		success:function(result){
			dojo.publish("agent/available", [result]);
		},
		failure:function(errcode, message){
			errMessage(["Failed to get agents due to", message]);
		}
	};
	Agent.webApi("get_avail_agents", options);
};
