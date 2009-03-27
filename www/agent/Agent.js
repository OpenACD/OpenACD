function Agent(username){
	this.login = username;
	this.securitylevel = "";
	this.profile = "";
	this.skills = [];
	this.state = "";
	this.statedata = "";
	
	var agentref = this;

	this.stopwatch = new Stopwatch();
	this.stopwatch.onTick = function(){}
	
	this.handleData = function(datalist){
		for(var i in datalist){
			 switch (datalist[i].command){
				case "astate":
					agentref.state = datalist[i].state;
					agentref.statedata = datalist[i].statedata;
					dojo.publish("agent/state", [datalist[i]]);
					agentref.stopwatch.reset();
					break;
					
				default:
					console.log("unhandled command");
					console.log(datalist[i].command);
			 }
		}
	}
	
	this.poll = function(){
		dojo.xhrGet({
			url:"/poll",
			handleAs:"json",
			error:function(response, ioargs){
				console.log(response);
				EventLog.log("Poll failed:  " + response.responseText);
			},
			load:function(response, ioargs){
				console.log(response);
				EventLog.log("Poll success, handling data");
				agentref.handleData(response.data);
			}
		})
	}
	
	var t = new dojox.timing.Timer(5000);
	t.onTick = function(){
		agentref.poll();
	}
	
	t.start();
	this.stopwatch.start();
	
	this.poll()
}

Agent.states = ["idle", "ringing", "precall", "oncall", "outgoing", "released", "warmtransfer", "wrapup"];

Agent.prototype.setState = function(state){
	//build the request
	var statedata = arguments[1]
	
	if(statedata){
		var requesturl = "/state/" + state + "/" + arguments[1];
	}
	else{
		var requesturl = "/state/" + state;
	}
	
	var agentref = this;
	
	dojo.xhrGet({
		url:requesturl,
		handleAs:"json",
		error:function(response, ioargs){
			console.log("error for set state");
			console.log(response);
			//EventLog.log("state change failed:  " + response.responseText);
			//dojo.publish("agent/state", [{"success":false, "state":state, "statedata":statedata, "message":responseText}]);
		},
		load:function(response, ioargs){
			EventLog.log("state change success:  " + state);
			agentref.state = state;
			agentref.statedata = statedata;
			agentref.stopwatch.reset();
			//dojo.publish("agent/state", [{"success":true, "state":state, "statedata":statedata}]);
		}
	})
}

Agent.prototype.logout = function(callback){
	agentref = this;
	dojo.xhrGet({
		url:"/logout",
		handleAs:"json",
		error:function(response, ioargs){
			console.log("error logging out");
			console.log(response);
		},
		load:function(response, ioargs){
			if(response.success){
				agentref.stopwatch.stop();
				agentref.stopwatch.reset();
				callback();
			}			
		}
	});
}