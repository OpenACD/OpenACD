function Agent(username){
	this.login = "";
	this.securitylevel = "";
	this.profile = "";
	this.skills = [];
	this.state = "";
	this.statedata = "";
	
	var agentref = this;

	this.stopwatch = new Stopwatch();
	this.stopwatch.onTick = function(){}
	
	this.handleData = function(datalist){
		/*for(var i in datalist){
			 switch (datalist[i].command){
				case "astate":
					dojo.publish("agent.states", 
					break;
					
				default 
			 }
		}*/
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
	
	dojo.xhrGet({
		url:requesturl,
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("state change failed:  " + response.responseText);
			dojo.publish("agent.states", [{"success":false, "state":state, "statedata":statedata, "message":responseText}]);
		},
		load:function(response, ioargs){
			EventLog.log("state change success:  " + state);
			this.state = state;
			this.statedata = statedata;
			this.stopwatch.reset();
			dojo.publish("agent.states", [{"success":true, "state":state, "statedata":statedata}]);
		}
	})
}