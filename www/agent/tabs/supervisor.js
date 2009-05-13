supervisorTab = function(){
	{};
};

supervisorTab.pollNodes = function(){
	dojo.xhrGet({
		url:"/supervisor/nodes",
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("Node poll failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			console.log(response);
			if(response.success){
				EventLog.log("Node poll success, handling data...");
				dojo.publish("supervisor/node/poll", [response.nodes]);
			}
		}
	});
};

supervisorTab.pollAgentProfiles = function(node){
	dojo.xhrGet({
		url:"/supervisor/" + node + "/agent_profiles",
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("Pulling agents profiles failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			if(response.success){
				EventLog.log("Agent profile poll success, handling data...");
				for(var i in response.result){
					dojo.publish("supervisor/" + response.result[i].node + "/agent_profiles", [response.result[i].profiles]);
				}
			}
		}
	});
};

supervisorTab.pollQueues = function(node){
	dojo.xhrGet({
		url:"/supervisor/" + node + "/queues",
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("Pulling queues failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			if(response.success){
				EventLog.log("Queue poll success, handling data...");
				for(var i in response.result){
					dojo.publish("supervisor/" + response.result[i].node + "/queues", [response.result[i].queues]);
				}
			}
		}
	});
};

supervisorTab.pollProfile = function(node, profile){
	dojo.xhrGet({
		url:"/supervisor/" + node + "/agent/" + profile,
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("Pulling agents failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			if(response.success){
				EventLog.log("Profile polling success, handling data...");
				for(var i in response.result){
					dojo.publish("supervisor/" + response.result[i].node + "/agent/" + profile, [response.result[i].agents]);
				}
			}
		}
	})
};

supervisorTab.pollAgentCall = function(node, agent){
	dojo.xhrGet({
		url:"/supervisor/" + node + "/agent/" + agent + "/callid",
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("Pulling agent call failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			if(response.success){
				EventLog.log("Pulling agent call success, handling data...");
				dojo.publish("supervisor/" + node + "/agent/" + agent + "/callid", [response.result])
			}
		}
	});
};

supervisorTab.pollQueue = function(node, queue){
	dojo.xhrGet({
		url:"/supervisor/" + node + "/queue/" + queue,
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("Pulling queue data failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			if(response.success){
				EventLog.log("Polling queue data success, handling data...");
				dojo.publish("supervisor/" + node + "/queue/" + queue, [response.result])
			}
		}
	});
};

supervisorTab.pollQueuedCall = function(node, queue, callid){
	dojo.xhrGet({
		url:"/supervisor/" + node + "/queue/" + queue + "/" + callid,
		handleAs:"json",
		error:function(response, ioargs){
			EventLog.log("pulling queued call failed:  " + response.responseText);
		},
		load:function(response, ioargs){
			if(response.success){
				EventLog.log("pulling queued call success");
				dojo.publish("supervisor/" + node + "queue/" + queue + "/" + callid, [response.result]);
			}
		}
	})
};
