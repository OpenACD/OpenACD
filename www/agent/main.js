dojo.addOnLoad(function(){
	EventLog.log("Inteface loaded");

	EventLog.logAgentState = dojo.subscribe("agent/state", function(data){
		var line = "Agent state changed to " + data.state;
		if(data.statedata){
			line += data.statedata.toString();
		}
		EventLog.log(line);
	});
	
	//dijit.byId("loginpane").show();
	dojo.xhrGet({
		url:"/checkcookie",
		handleAs:"json",
		error:function(response, ioargs){
			console.log("checkcookie failed!");
			console.log(response);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.byId("main").style.display="block";
				dojo.byId("main").style.visibility = "visible";
				agent = new Agent(response.login);
				buildReleaseMenu(agent);
				buildOutboundMenu(agent);
				dojo.byId("agentname").innerHTML = response.login;
				agent.state = response.state;
				dojo.publish("agent/state", [{"state":response.state, "statedata":response.statedata}]);
			}
			else{
				dijit.byId("loginpane").show();
			}
		}
	});
	
	//Agent.states = ["idle", "ringing", "precall", "oncall", "outgoing", "released", "warmtransfer", "wrapup"];

	dojo.byId("brand").stateChanger = dojo.subscribe("agent/state", function(data){
		var node = dojo.byId("brand");
		console.log(data.statedata);
		switch(data.state){
			case "ringing":
			case "oncall":
			case "outgoing":
			case "wrapup":
				node.innerHTML = data.statedata.brandname;
				dojo.byId("agentbrandp").style.display = "block";
			break;
			case "precall":
				node.innerHTML = data.statedata;
				dojo.byId("agentbrandp").style.display = "block";
				break;
			default:
				dojo.byId("agentbrandp").style.display = "none";
		}
	});

	dojo.byId("statedisp").stateChanger = dojo.subscribe("agent/state", function(data){
		var node = dojo.byId("statedisp");
		node.innerHTML = "State:" + data.state;
	});

	dijit.byId("bgoreleased").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bgoreleased");
		if(data.state == "released"){
			widget.attr('style', 'display:none');
		}
		else{
			widget.attr('style', 'display:inline');
		}
	});

	dijit.byId("releasedmenu").logout = dojo.subscribe("agent/logout", function(data){
		var widget = dijit.byId("releasedmenu");
		widget.destroyDescendants();
	});
	
	dijit.byId("bgoavail").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bgoavail");
		switch(data.state){
			case "released":
			case "wrapup":
				widget.attr('style', 'display:inline');
			break;
			default:
				widget.attr('style', 'display:none');
		}
	});

	dijit.byId("boutboundcall").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("boutboundcall");
		/*console.log("boutboundcall" + widget.attr('style'));
		switch(data.state){
			case "released":
			case "idle":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}*/
	});
	
	dijit.byId("outboundmenu").logout = dojo.subscribe("agent/logout", function(data){
		var menu = dijit.byId("outboundmenu");
		menu.destroyDescendants();
	});

	dijit.byId("dialbox").stateChanger = dojo.subscribe("agent/state", function(data){
		var div = dojo.byId("foo");
		switch(data.state){
			case "warmtransfer":
			case "precall":
				div.style.display="inline";
				break;
			default:
				div.style.display="none";
		}
	});

	dijit.byId("bcancel").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bcancel");
		switch(data.state){
			case "warmtransfer":
			case "precall":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("bdial").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bdial");
		switch(data.state){
			case "precall":
			case "warmtransfer":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dojo.byId("state").stateChanger = dojo.subscribe("agent/state", function(data){
		dojo.byId("state").innerHTML = data.state;
	});
	
	dijit.byId("banswer").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("banswer");
		console.log("banswer");
		console.log(data);
		if(data.statedata && data.statedata.ringpath == "inband"){
			switch(data.state){
				case "ringing":
					widget.attr('style', "display:inline");
					break;
				default:
					widget.attr('style', 'display:none');
			}
		}
	});
	
	dijit.byId("bhangup").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bhangup");
		console.log("bhangup");
		console.log(data);
		if(data.statedata && data.statedata.mediapath == "inband"){
			switch(data.state){
				case "oncall":
				case "warmtransfer":
				case "outbound":
					widget.attr('style', 'display:inline');
					break;
				default:
					widget.attr('style', 'display:none');
			}
		}
	});
	
	dijit.byId("miHangup").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("miHangup");
		if(data.statedata && data.statedata.mediapath == "inband"){
			switch(data.state){
				case "oncall":
				case "warmtransfer":
				case "outband":
					widget.setDisabled(false);
					break;
				default:
					widget.setDisabled(true);
			}
		}
	});

	dijit.byId("boutboundcall").stateChanger = dojo.subscribe("agent/state", function(data) {
		var widget = dijit.byId("boutboundcall");
		console.log("boutboundcall");
		console.log(data);
		console.log(data.state);
		switch(data.state){
			case "idle":
			case "released":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("eventLogText").eventLogPushed = dojo.subscribe("eventlog/push", function(text){
		var oldval = dijit.byId("eventLogText").value;
		dijit.byId("eventLogText").setValue(oldval + "\n" + text)
	});
	
	var loginform = dijit.byId("loginform")
	dojo.connect(loginform, "onSubmit", function(e){
		e.preventDefault();
		if (loginform.isValid()){
			dojo.xhrGet({
				url:"/getsalt",
				handleAs:"json",
				error:function(response, ioargs){
					dojo.byId("loginerrp").style.display = "block";
					dojo.byId("loginerrspan").innerHTML = response.responseText;
				},
				load:function(response, ioargs){
					EventLog.log("Recieved salt");
					salt = response.salt;
					attrs = loginform.attr("value");
					md5pass = dojox.encoding.digests.MD5(attrs.password, dojox.encoding.digests.outputTypes.Hex);
					salted = dojox.encoding.digests.MD5(salt + md5pass, dojox.encoding.digests.outputTypes.Hex);
					values = attrs;
					values.password = salted;
					dojo.xhrPost({
						url:"/login",
						handleAs:"json",
						content:values,
						load:function(response2, ioargs2){
							if(response2.success){
							EventLog.log("Logged in");
							dijit.byId("loginpane").hide();
							dojo.byId("main").style.display="block";
							dojo.byId("main").style.visibility = "visible";
							dojo.byId("agentname").innerHTML = attrs.username;
							console.log(response2);
							agent = new Agent(attrs.username);
							agent.stopwatch.onTick = function(){
								var elapsed = agent.stopwatch.time();
								var d = new Date();
								d.setHours(0);
								d.setMinutes(0);
								d.setSeconds(elapsed);
								dojo.byId("timerdisp").innerHTML = d.getHours() + ":" + d.getMinutes() + ":" + d.getSeconds();
							}
								buildReleaseMenu(agent);
								buildOutboundMenu(agent);
							}
							else{
								dojo.byId("loginerrp").style.display = "block";
								dojo.byId("loginerrspan").innerHTML = response2.message;
							}
						}
					});
				}
			});
		}
	});

	buildReleaseMenu = function(agent){
		dojo.xhrGet({
			url:"/releaseopts",
			handleAs:"json",
			error:function(response, ioargs){
				var menu = dijit.byId("releasedmenu");
				var item = new dijit.MenuItem({
					label:"Default",
					onClick:function(){agent.setState("released", "Default"); }
				});
				menu.addChild(item);
			},
			load:function(response, ioargs){
				if(response.success){
					var menu = dijit.byId("releasedmenu");
					var item = new dijit.MenuItem({
						label:"Default",
						onClick:function(){agent.setState("released", "Default"); }
					});
					menu.addChild(item);
				}
				else{
					var menu = dijit.byId("releasedmenu");
					var item = new dijit.MenuItem({
						label:"Default",
						onClick:function(){agent.setState("released", "Default"); }
					});
					menu.addChild(item);
				}
			}
		})
	}

	buildOutboundMenu = function(agent){
		var menu = dijit.byId("outboundmenu");
		dojo.xhrGet({
			url:"/brandlist",
			handleAs:"json",
			error:function(response, ioargs){
				console.log(response);
				var item = new dijit.MenuItem({
					label:"Failed to get brandlist1",
					disabled: true
				});
				menu.addChild(item);
			},
			load:function(response, ioargs){
				console.log(response);
				if(response.success){
					for(var i in response.brands) {
						var item = new dijit.MenuItem({
							label:response.brands[i].label,
							onClick:function(){agent.setState("precall", response.brands[i].label); }
						});
						menu.addChild(item);
					}
				}
				else{
					var item = new dijit.MenuItem({
						label:"Failed to get brandlist",
						disabled: true
					});
					menu.addChild(item);
				}
			}
		})
	}

	
	logout = function(agent){
		agent.logout(function(){
			dojo.byId("loginerrp").style.display = "none";
			dijit.byId("loginpane").show();
			dijit.byId("main").attr('style', 'visibility:hidden');
		});
	}
});
