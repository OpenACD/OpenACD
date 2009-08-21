function encodeHTML(str) {
	if (!str || !str.replace)
		return str;
	return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/, '&gt;');
}

function decodeHTML(str) {
	if (!str || !str.replace)
		return str;
	return str.replace(/&gt;/g, '>').replace(/&lt;/g, '<').replace(/&amp;/g, '&');
}

dojo.addOnLoad(function(){
	EventLog.log("Inteface loaded");

	EventLog.logAgentState = dojo.subscribe("agent/state", function(data){
		var line = "Agent state changed to " + data.state;
		if(data.statedata){
			line += data.statedata.toString();
		}
		EventLog.log(line);
	});
	
	dojo.xhrGet({
		url:"/checkcookie",
		handleAs:"json",
		error:function(response, ioargs){
			error(["checkcookie failed!", response]);
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
				dojo.byId("profiledisp").innerHTML = dojo.i18n.getLocalization("agentUI", "labels").PROFILE + ":  " + response.profile;
				dojo.publish("agent/state", [{"state":response.state, "statedata":response.statedata}]);
				if( (response.state == "oncall") && (response.statedata.mediapath == "inband") ){
					dojo.xhrGet({
						url:"/mediapull/",
						handleAs:"text",
						load:function(mediadata){
							dojo.publish("agent/mediapush", [{
								"media":response.statedata,
								"content":mediadata
							}]);
						}
					});
				}
				agent.stopwatch.onTick = function(){
					var elapsed = agent.stopwatch.time();
					var d = new Date();
					d.setHours(0);
					d.setMinutes(0);
					d.setSeconds(elapsed);
					var s = "" + d.getSeconds();
					if (d.getSeconds() < 10) {
						s = "0"+s;
					}
					s = d.getMinutes()+":"+s;
					if (d.getHours > 0) {
						s = d.getHours() + ":" + s;
					}
					dojo.byId("timerdisp").innerHTML = s;
				}
				agent.stopwatch.start();				
			}
			else{
				dijit.byId("loginpane").show();
			}
		}
	});
	
	//Agent.states = ["idle", "ringing", "precall", "oncall", "outgoing", "released", "warmtransfer", "wrapup"];

	
	dojo.byId("brand").stateChanger = dojo.subscribe("agent/state", function(data){
		var node = dojo.byId("brand");
		debug(["byId('brand') stateChanger", data.statedata]);
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

	dojo.byId("callerid").stateChanger = dojo.subscribe("agent/state", function(data){
		switch(data.state){
			case 'ringing':
			case 'oncall':
			case 'wrapup':
				dojo.byId("callerid").innerHTML = encodeHTML(data.statedata.callerid);
				dojo.byId("calleridp").style.display = "block";
				break;
			default:
				dojo.byId("calleridp").style.display = "none";
		}
	});
	
	dojo.byId("statedisp").stateChanger = dojo.subscribe("agent/state", function(data){
		var node = dojo.byId("statedisp");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		node.innerHTML = nlsStrings.STATE + ":" + nlsStrings[data.state.toUpperCase()];
	});

	dijit.byId("bgoreleased").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bgoreleased");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		switch (data.state) {
			case 'idle':
			case 'ringing':
			case 'precall':
				widget.setLabel(nlsStrings.GORELEASED);
				widget.attr('style', 'display:inline');
				break;
			case 'released':
				widget.attr('style', 'display:none');
				break;
			default:
				widget.setLabel(nlsStrings.QUEUERELEASE);
				widget.attr('style', 'display:inline');
		}
	});

	dijit.byId("releasedmenu").logout = dojo.subscribe("agent/logout", function(data){
		var widget = dijit.byId("releasedmenu");
		widget.destroyDescendants();
	});
	
	dijit.byId("bgoavail").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bgoavail");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		switch(data.state){
			case "released":
				widget.attr('style', 'display:inline');
				widget.setLabel(nlsStrings.GOAVAILABLE);
				break;
			case "wrapup":
				widget.attr('style', 'display:inline');
				widget.setLabel(nlsStrings.ENDWRAPUP);
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});

	dijit.byId("boutboundcall").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("boutboundcall");
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
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		dojo.byId("state").innerHTML = nlsStrings[data.state.toUpperCase()];
	});
	
	dijit.byId("banswer").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("banswer");
		debug(["banswer", data]);
		if(data.statedata && data.statedata.ringpath == "inband"){
			switch(data.state){
				case "ringing":
					widget.attr('style', "display:inline");
					break;
				default:
					widget.attr('style', 'display:none');
			}
		}
		else{
			widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("btransfer").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("btransfer");
		switch(data.state){
			case "oncall":
			case "warmtransfer":
			case "outbound":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("transferToAgentMenuDyn").agentsAvail = dojo.subscribe("agent/available", function(data){
		var widget = dijit.byId("transferToAgentMenuDyn");
		widget.destroyDescendants()
		dojo.forEach(data, function(i){
			var m = new dijit.MenuItem({
				label: i,
				onClick: function(){
					Agent.transfer(escape(i));
				}
			});
			widget.addChild(m);
		});
	});
	
	dijit.byId("bhangup").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bhangup");
		debug(["bhangup", data]);
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
		else{
			widget.attr('style', 'display:none');
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
		debug(["boutboundcall", data, data.state]);
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
	
	dojo.connect(dijit.byId("emailform"), "onSubmit", function(e){
		e.preventDefault();
		dojo.xhrPost({
			url:"/mediapush",
			handleAs:"json",
			error:function(response, ioargs){
				warning(["email send error ", response]);
			},
			load:function(response, ioargs){
				if(response.success){
					EventLog.log("sent mail");
					debug("success pushing mail");
				}
				else{
					warning(["pusing mail failed: ", response.message]);
				}
			},
			form:dijit.byId("emailform").domNode
		});
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
					if (response.status)
						dojo.byId("loginerrspan").innerHTML = response.responseText;
					else
						dojo.byId("loginerrspan").innerHTML = "Server is not responding";
				},
				load:function(response, ioargs){
					EventLog.log("Recieved salt");
					var salt = response.salt;
					var e = response.pubkey.E;
					var n = response.pubkey.N;
					var attrs = loginform.attr("value");
					var values = attrs;
					var rsa = new RSAKey();
					rsa.setPublic(n, e);
					debug("e: " + e);
					debug("n: " + n);
					debug("password: " + attrs.password);
					values.password = rsa.encrypt(salt + attrs.password);
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
							dojo.byId("profiledisp").innerHTML = dojo.i18n.getLocalization("agentUI", "labels").PROFILE + ":  " + response2.profile;
							debug(response2);
							agent = new Agent(attrs.username);
							agent.stopwatch.onTick = function(){
							var elapsed = agent.stopwatch.time();
							var d = new Date();
							d.setHours(0);
							d.setMinutes(0);
							d.setSeconds(elapsed);
							var s = "" + d.getSeconds();
							if (d.getSeconds() < 10) {
								s = "0"+s;
							}
							s = d.getMinutes()+":"+s;
							if (d.getHours > 0) {
								s = d.getHours() + ":" + s;
							}
							dojo.byId("timerdisp").innerHTML = s;
							}
							buildReleaseMenu(agent);
							buildOutboundMenu(agent);
							agent.stopwatch.start();
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
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		dojo.xhrGet({
			url:"/releaseopts",
			handleAs:"json",
			error:function(response, ioargs){
				var menu = dijit.byId("releasedmenu");
				var item = new dijit.MenuItem({
					label: nlsStrings.DEFAULT,
					onClick:function(){agent.setState("released", "Default"); }
				});
				menu.addChild(item);
			},
			load:function(response, ioargs){
				if(response.success){
					var menu = dijit.byId("releasedmenu");
					var item = new dijit.MenuItem({
						label: nlsStrings.DEFAULT,
						onClick:function(){agent.setState("released", "Default"); }
					});
					menu.addChild(item);
				}
				else{
					var menu = dijit.byId("releasedmenu");
					var item = new dijit.MenuItem({
						label: nlsStrings.DEFAULT,
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
				debug(response);
				var item = new dijit.MenuItem({
					label:"Failed to get brandlist1",
					disabled: true
				});
				menu.addChild(item);
			},
			load:function(response, ioargs){
				debug(["buildOutboundMenu", response]);
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

	dojo.byId("loginerrp").logout = dojo.subscribe("agent/logout", function(data){
		dojo.byId("loginerrp").style.display = "none";
	});
	
	dojo.byId("loginpane").logout = dojo.subscribe("agent/logout", function(data){
		dijit.byId("loginpane").show();
	})

	dijit.byId("main").logout = dojo.subscribe("agent/logout", function(data){
		dijit.byId("main").attr('style', 'visibility:hidden');
	});
	
	dijit.byId("main").pop = dojo.subscribe("agent/urlpop", function(data){
		if(dijit.byId("popup")){
			dijit.byId("popup").destroy();
		}
		
		var elem = document.createElement('div');
		elem.id = "popup";
		
		document.body.insertBefore(elem, document.body.firstChild);
		
		var popup = new dojox.layout.FloatingPane({
			title:"Url Pop",
			resizable: true,
			dockable:false,
			style: 'position:absolute; top: 100px; left: 400px; z-index:1000',
			content: '<iframe width="100%", height="100%" src="' + data.url + '" />'
		}, dojo.byId("popup"));
		
		popup.startup();
		popup.show();
	});

	logout = function(agent){
		agent.logout();
	}
	
	dijit.byId("main").mediapush = dojo.subscribe("agent/mediapush", function(eventdata){
		info(["main listing for media push paid off", eventdata]);
		if(dijit.byId("mediapush") == undefined){
			var mediaPane = new dojox.layout.FloatingPane({
				id:"mediapush",
				title:eventdata.media.id,
				resizable: true,
				dockable:false,
				closable:false,
				style: 'position:absolute; top:100px; left: 400px; z-index:50000',
				content: eventdata.content
			}, dojo.create("div", null, "main"));
			mediaPane.agentStateSub = dojo.subscribe("agent/state", function(data){
				mediaPane.destroy();
			});
			mediaPane.startup();
			mediaPane.show();
			return;
		}
		
		if(eventdata.mode == "append"){
			var oldcontent = dijit.byId("mediapush").content;
		}
		else{
			oldcontent = '';
		}
		
		dijit.byId("mediapush").content = oldcontent + eventdata.content;
	});		   
});

function endpointselect() {
	switch(dijit.byId("voipendpoint").attr('value')) {
		case "SIP Registration":
			dijit.byId("voipendpointdatahint").label = dojo.i18n.getLocalization("agentUI", "labels").SIPREGHINT;
			break;
		case "SIP URI":
			dijit.byId("voipendpointdatahint").label = dojo.i18n.getLocalization("agentUI", "labels").SIPHINT;
			break;
		case "IAX2 URI":
			dijit.byId("voipendpointdatahint").label = dojo.i18n.getLocalization("agentUI", "labels").IAXTWOHINT;
			break;
		case "H323 URI":
			dijit.byId("voipendpointdatahint").label = dojo.i18n.getLocalization("agentUI", "labels").HTHREETWOTHREEHINT;
			break;
		case "PSTN Number":
			dijit.byId("voipendpointdatahint").label = dojo.i18n.getLocalization("agentUI", "labels").PSTNHINT;
			break;
		default:
			dijit.byId("voipendpointdatahint").label = "???";
			break;
	}
}
