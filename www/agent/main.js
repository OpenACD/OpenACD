function encodeHTML(str) {
	if (!str || !str.replace){
		return str;
	}
	return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/, '&gt;');
}

function decodeHTML(str) {
	if (!str || !str.replace){
		return str;
	}
	return str.replace(/&gt;/g, '>').replace(/&lt;/g, '<').replace(/&amp;/g, '&');
}

function errMessage(message){
	var dialog = new dijit.Dialog({
		title: "<span style='color:#ff3333'>Error</span>",
		content: message.toString()
	});
	dojo.connect(dialog, 'onCancel', dialog, function(){
		dialog.destroy();
	});
	dialog.show();
}

function confirmDialog(conf){
	var defaultConf = {
		'yesLabel': 'Yes',
		'noLabel': 'No',
		'yesAction': function(){ return true},
		'noAction': function(){ return false},
		'question': 'Are you sure?',
		'title': 'Confirmation'
	};
	
	conf = dojo.mixin(defaultConf, conf);
	
	var dialog = new dijit.Dialog({
		title: conf.title,
		content: '<div style="align:center">' + 
			'<p>' + conf.question + '</p>' + 
			'<p><input dojoType="dijit.form.Button" type="button" label="' + conf.noLabel + '">' + 
			'<input dojoType="dijit.form.Button" type="button" label="' + conf.yesLabel + '" />' +
			'</div>'
	});
	dojo.connect(dialog, 'onCancel', dialog, function(){
		conf.noAction();
		dialog.destroy();
	});
	var kids = dialog.getChildren()
	dojo.connect(kids[0], 'onClick', dialog, function(){
		conf.noAction();
		dialog.destroy();
	});
	dojo.connect(kids[1], 'onClick', dialog, function(){
		conf.yesAction();
		dialog.destroy();
	});
	dialog.show();
}

function getTheme() {
	if (dojo.cookie('agentui-settings')) {
		var settings = dojo.fromJson(dojo.cookie('agentui-settings'));
		return settings.theme;
	}
	return undefined;
}

function setTheme(theme) {
	var settings = {};
	if (dojo.cookie('agentui-settings')) {
		settings = dojo.fromJson(dojo.cookie('agentui-settings'));
	}
	settings.theme = theme;
	dojo.cookie('agentui-settings', dojo.toJson(settings));
}

function storeTab(tab){
	var settings = {
		'tabs': []
	};
	if(dojo.cookie('agentui-settings')){
		settings = dojo.fromJson(dojo.cookie('agentui-settings'));
		if(! settings.tabs){
			settings.tabs = [];
		}
	}
	for(var i = 0; i < settings.tabs.length; i++){
		if(settings.tabs[i] == tab){
			return true;
		}
	}
	settings.tabs.push(tab);
	dojo.cookie('agentui-settings', dojo.toJson(settings));
	return true;
}

function dropTab(tab){
	var settings = {
		'tabs':[]
	};
	if(dojo.cookie('agentui-settings')){
		settings = dojo.fromJson(dojo.cookie('agentui-settings'));
		if(! settings.tabs){
			settings.tabs = [];
		}
	}
	var out = [];
	for(var i = 0; i < settings.tabs.length; i++){
		if(settings.tabs[i] != tab){
			out.push(settings.tabs[i]);
		}
	}
	settings.tabs = out;
	dojo.cookie('agentui-settings', dojo.toJson(settings));
	return true;
}

function loadTab(tabid){
	var href = '';
	var title = '';
	switch(tabid){
		case 'supervisorTab':
			href = 'tabs/supervisor.html';
			title = 'Supervisor';
			break;
		case 'queueDashboardTab':
			href = 'tabs/queue_dashboard.html';
			title = 'Queues';
			break;
		default:
			return false;
	}

	if(! window.tabCloseListeners){
		window.tabCloseListeners = {};
	}
	
	if(! dijit.byId(tabid)){							
		var t = new dojox.layout.ContentPane({
			title: title,
			executeScripts: true,
			id: tabid,
			closable: true
		});
		dijit.byId("tabPanel").addChild(t);
		window.tabCloseListeners[tabid] = dojo.subscribe('tabPanel-removeChild', function(child){
			if(child.id == tabid){
				dropTab(tabid);
				dojo.unsubscribe(window.tabCloseListeners[tabid]);
				delete window.tabCloseListeners[tabid];
			}
		});
	}
	dijit.byId(tabid).attr('href', href);
	dijit.byId("tabPanel").selectChild(tabid);
	var logoutListenerName = tabid + "LogoutListener";
	dijit.byId("tabPanel")[logoutListenerName] = dojo.subscribe("agent/logout", dijit.byId("tabPanel"), function(data){
		this.closeChild(t);
		dojo.unsubscribe(this[logoutListenerName]);
		storeTab(tabid);
	});
}

function inArray(needle, haystack){
	for(var i = 0; i < haystack.length; i++){
		if(haystack[i] == needle){
			return true;
		}
	}
	return false;
}

function replaceUrls(text){
	var exp = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/ig;
	return text.replace(exp,"<a href='$1' target='_blank'>$1</a>");
}

function load_media_tab(options){
	console.log("load_media-tab");
	if(! options.media){
		throw "media is required for tab";
	}
	if(! options.id){
		options.id = options.media;
	}
	if(! options.href){
		options.href = options.media + '_media.html';
	}
	if(options.fullpane == undefined){
		options.fullpane = true;
	}
	if(! options.title){
		options.title = options.media;
	}
	
	if(dijit.byId(options.id)){
		return false;
	}
	
	if(options.fullpane){
		var pane = new dojox.layout.ContentPane({
			title:options.media,
			executeScripts: "true",
			id: options.id,
			closable: false
		});
		pane.unloadListener = dojo.subscribe('agent/state', function(data){
			try{
				if(data.state == 'wrapup'){
					dojo.unsubscribe(pane.unloadListener);
					dojo.unsubscribe(pane.logoutListener);
					dijit.byId('tabPanel').closeChild(pane);
				}
			}
			catch (err){
				info(['media pane unload listener erred', err]);
			}
		});
		pane.logoutListener = dojo.subscribe('agent/logout', function(){
			try{
				dojo.unsubscribe(pane.unloadListener);
				dojo.unsubscribe(pane.logoutListener);
				dijit.byId('tabPanel').closeChild(pane);
			}
			catch(err){
				info(['media pan logout listener erred', err]);
			}
		});
		pane.attr('href', "tabs/" + options.href);
		dijit.byId('tabPanel').addChild(pane);
		dijit.byId('tabPanel').selectChild(options.id);
	} else {
		if(! options.width){
			options.width = '160px';
		}
		if(! options.height){
			options.height = '120px';
		}
		var elem = document.createElement('div');
		elem.id = options.id,
		document.body.insertBefore(elem, document.body.firstChild);
		var pane = new dojox.layout.FloatingPane({
			title: options.media,
			executeScripts: true,
			closable: false,
			dockable: false,
			href: 'tabs/' + options.href,
			resizable: true,
			style: 'position:absolute;top:120px;left:65%;z-index:1000;width:'+options.width+';height:'+options.height
		}, elem);
		//pane.attr('href', "tabs/" + options.href);
		pane.startup();
		pane.show();
		pane.unloadListener = dojo.subscribe('agent/state', function(data){
			try{
				if(data.state == 'wrapup'){
					dojo.unsubscribe(pane.unloadListener);
					dojo.unsubscribe(pane.logoutListener);
					pane.attr('closable', true);
					pane.close();
				}
			}
			catch (err){
				info(['media pane unload listener erred', err]);
			}
		});
		pane.logoutListener = dojo.subscribe('agent/logout', function(){
			try{
				dojo.unsubscribe(pane.unloadListener);
				dojo.unsubscribe(pane.logoutListener);
				pane.attr('closable', true);
				pane.close();
			}
			catch(err){
				info(['media pan logout listener erred', err]);
			}
		});
	}
}

function showErrorReportDialog(conf){
	if(! conf){
		conf = {};
	}
	var dialog = dijit.byId('reportIssueDialog');
	for(var i in dialog.inputs){
		var dij = dijit.byId(i);
		if(conf[dij.id]){
			dojo.removeClass(dij.domNode, 'softText');
			dij.attr('value', conf[dij.id]);
		} else {
			dojo.addClass(dij.domNode, 'softText');
			dij.attr('value', dialog.inputs[i]);
		}
	}
	dialog.show();
}

function reportIssue(humanReport){
	var simpleAgent = {
		login: agent.login,
		profile: agent.profile,
		securityLevel: agent.securityLevel,
		skew: agent.skew,
		skills: agent.skills,
		state: agent.state,
		statdata: agent.statedata
	}
	
	var simpleLog = [];
	var i = 0;
	var maxLog = 100;
	
	if(EventLog.logged.length > maxLog){
		i = EventLog.logged.length - maxLog;
	}
	for(i; i < EventLog.logged.length; i++){
		simpleLog.push(EventLog.logged[i]);
	}
	
	var agentuiSettings = null;
	if(dojo.cookie('agentui-settings')){
		agentui = dojo.fromJson(dojo.cookie('agentui-settings'));
	}
	
	var openTabs = dijit.byId('tabPanel').getChildren();
	for(i = 0; i < openTabs.length; i++){
		openTabs[i] = openTabs[i].id;
	}
	
	var forJson = {
		agent: simpleAgent,
		uisettings: agentuiSettings,
		tabs: openTabs,
		log: simpleLog
	}
	
	humanReport.uistate = dojo.toJson(forJson);
	
	dojo.xhrPost({
		url:'/report_issue',
		handleAs:'json',
		content: humanReport,
		load:function(res){
			if(res.success){
				return true;
			}
			
			errMessage(["submitting bug report failed", res.message]);
		},
		error: function(res){
			errMessage(["submitting bug report errored", res]);
		}
	});
}

dojo.addOnLoad(function(){
	//TODO:  Move logging/logger functions to other file.
	if(window.console.log === undefined){
		//stupid ie.
		window.console.log = function(){
			// la la la
		};
	}
	
	window._logLevelToString = function(level){
		switch(level){
			case 7:
				return "debug";
			case 6:
				return "info";
			case 5:
				return "notice";
			case 4:
				return "warning";
			case 3:
				return "error";
			case 2:
				return "critical";
			case 1:
				return "alert";
			case 0:
				return "emergency";
			default:
				return "unknown";
		}
	};

	window._logLevelToNumber = function(level){
		switch(level){
			case "debug":
				return 7;
			case "info":
				return 6;
			case "notice":
				return 5;
			case "warning":
				return 4;
			case "error":
				return 3;
			case "critical":
				return 2;
			case "alert":
				return 1;
			case "emergency":
				return 0;
			default:
				return -1;
		}
	};
	
	window._logLevelToFunction = function(level){
		if(level > 6){
			return 'log';
		}
		
		if(level > 4){
			return 'info';
		}
		
		if(level > 3){
			return 'warn';
		}
		
		return 'error';
	}

	window.getLogLevel = function(){
		return window._logLevelToString(window._logLevel);
	};

	window.setLogLevel = function(levelstring){
		var int = window._logLevelToNumber(levelstring);
			if(int >= 0){
				window._logLevel = int;
				notice(["log level set", levelstring]);
			}
			else{
				error(["log level cannot be", levelstring]);
			}
		};

	if(! window.console){
		window.console = {};
		window.console.log = function(){
			return true;
		};
		window.console.info = window.console.log;
		window.console.error = window.console.log;
		window.console.warn = window.console.log;
	}

	window.log = function(level, data){
		var levelNum = window._logLevelToNumber(level)
		if(levelNum <= window._logLevel){
			var func = window._logLevelToFunction(levelNum);
			console[func]([level, data]);
		}
	};

	window.debug = function(data){
		window.log("debug", data);
	};

	window.info = function(data){
		window.log("info", data);
	};

	window.notice = function(data){
		window.log("notice", data);
	};

	window.warning = function(data){
		window.log("warning", data);
	};

	window.error = function(data){
		window.log("error", data);
	};

	window.critical = function(data){
		window.log("critical", data);
	};

	window._alert = window.alert;

	window.alert = function(data){
		window._alert(data);
		window.log("alert", data);
	};

	window.emergency = function(data){
		window.log("emergency", data);
	};

	window._logLevel = 4; //default is warning

	//create a 'bugs' button and move it to a nice spot.
	var div = dojo.create('div', {'class':'rightFloater'}, 'tabPanel_tablist', 'first');
	var innerDiv = dojo.create('div', null, div);
	var bugsButton = new dijit.form.Button({
		label:'Report Issue',
		showLabel:false,
		iconClass:'cpxIconBug',
		onClick: function(){
			//dijit.byId('reportIssueDialog').show();
			showErrorReportDialog();
		}
	}, innerDiv);
	
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
				dijit.byId("tabPanel_tablist").domNode.style.visibility = 'visible';
				dijit.byId('tabPanel_tablist').logoutListener = dojo.subscribe("agent/logout", function(data){
					dijit.byId('tabPanel_tablist').domNode.style.visibility = 'hidden';
				});
				agent = new Agent(response.login, parseInt(response.statetime, 10), response.timestamp);
				agent.setSkew(response.timestamp);
				agent.profile = response.profile;
				agent.statedata = response.statedata;
				buildReleaseMenu(agent);
				buildOutboundMenu(agent);
				buildQueueMenu(agent);
				dojo.byId("agentname").innerHTML = response.login;
				agent.state = response.state;
				dojo.byId("profiledisp").innerHTML = dojo.i18n.getLocalization("agentUI", "labels").PROFILE + ":  " + response.profile;
				dojo.publish("agent/state", [{"state":response.state, "statedata":response.statedata}]);
				if( (response.state == "oncall") && (response.mediaload) ){
					var fixedres = response.mediaload;
					fixedres.media = response.statedata.type;
					dojo.publish("agent/mediaload", [fixedres]);
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
					if (d.getHours() > 0) {
						s = d.getHours() + ":" + s;
					}
					dojo.byId("timerdisp").innerHTML = s;
				};
				agent.stopwatch.start();
				var settings = {
					tabs:[]
				};
				if (dojo.cookie('agentui-settings')) {
					settings = dojo.fromJson(dojo.cookie('agentui-settings'));
					if(! settings.tabs){
						settings.tabs = [];
					}
				}
				for(var i = 0; i < settings.tabs.length; i++){
					// TODO This could get ugly if/when we add more tabs.
					if(settings.tabs[0] == "supervisorTab"){
						if(! dijit.byId("supervisorTab")){							
							var t = new dojox.layout.ContentPane({
								//href:"tabs/supervisor.html",
								title:"Supervisor",
								executeScripts: "true",
								id:"supervisorTab",
								closable:true
							});
							dijit.byId("tabPanel").addChild(t);
						}
						dijit.byId("supervisorTab").attr('href', "tabs/supervisor.html");
						dijit.byId("tabPanel").selectChild("supervisorTab");
						dijit.byId("tabPanel").logoutListener = dojo.subscribe("agent/logout", function(data){
							dijit.byId("tabPanel").closeChild(dijit.byId("supervisorTab"));
							dojo.unsubscribe(dijit.byId("tabPanel").logoutListener);
							storeTab('supervisorTab');
						});
					}
				}
				dojo.cookie('agentui-settings', dojo.toJson(settings)); 
			}
			else{
				dijit.byId("loginpane").show();
				dijit.byId('tabPanel_tablist').domNode.style.visibility = 'hidden';
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
				node.innerHTML = data.statedata.brandname;
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
	
	dojo.byId("calltypep").stateChanger = dojo.subscribe("agent/state", function(data){
		switch(data.state){
			case 'ringing':
			case 'oncall':
			case 'wrapup':
				dojo.byId("calltype").innerHTML = encodeHTML(data.statedata.type);
				dojo.byId("calltypep").style.display = "block";
				break;
			default:
				dojo.byId("calltypep").style.display = "none";
		}
	});
	
	dojo.byId("statedisp").stateChanger = dojo.subscribe("agent/state", function(data){
		var node = dojo.byId("statedisp");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		var innerh = nlsStrings.STATE + ":  " + nlsStrings[data.state.toUpperCase()];
		if(data.state == "released"){
			if(data.statedata.constructor == String){
				innerh += " (" + data.statedata + ")";
			}
			else{
				innerh += " (" + data.statedata.reason + ")";
			}
		}
		node.innerHTML = innerh;
	});

	dojo.byId("profiledisp").stateChanger = dojo.subscribe("agent/profile", function(data){
		var node = dojo.byId("profiledisp");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		var innerh = nlsStrings.PROFILE + ":  " + data.profile;
		node.innerHTML = innerh;
	});

	dijit.byId("bgoreleased").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("bgoreleased");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		switch (data.state) {
			case 'idle':
			case 'ringing':
			case 'precall':
				widget.attr('label', nlsStrings.GORELEASED);
				widget.attr('style', 'display:inline');
				break;
			case 'released':
				widget.attr('style', 'display:none');
				break;
			default:
				widget.attr('label', nlsStrings.QUEUERELEASE);
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
				widget.attr('label', nlsStrings.GOAVAILABLE);
				break;
			case "wrapup":
				widget.attr('style', 'display:inline');
				widget.attr('label', nlsStrings.ENDWRAPUP);
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});

	dijit.byId("transferToQueueMenu").logout = dojo.subscribe("agent/logout", function(data){
		var menu = dijit.byId("transferToQueueMenu");
		menu.destroyDescendants();
	});

	dijit.byId("dialbox").stateChanger = dojo.subscribe("agent/state", function(data){
		var div = dojo.byId("foo");
		switch(data.state){
			//case "warmtransfer":
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
			//case "warmtransfer":
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
			//case "warmtransfer":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtdial").stateChanger = dojo.subscribe("agent/state", function(data){
		var widget = dijit.byId("wtdial");
		switch(data.state){
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtdial").warmtransfer_listener = dojo.subscribe("agent/mediaevent/voice", dijit.byId("wtdial"), function(data){
		if(data.event == 'warm_transfer_failed'){
			this.attr('style', 'display:inline');
			dojo.byId('foo').style.display = 'inline';
		} else  {
			dojo.byId('foo').style.display = 'none';
			this.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtcancel").stateChanger = dojo.subscribe("agent/state", dijit.byId("wtcancel"), function(data){
		if(this.suppressHide){
			delete this.suppressHide;
			return true;
		}
		
		switch(data.state){
			default:
				this.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtcomplete").stateChanger = dojo.subscribe("agent/state", dijit.byId("wtcomplete"), function(data){
		this.attr('style', 'display:none');
	});
	
	dijit.byId('wtcomplete').warmtransfer_listener = dojo.subscribe("agent/mediaevent/voice", dijit.byId('wtcomplete'), function(data){
		if(data.event == 'warm_transfer_succeeded'){
			this.attr('style', 'display:inline');
		} else {
			this.attr('style', 'display:none');
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
			if(data.state ==  "ringing"){
				widget.attr('style', "display:inline");
			} else {
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
			case "outgoing":
				widget.attr('style', 'display:inline');
				break;
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("transferToAgentMenuDyn").agentsAvail = dojo.subscribe("agent/available", function(data){
		var widget = dijit.byId("transferToAgentMenuDyn");
		widget.destroyDescendants();
		dojo.forEach(data, function(i){
			var m = new dijit.MenuItem({
				label: i.name+"("+i.profile+") " + (i.state == "idle" ? "I" : "R"),
				onClick: function(){
					Agent.transfer(escape(i.name));
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
				case "outgoing":
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
		//if(data.statedata && data.statedata.mediapath == "inband"){
			switch(data.state){
				case "oncall":
				case "warmtransfer":
				case "outbound":
					widget.attr('disabled', false);
					widget.brutal_kill = true;
					if(data.statedata.mediapath == "inband"){
						widget.brutal_kill = false;
					}
					break;
				default:
					widget.attr('disabled', true);
			}
		//}
	});
	
	dojo.byId("eventLogText").eventLogPushed = dojo.subscribe("eventlog/push", function(text){
		var li = document.createElement('li');
		li.innerHTML = text;
		dojo.byId('eventLogText').appendChild(li);
		//var oldval = dijit.byId("eventLogText").value;
		//dijit.byId("eventLogText").attr('value', oldval + "\n" + text);
	});
	
	dojo.byId("eventLogText").eventLogShifted = dojo.subscribe("eventlog/shift", dojo.byId("eventLogText"), function(text){
		var firstKid = this.firstChild;
		this.removeChild(firstKid);
	});
	
	var loginform = dijit.byId("loginform");
	dojo.connect(loginform, "onSubmit", function(e){
		e.preventDefault();
		if (loginform.isValid()){
			dojo.xhrGet({
				url:"/getsalt",
				handleAs:"json",
				error:function(response, ioargs){
					dojo.byId("loginerrp").style.display = "block";
					if (response.status){
						dojo.byId("loginerrspan").innerHTML = response.responseText;
					}
					else{
						dojo.byId("loginerrspan").innerHTML = "Server is not responding";
						alert(response);
					}
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
								dijit.byId("tabPanel_tablist").domNode.style.visibility = 'visible';
								dijit.byId('tabPanel_tablist').logoutListener = dojo.subscribe("agent/logout", function(data){
									dijit.byId('tabPanel_tablist').domNode.style.visibility = 'hidden';
								});
								dojo.byId("agentname").innerHTML = attrs.username;
								dojo.byId("profiledisp").innerHTML = dojo.i18n.getLocalization("agentUI", "labels").PROFILE + ":  " + response2.profile;
								var settings = {};
								if (dojo.cookie('agentui-settings')) {
									settings = dojo.fromJson(dojo.cookie('agentui-settings'));
								}
								settings.username = attrs.username;
								settings.voipendpoint = attrs.voipendpoint;
								settings.voipendpointdata = attrs.voipendpointdata;
								settings.useoutbandring = dijit.byId('useoutbandring').attr('checked');
								if(settings.tabs){
									for(var i = 0; i < settings.tabs.length; i++){
										loadTab(settings.tabs[i]);
									}
								}
								dojo.cookie('agentui-settings', dojo.toJson(settings)); 
								debug(response2);
								agent = new Agent(attrs.username, parseInt(response2.statetime, 10));
								agent.profile = response2.profile;
								agent.setSkew(response2.timestamp);
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
									if (d.getHours() > 0) {
										s = d.getHours() + ":" + s;
									}
									dojo.byId("timerdisp").innerHTML = s;
								};
								buildReleaseMenu(agent);
								buildOutboundMenu(agent);
								buildQueueMenu(agent);
								agent.stopwatch.start();
							} else{
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
				warning(["getting release codes errored", response]);
				var menu = dijit.byId("releasedmenu");
				var item = new dijit.MenuItem({
					label: nlsStrings.DEFAULT,
					onClick:function(){agent.setState("released", "Default"); }
				});
				menu.addChild(item);
			},
			load:function(response, ioargs){
				var menu = dijit.byId("releasedmenu");
				var item = '';
				if(response.success){
					dojo.forEach(response.options, function(obj){
						item = new dijit.MenuItem({
							label: obj.label,
							onClick:function(){agent.setState("released", obj.id + ":" + obj.label + ":" + obj.bias); }
						});
						menu.addChild(item);
					});
					item = new dijit.MenuItem({
						label: nlsStrings.DEFAULT,
						onClick:function(){agent.setState("released", "Default"); }
					});
					menu.addChild(item);
				}
				else{
					warning(["getting release codes failed", response.message]);
					item = new dijit.MenuItem({
						label: nlsStrings.DEFAULT,
						onClick:function(){agent.setState("released", "Default"); }
					});
					menu.addChild(item);
				}
			}
		});
	};

	buildOutboundMenu = function(agent){
		//var menu = dijit.byId("outboundmenu");
		var widget;
		var store = new dojo.data.ItemFileReadStore({
				data: {
					'label': 'label',
					'identifier': 'id',
					'items': [
						{'label':'Failed to load brands', 'id':'0'}
						]
					}
				});

		if(!(widget = dijit.byId('boutboundcall'))){
			widget = new dijit.form.FilteringSelect({
					'searchAttr': 'label',
					'name':'boutboundcall',
					'store':store,
					'fetchProperties':{
						'sort':[{attribute:'label', descending:false}]
					},
					'promptMessage': dojo.i18n.getLocalization("agentUI","labels")["MKOUTBOUND"]
					}, 'boutboundcall');
			dojo.connect(widget, 'onChange', function(val){
					if(val !== ""){
					dijit.byId('tabPanel').selectChild('maintab');
					agent.initOutbound(val, "freeswitch");
					}
			});
		}

		dojo.xhrGet({
			url:"/brandlist",
			handleAs:"json",
			error:function(response, ioargs){
				warning(response);
			},
			load:function(response, ioargs){
				debug(["buildOutboundMenu", response]);
				if(response.success){
					store = new dojo.data.ItemFileReadStore({
						data: {
							'label': 'label',
							'identifier':'id',
							'items': response.brands
						}
					});
					widget.store = store;
				}
			}
		});
		widget.stateChanger = dojo.subscribe("agent/state", function(data){
				debug(["boutboundcall", data, data.state]);
				switch(data.state){
					case "idle":
					case "released":
						widget.domNode.style.display = 'inline-block';
						widget.attr('value', '');
						break;
					default:
						widget.domNode.style.display = 'none';
				}
		});
	};

	buildQueueMenu = function(agent){
		var menu = dijit.byId("transferToQueueMenu");
		dojo.xhrGet({
			url: "/queuelist",
			handleAs: "json",
			error:function(response, ioargs){
				debug(response);
				var item = new dijit.MenuItem({
					label:"Failed to get queuelist1",
					disabled: true
				});
				menu.addChild(item);
			},
			load:function(response, ioargs){
				debug(["buildQueueMenu", response]);
				var item = '';
				if(response.success){
					for(var i = 0; i < response.queues.length; i++) {
						item = new dijit.MenuItem({
							label: response.queues[i].name,
							onClick: function(){ Agent.queuetransfer(this.label); }
						});
						menu.addChild(item);
					}
				}
				else{
					item = new dijit.MenuItem({
						label:"Failed to get queuelist",
						disabled: true
					});
					menu.addChild(item);
				}
			}
		});
	};

	dojo.byId("loginerrp").logout = dojo.subscribe("agent/logout", function(data){
		if(data === true){
			dojo.byId("loginerrp").style.display = "none";
		}else{
			dojo.byId("loginerrp").style.display = "block";
			dojo.byId("loginerrspan").innerHTML = data;
		}
	});
	
	dojo.byId("loginpane").logout = dojo.subscribe("agent/logout", function(data){
		dijit.byId("loginpane").show();
	});

	dijit.byId("main").logout = dojo.subscribe("agent/logout", function(data){
		dijit.byId("main").attr('style', 'visibility:hidden');
	});
	
	dijit.byId("main").pop = dojo.subscribe("agent/urlpop", function(data){
		var name = 'popup';
		if(data.name){
			name = data.name;
		}
		
		var id = name + '_urlpop';
		
		var widget = false;
		if(dijit.byId(id)){
			if(name == 'popup'){
				dijit.byId(id).destroy();
			} else {
				widget = dijit.byId(id);
			}
		}
		
		var newContent = '<iframe width="99%", height="300px" src="' + data.url + '" />';

		if(widget === false){
			var elem = document.createElement('div');
			elem.id = id;
			document.body.insertBefore(elem, document.body.firstChild);
			
			widget = new dojox.layout.FloatingPane({
				title:name,
				resizable: true,
				dockable: false,
				style: 'position:absolute; top: 100px; left: 60%; z-index: 1000',
				content: newContent
			}, dojo.byId(id));
			// overriding close button to do a hide instead.
			widget.closable = false;
			widget._onCloseConnect = dojo.connect(widget.closeNode, 'onclick', widget, function(){
				this.hide();
			});
		} else {
			widget.attr('content', newContent);
		}
		
		widget.startup();
		widget.show();		
	});

	dijit.byId("main").blab = dojo.subscribe("agent/blab", function(data){
		debug(["blab data", data]);
		var dia = new dijit.Dialog({
			title: "Message from Supervisor",
			content: '<div style="width: 200px; height: 100px; overflow: auto;">' + data + '</div>'
		});
		dia.show();
	});

	logout = function(agent){
		agent.logout();
	};
	
	dijit.byId("main").mediaload = dojo.subscribe("agent/mediaload", function(eventdata){
		info(["listening for media load fired:  ", eventdata]);
		load_media_tab(eventdata);
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
