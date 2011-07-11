function errMessage(message){
	if(EventLog){
		EventLog.log("error displayed:  " + message);
	}
	var dialog = new dijit.Dialog({
		title: "<span style='color:#ff3333'>Error</span>",
		content: message.toString()
	});
	dojo.connect(dialog, 'onCancel', dialog, function(){
		dialog.destroy();
	});
	dialog.show();
}


function format_statedata(data, state) {
	switch (state) {
		case 'released':
			return data.reason;
		case 'idle':
			return '';
		case 'precall':
			return data.brandname;
		case 'oncall':
		case 'wrapup':
		case 'ringing':
		case 'outgoing':
			return 'id: '+ data.callid + " callerid: " + data.callerid + " type: " + data.type + " brand: " + data.brandname;
		case 'warmtransfer':
			return 'onhold: ' + data.onhold.callid + ' calling: ' + data.calling;
		default:
			data.toString();
	}
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
			'<p><input dojoType="dijit.form.Button" type="button" label="' + conf.yesLabel + '" />' +
			'<input dojoType="dijit.form.Button" type="button" label="' + conf.noLabel + '">' + 
			'</div>'
	});
	dojo.connect(dialog, 'onCancel', dialog, function(){
		conf.noAction();
		dialog.destroy();
	});
	var kids = dialog.getChildren()
	dojo.connect(kids[1], 'onClick', dialog, function(){
		conf.noAction();
		dialog.destroy();
	});
	dojo.connect(kids[0], 'onClick', dialog, function(){
		conf.yesAction();
		dialog.destroy();
	});
	dialog.show();
}

function queueTransferDialog(queueNom){
	var createDialog = function(queueOpts){
		var dialog = new dijit.Dialog({
			title:'Queue Transfer Options'
		});
		dialog.prompts = [];
		var form = dojo.create('form', {action:'javascript:void(0)',method:'post'}, dialog.containerNode);
		for(var i = 0; i < queueOpts.prompts.length; i++){
			var p = dojo.create('p', {}, form);
			dojo.create('label', {'for':queueOpts.prompts[i].name,innerHTML:queueOpts.prompts[i].label + ':','class':'narrow'}, p);
			var inputBase = dojo.create('input', {name:queueOpts.prompts[i].name}, p);
			dialog.prompts.push(new dijit.form.ValidationTextBox({
				regExp:queueOpts.prompts[i].regex,
				name:queueOpts.prompts[i].name,
				value:queueOpts.currentVars[queueOpts.prompts[i].name]
			}, inputBase));
		}
		if(queueOpts.skills.length > 0){
			p = dojo.create('p', {}, form);
			dojo.create('label', {'for':'skills',innerHTML:'Skills:','class':'narrow'}, p);
			dialog.select = dojo.create('select', {name:'skills',multiple:true,size:3}, p);
			for(i = 0; i < queueOpts.skills.length; i++){
				var outSkill = queueOpts.skills[i].atom;
				if(queueOpts.skills[i].expanded){
					outSkill = '{' + outSkill + ',' + queueOpts.skills[i].expanded + '}';
				}
				dojo.create('option', {value:outSkill,innerHTML:outSkill,toolTip:queueOpts.skills[i].description}, dialog.select);
			}
		}
		p = dojo.create('p', {}, form);
		dojo.create('label', {innerHTML:'&nbsp;'}, p);
		var submitNode = dojo.create('button', {}, p);
		var submitButton = new dijit.form.Button({
			label:'Submit'
		}, submitNode);
		dojo.connect(submitButton, 'onClick', dialog, function(){
			var urlopts = {};
			for(var i = 0; i < this.prompts.length; i++){
				if(this.prompts[i].isValid() == false){
					return false;
				}
				urlopts[this.prompts[i].name] = this.prompts[i].value;
			}
			var skills = [];
			if(this.select){
				for(i = 0; i < this.select.options.length; i++){
					if(this.select.options[i].selected){
						skills.push(this.select.options[i].value);
					}
				}
			}
			window.agentConnection.queuetransfer(queueNom, skills, urlopts);
			this.destroy();
		});
		dialog.show();
	}
	var qtoOptions = { // Options for getting the queue transfer options
		error:function(res){
			confirmDialog({
				'yesLabel':'Queue anyway',
				'noLabel':'Don\'t queue',
				'question':'Could not load queue transfer options (' + res + ').  Queue to ' + queueNom + ' anyway?',
				'yesAction':function(){ window.agentConnection.queuetransfer(queueNom, [], {}) },
				'title':'Queue Transfer Options Errored'
			});
		},
		success:function(res){
			if(res.prompts.length + res.skills.length == 0){
				window.agentConnection.queuetransfer(queueNom, [], {});
				return true;
			}
			createDialog(res);
		},
		failure:function(errcode, message){
			confirmDialog({
				'yesLabel':'Queue anyway',
				'noLabel':'Don\'t queue',
				'question':'Could not load queue transfer options (' + message + ').  Queue to ' + queueNom + ' anyway?',
				'yesAction':function(){ window.agentConnection.queueTransfer(queueNom, [], {}) },
				'title':'Queue Transfer Options Failed'
			});
		}
	};
	window.agentConnection.agentApi("get_queue_transfer_options", qtoOptions);
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
		case 'dashboardTab':
			href = 'tabs/dashboard.html';
			title = 'Dashboard';
			break;
		default:
			return false;
	}

	if(! window.tabCloseListeners){
		window.tabCloseListeners = {};
	}
	
	if(dijit.byId(tabid)){
		dijit.byId('tabPanel').closeChild(dijit.byId(tabid));
	}
	
	var t = new dojox.layout.ContentPane({
		title: title,
		executeScripts: true,
		id: tabid,
		closable: true
	});
	dijit.byId("tabPanel").addChild(t);
	window.tabCloseListeners[tabid] = dojo.subscribe('tabPanel-removeChild', function(child){
		if(child.id == tabid){
			dojo.unsubscribe(window.tabCloseListeners[tabid]);
			delete window.tabCloseListeners[tabid];
			dropTab(tabid);
		}
	});
	dijit.byId(tabid).attr('href', href);
	dijit.byId("tabPanel").selectChild(tabid);
	var logoutListenerName = tabid + "LogoutListener";
	dijit.byId("tabPanel")[logoutListenerName] = dojo.subscribe("OpenACD/Agent/logout", dijit.byId("tabPanel"), function(data){
		dojo.unsubscribe(window.tabCloseListeners[tabid]);
		this.closeChild(t);
		dojo.unsubscribe(this[logoutListenerName]);
	});
	storeTab(tabid);
}


function load_media_tab(options){
	console.log("load_media_tab");
	if(! options.media){
		throw "media is required for tab";
	}
	if(! options.channelId){
		throw "channelId is required for tab";
	}
	if(! options.id){
		options.id = options.media;
	}
	if(! (options.href || options.content) ){
		options.href = options.media + '_media.html';
	}
	//if(options.fullpane == undefined){
		options.fullpane = true;
	//}
	if(! options.title){
		options.title = options.media;
	}
	if(options.autoClose == undefined){
		options.autoClose = true;
	}
	
	if(dijit.byId(options.id)){
		if(options.overwrite && options.href){
			dijit.byId(options.id).attr('href', options.href);
		} else if(options.overwrite && options.content) {
			dijit.byId(options.id).attr('content', options.content);
		}
		return false;
	}
	
	//if(options.fullpane){
		var pane = new dojox.layout.ContentPane({
			title:options.title,
			executeScripts: "true",
			id: options.id,
			closable:options.closable 
		});
		pane.channelId = options.channelId;
		if(options.autoClose){
			pane.unloadListener = dojo.subscribe('OpenACD/AgentChannel', function(inChannelId, data){
				if(inChannelID != pane.channelId){
					return false;
				}
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
		}
		pane.logoutListener = dojo.subscribe('OpenACD/Agent/logout', function(){
			try{
				dojo.unsubscribe(pane.unloadListener);
				dojo.unsubscribe(pane.logoutListener);
				dijit.byId('tabPanel').closeChild(pane);
			}
			catch(err){
				info(['media pane logout listener erred', err]);
			}
		});
		if(options.content){
			pane.attr('content', options.content);
		} else {
			pane.attr('href', "tabs/" + options.href);
		}
		dijit.byId('tabPanel').addChild(pane);
		dijit.byId('tabPanel').selectChild(options.id);
	/*} else {
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
			title: options.title,
			executeScripts: true,
			closable: options.closable,
			dockable: false,
			href: 'tabs/' + options.href,
			resizable: true,
			style: 'position:absolute;top:30px;left:80%;z-index:800;width:'+options.width+';height:'+options.height
		}, elem);
		//pane.attr('href', "tabs/" + options.href);
		pane.startup();
		pane.show();
		if(options.autoClose){
			pane.unloadListener = dojo.subscribe('OpenACD/Agent/state', function(data){
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
		}
		pane.logoutListener = dojo.subscribe('OpenACD/Agent/logout', function(){
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
	}*/
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
		login: window.agentConnection.login,
		profile: window.agentConnection.profile,
		securityLevel: window.agentConnection.securityLevel,
		skew: window.agentConnection.skew,
		skills: window.agentConnection.skills,
		state: window.agentConnection.state,
		statdata: window.agentConnection.statedata
	}
	
	var coveredNode = dijit.byId('reportIssueDialog').domNode;
	var standby = new dojox.widget.Standby({
		target: coveredNode,
		zIndex:1000
	});
	dojo.doc.body.appendChild(standby.domNode);
	standby.startup();
	standby.show();	
	
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
		log: simpleLog,
		userAgent: window.navigator.userAgent
	}
	
	humanReport.uistate = dojo.toJson(forJson);

	// TODO convert this and backend this to refined api.	
	dojo.xhrPost({
		url:'/report_issue',
		handleAs:'json',
		content: humanReport,
		load:function(res){
			standby.hide();
			if(res.success){
				dijit.byId('reportIssueDialog').hide();
				return true;
			}
			
			errMessage(["submitting bug report failed", res.message]);
		},
		error: function(res){
			standby.hide();
			errMessage(["submitting bug report errored", res]);
		}
	});
}

dojo.addOnLoad(function(){
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

	// make the labels on the bug form nicer.
	var nodes = dojo.query('.translatecol', dojo.byId('reportIssueDialog'));
	console.log(nodes);
	for(var i = 0; i < nodes.length; i++){
		var label = dojo.i18n.getLocalization("agentUI", "labels")[nodes[i].innerHTML];
		if(label){
			nodes[i].innerHTML = label + ":";
		}
	}

	window.startGlobalTick();

	EventLog.log("Inteface loaded");
	
	EventLog.logAgentState = dojo.subscribe("OpenACD/Agent/state", function(data){
		var line = "Agent state changed to " + data.state;
		if(data.statedata){
			line += '('+format_statedata(data.statedata, data.state)+')';
		}
		EventLog.log(line);
	});
	
	var seedUI = function(confobj){
		var confs = {
			username:'',
			securityLevel:'agent',
			elapsed:'',
			skew:0,
			profile:'',
			statedata:'',
			state:'',
			voipendpoint:false,
			voipendpointdata:false,
			useoutbandring:true,
			usepersistantchannel:false,
			mediaload:false,
			timestamp:false
		};
		dojo.mixin(confs, confobj);
		console.log("confs", confs);
		dojo.byId("main").style.display="block";
		dojo.byId("main").style.visibility = "visible";
		dijit.byId("tabPanel_tablist").domNode.style.visibility = 'visible';
		dijit.byId('tabPanel_tablist').logoutListener = dojo.subscribe("OpenACD/Agent/logout", function(data){
			dijit.byId('tabPanel_tablist').domNode.style.visibility = 'hidden';
		});
		/*agent = new OpenACD.Agent(confs);
		agent.profile = confs.profile;
		agent.state = confs.state;
		agent.statedata = confs.statedata;
		if(agent.state){
			dojo.publish("OpenACD/Agent/state", [{"state":agent.state, "statedata":agent.statedata}]);*/
		if( (window.agentConnection.state == "oncall") && (confs.mediaload) ){
			var fixedres = confs.mediaload;
			fixedres.media = confobj.statedata.type;
			dojo.publish("OpenACD/Agent/mediaload", [fixedres]);
		}
		/*}*/
		buildReleaseMenu();
		buildOutboundMenu();
		buildQueueMenu();
		dojo.byId("agentname").innerHTML = confs.username;
		dojo.byId("profiledisp").innerHTML = dojo.i18n.getLocalization("agentUI", "labels").PROFILE + ":  " + confs.profile;
		window.agentConnection.stopwatch.onTick = function(){
			var elapsed = window.agentConnection.stopwatch.time();
			dojo.byId("timerdisp").innerHTML = formatseconds(elapsed);
		}
		window.agentConnection.stopwatch.start();
		var settings = {};
		if(dojo.cookie('agentui-settings')){
			settings = dojo.fromJson(dojo.cookie('agentui-settings'));
			if(! settings.tabs){
				settings.tabs = [];
			}
		}
		settings.username = confs.username;
		settings.voipendpoint = confs.voipendpoint ? confs.voipendpoint : settings.voipendpoint;
		settings.voipendpointdata = confs.voipendpointdata ? confs.voipendpointdata : settings.voipendpointdata;
		settings.useoutbandring = confs.useoutbandring ? confs.useoutbandring : settings.useoutbandring;
		settings.usepersistantchannel = confs.usepersistantchannel ? confs.usepersistnatchannel : settings.usepersistantchannel;
		if(settings.tabs){
			for(var i = 0; i < settings.tabs.length; i++){
				loadTab(settings.tabs[i]);
			}
		}
		if(confs.securityLevel == 'agent'){
			dijit.byId("tabsmenubutton").set('disabled', true);
		} else {
			dijit.byId("tabsmenubutton").set('disabled', false);
		}
		dojo.cookie('agentui-settings', dojo.toJson(settings));
		dijit.byId("loginpane").hide();
	}
	
	var checkCookieOpts = {
		error:function(response, ioargs){
			console.error("checkcookie failed!", response);
		},
		success:function(result){
			console.log('success', result);
			var seedConf = dojo.clone(result);
			seedConf.username = seedConf.login;
			seedConf.securityLevel = result.securityLevel;
			seedConf.elapsed = parseInt(result.statetime, 10);
			seedConf.skew = result.timestamp;
			seedUI(seedConf);
		},
		failure: function(errcode, message){
			dijit.byId("loginpane").show();
			dijit.byId('tabPanel_tablist').domNode.style.visibility = 'hidden';
		}
	};
	window.agentConnection = new OpenACD.Agent({});
	window.agentConnection.checkCookie(checkCookieOpts);
	var loginHandle = dojo.subscribe('OpenACD/Agent/login', function(agent){
		console.log('handling login', this, agent);
		var seedConf = {};
		seedConf.username = agent.username;
		seedConf.securityLevel = agent.securityLevel;
		seedConf.elapsed = parseInt(agent.stopwatch.time());
		seedConf.profile = agent.profile;
		seedConf.skew = agent.skew;
		seedUI(seedConf);
	});
	
	//Agent.states = ["idle", "ringing", "precall", "oncall", "outgoing", "released", "warmtransfer", "wrapup"];

	
	dojo.byId("brand").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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

	dojo.byId("callerid").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dojo.byId("calltypep").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dojo.byId("statedisp").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
		var node = dojo.byId("statedisp");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		var innerh = nlsStrings.STATE + ":  " + nlsStrings[data.state.toUpperCase()];
		switch(data.state){
			case "oncall":
				node.innerHTML = innerh;
				dojo.addClass(node, "profit");
				dojo.removeClass(node, "loss");
				break;
			
			case "wrapup":
				node.innerHTML = innerh;
				dojo.addClass(node, "loss");
				dojo.removeClass(node, "profit");
				break;
			
			default:
				if(data.state == "released"){
					if(data.statedata.constructor == String){
						innerh += " (" + data.statedata + ")";
					}
					else{
						innerh += " (" + data.statedata.reason + ")";
					}
				}
				dojo.removeClass(node, "profit");
				dojo.removeClass(node, "loss");
				node.innerHTML = innerh;
		}
	});

	dojo.byId("profiledisp").stateChanger = dojo.subscribe("OpenACD/Agent/profile", function(data){
		var node = dojo.byId("profiledisp");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		var innerh = nlsStrings.PROFILE + ":  " + data.profile;
		node.innerHTML = innerh;
	});

	dijit.byId("bgoreleased").stateChanger = dojo.subscribe("OpenACD/Agent/release", function(data){
		var widget = dijit.byId("bgoreleased");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		if(data.releaseData){
			widget.attr('style', 'display:none');
		} else {
			widget.attr('label', nlsStrings.GORELEASED);
			widget.attr('style', 'display:inline');
		}
	});

	dijit.byId("releasedmenu").logout = dojo.subscribe("OpenACD/Agent/logout", function(data){
		var widget = dijit.byId("releasedmenu");
		widget.destroyDescendants();
	});
	
	// TODO this no longer pulls double duty of going out of wrapup.
	dijit.byId("bgoavail").stateChanger = dojo.subscribe("OpenACD/Agent/release", function(data){
		var widget = dijit.byId("bgoavail");
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		if(data.releaseData){
			widget.attr('style', 'display:inline');
			widget.attr('label', nlsStrings.GOAVAILABLE);
		} else {
			widget.attr('style', 'display:none');
		}
	});

	dijit.byId("transferToQueueMenuDyn").logout = dojo.subscribe("OpenACD/Agent/logout", function(data){
		var menu = dijit.byId("transferToQueueMenuDyn");
		menu.destroyDescendants();
	});

	dijit.byId("dialbox").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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

	dijit.byId("bcancel").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dijit.byId("bdial").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dijit.byId("wtdial").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
		var widget = dijit.byId("wtdial");
		switch(data.state){
			default:
				widget.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtdial").warmtransfer_listener = dojo.subscribe("OpenACD/Agent/mediaevent/voice", dijit.byId("wtdial"), function(data){
		if(data.event == 'warm_transfer_failed'){
			this.attr('style', 'display:inline');
			dojo.byId('foo').style.display = 'inline';
		} else  {
			dojo.byId('foo').style.display = 'none';
			this.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtcancel").stateChanger = dojo.subscribe("OpenACD/Agent/state", dijit.byId("wtcancel"), function(data){
		if(this.suppressHide){
			delete this.suppressHide;
			return true;
		}
		
		switch(data.state){
			default:
				this.attr('style', 'display:none');
		}
	});
	
	dijit.byId("wtcomplete").stateChanger = dojo.subscribe("OpenACD/Agent/state", dijit.byId("wtcomplete"), function(data){
		this.attr('style', 'display:none');
	});
	
	dijit.byId('wtcomplete').warmtransfer_listener = dojo.subscribe("OpenACD/Agent/mediaevent/voice", dijit.byId('wtcomplete'), function(data){
		if(data.event == 'warm_transfer_succeeded'){
			this.attr('style', 'display:inline');
		} else {
			this.attr('style', 'display:none');
		}
	});
	
	dojo.byId("state").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		dojo.byId("state").innerHTML = nlsStrings[data.state.toUpperCase()];
	});
	
	dijit.byId("banswer").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dijit.byId("btransfer").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dijit.byId('transferToAgentMenu').startup();
	dijit.byId('transferToQueueMenu').startup();
	dijit.byId("transferToAgentMenuDyn").agentsAvail = dojo.subscribe("OpenACD/Agent/available", function(data){
		var widget = dijit.byId("transferToAgentMenuDyn");
		widget.destroyDescendants();
		dojo.forEach(data, function(i){
			var m = new dijit.MenuItem({
				label: i.name+"("+i.profile+") " + (i.state == "idle" ? "I" : "R"),
				onClick: function(){
					window.agentConnection.transfer(escape(i.name));
				}
			});
			widget.addChild(m);
		});
	});
	
	dijit.byId("bhangup").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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
	
	dijit.byId("miHangup").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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

	dijit.byId("miRingtest").stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
		var widget = dijit.byId("miRingtest");
		//if(data.statedata && data.statedata.mediapath == "inband"){
			switch(data.state){
				case "released":
					widget.attr('disabled', false);
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
			var errorF = function(response, ioargs){
				dojo.byId("loginerrp").style.display = "block";
				if (response.status){
					dojo.byId("loginerrspan").innerHTML = response.responseText;
				}
				else{
					dojo.byId("loginerrspan").innerHTML = "Server is not responding";
					alert(response);
				}
			};
			var failF = function(errcode, message){
				dojo.byId("loginerrp").style.display = "block";
				dojo.byId("loginerrspan").innerHTML = message;
			};
			var attrs = loginform.attr("value");
			window.agentConnection.username = attrs.username;
			window.agentConnection.password = attrs.password;
			window.agentConnection.loginOptions = {
				voipendpoint: attrs.voipendpoint,
				voipendpointdata: attrs.voipendpointdata
			}
			if(attrs.useoutbandring){
				window.agentConnection.useoutbandring = true;
			}
			window.agentConnection.login();
		} else {
			console.warn('Form has invalid value');
		}
	});

	buildReleaseMenu = function(){
		var nlsStrings = dojo.i18n.getLocalization("agentUI","labels");
		var menu = dijit.byId("releasedmenu");
		var addItems = function(items){
			var i = 0;
			var childs = menu.getChildren();
			for(i = 0; i < childs.length; i++){
				menu.removeChild(childs[i]);
			}
			for(i = 0; i < items.length; i++){
				menu.addChild(items[i]);
			}
		}
		var opts = {
			error:function(response, ioargs){
				warning(["getting release codes errored", response]);
				var item = new dijit.MenuItem({
					label: nlsStrings.DEFAULT,
					onClick:function(){window.agentConnection.setRelease("Default"); }
				});
				addItems([item]);
			},
			success:function(response, ioargs){
				var items = [];
				dojo.forEach(response.options, function(obj){
					items.push(new dijit.MenuItem({
						label: obj.label,
						onClick:function(){window.agentConnection.setRelease(obj.id + ":" + obj.label + ":" + obj.bias); }
					}));
				});
				items.push(new dijit.MenuItem({
					label: nlsStrings.DEFAULT,
					onClick:function(){window.agentConnection.setRelease("Default"); }
				}));
				addItems(items);
			},
			failure:function(errcode, message){
				warning(["getting release codes failed", response.message]);
				item = new dijit.MenuItem({
					label: nlsStrings.DEFAULT,
					onClick:function(){window.agentConnection.setRelease("Default"); }
				});
				addItems([item]);
			}
		};
		window.agentConnection.agentApi("get_release_opts", opts);
	};

	buildOutboundMenu = function(){
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
					window.agentConnection.initOutbound(val, "freeswitch");
					}
			});
		}

		var brandListOpts = {
			error:function(response, ioargs){
				warning(response);
			},
			success:function(response, ioargs){
				debug(["buildOutboundMenu", response]);
				store = new dojo.data.ItemFileReadStore({
					data: {
						'label': 'label',
						'identifier':'id',
						'items': response
					}
				});
				widget.store = store;
			}
		};
		window.agentConnection.agentApi("get_brand_list", brandListOpts);
		widget.stateChanger = dojo.subscribe("OpenACD/Agent/state", function(data){
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

	buildQueueMenu = function(){
		var menu = dijit.byId("transferToQueueMenuDyn");
		var qListOpts = {
			error:function(response, ioargs){
				debug(response);
				var item = new dijit.MenuItem({
					label:"Failed to get queuelist1",
					disabled: true
				});
				menu.addChild(item);
			},
			success:function(response, ioargs){
				debug(["buildQueueMenu", response]);
				var item = '';
				for(var i = 0; i < response.length; i++) {
					item = new dijit.MenuItem({
						label: response[i].name,
						onClick: function(){ queueTransferDialog(this.label); }
					});
					menu.addChild(item);
				}
			},
			failure:function(errcode, message){
				item = new dijit.MenuItem({
					label:"Failed to get queuelist",
					disabled: true
				});
				menu.addChild(item);
			}
		};
		window.agentConnection.agentApi("get_queue_list", qListOpts);
	};

	dojo.byId("loginerrp").logout = dojo.subscribe("OpenACD/Agent/logout", function(data){
		if(data === true){
			dojo.byId("loginerrp").style.display = "none";
		}else{
			dojo.byId("loginerrp").style.display = "block";
			dojo.byId("loginerrspan").innerHTML = data;
		}
	});
	
	dojo.byId("loginpane").logout = dojo.subscribe("OpenACD/Agent/logout", function(data){
		dijit.byId("loginpane").show();
	});

	dijit.byId("main").logout = dojo.subscribe("OpenACD/Agent/logout", function(data){
		dijit.byId("main").attr('style', 'visibility:hidden');
	});
	
	dijit.byId("main").pop = dojo.subscribe("OpenACD/Agent/urlpop", function(data){
		if(EventLog){
			EventLog.log("URL popped:  " + data.url);
		}
		var popOptions = {
			media:'ring',
			title:'popup',
			content: '<iframe width="99%" height="300px" src="' + data.url + '" />',
			autoClose:false,
			closable:true,
			overwrite:true
		}

		if(data.name){
			popOptions.title = data.name;
		}
		
		popOptions.id = popOptions.title + '_urlpop';
		
		load_media_tab(popOptions);
	});

	dijit.byId("main").blab = dojo.subscribe("OpenACD/Agent/blab", function(data){
		debug(["blab data", data]);
		var dia = new dijit.Dialog({
			title: "Message from Supervisor",
			content: '<div style="width: 200px; height: 100px; overflow: auto;">' + data + '</div>'
		});
		/*soundManager.play('gong');*/
		soundManager.play('chime');
		dia.show();
	});

	logout = function(agent){
		agent.logout();
	};
	
	dijit.byId("main").mediaload = dojo.subscribe("OpenACD/Agent/mediaload", function(eventdata){
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
