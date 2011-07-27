// Currently dependant on dojo

if(! window.OpenACD){
	function OpenACD(){
		this.createdBy = 'OpenACDAgent';
		throw new Error('lib; not for new');
	}
}

/**
Create a new AgentChannel.
@param {Object} options The options object.
@param {String} options.channelId The id of the channel.
@param {Object} options.stateData The statedata, or media object, for the
	channel.
@param {Object} options.agent The OpenACD.Agent to use for server 
	communication.
@param {Integer} [options.stateTime] Unix timestamp of the last time the
	channel changes state based on the server.
@param {Integer} [options.timestamp] Unix timestamp of 'now' from the 
	server.  Used to determin skew.
@class Represents a channel for an agent.  The Agent object is expected
to maintain a hash and forward appropriate events to a channel.
*/
OpenACD.AgentChannel = function(options){
	this.channelId = '';
	this.stateData = false;
	this.agent = false;
	this.stateTime = new Date();
	this.timestamp = this.stateTime;
	
	if(options.channelId){
		this.channelId = options.channelId;
	}
	if(options.stateData){
		this.stateData = options.stateData;
	}
	if(options.agent){
		this.agent = options.agent;
	}
	if(options.stateTime){
		this.stateTime = options.stateTime;
	}
	if(options.timestamp){
		this.timestamp = options.timestamp;
	}
}

OpenACD.AgentChannel.prototype.handleStateChange = function(state, stateData){
	this.state = state;
	this.stateData = stateData;
	try{
		dojo.publish("OpenACD/AgentChannel", [this.channelId, state, stateData]);
	} catch(err) {
		console.error("OpenACD/AgentChannel", err);
	}
}

OpenACD.AgentChannel.prototype.destroy = function(data){
	console.log('Channel ending', data);
	try{
		dojo.publish("OpenACD/AgentChannel", [this.channelId, 'destroy']);
	} catch(err) {
		console.error("OpenACD/AgentChannel", err);
	}
}

/**
Attempt to set the agent channel state.  Note that handling the state 
change is not required in the success function as all state changes are 
reported though the poll and publish mechanism.
@param {String} state The name of the state to go into.
@param [statedata] Any additional data about the state.
*/
OpenACD.AgentChannel.prototype.setState = function(state){
	//build the request
	var options = {};
	var statedata = false;
	if(arguments.length == 2){
		statedata = arguments[1];
	}

	if(statedata){
		this.agent.agentApi("set_state", options, this.channelId, state, statedata);
	} else {
		this.agent.agentApi("set_state", options, this.channelId, state);
	}
};

/**
Attempt to end the agent channel by ending the wrapup state.  Like all other
state changes, there is no need to handle the return directly.
*/
OpenACD.AgentChannel.prototype.endWrapup = function(){
	this.agent.agentApi("end_wrapup", {}, this.channelId);
}

/**
Create a new Agent.
@param {Object} options The options object
@param {String} options.username Agent login name.
@param {String} options.password Agent password.
@param {Integer} [options.statetime] Unix timestamp of the last agent state
	change based on the server.
@param {Integer} [options.timestamp] Unix timestamp of 'now' from the server.
	Used to determine skew.
@calss Represets the connection for an agent.  Provides easy functions for 
communicating with OpenACD as an agent.
*/
OpenACD.Agent = function(options){
	this.username = '';
	this.password = '';
	this.loggedIn = false;
	this.securitylevel = "";
	this.profile = "";
	this.skills = [];
	this.channels = {};
	this.state = "";
	this.statedata = "";
	this.releaseData = false;
	this.pollfailures = 0;
	this.skew = 0;
	this._nags = {};

	if(options.username){
		this.username = options.username;
	}

	if(options.password){
		this.password = options.password;
	}

	if(options.timestamp){
		this.setSkew(options.timestamp);
	}

	var statetime = 0;
	if(options.statetime){
		statetime = options.statetime;
	}
	this.stopwatch = new OpenACD.Stopwatch(statetime);
	this.stopwatch.onTick = function(){};
	this.setUpInternalSubscriptions(true);
}

OpenACD.Agent.prototype.setUpInternalSubscriptions = function(unsub){
	if(this.internalSubscriptions && unsub){
		for(var i = 0; i < this.internalSubscriptions.length; i++){
			dojo.unsubscribe(this.internalSubscriptions[i]);
		}
		this.internalSubscriptions = false;
	}
	if(this.internalSuscriptions){
		return;
	}

	this.internalSubscriptions = [];
	var basechan = "OpenACD/Agent/internal/";
	var chanCbMap = {
		"salt":{
			"success": "_handleGetSaltSuccess",
			"failure": "_handleGetSaltFailure",
			"error": "_handleGetSaltError"
		},
		"login":{
			"success": "_handleLoginSuccess",
			"failure": "_handleLoginFailure",
			"error": "_handleLoginError"
		},
		"poll":{
			"success": "_handlePollSuccess",
			"failure": "_handlePollFailure",
			"error": "_handlePollError"
		},
		"checkCookie":{
			"success": "_handleCheckCookieSuccess",
			"failure": "_handleCheckCookieFailure",
			"error": "_handleCheckCookieError"
		}
	};
	
	var api = "";
	var success = "";
	for(api in chanCbMap){
		for(success in chanCbMap[api]){
			this.internalSubscriptions.push(dojo.subscribe(basechan + api + "/" + success, this, this[chanCbMap[api][success]]));
		}
	}
}
	
/**
Set the skew of the client from the server.
@param {Integer} timestamp Unix timestamp from the server to determine the 
	skew.
*/
OpenACD.Agent.prototype.setSkew = function(timestamp){
	var now = new Date();
	now = Math.floor(now.getTime() / 1000);
	this.skew = now - timestamp;
};

/**
@private
*/
OpenACD.Agent.prototype._handleServerCommand = function(datalist){
	for(var i = 0; i < datalist.length; i++){
		 switch (datalist[i].command){
			case "pong":
				this.setSkew(datalist[i].timestamp);
				break;

			case "arelease":
				this.relaseData = datalist[i].releaseData;
				try{
					dojo.publish("OpenACD/Agent/release", [datalist[i]]);
				} catch(err) {
					console.error("OpenACD/Agent/release", err);
				}
				this.stopwatch.reset();
				break;

			case "setchannel":
			case "astate":
				var chan;
				if(this.channels[datalist[i].channelid]){
					chan = this.channels[datalist[i].channelid];
				} else {
					chan = new OpenACD.AgentChannel({
						channelId: datalist[i].channelid,
						agent: this,
						state: datalist[i].state,
						stateData: datalist[i].statedata
					});
				}
				console.log('das chan', chan);
				chan.handleStateChange(datalist[i].state, datalist[i].statedata);
				this.channels[datalist[i].channelid] = chan;
				if(datalist[i].state == 'wrapup'){
					this.setNag(datalist[i].channelid, "You have been in wrapup for more than 3 minutes.  Perhaps you forgot?", 1000 * 60 * 3);
				} else {
					this.clearNag(datalist[i].channelid);
				}
				break;

			case "endchannel":
				if(this.channels[datalist[i].channelid]){
					this.channels[datalist[i].channelid].destroy(datalist[i]);
					delete this.channels[datalist[i].channelid];
				}
				this.clearNag(datalist[i].channelid);
				break;

			case "aprofile":
				this.state = datalist[i].profile;
				try{
					dojo.publish("OpenACD/Agent/profile", [datalist[i]]);
				} catch (err) {
					console.error("OpenACD/Agent/profile", err);
				}
				break;

			case "urlpop":
				try{
					dojo.publish("OpenACD/Agent/urlpop", [datalist[i]]);
				} catch (err) {
					console.error("OpenACD/Agent/urlpop", err);
				}
				break;
			
			case "blab":
				try{
					dojo.publish("OpenACD/Agent/blab", [datalist[i].text]);
				} catch (err) {
					console.error("OpenACD/Agent/blab", err);
				}
				break;
			
			case "mediaload":
				try{
					dojo.publish("OpenACD/Agent/mediaload", [datalist[i]]);
				} catch (err) {
					console.error("OpenACD/Agent/mediaload", err);
				}
				break;
			
			case "mediaevent":
				try{
					dojo.publish("OpenACD/Agent/mediaevent/" + datalist[i].media, [datalist[i]]);
				} catch (err) {
					console.error("OpenACD/Agent/mediaevent", err);
				}
				break;
			
			default:
				try{
					dojo.publish("OpenACD/Agent/" + datalist[i].command, [datalist[i]]);
				} catch (err) {
					console.error("OpenACD/Agent/" + datalist[i].command, err);
				}
				console.info("non-local command", datalist[i].command);
		 }
	}
};

/**
@private
*/
OpenACD.Agent.prototype._handlePollError = function(response, ioargs){
	if (response.dojoType == "timeout") {
		this.poll();
		return;
	}

	if(ioargs.xhr.status === 0){
		console.warn("status 0, prolly due to logout", response);
		this.loggedIn = false;
		try{
			this.loggedIn = false;
			dojo.publish("OpenACD/Agent/logout", [response.responseText]);
		} catch (err) {
			console.error("OpenACD/Agent/logout", err);
		}
		return;
	}

	console.warn("poll errored", response, ioargs.xhr.status);
	//EventLog.log("Poll failed:  " + response.responseText);
	this.pollfailures += 1;
	if (this.pollfailures >= 5 || ioargs.xhr.status == 403) {
		this.loggedIn = false;
		try{
			this.loggedIn = false;
			dojo.publish("OpenACD/Agent/logout", [response.responseText]);
		} catch (err) {
			console.error("OpenACD/Agent/logout", err);
		}
	}
	else if (ioargs.xhr.status != 200) {
		this.poll();
	} else {
		console.error("NOT re-polling");
	}
};

/**
@private
*/
OpenACD.Agent.prototype._handlePollSuccess = function(response){
	this.pollfailures = 0;
	this.poll();
	this._handleServerCommand(response);
};

/**
@private
*/
OpenACD.Agent.prototype._handlePollFailure = function(errcode, msg){
	console.error('poll failed', msg);
	this.loggedIn = false;
	try{
		this.loggedIn =false;
		dojo.publish("OpenACD/Agent/logout", [msg]);
	} catch (err) {
		console.error("OpenACD/Agent/logout", err);
	}
}

/**
Start a long poll to the server.  Generally doesn't need to be called 
directly as a successful login starts this automatically.
*/
OpenACD.Agent.prototype.poll = function(){
	var options = {
		timeout: 30000,
		error:this.makeInternalPublishCb("poll/error"),
		success:this.makeInternalPublishCb("poll/success"),
		failure:this.makeInternalPublishCb("poll/failure")
	};
	this._pollHandle = this.agentApi("poll", options);
};

OpenACD.Agent.states = [
	"idle",
	"ringing",
	"precall",
	"oncall",
	"outgoing",
	"released",
	"warmtransfer",
	"wrapup"
];

/**
Make a request for the agent api.
@param {String} func The name of the request to make on the server.  See the
	documentaion on the agent_web_listener and agent_web_connection for 
	OpenACD for valid entries.
@param {Object} Opts The callback functions.
@param {Function} opts.success If the server responds afirmitively, this
	function is called with the result object as the argument.
@param {Function} opts.failure If the server responds negatively, this 
	function is called withe the errCode and message as arguments.
@param {Function} opts.error If an error occured during the request (or 
	running the success or failure functions), this function is called with
	the error or response and ioargs as arguments.
@param [args*] The remaining arguments are used as the arguments for the 
	request on the server side.  See the OpenACD documentation for 
	agent_web_listener and agent_web_connection.
*/
OpenACD.Agent.prototype.agentApi = function(func, options){
	var args = ['api'];
	for(var i = 0; i < arguments.length; i++){
		args.push(arguments[i]);
	}
	this.webApi.apply(this, args);
}

/**
Make a request for the supervisor api.
@param {String} func The name of the request to make on the server.  See the
	documentaion on the agent_web_listener and agent_web_connection for 
	OpenACD for valid entries.
@param {Object} Opts The callback functions.
@param {Function} opts.success If the server responds afirmitively, this
	function is called with the result object as the argument.
@param {Function} opts.failure If the server responds negatively, this 
	function is called withe the errCode and message as arguments.
@param {Function} opts.error If an error occured during the request (or 
	running the success or failure functions), this function is called with
	the error or response and ioargs as arguments.
@param [args*] The remaining arguments are used as the arguments for the 
	request on the server side.  See the OpenACD documentation for 
	agent_web_listener and agent_web_connection.
*/
OpenACD.Agent.prototype.supervisorApi = function(func, options){
	var args = ['supervisor'];
	for(var i = 0; i < arguments.length; i++){
		args.push(arguments[i]);
	}
	this.webApi.apply(this, args);
}

/**
Make a request to the server.  Most common requests are abstracted out, 
making this a fallback.
@param {String} api The api section of the request.  'api' and 'supervisor'
	are the only options supported currently.
@param {String} func The name of the request to make on the server.  See the
	documentaion on the agent_web_listener and agent_web_connection for 
	OpenACD for valid entries.
@param {Object} Opts The callback functions.
@param {Function} opts.success If the server responds afirmitively, this
	function is called with the result object as the argument.
@param {Function} opts.failure If the server responds negatively, this 
	function is called withe the errCode and message as arguments.
@param {Function} opts.error If an error occured during the request (or 
	running the success or failure functions), this function is called with
	the error or response and ioargs as arguments.
@param [args*] The remaining arguments are used as the arguments for the 
	request on the server side.  See the OpenACD documentation for 
	agent_web_listener and agent_web_connection.
*/
OpenACD.Agent.prototype.webApi = function(api, func, opts){
	if(! (api == 'api' || api == 'supervisor') ){
		throw new Error('Unknown api section ' + api);
	}
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
	for(var i = 3; i < arguments.length; i++){
		args.push(arguments[i]);
	}
	var xhrOverrides = {
		url:"/" + api,
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
	
	return dojo.xhrPost.call(this, xhrOpts);
};

/**
Set the release mode of the agent.  Handling of the change is not required
as all state changes are reported through the poll and publish mecahnism.
@param {String} Release code as "Id:Label:Bias" string, or "none", or 
	"Default".
*/
OpenACD.Agent.prototype.setRelease = function(release){
	this.agentApi("set_release", {}, release);
};

/**
Attempt to start and outbound call.
@param {String} Client Name of the client the call is for.
@param {String} Type Media type of the call.
@param {Object} [callbacks] The success, fail, and error callbacks.
@param {Function} [callbacks.success] Function to call on success.
@param {Function} [callbacks.failure] Funciton to call on failure.
@param {Function} [callbacks.error] Function to call on error.
*/
OpenACD.Agent.prototype.initOutbound = function(Client, Type, callbacks) {
	if(!callbacks){
		callbacks = {};
	}
	this.agentApi("init_outbound", callbacks, Client, Type);
};

/**
@private
*/
/*OpenACD.Agent.prototype._cleanTmpFunc = function(all){
	if(all){
		delete this._tmpSuccess;
	}
	delete this._tmpFail;
	delete this._tmpErr;
}*/

/**
@private
*/
OpenACD.Agent.prototype._handleGetSaltSuccess = function(response){
	var salt = response.salt;
	var e = response.pubkey.E;
	var n = response.pubkey.N;
	var rsa = new RSAKey();
	rsa.setPublic(n, e);
	var password = rsa.encrypt(salt + this.password);
	var loginOpts = {
		success:this.makeInternalPublishCb("login/success"),
		error:this.makeInternalPublishCb("login/error"),
		failure:this.makeInternalPublishCb("login/failure")
	};
	if(this.loginOptions){
		this.agentApi("login", loginOpts, this.username, password, this.loginOptions);
	} else {
		this.agentApi("login", loginOpts, this.username, password);
	}
}

/**
@private
*/
OpenACD.Agent.prototype._handleGetSaltError = function(res, io){
	console.error('Getting salt errored', res);
}

/**
@private
*/
OpenACD.Agent.prototype._handleGetSaltFailure = function(errCode, Msg){
	console.error('Getting salt failed', errCode, Msg);
}

/**
@private
*/

/**
@private
*/
OpenACD.Agent.prototype._handleLoginError = function(results, io){
	console.error('Login errored', results);
}

/**
@private
*/
OpenACD.Agent.prototype._handleLoginFailure = function(errCode, msg){
	console.error('Login failed', errCode, msg);
}

/**
@private
*/
OpenACD.Agent.prototype._handleLoginSuccess = function(results){
	this.profile = results.profile;
	this.securityLevel = results.securityLevel;
	this.timestamp = results.timestamp;
	this.poll();
	try{
		this.loggedIn = true;
		dojo.publish("OpenACD/Agent/login", [this]);
	} catch (err) {
		console.error("OpenACD/Agent/login", err);
	}
}

/**
Attempt to login the agent using the already establish username and 
password.
@param {Function} successCB What to do when the login is successful.  Note a
	publish is also done after login.
@param {Function} failCB What to do if the login failed.
@param {Function} errCB What to do if the login errored.
*/
OpenACD.Agent.prototype.login = function(successCB, failCB, errCB){
	if(! this.username && this.password){
		throw new Error('Cannot login without a username and password');
	}
	if(! successCB){
		successCB = function(){};
	}
	if(! failCB){
		failCB = function(){};
	}
	if(! errCB){
		errCB = function(){};
	}
	this.loginState = {
		success: successCB,
		failure: failCB,
		error: errCB
	}
	var getSaltOpts = {
		error:this.makeInternalPublishCb("salt/error"),
		success:this.makeInternalPublishCb("salt/success"),
		failure:this.makeInternalPublishCb("salt/failure"),
	};
	this.agentApi("get_salt", getSaltOpts);
}

/**
@private
*/
OpenACD.Agent.prototype.makeInternalPublishCb = function(chan){
	chan = "OpenACD/Agent/internal/" + chan;
	out = function(){
		dojo.publish(chan, arguments);
	};
	return out;
}

/**
End the polling and kill the agent on the server side.
*/
OpenACD.Agent.prototype.logout = function(){
	// cancel this up front so
	if(this._pollHandle){
		this._pollHandle.cancel();
	}
	// this should always succeed, so send the agent/logout in all cases
	var options = {
		error:function(response, ioargs){
			console.error("error logging out", response);
			try{
				dojo.publish("OpenACD/Agent/logout", [true]);
			} catch (err) {
				console.error("OpenACD/Agent/logout", err);
			}
		},
		success:function(){
			try{
				dojo.publish("OpenACD/Agent/logout", [true]);
			} catch (err) {
				console.error("OpenACD/Agent/logout", err);
			}
		}
	};
	this.agentApi("logout", options);
	this.loggedIn = false;
};

/**
After init_outbound, dial/contact the given contact.
@param {String} digits Numbers or contact string.
@param {Object} [options] Callbacks
*/
OpenACD.Agent.prototype.dial = function(digits, options) {
	this.agentApi("dial", options, digitis);
};

/**
When an agent is oncall, send a request to the media
@param data Information to send.
@param {Object} [options] Callbacks for success, failure, and error.
*/
OpenACD.Agent.prototype.mediaPush = function(data, options){
	if(this.state == "oncall"){
		this.agentApi("mediapush", options, data);
		return true;
	}
	return false;
};

/**
Set a message to appear to the agent after a time.  Nagging is cleared auto-
matically when an agent goes idle.
@param {String} message What to tell the agent.
@param {Number} time How many milliseconds to wait before nagging.
@returns Reference usuable with clearNag
*/
OpenACD.Agent.prototype.setNag = function(channelid, message, time){
	var ref = this;
	var nag = setTimeout(function(){
		try{
			dojo.publish("OpenACD/Agent/blab", [message]);
		} catch (err) {
			console.error("OpenACD/Agent/blab", err);
		}
		delete(ref._nags[channelid]);
	}, time);
	this._nags[channelid] = nag;
	return nag;
}

/**
Remove a queues nag.
@param nagref A refernce returned from setNag.
*/
OpenACD.Agent.prototype.clearNag = function(nagref){
	if(this._nags[nagref]){
		clearTimeout(this._nags[nagref]);
	}
	delete this._nags[nagref];
}

/**
Begin a transfer to another agent.
@param {String} aname Login of the agent to transfer to.
@param {Object} options Callbacks for success, failure, and error.
@param [arg*] Remaining arguments are used as arbitrary data for the 
	transfer.
*/
OpenACD.Agent.prototype.agentTransfer = function(aname, options){
	var applyArgs = ["agent_transfer", options, aname];
	for(var i = 2; i < arguments.length; i++){
		applyArgs.push(arguments[i]);
	}
	this.agentApi.apply(this, applyArgs);
};

/**
Initiate a warm transfer
@param {String} num Number/contact to reach.
@param {Object} options Callbacks for success, failure, and error.
*/
OpenACD.Agent.prototype.warmtransfer = function(num, options) {
	this.agentApi("warm_transfer", options, num);
};

/**
Cancel a warm transfer
@param {Object} options Callbacks for success, failure, and error.
*/
OpenACD.Agent.prototype.warmtransfercancel = function(options) {
	this.agentApi("warm_transfer_cancel", options);
};

/**
Complete a warm transfer; eg, connect the orignal contact to the 3rd party.
@param {Object} options Callbacks for success, failure, and error.
*/
OpenACD.Agent.prototype.warmtransfercomplete = function(options) {
	this.agentApi("warm_transfer_complete", options);
};

/**
Send a media the agent is on call with to a different queue.
@param {String} queue Name of the queue to transfer to.
@param {Array} skills List of skills to apply to the media.
@param {Object} urlopts Key/values for a new urlpop, or information for the
	next agent to answer the call.
@param {Object} options Callbacks for success, failure, and error.
*/
OpenACD.Agent.prototype.queuetransfer = function(queue, skills, urlopts, options) {
	urlopts.skills = skills;
	this.agentApi("queue_transfer", options, queue, urlopts);
};

/**
Get a list of the currently available agents.  An agent is 'available' if 
they are idle or released.
@param {Object} options Callbacks for success, failure, and error.
*/
OpenACD.Agent.prototype.getAvailAgents = function(options) {
	this.agentApi("get_avail_agents", options);
};

/**
Check if the agent is already logged in.  This allows an agent to reload the
browser without trashing thier interface (provided the correct callbacks
are in place.
@param {Object} options Callbacks on success, fail, et al.
*/
OpenACD.Agent.prototype.checkCookie = function(options){
	if(this.loggedIn){
		try{
			dojo.publish("OpenACD/Agent/login", [this]);
		} catch (err) {
			console.error("OpenACD/Agent/login", err);
		}
		return true;
	}
	var userSuccess = options.success || function(){};
	var userFail = options.failure || function(){};
	var userErr = options.error || function(){};
	var mySuccess = this.makeInternalPublishCb("checkCookie/success");
	var trueOpts = {
		success:function(res){
			mySuccess(res);
			userSuccess(res);
		},
		failure:userFail,
		error:userErr
	};
	this.agentApi("check_cookie", trueOpts);
}

OpenACD.Agent.prototype._handleCheckCookieSuccess = function(res){
	this.username = res.login;
	this.securityLevel = res.securityLevel;
	this.elapsed = res.elapsed;
	this.setSkew(res.timestamp);
	this.poll();
	try{
		this.loggedIn = true;
		dojo.publish("OpenACD/Agent/login", [this]);
		this.state = res.state;
		this.statedata = res.statedata;
		dojo.publish("OpenACD/Agent/state", [res]);
	} catch (err) {
		console.error("OpenACD/Agent/checkCookie", err);
	}
}

OpenACD.Agent.prototype._handleCheckCookieFailure = function(res, msg){
	console.warn('failed check cookie', res, msg);
}

OpenACD.Agent.prototype._handleCheckCookieError = function(res){
	console.error('Error check cookie', res);
}

/*
 	_____
 /  |  \
/ \ |   \
|  \|   |
|       |
\       /
 \_____/
*/

/**
Create a new Stopwatch
@param {Number} [elapsed] Start the stopwatch at the elapsed time instead 
	of 0.
@class Sets up a function to be called every second.  Useful for updating 
clock outputs.
@property {Function} onTick Function to call on each second.
*/
OpenACD.Stopwatch = function(elapsed){
	this.tref = null;
	if (isNaN(elapsed)) {
		this.elapsed = 0;
	} else {
		var now = Math.round(new Date().getTime() / 1000);
		if (now > elapsed) {
			this.elapsed = now - elapsed;
		} else {
			console.warn('now is less than elapsed', now, elapsed);
			this.elapsed = 0;
		}
	}
	this.onTick = function(){};
	this.subscription = dojo.subscribe('OpenACD/Stopwatch/onTick/internal', this, this.onTickInternal);
}

/**
@private
*/
OpenACD.Stopwatch.prototype.onTickInternal = function(){
	this.elapsed += 1;
	this.onTick();
	this.tref = setTimeout(function(){
		dojo.publish('OpenACD/Stopwatch/onTick/internal', []);
	}, 1000);
};

/**
Get how long the stopwatch has been running since last reset.
@returns Number
*/
OpenACD.Stopwatch.prototype.time = function(){
	return this.elapsed;
};

/**
Sets the elpased time to 0;
*/
OpenACD.Stopwatch.prototype.reset = function(){
	this.elapsed = 0;
};

/**
Start the stopwatch.
*/
OpenACD.Stopwatch.prototype.start = function(){
	if (this.tref) {
		this.stop();
	}
	this.tref = setTimeout(function(){
		dojo.publish('OpenACD/Stopwatch/onTick/internal', []);
	}, 1000);
}

/**
Stop/pause the stopwatch.  Does not reset the elpased time.
*/
OpenACD.Stopwatch.prototype.stop = function(){
	if(this.tref){
		clearTimeout(this.tref);
	}
	this.tref = null;
}
