dojo.provide("agentUI.logLib");

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