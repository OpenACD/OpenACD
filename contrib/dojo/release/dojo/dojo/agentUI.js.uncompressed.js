/*
	Copyright (c) 2004-2011, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/

/*
	This is an optimized version of Dojo, built for deployment and not for
	development. To get sources and documentation, please visit:

		http://dojotoolkit.org
*/

//>>built
require({cache:{
'dojox/main':function(){
define("dojox/main", ["dojo/_base/kernel"], function(dojo) {
	// module:
	//		dojox/main
	// summary:
	//		The dojox package main module; dojox package is somewhat unusual in that the main module currently just provides an empty object.

	return dojo.dojox;
});
},
'agentUI/util':function(){
// wrapped by build app
define(["dojo","dijit","dojox","dojo/require!agentUI/logLib"], function(dojo,dijit,dojox){
dojo.require("agentUI.logLib");

dojo.provide("agentUI.util");

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

function formatseconds(seconds) {
	var d = new Date();
	d.setHours(0);
	d.setMinutes(0);
	d.setSeconds(seconds);
	var s = "" + d.getSeconds();
	if (d.getSeconds() < 10) {
		s = "0"+s;
	}
	s = d.getMinutes()+":"+s;
	if (d.getHours() > 0) {
		if (d.getMinutes() < 10) {
			s = "0"+s;
		}
		s = d.getHours() + ":" + s;
	}
	return s;
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
	var exp = /(\b(https?|ftp):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/ig;
	return text.replace(exp,"<a href='$1' target='_blank'>$1</a>");
}

window.declareTick = function(){
	window.globalTick = setTimeout(window.declareTick, 1000);
	dojo.publish('globaltick', []);
}

window.stopGlobalTick = function(){
	clearTimeout(window.globalTick);
}

window.startGlobalTick = function(){
	window.stopGlobalTick();
	window.globalTick = setTimeout(window.declareTick, 1000);
}

});

},
'dijit/main':function(){
define("dijit/main", [
	"dojo/_base/kernel"
], function(dojo){
	// module:
	//		dijit
	// summary:
	//		The dijit package main module

	return dojo.dijit;
});

},
'agentUI/logLib':function(){
// wrapped by build app
define(["dojo","dijit","dojox"], function(dojo,dijit,dojox){
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
	var levelint = window._logLevelToNumber(levelstring);
	if(levelint >= 0){
		window._logLevel = levelint;
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

});

}}});

define("dojo/agentUI", [], 1);
