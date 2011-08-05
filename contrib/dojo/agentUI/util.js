dojo.require("agentUI.logLib");

dojo.provide("agentUI.util");

window.encodeHTML = function(str) {
	if (!str || !str.replace){
		return str;
	}
	return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/, '&gt;');
}

window.decodeHTML = function(str) {
	if (!str || !str.replace){
		return str;
	}
	return str.replace(/&gt;/g, '>').replace(/&lt;/g, '<').replace(/&amp;/g, '&');
}

window.formatseconds = function(seconds) {
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

window.inArray = function(needle, haystack){
	for(var i = 0; i < haystack.length; i++){
		if(haystack[i] == needle){
			return true;
		}
	}
	return false;
}

window.replaceUrls = function(text){
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
