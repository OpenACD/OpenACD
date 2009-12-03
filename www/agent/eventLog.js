function EventLog(){	
}

EventLog.logged = [];

EventLog.log = function(text){
	var d = new Date();
	var line = d.toLocaleString() + " - " + text;
	EventLog.logged.push(line);
	dojo.publish("eventlog/push", [line]);
	if(EventLog.log.length > 10000){
		var shifted = Eventlog.log.shift();
		dojo.publish("eventlog/shift", [shifted]);
	}
	return line;
};
