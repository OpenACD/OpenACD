function EventLog(){	
}

EventLog.logged = [];

EventLog.log = function(text){
	var d = new Date();
	var line = d.toLocaleString() + " - " + text;
	EventLog.logged.push(line);
	if(EventLog.log.length > 10000){
		Eventlog.log.shift()
	}
	return line
}