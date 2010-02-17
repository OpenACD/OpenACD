function capitalize(string) {
	return string.charAt(0).toUpperCase() + string.slice(1);
}

function timeSince(timestamp){
	if(isNaN(timestamp)){
		return timestamp;
	}

	var now = Math.floor(new Date().getTime() / 1000);
	var elapsed = now - timestamp;
	if(elapsed < 60){
		return elapsed + " second" + (elapsed != 1 ? "s" : "");
	}
	var minutes = Math.floor(elapsed/60);
	if(minutes < 60){
		return minutes + " minute" + (minutes != 1 ? "s" : "") + " " + elapsed % 60 + " second" + (elapsed % 60 != 1 ? "s" : "");
	}
	var hours = Math.floor(minutes/60);
	if(hours < 24){
		return hours + " hour" + (hours != 1 ? "s" : "") + " " + minutes % 60 + " minute" + (minutes % 60 != 1 ? "s" : "");
	}
	var days = Math.floor(hours/24);
	return days + " day"+ (days != 1 ? "s" : "") + " " + hours % 24 + " hour" + (hours % 24 != 1 ? "s" : "");
}

function drawCalls(response) {
	var table = dojo.byId("calllist");

	while(table.rows.length > 1){
		table.deleteRow(1);
	}

	var rownum = 1;
	var row;
	var c1;
	dojo.forEach(response.clients,
			function(client) {
			var len = dojo.filter(client.medias, function(obj) { 
				var fullm = response.rawData[obj];
				return fullm.state == 'queue'; 
			}).length;
			if (client.medias.length > 0 && len > 0/*&& client.totalInbound > 0*/) {
			row = table.insertRow(rownum);
			c1 = row.insertCell(0);
			c1.innerHTML = client.label;
			if (response.rawData[client.medias[0]].type != 'voice') {
				c1.innerHTML += "(" + response.rawData[client.medias[0]].type + ")";
			}
			c1 = row.insertCell(1);
			c1.innerHTML = dojo.filter(client.medias, function(obj) { 
				var fullm = response.rawData[obj];
				return fullm.state == 'queue'; 
			}).length;
			c1 = row.insertCell(2);
			c1.innerHTML = ''; /*dojo.filter(client.medias, function(obj) { 
				var fullm = response.rawData[obj];
				return fullm.state == 'ivr'; 
			}).length;*/
			c1 = row.insertCell(3);
			var longest = dojo.filter(client.medias, function(obj) { 
				var fullm = response.rawData[obj];
				return (fullm.state == 'queue' /*|| fullm.state == 'ivr'*/);
			});
			/*if (longest.length === 0) {
				var filtered = dojo.filter(client.medias, function(obj) { 
					var fullm = response.rawData[obj];
					return fullm.state == 'queue' || fullm.state == 'ivr'; 
				});
				if(filtered.length > 0){
					longest = filtered[0].ivr;
				} else {
					longest = Math.floor(new Date().getTime() / 1000);
				}
			} else {
				
			longest = longest[0].queued;
			}*/
			var now = Math.floor(new Date().getTime() / 1000);
			var eldest = now;
			for(var i = 0; i < longest.length; i++){
				var fullm = response.rawData[longest[i]];
				var lastHistory = fullm.history.length - 1;
				if(fullm.history[lastHistory].timestamp < eldest){
					eldest = fullm.history[lastHistory].timestamp;
				}
			}
			longest = eldest;
			c1.innerHTML = timeSince(longest);
			rownum++;
			}
			});
	if (rownum == 1) {
		row = table.insertRow(1);
		c1 = row.insertCell(0);
		c1.setAttribute("colspan", 3);
		c1.innerHTML = "No Calls";
	}
}


function drawAgents(response) {
	var table = dojo.byId("agentlist");

	while(table.rows.length > 1){
		table.deleteRow(1);
	}

	var row;
	var c1;

	if (response.agentProfiles.length === 0) {
		row = table.insertRow(1);
		c1 = row.insertCell(0);
		c1.setAttribute("colspan", 3);
		c1.innerHTML = "No Agents";
	} else {
		var rownum = 1;
		dojo.forEach(response.agentProfiles.sort(function(a, b) { return a.name > b.name}), 
				function(profile) {
				if (profile.agents.length == 0)
					return;
				row = table.insertRow(rownum);
				c1 = row.insertCell(0);
				c1.setAttribute("colspan", 2);
				row.setAttribute("class", "profile");
				c1.innerHTML = profile.name;
				rownum++;
				dojo.forEach(profile.agents,
					function(agent) {
						row = table.insertRow(rownum);
						c1 = row.insertCell(0);
						c1.innerHTML = "&nbsp;&nbsp;"+agent.login;
						c1 = row.insertCell(1);
						c1.innerHTML = capitalize(agent.state);

						switch (agent.state) {
						case "ringing":
						case "oncall":
						case "wrapup":
						case "precall":
						case "outgoing":
						if (agent.statedata.type != "voice") {
							c1.innerHTML += "(" + agent.statedata.type + ")";
						}
						c1.innerHTML += " " + agent.statedata.clientLabel;
						break;
						case "warmtransfer":
						c1.innerHTML += "(" + agent.statedata.onhold.clientLabel + ") (" + agent.statedata.calling + ")";
						break;
						case "released":
							c1.innerHTML += "("+agent.statedata+")";
						break;
						}

						c1 = row.insertCell(2);
						c1.innerHTML = timeSince(agent.timestamp);

						row.setAttribute("class", "agent");
						row.setAttribute("state", agent.state);
						rownum++;
					});
				}
		);
	}
}

function update() {
	dojo.xhrGet({
		url:"all.json",
		handleAs:"json",
		error:function(response, ioargs){
			console.error(response);
			window.setTimeout(update, 5000);
		},
		load:function(response, ioargs){
			drawCalls(response);
			drawAgents(response);
			if(window.spew){
				console.log(response);
			}
			window.setTimeout(update, 5000);
		}
	});
}

dojo.addOnLoad(update);
