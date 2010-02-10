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
	dojo.forEach(response.clients_in_queues,
			function(client) {
			if (client.medias.length > 0 /*&& client.totalInbound > 0*/) {
			row = table.insertRow(rownum);
			c1 = row.insertCell(0);
			c1.innerHTML = client.label;
			if (client.medias[0].type != 'voice') {
				c1.innerHTML += "(" + client.medias[0].type + ")";
			}
			c1 = row.insertCell(1);
			c1.innerHTML = dojo.filter(client.medias, function(obj) { return !!obj.queued; }).length;
			c1 = row.insertCell(2);
			c1.innerHTML = dojo.filter(client.medias, function(obj) { return !obj.queued && !!obj.ivr; }).length;
			c1 = row.insertCell(3);
			var longest = dojo.filter(client.medias, function(obj) { return !!obj.queued; });
			if (longest.length === 0) {
				var filtered = dojo.filter(client.medias, function(obj) { return !obj.queued && !!obj.ivr; });
				if(filtered.length > 0){
					longest = filtered[0].ivr;
				} else {
					longest = Math.floor(new Date().getTime() / 1000);
				}
			} else {
			longest = longest[0].queued;
			}
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
		dojo.forEach(response.agentProfiles,
				function(profile) {
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
						if (agent.stateData.type != "voice") {
							c1.innerHTML += "(" + agent.stateData.type + ")";
						}
						c1.innerHTML += " " + agent.stateData.brand;
						break;
						case "warmtransfer":
						c1.innerHTML += "(" + agent.stateData.onhold.brand + ") (" + agent.stateData.calling + ")";
						break;
						case "released":
							c1.innerHTML += "("+agent.stateData.label+")";
						break;
						}

						c1 = row.insertCell(2);
						c1.innerHTML = timeSince(agent.lastchange);

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
