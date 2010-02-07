
var specialtext=[];
var nextspecialtext=0;
var chars="M1234567890ABCDEFGHIJKLNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz !@#$%^&*()_+-=[]\\{}|;\':<>?,./`~";
var emsize=Array(1, 0.625, 0.6875, 0.625, 0.6875, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.75, 0.75, 0.75, 0.8125, 0.6875, 0.6875, 0.8125, 0.8125, 0.375, 0.4375, 0.75, 0.625, 0.8125, 0.8125, 0.625, 0.8125, 0.75, 0.6875, 0.625, 0.8125, 0.75, 1, 0.6875, 0.6875, 0.75, 0.625, 0.6875, 0.5625, 0.6875, 0.625, 0.375, 0.6875, 0.625, 0.375, 0.375, 0.625, 0.375, 0.9375, 0.625, 0.625, 0.6875, 0.6875, 0.5, 0.5625, 0.5, 0.6875, 0.5625, 0.875, 0.5625, 0.5625, 0.5625, 0.3125, 0.375, 1, 0.8125, 0.625, 1, 0.8125, 0.875, 0.5, 0.375, 0.375, 0.5, 0.875, 0.375, 0.875, 0.375, 0.375, 0.3125, 0.625, 0.625, 0.375, 0.375, 0.3125, 0.3125, 0.875, 0.875, 0.5625, 0.3125, 0.3125, 0.3125, 0.5, 0.84)

function sign(x) {
	if (x > 0) {return 1;}
	if (x < 0) {return -1;}
	return 0;
}

function resizespan2(spn) {
	var parentht = parseInt(spn.parentNode.clientHeight, 10);
	var parentwd = parseInt(spn.parentNode.clientWidth, 10);
	if (parentht === 0) {return;}
	var ems = 0;
	var offset = -1;
	for (var i=0;i<spn.innerHTML.length;i++){
		offset = chars.indexOf(spn.innerHTML[i]);
		if (offset < 1) {
			ems ++;
		} else {
			ems += emsize[offset];
		}
	}
	if (ems == 0) {return;}
	fontsz = parseInt(parentwd/ems);
	var ruler = document.getElementById('ruler');
	ruler.innerHTML = spn.innerHTML;
	ruler.style.fontSize = fontsz + 'px';
	if (ruler.offsetHeight > parentht) {
		fontsz = parseInt((fontsz * parentht)/ruler.offsetHeight);
	}
	spn.style.fontSize = fontsz + 'px';
}

function resizespan1(spn) {
	var fontmax=140;
	var curfont = parseInt(spn.style.fontSize, 10);
	var parentht = parseInt(spn.parentNode.clientHeight, 10) * 0.95;
	var parentwd = parseInt(spn.parentNode.clientWidth, 10) * 0.95;
	if (parentht === 0) {return;}
	var ht;
	var wd;
	var hdiff;
	var wdiff;
	var hdir;
	var wdir;
	var closest;
	var cnt = 0;
	var done = false;	
	var lastdiff = 99999;
	var diff = 0;
	var laststep,prevstep;
	if (isNaN(curfont) || (curfont < 1)) {
		curfont = 10;
	}
	var step = Math.floor( curfont / 2 + 0.5);

	while (!done) {
		spn.style.fontSize = curfont + "px";
		ht = spn.offsetHeight;
		wd = spn.offsetWidth;
		hdiff = parentht - ht;
		wdiff = parentwd - wd;
		hdir = sign(hdiff);
		wdir = sign(wdiff);
		if (hdir == -1) {
			diff = hdiff;
		}
		if (wdir == -1) {
			diff = wdiff;
		}
		if ((wdir > 0) && (hdir > 0)) {
			closest = curfont;
		}
		if (hdir == wdir) {
			diff = (Math.abs(hdiff) < Math.abs(wdiff)) ? hdiff : wdiff;
		}
		prevstep = laststep;
		laststep = step;
		if (sign(diff) == sign(lastdiff)) {
			step = step * 2;
		} else {
			step = Math.floor(step / 2 + 0.5);
			//step = parseInt(diff / 2 + 0.5);
		}
		if ((step == laststep) || (step == prevstep)) {
			step = Math.floor(step / 2 + 0.5);
		}
		//					console.log("("+ curfont + ")(" + ht + "," + wd + ")(" + parentht + "," + parentwd + ")(" + diff + "," + lastdiff + ")(" + step + "," + laststep + ") " + done);
		curfont = curfont + (step * sign(diff));
		if (isNaN(curfont) || (curfont < 1)) {curfont = 1;}
		spn.style.fontSize = curfont;
		done = (((Math.abs(hdiff) < 2) || (ht <= parentht)) && ((Math.abs(wdiff) < 2) || (wd <= parentwd)) && (diff < (parentwd / 10)) && (Math.abs(step) < 3)) ;
		lastdiff = diff;
		if (cnt++ > 20) {
			done = true;
			spn.style.fontSize = closest;
		}
	}
}

resizespan = resizespan2;

function resizeall() {
	var spans = document.getElementsByTagName("span");
	for (var i=0;i<spans.length;i++) {
		if (spans[i].attributes.resize) {
			resizespan(spans[i]);
		}
	}
	/*setTimeout('resizeall()',10000);*/
}

function updatespan(span, value) {
	var oldvalue = span.innerHTML;
	span.innerHTML = value;
	if (value != oldvalue) {
		resizespan(span);
	}
}

function swapstat() {
	var hist = document.getElementById("historic");
	var divs = hist.getElementsByTagName("div");
	var stats = [];
	var active = -1;
	for (var i = 0; i < divs.length; i++) {
		if (divs[i].className == 'stat') {
			stats.push(divs[i]);
			if (active != -1) {
				divs[i].style.zIndex = -1;
			}
			if (divs[i].style.zIndex > 0) {
				active = stats.length - 1;
			}
		}
	}
	if (active == -1) {
		stats[0].style.zIndex = 99;
	} else {
		var nextstat = active + 1;
		if (nextstat >= stats.length) {
			nextstat = 0;
		}
		stats[active].style.zIndex = -1;
		stats[nextstat].style.zIndex = 99;
	}
}
function swapalert(alertdiv) {
	var adiv = dojo.byId(alertdiv);
	var divs = dojo.query(".alerttext", adiv);
	var stxt = dojo.query(".specialtext",adiv)[0];
	stxt.firstChild.innerHTML = ".";
	dojo.query(".alertcount", adiv)[0].firstChild.innerHTML = divs.length;
	if (divs.length === 0) {
		if (specialtext.length > 0) {
			stxt.style.display = 'block';
			stxt.firstChild.innerHTML = specialtext[nextspecialtext];
			nextspecialtext += 1;
			if (nextspecialtext >= specialtext.length) {
				nextspecialtext = 0;
			}
			stxt.firstChild.style.fontSize = '150px';
			resizespan(stxt.firstChild);
		}
		return;
	}
	stxt.style.display = 'none';
	var alerts = [];
	var active = -1;
	var i = 0;
	for (i = 0; i < divs.length; i++) {
		alerts.push(divs[i]);
		if (active != -1) {
			divs[i].style.zIndex = -1;
		}
		if (parseInt(divs[i].style.zIndex, 10) > 0 ) {
			active = alerts.length - 1;
		}
	}
	if (active == -1) {
		alerts[0].style.zIndex = 99;
	} else {
		var nextalert = active + 1;
		if (nextalert >= alerts.length) {
			nextalert = 0;
		}
		alerts[active].style.zIndex = -1;
		alerts[nextalert].style.zIndex = 99;
	}
}

function updateStats(node, stats) {
	updatespan(dojo.query(".inbound", node)[0].firstChild, stats.inbound);
	updatespan(dojo.query(".outbound", node)[0].firstChild, stats.outbound);
	updatespan(dojo.query(".abandon", node)[0].firstChild, stats.abandoned);
	if (stats.abandoned > 0) {
		updatespan(dojo.query(".percent", node)[0].firstChild, Math.floor((stats.abandoned / stats.inbound) * 100) + "%");
	} else {
		updatespan(dojo.query(".percent", node)[0].firstChild, "0%");
	}
}

function getStatsSince(time, calls) {
	var inbound = 0;
	var outbound = 0;
	var abandoned = 0;
	var callssince = dojo.filter(calls, function (obj) { return obj.time >= time;});
	for(var i in callssince) {
		if (callssince[i].direction == "inbound") {
			inbound += 1;
			if (callssince[i].didAbandon) {
				abandoned += 1;
			}
		} else {
			outbound += 1;
		}
	}
	return {inbound: inbound, outbound: outbound, abandoned: abandoned};
}

function getAgentsInProfiles(profiles, filter) {
	var agents = [];
	for(var i in profiles){
		if (filter == "*" || dojo.indexOf(filter, profiles[i].name) != -1) {
			agents = agents.concat(profiles[i].agents);
		}
	}
	return agents;
}

function updateAgents(node, agents) {
	updatespan(dojo.query(".total", node)[0].firstChild, agents.length);
	updatespan(dojo.query(".available", node)[0].firstChild,
			dojo.filter(agents, function (obj) { return obj.state == "idle";}).length);
	updatespan(dojo.query(".oncall", node)[0].firstChild,
			dojo.filter(agents, function (obj) { return obj.state != "idle" && obj.state != "released" && obj.state != "wrapup";}).length);
	updatespan(dojo.query(".released", node)[0].firstChild,
			dojo.filter(agents, function (obj) { return obj.state == "released";}).length);
	updatespan(dojo.query(".wrapup", node)[0].firstChild,
			dojo.filter(agents, function (obj) { return obj.state == "wrapup";}).length);
}

function getQueue(queuegroups, name) {
	for(var i in queuegroups) {
		var group = queuegroups[i].queues;
		for(var j in group) {
			if (group[j].name == name) {
				return group[j];
			}
		}
	}
}

function updateQueue(node, queue) {
	if (!queue) {
		updatespan(dojo.query(".callcount", node)[0].firstChild, "0/0");
		updatespan(dojo.query(".holdtime", node)[0].firstChild, "00:00");
		return;
	}
	updatespan(dojo.query(".callcount", node)[0].firstChild,
			dojo.filter(queue.medias, function(obj) { return obj.type == "voice" && !obj.ended; }).length + "/" +
			dojo.filter(queue.medias, function(obj) { return obj.type != "voice" && !obj.ended; }).length);
	var longest = dojo.filter(queue.medias, function(obj) { return !obj.ended;})[0];
	if (!longest) {
		updatespan(dojo.query(".holdtime", node)[0].firstChild, "00:00");
		return;
	}
	var now = Math.floor(new Date().getTime() / 1000);
	var elapsed = now - longest.queued;

	if (elapsed < 10) {
		updatespan(dojo.query(".holdtime", node)[0].firstChild, "00:0" + elapsed);
	} else if(elapsed < 60) {
		updatespan(dojo.query(".holdtime", node)[0].firstChild, "00:" + elapsed);
	} else {
		var seconds = (elapsed % 60);
		if (seconds < 10) {
			seconds = "0" + seconds;
		}
		updatespan(dojo.query(".holdtime", node)[0].firstChild, Math.floor(elapsed / 60) + ":" + seconds);
	}
}

function agentAlerts(agents) {
	var adiv = dojo.byId("agentalerts");
	var now = new Date().getTime() / 1000;
	for(var i in agents) {
		if (agents[i].state == "released" && (now - agents[i].lastchange) > 6) {
			var s;
			if (dojo.byId(agents[i].login)) {
				s =dojo.byId(agents[i].login).firstChild;
			}else{
				var a = document.createElement("div");
				s = document.createElement("span");
				a.setAttribute("class", "alerttext");
				a.id = agents[i].login;
				/*s.setAttribute("resize", "true");*/
				a.appendChild(s);
				dojo.byId("agentalerts").appendChild(a);
			}
			updatespan(s, agents[i].login + " has been released for more than 10 minutes");
		} else {
			if (dojo.byId(agents[i].login)) {
				adiv.removeChild(dojo.byId(agents[i].login));
			}
		}
	}
	/* remove any orphaned alerts */
	dojo.query(".alerttext", adiv).forEach(function(e) {
			if (dojo.filter(agents, function(obj) { return obj.login == e.id; }).length === 0) {
				adiv.removeChild(e);
			}
	});
}

function update() {
	dojo.xhrGet({
		url:"all.json",
		handleAs:"json",
		error:function(response, ioargs){
			console.error(response);
		},
		load:function(response, ioargs){
			updateStats(dojo.byId("histhour"), response.hour);
			updateStats(dojo.byId("histday"), response.totals);
			var now = new Date();
			updateStats(dojo.byId("histmid"), getStatsSince(new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime() / 1000, response.rawdata));
			var config = tv_config();
			updateAgents(dojo.byId("agroup1"), getAgentsInProfiles(response.agentProfiles, config.agroup1));
			updateAgents(dojo.byId("agroup2"), getAgentsInProfiles(response.agentProfiles, config.agroup2));
			updateAgents(dojo.byId("agroup3"), getAgentsInProfiles(response.agentProfiles, config.agroup3));
			updateAgents(dojo.byId("agroup4"), getAgentsInProfiles(response.agentProfiles, config.agroup4));

			updateQueue(dojo.byId("qgroup1"), getQueue(response.queueGroups, config.qgroup1));
			updateQueue(dojo.byId("qgroup2"), getQueue(response.queueGroups, config.qgroup2));
			updateQueue(dojo.byId("qgroup3"), getQueue(response.queueGroups, config.qgroup3));
			updateQueue(dojo.byId("qgroup4"), getQueue(response.queueGroups, config.qgroup4));
			updateQueue(dojo.byId("qgroup5"), getQueue(response.queueGroups, config.qgroup5));

			agentAlerts(getAgentsInProfiles(response.agentProfiles, "*"));

			if(window.spew){
				console.log(response);
			}
		}
	});
}

function getspecialtext() {
	dojo.xhrGet({url:"text.txt",
		error:function(response, ioargs){
			console.error(response);
		},
		load:function(response,ioargs){
			specialtext = response.split(String.fromCharCode(10));
			if (nextspecialtext >= specialtext.length) {
				nextspecialtext = 0;
			}
		}
	});
}

function swapstuff() {
	if (typeof tv_config == 'function') {
		update();
		swapstat();
		if (specialtext.length === 0) {
			getspecialtext();
		}
		swapalert("agentalerts");
		swapalert("globalalerts");
		setTimeout(swapstuff, 5000);
	} else {
		alert("Please review the tv_config.js.sample file, configure it and rename it to tv_config.js before continuing");
	}
}
//setTimeout("swapstuff();",5000);
setTimeout(resizeall, 10000);
window.onresize=function () { resizeall(); };

