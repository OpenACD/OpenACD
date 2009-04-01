
currenttab = undefined;

function switchtab(tab) {
	if (currenttab != tab)
		console.log("switched tab to "+tab);
	currenttab = tab;
}

function inspect(obj){
	console.log(obj);
	for(var i in obj){
		if(typeof(obj[i]) == "function"){
			console.log("  " + i + ": function()");
		}
		else{
			console.log("  " + i + ": " + obj[i])
		}
	}
}

var agentsTreeRefreshHandle = dojo.subscribe("agents/tree/refreshed", function(data){
	dojo.connect(agents.tree, "onClick", function(i){
		console.log(i);
		if(i.type == "profile"){
			dijit.byId('agentsMain').selectChild('agentProfileEditor');
		}
		else{
			dijit.byId('agentsMain').selectChild('agentEditor');
		}
	});
});

dojo.addOnLoad(function(){
	var loginform = dijit.byId("loginform")
	dojo.connect(loginform, "onSubmit", function(e){
		e.preventDefault();
		if (loginform.isValid()){
			dojo.xhrGet({
				url:"/getsalt",
				handleAs:"json",
				error:function(response, ioargs){
					dojo.byId("loginerrp").style.display = "block";
					dojo.byId("loginerrspan").innerHTML = response.responseText;
				},
				load:function(response, ioargs){
					salt = response.salt;
					attrs = loginform.attr("value");
					md5pass = dojox.encoding.digests.MD5(attrs.password, dojox.encoding.digests.outputTypes.Hex);
					salted = dojox.encoding.digests.MD5(salt + md5pass, dojox.encoding.digests.outputTypes.Hex);
					values = attrs;
					values.password = salted;
					dojo.xhrPost({
						url:"/login",
						handleAs:"json",
						content:values,
						load:function(response2, ioargs2){
							if(response2.success){
								dijit.byId("loginpane").hide();
								dojo.byId("main").style.display="block";
								dojo.byId("main").style.visibility = "visible";
								agents.init();
								agents.refreshTree("agentsList");
							}
							else{
								dojo.byId("loginerrp").style.display = "block";
								dojo.byId("loginerrspan").innerHTML = response2.message;
							}
						}
					});
				}
			});
		}
	});

//	var theForm = dijit.byId("editSkillPane");
	// another dojo.connect syntax: call a function directly	
//	dojo.connect(theForm,"onsubmit",setSkill);
	
	dojo.xhrGet({
		url:"/checkcookie",
		handleAs:"json",
		error:function(response, ioargs){
			console.log("checkcookie failed!");
			console.log(response);
		},
		load:function(response, ioargs){
			if(response.success){
				dojo.byId("main").style.display="block";
				dojo.byId("main").style.visibility = "visible";
				agents.init();
				agents.refreshTree("agentsList");
				skills.init();
			}
			else{
				dijit.byId("loginpane").show();
			}
		}
	});
});


