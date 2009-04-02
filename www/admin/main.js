
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

function goober(){
	console.log('goober');
}

var agentsTreeRefreshHandle = dojo.subscribe("agents/tree/refreshed", function(data){
	dojo.connect(agents.tree, "onClick", function(item){
		if(item.type[0] == "profile"){
			dojo.byId("agentProfileOldName").value = item.name[0];
			dijit.byId("agentProfileName").attr("value", item.name[0]);
			if(item.name[0] == "Default"){
				dijit.byId("agentProfileName").setDisabled(true);
			}
			else{
				dijit.byId("agentProfileName").setDisabled(false);
			}
			dijit.byId('agentsMain').selectChild('agentProfileEditor');
			var node = dijit.byId("agentProfileSkills").domNode;
			while(node.hasChildNodes()){
				node.removeChild(node.lastChild);
			}
				 
			var setSkills = function(groups, profileSkills){
				console.log(groups);
				
				skills.skillSelection(dijit.byId("agentProfileSkills").domNode);
				/*
				for(var i in groups){
					var optgroup = dojo.doc.createElement('optgroup');
					optgroup.label = groups[i].name[0];
					for(var j in groups[i].skills){
						var option = dojo.doc.createElement('option');
						option.value = groups[i].skills[j].atom[0];
						//console.log(groups[i].skills[j].atom[0]);
						option.innerHTML = groups[i].skills[j].name[0];
						option.title = groups[i].skills[j].description[0];
						optgroup.appendChild(option);
					}
					dijit.byId("agentProfileSkills").domNode.appendChild(optgroup);
				}*/
				dojo.xhrGet({
					url:"/skills/" + item.name[0],
					handleAs:"json",
					load:function(resp, ioargs){
						var selected = function(needle){
							for(var i in resp.items){
								if(resp.items[i].atom == needle){
									return true;
								}
							};
							return false;
						}
						var reserved = {
							"_queue":true,
							"_agent":true,
							"_node":true,
							"_brand":true
						};
						var sel = dijit.byId("agentProfileSkills").domNode;
						for(var i in sel.childNodes){
							var selkid = sel.childNodes[i];
							for(var j in selkid.childNodes){
								var node = selkid.childNodes[j];
								if(! reserved[node.value]){
									if(selected(node.value)){
										node.selected = true;
									}
									else{
										node.selected = false;
									}
								}
								else{
									node.disabled = true;
								}
							}
						}
					}
				});
			};
			
			skills.store.fetch({
				query:{type:"group"},
				onComplete:setSkills
			});
				 
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
				skills.skillSelection(dijit.byId('agentNewProfileSkills').domNode);
			}
			else{
				dijit.byId("loginpane").show();
			}
		}
	});
});


