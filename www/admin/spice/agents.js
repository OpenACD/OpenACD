/* Functions and operations for the agents tab */

dojo.provide("spice.agents");

agents = function(){
	return {}
}

agents.updateModule = function(subform){
	dojo.xhrPost({
		url:"agents/modules/update",
		handleAs:"json",
		form:subform,
		error:function(response, ioargs){
			errMessage(["update module errored", response]);
			console.log(response);
		},
		load:function(response, ioargs){
			console.log(response);
		}
	})
}

agents.getModules = function(targetform){
	dojo.xhrGet({
		url:"agents/modules/get",
		handleAs:"json",
		load:function(response, ioargs){
			targetform.attr('value', response.result);
			var kids = targetform.getDescendants();
			for(var i in kids){
				if(kids[i].id == 'agentModuleTCPListenEnabled'){
					kids[i].attr('checked', response.result.agentModuleTCPListenEnabled);
				}
				if(kids[i].id == 'agentModuleWebListenEnabled'){
					kids[i].attr('checked', response.result.agentModuleWebListenEnabled);
				}
			}
		}
	})
}

agents.getSpiceIntegration = function(targetform){
	dojo.xhrGet({
		url:"agents/spiceintegration/get",
		handleAs:"json",
		load:function(response){
			targetform.attr('value', response.result);
			var kids = targetform.getDescendants();
			for(var i in kids){
				if(kids[i].id == 'spiceIntegrationEnabled'){
					kids[i].attr('checked', response.result.spiceIntegrationEnabled);
				}
			}
		}
	});
}

agents.setSpiceIntegration = function(subform){
	dojo.xhrPost({
		url:"agents/spiceintegration/set",
		handleAs:"json",
		form:subform,
		error:function(response){
			errMessage(["setting spicecsm integration errored", response]);
			console.log(["error setting spice integration", response]);
		},
		load:function(res){
			if(! res.success){
				errMessage(["Spice Integration Failed", res.message]);
			}
		}
	});
}

agents.store = new dojo.data.ItemFileReadStore({
	data:{
		"items":[]
	}
});

agents.model = new dijit.tree.ForestStoreModel({
	store: agents.store,
	labelAttr:"name",
	query:{"type":"profile"},
	childrenAttrs:["agents"],
	rootId:"agents",
	rootLabel:"Agents"
});

agents.init = function(){
	agents.store = new dojo.data.ItemFileWriteStore({
		url:"/agents/profiles/get"
	});
	agents.store.fetch();
	agents.model = new dijit.tree.ForestStoreModel({
		store: agents.store,
		labelAttr:"name",
		query:{"type":"profile"},
		childrenAttrs:["agents"],
		rootId:"agents",
		rootLabel:"Agents",
		
	});
}

agents.tree = false;

agents.refreshTree = function(targetnode){
	var parent = dojo.byId(targetnode).parentNode;
	//agents.store.fetch();
	agents.init();
	if(dijit.byId(agents.tree.id)){
		dijit.byId(agents.tree.id).destroy();
	};
	var n = dojo.doc.createElement('div');
	n.id = targetnode;
	parent.appendChild(n);
	agents.tree = new dijit.Tree({
		store: agents.store,
		model: agents.model,
		showRoot:false
	}, targetnode);
	dojo.publish("agents/tree/refreshed", []);
}

agents.updateProfile = function(submitForm, treenode){
	var values = dijit.byId(submitForm).getValues();
	var xhrurl = "/agents/profiles/" + values.oldname + "/update";
	dojo.xhrPost({
		url:xhrurl,
		handleAs:"json",
		form:submitForm,
		load:function(response, ioargs){
			agents.refreshTree(treenode);
		}
	});
}

agents.newProfile = function(submitForm, treenode){
	dojo.xhrPost({
		url:"/agents/profiles/new",
		handleAs:"json",
		form:submitForm,
		load:function(response, ioargs){
			agents.refreshTree(treenode);
		}
	})
}

agents.updateAgent = function(subform, node){
	var values = dijit.byId(subform).getValues();
	agents.store.fetchItemByIdentity({
		identity:values.profile,
		onItem:function(item, req){
			values.profile = agents.store.getValue(item, 'name');
		}
	});
	dojo.xhrPost({
		url:"/agents/agents/" + values.oldlogin + "/update",
		handleAs:"json",
		content:values,
		error:function(response, ioargs){
			console.log(response);
		},
		load:function(response, ioargs){
			agents.refreshTree(node);
		}
	})
}

agents.newAgent = function(subform, node){
	var values = dijit.byId(subform).getValues();
	agents.store.fetchItemByIdentity({
		identity:values.profile,
		onItem:function(item, req){
			values.profile = item.name[0];
		}
	});
	dojo.xhrPost({
		url:"/agents/agents/new",
		handleAs:"json",
		content:values,
		error:function(response, ioargs){
			errMessage(["New agent errored", response]);
			console.log(response);
		},
		load:function(response, ioargs){
			if(! response.success){
				errMessage(["New agent failed", response.message]);
			}
			else{
				agents.refreshTree(node);
			}
		}
	})
}

agents.getSkills = function(profile, callback){
	dojo.xhrGet({
		url:"/skills/" + profile,
		handleAs:"json",
		load:callback
	})
}
