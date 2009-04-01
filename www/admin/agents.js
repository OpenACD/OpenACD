/* Functions and operations for the agents tab */

var agents = function(){
	return {}
}

agents.updateModule = function(subform){
	dojo.xhrPost({
		url:"agents/editmodules",
		handleAs:"json",
		form:subform,
		error:function(response, ioargs){
			console.log("agent_auth module update failed");
			console.log(response);
		},
		load:function(response, ioargs){
			console.log("agent_auth module update successful");
			console.log(response);
		}
	})
}

agents.getModules = function(targetform){
	dojo.xhrGet({
		url:"agents/getmodules",
		handleAs:"json",
		load:function(response, ioargs){
			console.log(response);
			targetform.setValues(response.result);
		}
	})
}

agents.getProfile = function(profile, targetform){
	var setAvailSkills = function(skilljson){
		console.log('hi');
	}
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
	console.log("agent.init");
	agents.store = new dojo.data.ItemFileWriteStore({
		url:"/agents"
	});
	agents.store.fetch();
	agents.model = new dijit.tree.ForestStoreModel({
		store: agents.store,
		labelAttr:"name",
		query:{"type":"profile"},
		childrenAttrs:["agents"],
		rootId:"agents",
		rootLabel:"Agents"
	});
}

agents.tree = false;

agents.refreshTree = function(targetnode){
	agents.store.fetch();
	if(dijit.byId(agents.tree.id)){
		dijit.byId(agents.tree.id).destroy();
	}
	agents.tree = new dijit.Tree({
		store: agents.store,
		model: agents.model,
		showRoot:false
	}, targetnode);
	dojo.publish("agents/tree/refreshed", []);
}
