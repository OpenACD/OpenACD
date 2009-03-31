/* Functions and operations for the agents tab */

var agents = function(){
	return {}
}

agents.store = new dojo.data.ItemFileReadStore({
	url:"/agents"
});

agents.model = new dijit.tree.ForestStoreModel({
	store: agents.store,
	labelAttr:"name",
	query:{"type":"profile"},
	childrenAttrs:["agents"],
	rootId:"agents",
	rootLabel:"Agents"
});

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