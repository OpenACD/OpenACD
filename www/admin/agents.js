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

