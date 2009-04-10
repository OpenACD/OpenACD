/* Functions and operations for the queues tab */

dojo.provide("spice.queues");

var queues = function(){
	return {}
};

queues.recipeConditionsStore = new dojo.data.ItemFileReadStore({
	data:{
		identifier:"value",
		label:"label",
		"items":[
			{"label":"Tick interval",
			"value":"ticks",
			"type":"property",
			"regExp":"[\\d]+",
			"comparisons":[
				{_reference:"="}
			]},
			{"label":"Agents Available",
			"value":"agents_avail",
			"type":"property",
			"regExp":"[\\d]+",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":"=",
			"value":"=",
			"type":"comparison"},
			{"label":">",
			"value":">",
			"type":"comparison"},
			{"label":"<",
			"value":"<",
			"type":"comparison"},
			{"label":"Agents Eligible",
			"value":"agents_eligible",
			"type":"property",
			"regExp":"[\\d]+",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":"Calls in Queue",
			"value":"call_count",
			"type":"property",
			"regExp":"[\\d]+",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":"Position in Queue",
			"value":"queue_position",
			"type":"property",
			"regExp":"[\\d]+",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]}
		]
	}
});
/*
queues.recipeBuilderStore = new dojo.data.ItemFileReadStore({
	data:{
		"items":[
			{"label":"ticks",
			"type":"property",
			"valueType":"number",
			"comparisons":[
				{"label":"=",
				"type":"comparison"}
			]},
			{"label":"Agents Available",
			"type":"property",
			"valueType":"number",
			"comparisons":[
				{"label":">",
				"type":"comparison"},
				{"label":"=",
				"type":"comparison"},
				{"label":"<",
				"type":"comparison"}
			]}
		]
	}
});*/

queues.store = new dojo.data.ItemFileWriteStore({
	data:{
		"items":[]
	}
});

queues.model = new dijit.tree.ForestStoreModel({
	store: queues.store,
	labelAttr:"name",
	query:{"type":"group"},
	childrenAttrs:["queues"],
	rootId:"queues",
	rootLabel:"Queues"
});

queues.tree = false;

queues.init = function(){
	queues.store = new dojo.data.ItemFileWriteStore({
		url:"/queues/groups/get"
	});
	queues.store.fetch();
	queues.model = new dijit.tree.ForestStoreModel({
		store: queues.store,
		labelAttr:"name",
		query:{"type":"group"},
		childrenAttrs:["queues"],
		rootId:"queues",
		rootLabel:"queues"
	});
}

queues.refreshTree = function(node){
	var parent = dojo.byId(node).parentNode;
	queues.init();
	if(dijit.byId(queues.tree.id)){
		dijit.byId(queues.tree.id).destroy();
	}
	var n = dojo.doc.createElement('div');
	n.id = node;
	parent.appendChild(n);
	queues.tree = new dijit.Tree({
		store: queues.store,
		model: queues.model,
		showRoot:false
	}, node);
	dojo.publish("queues/tree/refreshed", []);
}