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

queues.setGroup = function(form, reciper, refreshnode){
	var vals = form.getValues();
	if(! vals.name){
		vals.name = vals.oldname;
	}
	vals.recipe = dojo.toJson(reciper.getValue());
	dojo.xhrPost({
		url:"/queues/groups/" + vals.oldname + "/update",
		handleAs:"json",
		content:vals,
		load:function(resp, ioargs){
			if(! resp.success){
				console.log(resp.message);
			}
			else{
				queues.refreshTree(refreshnode);
			}
		}
	});
}

queues.newGroup = function(form, reciper, refreshnode){
	var vals = form.getValues();
	vals.recipe = dojo.toJson(reciper.getValue());
	dojo.xhrPost({
		url:"/queues/groups/new",
		handleAs:"json",
		content:vals,
		load:function(resp, ioargs){
			if(! resp.success){
				console.log(resp.message);
			}
			else{
				queues.refreshTree(refreshnode);
			}
		}
	});
}

queues.fromStoreToObj = function(store){
	var out = [];
	for(var i in store){
		console.log(store[i].arguments);
		if(store[i].arguments.length > 1){
			args = store[i].arguments;
		}
		else{
			args = store[i].arguments[0];
		}
		var protoRecipe = {
			"conditions": [],
			"action": store[i].action[0],
			"arguments": args,
			"runs": store[i].runs[0]
		};
		var conds = store[i].conditions;
		for(var j in conds){
			var protoCondition = {
				"property":conds[j].property[0],
				"comparison":conds[j].comparison[0],
				"value":conds[j].value[0]
			};
			protoRecipe.conditions.push(protoCondition);
		}
		out.push(protoRecipe);
	}
	return out;
}