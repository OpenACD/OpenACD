/* Functions and operations for the queues tab */

dojo.provide("spice.queues");

queues = function(){
	return {}
};

dojo.requireLocalization("admin", "recipeEditor");

queues.recipeConditionsStore = new dojo.data.ItemFileReadStore({
	data:{
		identifier:"value",
		label:"label",
		"items":[
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").TICKINTERVAL,
			"value":"ticks",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").AGENTSAVAILABLE,
			"value":"agents_avail",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPAREEQUAL,
			"value":"=",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPAREGREATERTHAN,
			"value":">",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPARELESSTHAN,
			"value":"<",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPARENOTEQUAL,
			"value":"!=",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").AGENTSELIGIBLE,
			"value":"agents_eligible",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").CALLSINQUEUE,
			"value":"calls_queued",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").POSITIONINQUEUE,
			"value":"queue_position",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").CLIENT,
			"value":"client",
			"type":"property",
			"filter":"any",
			"comparisons":[
				{_reference:"="},
				{_reference:"!="}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").HOUR,
			"value":"hour",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").WEEKDAY,
			"value":"weekday",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:"<"},
				{_reference:">"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").MEDIATYPE,
			"value":"mediatype",
			"type":"property",
			"filter":"any",
			"comparisons":[
				{_reference:"="},
				{_reference:"!="}
			]}
		]
	}
});

queues.store = new dojo.data.ItemFileWriteStore({
	data:{
		"identifier":'name',
		"label":'name',
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
		url:"/queues/groups/get",
		typeMap:{"object":function(obj){ return obj; }}
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
				errMessage(["Setting queue group failed", resp.message]);
				console.log(resp.message);
			}
			else{
				queues.refreshTree(refreshnode);
			}
		},
		error: function(res){
			errMessage(["Setting queue group errored", res]);
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
				errMessage(["Creating queue group failed", resp.message]);
				console.log(resp.message);
			}
			else{
				queues.refreshTree(refreshnode);
			}
		},
		error: function(res){
			errMessage(["Creating queue group errored", res]);
		}
	});
}

queues.fromStoreToObj = function(store){
	var out = [];
	for(var i in store){
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

queues.deleteGroup = function(group, node){
	dojo.xhrGet({
		url:"/queues/groups/" + group + "/delete",
		handleAs:"json",
		load:function(resp, ioargs){
			if(! resp.success){
				errMessage(["Deleting queue group failed", resp.message]);
				console.log(resp.message);
			}
			else{
				queues.refreshTree(node);
			}
		},
		error: function(res){
			errMessage(["Deleting queue group errored", res]);
		}
	});
}

queues.getQueue = function(queue, callback){
	dojo.xhrGet({
		url:"/queues/queue/" + queue + "/get",
		handleAs:"json",
		load:function(resp, ioargs){
			if(resp.success){
				callback(resp.queue);
			}
			else{
				errMessage(["Getting queues failed", resp.message]);
				console.log(resp.message);
			}
		},
		error: function(res){
			errMessage(["Getting queues errored", res]);
			console.log(res);
		}
	});
}

queues.setQueue = function(queue, form, reciper, refreshnode){
	var vals = form.attr('value');
	vals.recipe = dojo.toJson(reciper.getValue());
	vals.skills = form.domNode.skills.getValues();
	var doxhr = function(){
		dojo.xhrPost({
			url:"/queues/queue/" + queue + "/update",
			handleAs:"json",
			content:vals,
			load:function(resp, ioargs){
				if(resp.success){
					queues.refreshTree(refreshnode);
				}
				else{
					errMessage(["queue update failed", resp.message]);
					console.log(["queue update failed", resp.message]);
				}
			},
			error: function(res){
				errMessage(["queue update errored", res]);
				console.log(["queue update errored", res]);
			}
		});
	};
	queues.store.fetchItemByIdentity({
		identity:vals.group,
		onItem:function(i){
			vals.group = queues.store.getValue(i, 'name');
			doxhr();
		}
	});
}

queues.deleteQueue = function(queue, refreshnode){
	dojo.xhrGet({
		url:"/queues/queue/" + queue + "/delete",
		handleAs:"json",
		load:function(resp, ioargs){
			if(resp.success){
				queues.refreshTree(refreshnode);
			}
			else{
				errMessage(["Queue delete failed", resp.message]);
				console.log(resp.message);
			}
		},
		error: function(res){
			errMessage(["queue delete errored", res]);
			console.log(res);
		}
	});
}

queues.newQueue = function(form, reciper, node){
	var vals = form.getValues();
	vals.recipe = dojo.toJson(reciper.getValue());
	vals.skills = form.domNode.skills.getValues();
	var doxhr = function(){
		dojo.xhrPost({
			url:"/queues/queue/new",
			handleAs:"json",
			content:vals,
			load:function(resp, ioargs){
				if(resp.success){
					queues.refreshTree(node);
				}
				else{
					errMessage(["new queue failed", resp.message]);
					console.log(["new queue failed", resp.message]);
				}
			},
			error: function(res){
				errMessage(["new queue errored", res]);
				console.log(["new queue errored", res]);
			}
		});
	};
	queues.store.fetchItemByIdentity({
		identity:vals.group,
		onItem:function(i){
			vals.group = queues.store.getValue(i, 'name');
			doxhr();
		}
	});
}
