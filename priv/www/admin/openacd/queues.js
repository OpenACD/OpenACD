/* Functions and operations for the queues tab */

dojo.provide("openacd.queues");

queues = function(){
	return {};
};

dojo.requireLocalization("admin", "recipeEditor");

queues.store = new dojo.store.Memory({data:[]});
/*queues.store = new dojo.data.ItemFileWriteStore({
	data:{
		"identifier":'name',
		"label":'name',
		"items":[]
	}
});*/

queues.model = new dijit.tree.ForestStoreModel({
	store: new dojo.data.ObjectStore({objectStore:queues.store}),
	labelAttr:"name",
	query:{"type":"group"},
	childrenAttrs:["queues"],
	rootId:"queues",
	rootLabel:"Queues"
});

queues.tree = false;

/*queues.init = function(){
	dojo.xhrGet({
		url:'/queues/groups/get',
		handleAs:'json',
		load:function(res){
			if(res.success != true){
				return;
			}
			queues.store = new dojo.store.Memory({data:res.items});
			queues.model = new dijit.tree.ForestStoreModel({
				store:new dojo.data.ObjectStore({objectStore:queues.store}),
				labelAttr:"name",
				query:{"type":"group"},
				childrenAttrs:["queues"],
				rootId:"queues",
				rootLabel:"queues"
			});
		}
	});
}*/

queues.refreshTree = function(node){
	var parentNode = dojo.byId(node).parentNode;
	dojo.xhrGet({
		url:'/queues/groups/get',
		handleAs:'json',
		load:function(res){
			if(res.success != true){
				return;
			}
			if(dijit.byId(queues.tree.id)){
				dijit.byId(queues.tree.id).destroy();
			}
			queues.store = new dojo.store.Memory({data:res.items});
			queues.model = new dijit.tree.ForestStoreModel({
				store:new dojo.data.ObjectStore({objectStore:queues.store}),
				labelAttr:"name",
				query:{"type":"group"},
				childrenAttrs:["queues"],
				rootId:"queues",
				rootLabel:"queues"
			});
			queues.tree = new dijit.Tree({
				store: new dojo.data.ObjectStore({objectStore:queues.store}),
				model: queues.model,
				showRoot:false
			}, dojo.byId(node));
			dojo.publish("queues/tree/refreshed", []);
			var n = dojo.doc.createElement('div');
			n.id = node;
			parentNode.appendChild(n);
		}
	});
};

queues.setGroup = function(vals, refreshnode){
	dojo.xhrPost({
		url:"/queues/groups/" + vals.oldname + "/update",
		handleAs:"json",
		content:vals,
		load:function(resp, ioargs){
			if(! resp.success){
				errMessage(["Setting queue group failed", resp.message]);
				console.warn(resp.message);
			}
			else{
				queues.refreshTree(refreshnode);
			}
		},
		error: function(res){
			errMessage(["Setting queue group errored", res]);
		}
	});
};

queues.newGroup = function(form, reciper, refreshnode){
	var vals = form.get('value');
	vals.recipe = dojo.toJson(reciper.getValue());
	dojo.xhrPost({
		url:"/queues/groups/new",
		handleAs:"json",
		content:vals,
		load:function(resp, ioargs){
			if(! resp.success){
				errMessage(["Creating queue group failed", resp.message]);
				console.warn(resp.message);
			}
			else{
				queues.refreshTree(refreshnode);
			}
		},
		error: function(res){
			errMessage(["Creating queue group errored", res]);
		}
	});
};

/* TODO This function shouldn't be needed.  Anything that needs it should be
redone either:
a:  pulling the data using store.getValue(item, 'value'), or
b:  using a type map to avoid changing the object to a store data struct.*/
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
};

queues.deleteGroup = function(group, node){
	dojo.xhrGet({
		url:"/queues/groups/" + group + "/delete",
		handleAs:"json",
		load:function(resp, ioargs){
			if(! resp.success){
				errMessage(["Deleting queue group failed", resp.message]);
				console.warn(resp.message);
			}
			else{
				queues.refreshTree(node);
			}
		},
		error: function(res){
			errMessage(["Deleting queue group errored", res]);
		}
	});
};

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
				console.warn(resp.message);
			}
		},
		error: function(res){
			errMessage(["Getting queues errored", res]);
			console.warn(res);
		}
	});
};

queues.setQueue = function(queue, form, reciper, refreshnode){
	var vals = form.get('value');
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
					console.warn(["queue update failed", resp.message]);
				}
			},
			error: function(res){
				errMessage(["queue update errored", res]);
				console.warn(["queue update errored", res]);
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
};

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
				console.warn(resp.message);
			}
		},
		error: function(res){
			errMessage(["queue delete errored", res]);
			console.warn(res);
		}
	});
};

queues.newQueue = function(form, reciper, node){
	var vals = form.get('value');
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
					console.warn(["new queue failed", resp.message]);
				}
			},
			error: function(res){
				errMessage(["new queue errored", res]);
				console.warn(["new queue errored", res]);
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
};
