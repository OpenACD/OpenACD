/* Functions and operations for the agents tab */

dojo.provide("openacd.agents");

agents = function(){
	return {};
};

agents.updateModule = function(subform){
	dojo.xhrPost({
		url:"agents/modules/update",
		handleAs:"json",
		form:subform,
		error:function(response, ioargs){
			errMessage(["update module errored", response]);
			console.warn(response);
		},
		load:function(response, ioargs){
			//onsole.log(response);
		}
	});
};

agents.getModules = function(targetform){
	if(!!targetform == false){
		return;
	}
	dojo.xhrGet({
		url:"agents/modules/get",
		handleAs:"json",
		load:function(response, ioargs){
			var kids = targetform.getDescendants();
			for(var i = 0; i < kids.length; i++){
				if(kids[i].id.match(/Enabled$/)){
					kids[i].set('checked', response.result[kids[i].id]);
				} else {
					kids[i].set('value', response.result[kids[i].id]);
				}
			}
		}
	});
};

/*agents.store = new dojo.data.ItemFileReadStore({
	data:{
		"items":[]
	}
});*/
agents.store = new dojo.store.Memory({data:[]});

agents.model = new dijit.tree.ForestStoreModel({
	store: new dojo.data.ObjectStore({objectStore:agents.store}),
	labelAttr:"name",
	query:{"type":"profile"},
	childrenAttrs:["agents"],
	rootId:"agents",
	rootLabel:"Agents"
});

agents.init = function(){
	return true;
}

/*
	dojo.xhrGet({
		url:'/agents/profiles/get',
		handleAs:'json',
		load:function(res){
			if(res.success != true){
				return;
			}
			agents.store = new dojo.store.Memory({data:res.items});
			
	/*agents.store = new dojo.data.ItemFileWriteStore({
		url:"/agents/profiles/get"
	});
	agents.store.fetch();
	agents.store.query = function(){
		agents.store.fetch.call(arguments);
	};*/
/*	agents.model = new dijit.tree.ForestStoreModel({
		store: new dojo.data.ObjectStore({objectStore:agents.store}),
		labelAttr:"name",
		query:{"type":"profile"},
		childrenAttrs:["agents"],
		rootId:"agents",
		rootLabel:"Agents"
	});
	}});
};*/

agents.tree = false;

agents.refreshTree = function(targetnode){
	var parent = dojo.byId(targetnode).parentNode;
	//agents.store.fetch();
	//agents.init();
	dojo.xhrGet({
		url:'/agents/profiles/get',
		handleAs:'json',
		load:function(res){
			if(res.success != true){
				return;
			}
			agents.store = new dojo.store.Memory({data:res.items});
			agents.model = new dijit.tree.ForestStoreModel({
				store:new dojo.data.ObjectStore({objectStore:agents.store}),
				labelAttr:"name",
				query:{"type":"profile"},
				childrenAttrs:["agents"],
				rootId:"agents",
				rootLabel:"Agents"
			});
			if(dijit.byId(agents.tree.id)){
				dijit.byId(agents.tree.id).destroy();
			}
			var n = dojo.doc.createElement('div');
			n.id = targetnode;
			parent.appendChild(n);
			agents.tree = new dijit.Tree({
				store: new dojo.data.ObjectStore({objectStore:agents.store}),
				model: agents.model,
				showRoot:false
			}, targetnode);
			dojo.publish("agents/tree/refreshed", []);
		}
	});
};

agents.updateProfile = function(submitForm, treenode){
	var values = dijit.byId(submitForm).get('value');
	values.skills = dijit.byId(submitForm).domNode.skills.getValues();
	var xhrurl = "/agents/profiles/" + values.oldname + "/update";
	dojo.xhrPost({
		url:xhrurl,
		handleAs:"json",
		content:values,
		load:function(response, ioargs){
			agents.refreshTree(treenode);
		}
	});
};

agents.newProfile = function(submitForm, treenode){
	var values = dijit.byId(submitForm).get('value');
	values.skills = dijit.byId(submitForm).domNode.skills.getValues();
	dojo.xhrPost({
		url:"/agents/profiles/new",
		handleAs:"json",
		content:values,
		load:function(response, ioargs){
			agents.refreshTree(treenode);
		}
	});
};

agents.updateAgent = function(subform, node){
	var values = dijit.byId(subform).get('value');
	values.skills = dijit.byId(subform).domNode.skills.getValues();
	values.profile = agents.store.query({'id':values.profile})[0].name;
	dojo.xhrPost({
		url:"/agents/agents/" + values.agentId + "/update",
		handleAs:"json",
		content:values,
		error:function(response, ioargs){
			console.warn(response);
		},
		load:function(response, ioargs){
			agents.refreshTree(node);
		}
	});
};

agents.newAgent = function(subform, node){
	var values = dijit.byId(subform).get('value');
	values.skills = dijit.byId(subform).domNode.skills.getValues();
	values.profile = agents.store.query({'id':values.profile})[0].name;
	dojo.xhrPost({
		url:"/agents/agents/new",
		handleAs:"json",
		content:values,
		error:function(response, ioargs){
			errMessage(["New agent errored", response]);
			console.warn(response);
		},
		load:function(response, ioargs){
			if(! response.success){
				errMessage(["New agent failed", response.message]);
			}
			else{
				agents.refreshTree(node);
			}
		}
	});
};

agents.getSkills = function(profile, callback){
	dojo.xhrGet({
		url:"/skills/" + profile,
		handleAs:"json",
		load:callback
	});
};
