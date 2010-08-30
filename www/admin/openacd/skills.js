dojo.provide("openacd.skills");

skills = function(){
	return {};
};

skills.store = new dojo.data.ItemFileReadStore({
	data:{
		"items":[]
	}
});

skills.model = new dijit.tree.ForestStoreModel({
	store:skills.store,
	labelAttr:"name",
	query:{"type":"group"},
	childrenAttrs:"skills",
	rootId:"skills",
	rootLabel:"Skills"
});

skills.tree = false;

skills.init = function(){
	skills.store = new dojo.data.ItemFileWriteStore({
		url:"/skills/groups/get"
	});
	skills.store.fetch();
	skills.model = new dijit.tree.ForestStoreModel({
		store:skills.store,
		labelAttr: "name",
		query:{"type":"group"},
		childrenAttrs:["skills"],
		rootId:"skills",
		rootLabel:"Skills"
	});
	dojo.publish("skills/init", []);
};

skills.refreshTree = function(targetnode){
	var parent = dojo.byId(targetnode).parentNode;
	skills.store = new dojo.data.ItemFileWriteStore({
		url:"/skills/groups/get"
	});
	skills.store.fetch();
	skills.model = new dijit.tree.ForestStoreModel({
		store:skills.store,
		labelAttr: "name",
		query:{"type":"group"},
		childrenAttrs:["skills"],
		rootId:"skills",
		rootLabel:"Skills"
	});
	if(dijit.byId(skills.tree.id)){
		dijit.byId(skills.tree.id).destroy();
	}
	var n = dojo.doc.createElement('div');
	n.id = targetnode;
	parent.appendChild(n);
	skills.tree = new dijit.Tree({
		store: skills.store,
		model: skills.model,
		showRoot: false
	}, targetnode);
	dojo.publish("skills/tree/refreshed", []);
};

skills.createSelect = function(callback, selected, hidden, expand){
	var selectNode = document.createElement('select');
	selectNode.multiple = 'true';
	selectNode.getValues = function(){
		var nodelist = dojo.query('> optgroup > option', selectNode);
		var out = [];
		for(var i =0; i < nodelist.length; i++){
			if(nodelist[i].selected){
				out.push(nodelist[i].value);
			}
		}
		return out;
	};
	var groupsFetched = function(groups){
		for(var i = 0; i < groups.length; i++){
			var groupname = skills.store.getValue(groups[i], 'name');
			var optgroup = document.createElement('optgroup');
			optgroup.label = groupname;
			dojo.place(optgroup, selectNode);
			
			var skillItems = skills.store.getValues(groups[i], 'skills');
			for(var j = 0; j < skillItems.length; j++){
				var skillAtom = skills.store.getValue(skillItems[j], 'atom');
				if(inArray(skillAtom, hidden)){
					continue;
				}
				var skillName = skills.store.getValue(skillItems[j], 'name');
				var skillDesc = skills.store.getValue(skillItems[j], 'description');
				var selectedStr = '';
				var optionNode = document.createElement('option');
				optionNode.value = skillAtom;
				optionNode.title = skillDesc;
				optionNode.innerHTML = skillName;
				if(inArray(skillAtom, selected)){
					optionNode.selected = true;
				}
				dojo.place(optionNode, optgroup);

			}			
		}
	};
	
	skills.store.fetch({
		query:{
			'type':'group'
		},
		onComplete:groupsFetched
	});
	
	var expandCallback = function(expantions, expandLabel){
		//var expandLabel = expand[thei];
		var optgroup = document.createElement('optgroup');
		optgroup.label = expandLabel;
		dojo.place(optgroup, selectNode);
		//optgroup.label = expand[ii];
		for(var j = 0; j < expantions.length; j++){
			var val = '{' + expandLabel + ',' + expantions[j] + '}';
			if(inArray(val, hidden)){
				continue;
			}
			var selectStr = '';
			var optionNode = document.createElement('option');
			optionNode.value = val;
			optionNode.innerHTML = expantions[j];
			if(inArray(val, selected)){
				optionNode.selected = true;
			}
			var option = dojo.place(optionNode, optgroup);
		}
	};
	
	for(var ii = 0; ii < expand.length; ii++){		
		skills.expandSkill(expandCallback, expand[ii]);
	}
	
	callback(selectNode);
};

skills.expandSkill = function(callback, magicskill){
	dojo.xhrGet({
		url:"/skills/skill/" + magicskill + "/expand",
		handleAs:"json",
		load:function(response, ioargs){
			callback(response.items, magicskill);
		},
		error: function(res){
			errMessage(["expanding skill errored", res]);
			console.warn(["expanding skill errored", res]);
		}
	});
};

skills.updateGroup = function(submform, node){
	var values = dijit.byId(submform).get('value');
	dojo.xhrPost({
		url:"/skills/groups/" + values.oldname + "/update",
		form:submform,
		handleAs:"json",
		load:function(response, ioargs){
			skills.refreshTree(node);
		},
		error:function(response, ioargs){
			errMessage(["skill group update errored", response]);
			console.warn(["skill group update errored", response]);
		}
	});
};

skills.updateSkill = function(submform, node){
	var values = dijit.byId(submform).get('value');
	var atom = dijit.byId('skillAtom').getValue();
	dojo.xhrPost({
		url:"/skills/skill/" + atom + "/update",
		form:submform,
		content:{
			'atom':atom
		},
		handleAs:"json",
		load:function(response, ioargs){
			skills.refreshTree(node);
		},
		error:function(response, ioargs){
			errMessage(["update skill errored", response]);
			console.warn(["update skill errored", response]);
		}
	});
};

skills.newSkill = function(submform, node){
	var values = dijit.byId(submform).get('value');
	dojo.xhrPost({
		url:"/skills/skill/new",
		form:submform,
		handleAs:"json",
		load:function(response, ioargs){
			skills.refreshTree(node);
		},
		error:function(response, ioargs){
			errMessage(["create skill errored", response]);
			console.warn(["create skill errored", response]);
		}
	});
};
