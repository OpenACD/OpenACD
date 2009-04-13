dojo.provide("spice.skills");

var skills = function(){
	return {};
}

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
})

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
		rootLabel:"Skills",
	});
	dojo.publish("skills/init", []);
}

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
		rootLabel:"Skills",
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
		showRoot: false,
	}, targetnode);
	dojo.publish("skills/tree/refreshed", []);
}

skills.skillSelection = function(targetnode){
	skills.store.fetch({
		query:{type:"group"},
		onComplete:function(groups, query){
			for(var i in groups){
				var optgroup = dojo.doc.createElement('optgroup');
				optgroup.label = groups[i].name[0];
				for(var j in groups[i].skills){
					var option = dojo.doc.createElement('option');
					option.value = groups[i].skills[j].atom[0];
					option.innerHTML = groups[i].skills[j].name[0];
					option.title = groups[i].skills[j].description[0];
					optgroup.appendChild(option);
				}
				targetnode.appendChild(optgroup);
			}
		}
	});
}

skills.newSelection = function(callback, selected, reserved, expand){
	var select = new dijit.form.MultiSelect({});
	var isSelected = function(skill){
		for(var i in selected){
			if(selected[i] == skill){
				return true
			}
		}
		return false
	}
	var isReserved = function(skill){
		for(var i in reserved){
			if(reserved[i] == skill){
				return true;
			}
		}
		return false;
	}
	skills.store.fetch({
		query:{type:"group"},
		onComplete:function(groups, query){
			var select = dojo.doc.createElement('select');
			select.multiple = true;
			for(var i in groups){
				var optgroup = dojo.doc.createElement('optgroup');
				optgroup.label = groups[i].name[0];
				for(var j in groups[i].skills){
					var option = dojo.doc.createElement('option');
					option.value = groups[i].skills[j].atom[0];
					option.innerHTML = groups[i].skills[j].name[0];
					option.title = groups[i].skills[j].description[0];
					option.selected = isSelected(groups[i].skills[j].atom[0]);
					option.disabled = isReserved(groups[i].skills[j].atom[0]);
					optgroup.appendChild(option);
				}
				select.appendChild(optgroup);
			}
			for(var i in expand){
				var appendExpanded = function(expanded){
					var optgroup = dojo.doc.createElement('optgroup');
					optgroup.label = expand[i];
					for(var j in expanded){
					   var option = dojo.doc.createElement('option');
					   var val = '{' + expand[i] + ',' + expanded[j] + '}';
					   option.value = val;
					   option.innerHTML = expanded[j];
					   option.selected = isSelected(val);
					   option.disabled = isReserved(val);
					   optgroup.appendChild(option);
					}
					select.appendChild(optgroup);
				}
				
				skills.expandSkill(appendExpanded, expand[i]);
			}
			
			callback(select);
		}
	});
}

skills.expandSkill = function(callback, magicskill){
	dojo.xhrGet({
		url:"/skills/skill/" + magicskill + "/expand",
		handleAs:"json",
		load:function(response, ioargs){
			callback(response.items);
		}
	});
}

skills.updateGroup = function(submform, node){
	var values = dijit.byId(submform).getValues();
	dojo.xhrPost({
		url:"/skills/groups/" + values.oldname + "/update",
		form:submform,
		handleAs:"json",
		load:function(response, ioargs){
			skills.refreshTree(node);
		},
		error:function(response, ioargs){
			console.log(response.message);
		}
	});
}

skills.updateSkill = function(submform, node){
	var values = dijit.byId(submform).getValues();
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
			console.log(response.message);
		}
	})
}

skills.newSkill = function(submform, node){
	var values = dijit.byId(submform).getValues();
	dojo.xhrPost({
		url:"/skills/skill/new",
		form:submform,
		handleAs:"json",
		load:function(response, ioargs){
			skills.refreshTree(node);
		},
		error:function(response, ioargs){
			console.log(response.message);
		}
	});
}