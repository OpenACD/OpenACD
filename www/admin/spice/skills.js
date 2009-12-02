dojo.provide("spice.skills");

skills = function(){
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

skills.createSelect = function(callback, selected, hidden, expand){
	var selectNode = document.createElement('select');
	selectNode.multiple = 'true';
	var groupsFetched = function(groups){
		for(var i = 0; i < groups.length; i++){
			var groupname = skills.store.getValue(groups[i], 'name');
			var optgroup = dojo.place('<optgroup>', selectNode);
			optgroup.label = groupname;
			
			var skillItems = skills.store.getValues(groups[i], 'skills');
			for(var j = 0; j < skillItems.length; j++){
				var skillAtom = skills.store.getValue(skillItems[j], 'atom');
				if(inArray(skillAtom, hidden)){
					continue;
				}
				var skillName = skills.store.getValue(skillItems[j], 'name');
				var skillDesc = skills.store.getValue(skillItems[j], 'description');
				/*optionNode.value = skillAtom;
				optionNode.title = skillDesc;
				optionNode.innerHTML = skillName;*/
				var selectedStr = '';
				if(inArray(skillAtom, selected)){
					selectedStr = ' selected="true"';
				}
				var optionNode = dojo.place('<option value="' + skillAtom + '" title="' + skillDesc + '"' + selectedStr + '>' + skillName + '</option>', optgroup);
			}			
		}
	}
	
	skills.store.fetch({
		query:{
			'type':'group'
		},
		onComplete:groupsFetched
	});
	
	for(var ii = 0; ii < expand.length; ii++){
		var thei = ii;
		//var expandLabel = expand[ii];
		var expandCallback = function(expantions, expandLabel){
			//var expandLabel = expand[thei];
			var optgroup = dojo.place('<optgroup label="' + expandLabel + '"></optgroup>', selectNode);
			//optgroup.label = expand[ii];
			for(var j = 0; j < expantions.length; j++){
				var val = '{' + expandLabel + ',' + expantions[j] + '}';
				if(inArray(val, hidden)){
					continue;
				}
				var selectStr = '';
				if(inArray(val, selected)){
					selectStr = ' selected="true"';
				}
				var option = dojo.place('<option value="' + val + '"' + selectStr +  '>' + expantions[j] + '</option>', optgroup);
			}
		}
		
		skills.expandSkill(expandCallback, expand[ii]);
		
		/*
		var xhrCallback = function(res){
			if(res.success && res.items.length){
				var expantions = res.items;
				var optgroup = dojo.place('<optgroup label="' + expandLabel + '"></optgroup>', selectNode);
				//optgroup.label = expand[ii];
				for(var j = 0; j < expantions.length; j++){
					var val = '{' + expandLabel + ',' + expantions[j] + '}';
					if(inArray(val, hidden)){
						continue;
					}
					var selectStr = '';
					if(inArray(val, selected)){
						selectStr = ' selected="true"';
					}
					var option = dojo.place('<option value="' + val + '"' + selectStr +  '>' + expantions[j] + '</option>', optgroup);
				}
			}
		}
		
		dojo.xhrGet({
			url:'/skills/skill/' + expandLabel + '/expand/',
			handleAs:'json',
			load:xhrCallback,
			error:function(res){
				console.log(["expanding skill failed", res]);
			}
		});*/
	}
	
	callback(selectNode);
}

/*
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
		for(var i =0; i < selected.length; i++){
			if(selected[i] == skill){
				return true
			}
		}
		return false
	}
	var isReserved = function(skill){
		for(var i =0; i < reserved.length; i++){
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
			for(var i = 0; i < groups.length; i++){
				var optgroup = dojo.doc.createElement('optgroup');
				optgroup.label = groups[i].name[0];
				for(var j = 0; j < groups[i].skills.length; j++){
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
			for(var i = 0; i < expand.length; i++){
				var appendExpanded = function(expanded){
					var optgroup = dojo.doc.createElement('optgroup');
					optgroup.label = expand[i];
					for(var j =0; j < expanded.length; j++){
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
}*/

skills.expandSkill = function(callback, magicskill){
	dojo.xhrGet({
		url:"/skills/skill/" + magicskill + "/expand",
		handleAs:"json",
		load:function(response, ioargs){
			callback(response.items, magicskill);
		},
		error: function(res){
			errMessage(["expanding skill errored", res]);
			console.log(["expanding skill errored", res]);
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
			errMessage(["skill group update errored", response]);
			console.log(["skill group update errored", response]);
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
			errMessage(["update skill errored", response]);
			console.log(["update skill errored", response]);
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
			errMessage(["create skill errored", response]);
			console.log(["create skill errored", response]);
		}
	});
}