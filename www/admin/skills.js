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
		url:"/skills",
	});
	skills.store.fetch();
	skills.model = new dijit.tree.ForestStoreModel({
		store:skills.store,
		labelAttr: "name",
		query:{"type":"group"},
		childrenAttrs:"skills",
		rootId:"skills",
		rootLabel:"Skills"
	});
}

skills.refreshTree = function(targetnode){
	skills.store.fetch();
	if(dijit.byId(skills.tree.id)){
		dijit.byId(skills.tree.id).destroy();
	}
	skills.tree = new dijit.Tree({
		store: skills.store,
		model: skills.model,
		showRoot: false,
	}, targetnode);
}

skills.getSkills = function(profile, callback){
	dojo.xhrGet({
		url:"/skills/" + profile,
		handleAs:"json",
		load:callback
	})
}