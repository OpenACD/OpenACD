var skills = function(){
	return {};
}
/*
skills.getSkills = function(callback){
	var geturl = "skills/getskills";
	if(arguments[1]){
		geturl += "/" + arguments[1];
	}
	dojo.xhrGet({
		url:"skills/getskills",
		handleAs:"json",
		load:function(response, ioargs){
			callback(response);
		}
	});
}*/

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