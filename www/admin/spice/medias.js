dojo.provide("spice.medias");

var medias = function(){
	return {};
}

medias.store = new dojo.data.ItemFileReadStore({
	data:{
		"identifier":'name',
		"label":'name',
		"items":[]
	}
});

medias.model = new dijit.tree.ForestStoreModel({
	store: medias.store,
	labelAttr: 'name',
	query:{"type":"node"},
	childrenAttrs:["medias"],
	rootId:"nodes",
	rootLabel:'nodes'
});

medias.tree = false;

medias.init = function(){
	medias.store = new dojo.data.ItemFileReadStore({
		url:"/medias/poll"
	});
	medias.store.fetch();
	medias.model = new dijit.tree.ForestStoreModel({
		store: medias.store,
		labelAttr: 'name',
		query:{"type":"node"},
		childrenAttrs:["medias"],
		rootId:"nodes",
		rootLabel:'nodes'
	});
}

medias.refreshTree = function(node){
	var parent = dojo.byId(node).parentNode;
	queues.init();
	if(dijit.byId(medias.tree.id)){
		dijit.byId(medias.tree.id).destroy();
	}
	var n = dojo.doc.createElement('div');
	n.id = node;
	parent.appendChild(n);
	medias.tree = new dijit.Tree({
		store: medias.store,
		model: medias.model,
		showRoot: false
	}, node);
	dojo.publish("medias/tree/refreshed", []);
}
