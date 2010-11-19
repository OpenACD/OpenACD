dojo.provide("openacd.modules");

modules = function(){
	return {};
};

modules.store = new dojo.data.ItemFileReadStore({
	data:{
		"identifier":'id',
		"label":'name',
		"items":[]
	}
});

modules.model = new dijit.tree.ForestStoreModel({
	store: modules.store,
	labelAttr: 'name',
	query:{"type":"node"},
	childrenAttrs:["modules"],
	rootId:"nodes",
	rootLabel:'nodes'
});

modules.tree = false;

modules.init = function(){
	modules.store = new dojo.data.ItemFileReadStore({
		url:"/modules/poll"
	});
	modules.store.fetch();
	modules.model = new dijit.tree.ForestStoreModel({
		store: modules.store,
		labelAttr: 'name',
		query:{"type":"node"},
		childrenAttrs:["modules"],
		rootId:"nodes",
		rootLabel:'nodes'
	});
};

modules.refreshTree = function(node){
	var parent = dojo.byId(node).parentNode;
	queues.init();
	if(dijit.byId(modules.tree.id)){
		dijit.byId(modules.tree.id).destroy();
	}
	var n = dojo.doc.createElement('div');
	n.id = node;
	parent.appendChild(n);
	modules.tree = new dijit.Tree({
		store: modules.store,
		model: modules.model,
		showRoot: false
	}, node);
	dojo.publish("modules/tree/refreshed", []);
};

/*medias.setMedia = function(erlnode, mediatype, args, treenode){
	dojo.xhrPost({
		url:"medias/" + erlnode + "/" + mediatype + "/" + "update",
		handleAs:"json",
		content:args,
		load:function(resp, ioargs){
			if(resp.success){
				medias.refreshTree(treenode);
			}
			else{
				errMessage(["Setting media failed", resp.message]);
				//onsole.log(resp.message);
			}
		},
		error:function(res){
			errMessage(["Setting media errored", res]);
			console.warn(["Setting media errored", res]);
		}
	});
};*/
