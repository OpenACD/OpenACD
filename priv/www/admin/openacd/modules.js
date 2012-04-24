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
	//queues.init();
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

modules.getNodeStatus = function(domNode){
	dojo.xhrPost({
		url:"modules/status",
		handleAs:"json",
		load:function(res){
			if(res.success){
				if(res.error){
					dojo.byId("nodesErrors").innerHTML = res.error;
				} else {
					dojo.byId("nodesErrors").innerHTML = "";
				}
				var table = dojo.place("<table> <tr> <th>Node</th> <th>Up Since</th> <th>Modules</th> <th>Plugins</th> </tr> </table>", domNode, "last");
				for(var i in res.nodes){
					var tr = "<tr><td>" + i + "</td>";
					if(res.nodes[i].isUp){
						var date = new Date(res.nodes[i].uptime);
						tr += "<td>" + date.toString() + "</td>";
					} else {
						tr += "<td class=\"downNode\">Down</td>";
					}
					var list = "<ul>";
					for(var j in res.nodes[i].modules){
						list += "<li>" + res.nodes[i].modules[j] + "</li>";
					}
					list += "</ul>";
					tr += "<td>" + list + "</td>";
					var plugList = "<ul>";
					for(var k in res.nodes[i].plugins){
						plugList += "<li>" + k + ": ";
						if(res.nodes[i].plugins[k] == "stopped"){
							plugList += "<span class=\"downNode\">stopped</span>";
						} else {
							plugList += "running";
						}
						plugList += "</li>";
					}
					tr += "<td>" + plugList + "</td>";
					tr += "</tr>";
					dojo.place(tr, table, "last");
				}
				
				return true;
			}
			errMessage(["Getting node statuses failed", res.message]);
		},
		error:function(res){
			errMessage(["Getting node statuses errored", res]);
		}
	});
}
