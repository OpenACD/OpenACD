dojo.provide("spice.clients");

clients = function(){
	return {};
}

clients.store = null;

clients.init = function(){
	clients.store = new dojo.data.ItemFileWriteStore({
		url:"/clients/getClients",
		hierarchical:false,
		typeMap:{
			"json":function(obj){
				return obj
			}
		}
	});
	clients.store._saveCustom = function(savecomplete){
		var changeset = mailMappings._pending;
		var updates = [];
		for(var i in changeset._modifiedItems){
			var item = null;
			if(mailMappings._itemsByIdentity){
				item = mailMappings._itemsByIdentity[i];
			}
			else{
				item = mailMappings._arrayOfAllItems[i];
			}
			updates.push(item);
		}
		savecomplete();
	}
	dijit.byId('clientsGrid')._setStore(clients.store);
	dijit.byId('clientsGrid')._refresh();
}	

clients.setDefault = function(inconf){
	var defaultConf = {
		url_pop: "http://www.google.com/#q=#{label}"
	};
	
	var conf = dojo.mixin(defaultConf, inconf);
		
	dojo.xhrPost({
		url:"/clients/setDefault",
		handleAs:'json',
		content:conf,
		load:function(res){
			if(! res.success){
				console.log(["client default set failure", res]);
				return;
			}
			
			clients.store.fetch({
				query:{
					label:""
				},
				onLoad:function(items){
					var item = items[0];
					for(var i in conf){
						clients.store.setValue(item, i, conf[i]);
					}
					clients.store.save();
				}
			});
		},
		error:function(res){
			console.log(["client default set error", res])
		}
	});
}