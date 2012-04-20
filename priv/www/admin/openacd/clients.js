dojo.provide("openacd.clients");

clients = function(){
	return {};
};

clients.store = null;

clients.init = function(){
	clients.store = new dojo.data.ItemFileWriteStore({
		url:"/clients/getClients",
		hierarchical:false,
		typeMap:{
			"json":function(obj){
				return obj;
			}
		}
	});
	clients.store._saveCustom = function(savecomplete){
		var changeset = clients.store._pending;
		var updates = [];
		for(var i in changeset._modifiedItems){
			var item = null;
			if(clients.store._itemsByIdentity){
				item = clients.store._itemsByIdentity[i];
			}
			else{
				item = clients.store._arrayOfAllItems[i];
			}
			updates.push(item);
		}
		savecomplete();
	};
	dijit.byId('clientsEditableGrid')._setStore(clients.store);
	dijit.byId('clientsEditableGrid')._refresh();
	dijit.byId('clientsStaticGrid')._setStore(clients.store);
	dijit.byId('clientsStaticGrid')._refresh();
};

clients.gridHackFilter = function(){
	//onsole.log(arguments);
	return true;
};

clients.setDefault = function(inconf){
	var defaultConf = {
		url_pop: "http://www.google.com/#q=#{label}",
		autowrapup: 0,
		ringout: 60
	};
	
	var conf = dojo.mixin(defaultConf, inconf);
		
	dojo.xhrPost({
		url:"/clients/setDefault",
		handleAs:'json',
		content:conf,
		load:function(res){
			if(! res.success){
				errMessage(["client default set failure", res.message]);
				console.warn(["client default set failure", res]);
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
			errMessage(["client default set error", res]);
			console.warn(["client default set error", res]);
		}
	});
};
