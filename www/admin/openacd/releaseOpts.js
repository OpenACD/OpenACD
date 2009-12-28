dojo.provide("openacd.releaseOpts");

releaseOpts = function(){};

releaseOpts.store = releaseOpts.store = new dojo.data.ItemFileWriteStore({
	url:"/release_opts/get_all"
});

releaseOpts.init = function(){
	releaseOpts.store._forceLoad();
	releaseOpts.store._saveCustom = function(savecomplete){
		var changeset = releaseOpts.store._pending;
		var updates = [];
		for(var i in changeset._modifiedItems){
			var item = null;
			if(releaseOpts.store._itemsByIdentity){
				item = releaseOpts.store._itemsByIdentity[i];
			}
			else{
				item = releaseOpts.store._arrayOfAllItems[i];
			}
			updates.push(item);
		}
		savecomplete();
	};
	
	dijit.byId('agentReleaseCodesGrid')._setStore(releaseOpts.store);
	dijit.byId('agentReleaseCodesGrid')._refresh();
};

releaseOpts.addOption = function(obj, load, error){
	dojo.xhrPost({
		url:"/release_opts/add",
		content:obj,
		handleAs:'json',
		'load':function(res){
			load(res);
		},
		'error':function(res){
			error(res);
		}
	});
};

releaseOpts.updateOption = function(id, obj, loaded, err){
	dojo.xhrPost({
		url:"/release_opts/update/" + id,
		content: obj,
		handleAs:'json',
		load:function(res){
			if(res.success){
				loaded(res);
				return true;
			}
			errMessage(['update opt failed', id, res.message]);
			console.log(['update opt failed', id, res.message]);
		},
		error:function(res){
			console.log(['update opt errored', res]);
			err(res);
		}
	});
};

releaseOpts.dropOption = function(item, loaded, err){
	var id = releaseOpts.store.getValue(item, 'id');
	dojo.xhrPost({
		url:'/release_opts/drop/' + id,
		handleAs:'json',
		load:function(res){
			if(res.success){
				releaseOpts.store.deleteItem(item);
				loaded(res);
				return;
			}
			errMessage(['drop opt failed', id, res.message]);
			console.log(['drop opt failed', id, res.message]);
		},
		error:function(res){
			errMessage(['drop opt erred', id, res]);
			console.log(['drop opt erred', id, res]);
		}
	});
};
