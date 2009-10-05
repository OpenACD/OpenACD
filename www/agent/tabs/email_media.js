if(typeof(emailPane) == 'undefined'){

	emailPane = function(){};

	emailPane.getSkeleton = function(){
		dojo.xhrPost({
			url:"/media",
			handleAs:"json",
			content:{
				"command":"get_skeleton",
				"arguments":[],
				"mode":"call"
			},
			load:function(res){
				dojo.publish("emailPane/get_skeleton", [res]);
			},
			error:function(res){
				warning(["err getting email skeleton", res]);
			}
		});
	}

	emailPane.getPath = function(path){
		path = path.join("/");
		dojo.xhrPost({
			url:"/media",
			content:{
				"command":"get_path",
				"arguments":path,
				"mode":"call"
			},
			load:function(res){
				debug(["load done", res, "emailPane/get_path/" + path])
				dojo.publish("emailPane/get_path/" + path, [res]);
			},
			error:function(res){
				warning(["err getting path", res])
			}
		});
	}
	
	emailPane.pathsToFetch = function(skeleton, path, fetches){
		if(! path){
			path = [];
		}
		if(! fetches){
			fetches = [];
		}

		if( (skeleton.type == "multipart") && (skeleton.subtype == "alternative") ){
			var getting = "";
			var pushon = false;
			for(var i = 0; i < skeleton.parts.length; i++){
				if(skeleton.parts[i].subtype == "html"){
					getting = "html";
					pushon = i + 1;
				}
				if( (skeleton.parts[i].subtype == "plain") && (getting != "html") ){
					getting = "plain";
					pushon = i + 1;
				}
			}
			
			if(pushon){
				path.push(pushon);
				fetches.push(path);
			}
			
			return fetches;
		}
		
		if( (skeleton.type == "multipart") && (skeleton.subtype == "mixed") ){
			for(var i = 0; i < skeleton.parts.length; i++){
				path.push(i + 1);
				fetches = emailPane.pathsToFetch(skeleton.parts[i], path, fetches);
			}
			
			return fetches;
		}
		
		if(skeleton.type == "message"){
			path.push(1);
			fetches = emailPane.pathsToFetch(skeleton.parts[0], path, fetches);
			return fetches;
		}
		
		if(skeleton.type == "text"){
			fetches.push(path);
			return fetches;
		}
		
		return fetches;
	}
	
	emailPane.fetchPaths = function(paths){
		if(emailPane.fetchSub){
			return false;
		}
		
		var fetched = "";
		
		debug(["subbed to", "emailPane/get_path/" + paths[0].join("/")]);
		emailPane.fetchSub = dojo.subscribe("emailPane/get_path/" + paths[0].join("/"), function(res){
			debug(["sub hit", "emailPane/get_path/" + paths[0].join("/"), res]);
			dojo.unsubscribe(emailPane.fetchSub);
			emailPane.fetchSub = false;
			fetched += res;
			paths.shift();
			if(paths.length > 0){
				emailPane.fetchPaths(paths);
			}
			else{
				dojo.publish("emailPane/fetchPaths/done", [fetched]);
			}
		});
		
		emailPane.getPath(paths[0]);
	}
}

emailPane.sub = dojo.subscribe("emailPane/get_skeleton", function(skel){
	debug(skel);
	dojo.unsubscribe(emailPane.sub);
	emailPane.skel = skel;
	var paths = emailPane.pathsToFetch(skel);
	var widget = dijit.byId('emailBody');
	widget.subs = [];
	/*for(var i = 0; i < paths.length; i++){
		widget.subs.push(dojo.subscribe("emailPane/get_path/" + paths[i].join("/"), function(res){
			var val = widget.getValue();
			widget.setValue(val + res);
		}));
	}*/
	
	widget.subs.push(dojo.subscribe("emailPane/fetchPaths/done", function(fetched){
		debug(fetched);
		while(widget.subs.length > 0){
			var sub = widget.subs.pop();
			dojo.unsubscribe(sub);
			widget.setValue(fetched);
		}
	}));
	
	emailPane.fetchPaths(paths);
});

emailPane.getSkeleton();