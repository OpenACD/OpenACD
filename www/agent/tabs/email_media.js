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

		var copyPath = function(arr){
			var out = [];
			for(var i = 0; i < arr.length; i++){
				out.push(arr[i]);
			}
			return out;
		}
		
		debug(["pathsToFetch path", path]);
		if( (skeleton.type == "multipart") && (skeleton.subtype == "alternative") ){
			var getting = 0;
			var pushon = false;
			for(var i = 0; i < skeleton.parts.length; i++){
				if( (skeleton.parts[i].subtype == "plain") && (getting < 1) ){
					getting = 1;
					pushon = i + 1;
				}				
				if( (skeleton.parts[i].subtype == "html") && ( getting < 2 ) ){
					getting = 2;
					pushon = i + 1;
				}
				if( (skeleton.parts[i].type == "multipart") && (getting < 3) ){
					getting = 3;
					pushon = i + 1;
				}
			}
			
			if(pushon){
				var tpath = copyPath(path);
				tpath.push(pushon);
				if(getting == 3){
					fetches = emailPane.pathsToFetch(skeleton.parts[pushon - 1], tpath, fetches);
				}
				else{
					fetches.push(path);
				}
			}
			
			debug(["fetches", fetches]);
			return fetches;
		}
		
		if( (skeleton.type == "multipart") && (skeleton.subtype == "mixed") ){
			for(var i = 0; i < skeleton.parts.length; i++){
				var tpath = copyPath(path);
				tpath.push(i + 1);
				fetches = emailPane.pathsToFetch(skeleton.parts[i], tpath, fetches);
			}
			
			return fetches;
		}
		
		if(skeleton.type == "message"){
			var tpath = copyPath(path);
			tpath.push(1);
			fetches = emailPane.pathsToFetch(skeleton.parts[0], tpath, fetches);
			return fetches;
		}
		
		if(skeleton.type == "text" && skeleton.subtype != "rtf"){
			fetches.push(path);
			return fetches;
		}
		
		return fetches;
	}
	
	emailPane.fetchPaths = function(paths, fetched){
		if(emailPane.fetchSub){
			return false;
		}
		
		if(! fetched){
			fetched = "";
		}
		
		debug(["subbed to", "emailPane/get_path/" + paths[0].join("/")]);
		emailPane.fetchSub = dojo.subscribe("emailPane/get_path/" + paths[0].join("/"), function(res){
			debug(["sub hit", "emailPane/get_path/" + paths[0].join("/"), res]);
			dojo.unsubscribe(emailPane.fetchSub);
			emailPane.fetchSub = false;
			fetched += res;
			paths.shift();
			if(paths.length > 0){
				emailPane.fetchPaths(paths, fetched);
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
	
	widget.subs.push(dojo.subscribe("emailPane/fetchPaths/done", function(fetched){
		debug(fetched);
		while(widget.subs.length > 0){
			var sub = widget.subs.pop();
			dojo.unsubscribe(sub);
			emailPane.fetchCache = fetched;
			widget.setValue(fetched);
		}
	}));
	
	emailPane.fetchPaths(paths);
});

emailPane.getSkeleton();