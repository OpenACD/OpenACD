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
				dojo.publish("emailTab/get_skeleton", [res]);
			},
			error:function(res){
				warning(["err getting email skeleton", res]);
			}
		});
	}

	emailPane.getPath = function(path){
		dojo.xhrPost({
			url:"/media",
			content:{
				"command":"get_path",
				"arguments":path,
				"mode":"call"
			},
			load:function(res){
				dojo.publish("emailTab/get_path/" + path, [res]);
			},
			error:function(res){
				warning(["err getting path", res])
			}
		});
	}
	
	emailPane.pathsToFetch = function(skeleton, path, fetches){
		if(! path){
			path = "";
		}
		if(! fetches){
			fetches = [];
		}
		
		if( (skeleton.type == "multipart") && (skeleton.subtype == "alternative") ){
			var getting = "";
			var getpath = "";
			for(var i = 0; i < skeleton.parts.length; i++){
				var newpath = path + "/" + (i + 1);
				if(skeleton.parts[i].subtype == "html"){
					getting = "html";
					getpath = newpath;
				}
				if( (skeleton.parts[i].subtype == "plain") && (getting != "html") ){
					getting = "plain";
					getpath = newpath;
				}
			}
			
			fetches.push(getpath);
			return fetches;
		}
		
		if( (skeleton.type == "multipart") && (skeleton.subtype == "mixed") ){
			for(var i = 0; i < skeleton.parts.length; i++){
				fetches = emailPane.pathsToFetch(skeleton.parts[i], path + "/" + (i + 1), fetches);
			}
			
			return fetches;
		}
		
		if(skeleton.type == "message"){
			fetches = emailPane.pathsToFetch(skeleton.parts[0], path + "/" + 1, fetches);
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
		
		emailPane.fetchSub = dojo.subscribe("emailTab/get_path/" + paths[0], function(){
			dojo.unsubscribe(emailPane.fetchSub);
			paths.shift();
			if(paths.length > 0){
				emailPane.fetchPaths(paths);
				emailPane.getPath(paths[0]);
			}
		});
	}
}