//>>built
// wrapped by build app
define("agentUI/emailLib", ["dojo","dijit","dojox","dojo/require!agentUI/logLib,agentUI/util"], function(dojo,dijit,dojox){
dojo.require("agentUI.logLib");
dojo.require("agentUI.util");

dojo.provide("agentUI.emailLib");

if(typeof(emailLib) === 'undefined'){
	emailLib = function(){
		throw new Error("emailLib is a lib, and so can't be instantiated");
	};
	
	dojo.create('script', {type:'text/javascript', src:'/html-sanitizer-minified.js'}, dojo.query('head')[0], 'last');
	
	emailLib.getSkeleton = function(){
		// yay globals!
		window.agentConnection.webApi('api', 'media_command', {
			success:function(res){
				dojo.publish('emailLib/get_skeleton', [res]);
			}
		}, 'get_skeleton', 'call');
	};
	
	emailLib.getPath = function(path){
		window.agentConnection.webApi('api', 'media_command', {
			success:function(res){
				dojo.publish("emailLib/get_path/" + path, [res]);
			},
			error:function(err, ioxhr){
				//console.error('getPath error', arguments);
				dojo.publish("emailLib/get_path/" + path, [ioxhr.xhr.responseText]);
			},
			failure:function(code, msg){
				console.warn('getPath failure', code, msg);
			}
		}, 'get_path', 'call', [path]);
	};
	
	emailLib.pathsToFetch = function(skeleton, path, fetches){
		if(! path){
			path = [];
		}
		if(! fetches){
			fetches = [];
		}
		
		var copyPath = function(arr){
			var out = [];
			var i;
			for(i = 0; i < arr.length; i++){
				out.push(arr[i]);
			}
			return out;
		};
		
		debug(["pathsToFetch", skeleton, path]);
		var tpath;
		var i = 0;
		if( (skeleton.type === "multipart") && (skeleton.subtype === "alternative") ){
			var getting = 0;
			var pushon = false;
			var texttype = "";
			for(i = 0; i < skeleton.parts.length; i++){
				if( (skeleton.parts[i].subtype === "plain") && (getting < 1) ){
					getting = 1;
					pushon = i + 1;
					texttype = 'plain';
				}				
				if( (skeleton.parts[i].subtype === "html") && ( getting < 2 ) ){
					getting = 2;
					pushon = i + 1;
					texttype = 'html';
				}
				if( (skeleton.parts[i].type === "multipart") && (getting < 3) ){
					getting = 3;
					pushon = i + 1;
				}
			}
			
			if(pushon){
				tpath = copyPath(path);
				tpath.push(pushon);
				if(getting === 3){
					fetches = emailLib.pathsToFetch(skeleton.parts[pushon - 1], tpath, fetches);
				}
				else{
					fetches.push({
						'mode':'fetch',
						'path':tpath,
						'textType':texttype
					});
				}
			}
			
			debug(["fetches", fetches]);
			return fetches;
		}
		
		if( (skeleton.type === "multipart") ){
			for(i = 0; i < skeleton.parts.length; i++){
				tpath = copyPath(path);
				tpath.push(i + 1);
				fetches = emailLib.pathsToFetch(skeleton.parts[i], tpath, fetches);
			}
			
			return fetches;
		}
		
		if(skeleton.type === "message" && skeleton.subtype === "delivery-status"){
			fetches.push({
				'mode':'fetch',
				'path':path,
				'textType':'plain'
			});
			return fetches;
		}
		
		if(skeleton.type === "message"){
			tpath = copyPath(path);
			tpath.push(1);
			fetches.push({
				'mode':'a',
				'path':path,
				'label':skeleton.properties['disposition-params'].filename
			});
			fetches = emailLib.pathsToFetch(skeleton.parts[0], tpath, fetches);
			return fetches;
		}
		
		if(skeleton.type === "text" && skeleton.subtype !== "rtf"){
			fetches.push({
				'mode':'fetch',
				'path':path,
				'textType':skeleton.subtype
			});
			return fetches;
		}
		
		if(skeleton.type === "image"){
			fetches.push({
				'mode':'img',
				'path':path
			});
			return fetches;
		}
		
		fetches.push({
			'mode':'a',
			'path':path,
			'label':skeleton.properties['disposition-params'].filename
		});
		
		return fetches;
	};
	
	emailLib.fetchPaths = function(fetchObjs, fetched){
		debug(["fetchPaths", fetchObjs[0]]);
		if(emailLib.fetchSub){
			return false;
		}
		
		if(! fetched){
			fetched = "";
		}
		
		var jpath = fetchObjs[0].path.join("/");
		
		console.log("subbed to", "emailLib/get_path/" + fetchObjs[0].path.join("/"));
		if(fetchObjs[0].mode === 'a'){
			fetched += '<a href="/' + jpath + '" target="_blank"><img src="/images/dl.png" style="border:none"/>' + fetchObjs[0].label + '</a>';
		}
		else if(fetchObjs[0].mode === 'img'){
			fetched += '<img src="/' + jpath + '" />';
		}
		else if(fetchObjs[0].mode === 'fetch'){
			emailLib.fetchSub = dojo.subscribe("emailLib/get_path/" + jpath, function(res){
				console.log("sub hit", "emailLib/get_path/" + jpath, res);
				dojo.unsubscribe(emailLib.fetchSub);
				emailLib.fetchSub = false;
				if(fetchObjs[0].textType){
					if(fetchObjs[0].textType === 'html'){
						fetched += html_sanitize(res, emailLib.urlSanitize, emailLib.nameIdSanitize);
					}else{
						res = emailLib.scrubString(res).replace(/\n/g, '<br />');
						fetched += '<span style="font-family:monospace;">' + replaceUrls(res) + '</span>';
					}
				}
				else{
					fetched += res;
				}
				fetchObjs.shift();
				if(fetchObjs.length > 0){
					emailLib.fetchPaths(fetchObjs, fetched);
				}
				else{
					dojo.publish("emailLib/fetchPaths/done", [fetched]);
				}
			});			
			emailLib.getPath(fetchObjs[0].path);
			return;
		}
		fetchObjs.shift();
		if(fetchObjs.length > 0){
			emailLib.fetchPaths(fetchObjs, fetched);
		}
		else{
			dojo.publish("emailLib/fetchPaths/done", [fetched]);
		}
	};
	
	//scrub &, <, and > so it's displayable via html
	emailLib.scrubString = function(instr){
		if(instr === undefined){
			return '';
		}
		return instr.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;'); //"
	};

	emailLib.urlSanitize = function(url){
		// following 'borrowed' from
		// http://stackoverflow.com/questions/736513/how-do-i-parse-a-url-into-hostname-and-path-in-javascript
		var l = document.createElement("a");
		l.href = url;
		switch(l.protocol){
			case 'cid:':
				return escape(url);
				//break;
			case 'mailto:':
				return url;
				//break;
			case 'http:':
				if(	(l.hostname === window.location.hostname) && 
					(l.port === window.location.port) ){
					return url;
				} else {
					l.protocol = 'scrub';
				return l.href;
				}
				//break;
			default:
				l.protocol = 'scrub';
				return l.href;
		}
	};

	emailLib.nameIdSanitize = function(name){
		return "santizationPrefix-" + name;
	};

	emailLib.getFrom = function(callback){
		window.agentConnection.webApi('api', 'media_command', {
			success:function(res){
				callback(res);
			},
			failure:function(code, msg){
				console.warn("getFrom failed", code, msg);
			},
			error:function(err){
				console.error("err getFrom", err);
			}
		}, 'get_from', 'call');
	};
}

});
