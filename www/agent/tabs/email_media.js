dojo.require("dojox.form.FileUploader");
dojo.require("dojox.widget.Standby");
dojo.require("dojox.html.styles");
dojo.require("dojo.io.iframe");
dojo.requireLocalization("agentUI", "emailPane");

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
		
		debug(["pathsToFetch", skeleton, path]);
		if( (skeleton.type == "multipart") && (skeleton.subtype == "alternative") ){
			var getting = 0;
			var pushon = false;
			var texttype = "";
			for(var i = 0; i < skeleton.parts.length; i++){
				if( (skeleton.parts[i].subtype == "plain") && (getting < 1) ){
					getting = 1;
					pushon = i + 1;
					texttype = 'plain';
				}				
				if( (skeleton.parts[i].subtype == "html") && ( getting < 2 ) ){
					getting = 2;
					pushon = i + 1;
					texttype = 'html';
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
		
		if( (skeleton.type == "multipart") ){
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
			fetches.push({
				'mode':'a',
				'path':path,
				'label':skeleton.properties['disposition-params'].filename
			});
			fetches = emailPane.pathsToFetch(skeleton.parts[0], tpath, fetches);
			return fetches;
		}
		
		if(skeleton.type == "text" && skeleton.subtype != "rtf"){
			fetches.push({
				'mode':'fetch',
				'path':path,
				'textType':skeleton.subtype
			});
			return fetches;
		}
		
		if(skeleton.type == "image"){
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
	}
	
	emailPane.fetchPaths = function(fetchObjs, fetched){
		debug(["fetchPaths", fetchObjs[0]]);
		if(emailPane.fetchSub){
			return false;
		}
		
		if(! fetched){
			fetched = "";
		}
		
		var jpath = fetchObjs[0].path.join("/");
		
		debug(["subbed to", "emailPane/get_path/" + fetchObjs[0].path.join("/")]);
		if(fetchObjs[0].mode == 'a'){
			fetched += '<a href="/' + jpath + '" target="_blank"><img src="/images/dl.png" style="border:none"/>' + fetchObjs[0].label + '</a>';
		}
		else if(fetchObjs[0].mode == 'img'){
			fetched += '<img src="/' + jpath + '" />';
		}
		else if(fetchObjs[0].mode == 'fetch'){
			emailPane.fetchSub = dojo.subscribe("emailPane/get_path/" + jpath, function(res){
				debug(["sub hit", "emailPane/get_path/" + jpath, res]);
				dojo.unsubscribe(emailPane.fetchSub);
				emailPane.fetchSub = false;
				if(fetchObjs[0].textType){
					if(fetchObjs[0].textType == 'plain'){
						fetched += '<pre>' + res + '</pre>';
					}
					else if(fetchObjs[0].textType == 'html'){
						fetched += res;
					}
				}
				else{
					fetched += res;
				}
				fetchObjs.shift();
				if(fetchObjs.length > 0){
					emailPane.fetchPaths(fetchObjs, fetched);
				}
				else{
					dojo.publish("emailPane/fetchPaths/done", [fetched]);
				}
			});			
			emailPane.getPath(fetchObjs[0].path);
			return;
		}
		fetchObjs.shift();
		if(fetchObjs.length > 0){
			emailPane.fetchPaths(fetchObjs, fetched);
		}
		else{
			dojo.publish("emailPane/fetchPaths/done", [fetched]);
		}
	}
	
	//scrub &, <, and > so it's displayable via html
	emailPane.scrubString = function(instr){
		instr = instr.replace('&', '&amp;');
		instr = instr.replace('<', '&lt;');
		instr = instr.replace('>', '&gt;');
		instr = instr.replace('"', '&quot;');
		return instr;
	}
}

emailPane.sub = dojo.subscribe("emailPane/get_skeleton", function(skel){
	debug(skel);
	dojo.unsubscribe(emailPane.sub);
	emailPane.skel = skel;
	
	/*var fileUpload = dijit.byId('emailAttachFiles');
	if(! fileUpload.transformed){
		var temp = new dojox.form.FileUploader({
			id:'fileUploader',
			button:dijit.byId("emailAttachFiles"),
			uploadUrl:"/media",
			postData:{
				command:'attach',
				mode:'call',
				filename:'fileUpload',
				htmlFieldName:'attachFiles'
			},
			force:"html",
			htmlFieldName:'attachFiles',
			selectMultipleFiles:true
		});
		temp.transformed = true;
		dojo.connect(temp, 'onChange', function(data){
			var listNode = dojo.byId('attachmentList');
			var newhtml = data[0].name;
			fileUpload.filename = data[0].name;
			listNode.innerHTML = newhtml;
		});
		dojo.connect(temp, 'onComplete', function(data){
			debug(['fileUpload complete', data]);
			dojo.publish("emailPane/attachment/add", [data[0]]);
		});
	}*/
	dojo.byId('attachmentList').clearSub = dojo.subscribe('emailPane/attachment/add', dojo.byId('attachmentList'), function(){
		this.innerHTML = '';
	});
	dojo.byId('emailToSpan').innerHTML = emailPane.scrubString(skel.headers['To']);
	dojo.byId('emailFromSpan').innerHTML = emailPane.scrubString(skel.headers['From']);
	dojo.byId('emailSubjectSpan').innerHTML = emailPane.scrubString(skel.headers['Subject']);
	dojo.byId('emailRawHeadersSpan').innerHTML = function(){
		var out = ['<pre>'];
		for(var i in skel.headers){
			out.push(emailPane.scrubString(i) + ': ' + emailPane.scrubString(skel.headers[i]));
		}
		out.push('</pre>');
		return out.join('<br />');
	}();
	
	dijit.byId('emailSubject').attr('value', 're:  ' + emailPane.scrubString(skel.headers['Subject']));
	dijit.byId('emailFrom').attr('value', skel.headers['To']);
	if(skel.headers['Reply-To']){
		dijit.byId('emailTo').attr('value', skel.headers['Reply-To']);
	}
	else{
		dijit.byId('emailTo').attr('value', skel.headers['From']);
	}
	
	var paths = emailPane.pathsToFetch(skel);
	var disp = dojo.byId("emailViewDiv");
	disp.sub = dojo.subscribe("emailPane/fetchPaths/done", function(fetched){
		debug(fetched);
		dojo.unsubscribe(disp.sub);
		disp.innerHTML = fetched;
	});
		
	emailPane.fetchPaths(paths);
});

dojo.byId('attachedList').rebuildList = function(filenames){
	while(this.firstChild){
		this.removeChild(this.firstChild);
	}
	for(var i = 0; i < filenames.length; i++){
		var li = this.appendChild(dojo.doc.createElement('li'));
		li.innerHTML += filenames[i];
		li.insertBefore(dojo.doc.createElement('input'), li.firstChild);
		var buttonNode = li.firstChild;
		buttonNode.type = 'image';
		buttonNode.src = '/images/redx.png';
		buttonNode.fileIndex = i;
		buttonNode.filename = filenames[i];
		li.firstChild.onclick = function(e){
			var index = e.originalTarget.fileIndex;
			var nom = e.originalTarget.filename;
			dojo.xhrPost({
				url:"/media",
				content:{
					command:'detach',
					mode:'call',
					arguments:dojo.toJson([index + 1, nom])
				},
				handleAs:'json',
				load:function(res){
					if(res.success){
						dojo.publish("emailPane/attachment/drop", [res.filenames]);
						return true;
					}
					
					warning(['detach failed', res]);
				},
				error:function(res){
					warning(['detach errored', res]);
				}
			});
		}
	}
};

dojo.byId('attachedList').attachListAddSub = dojo.subscribe("emailPane/attachment/add", dojo.byId('attachedList'), function(data){
	if(data.success){
		this.rebuildList(data.filenames);
	}
	else{
		console.log(data.message);
	}
});

dojo.byId('attachedList').attachListDropSub = dojo.subscribe("emailPane/attachment/drop", dojo.byId('attachedList'), function(data){
	this.rebuildList(data);
});


emailPane.getSkeleton();