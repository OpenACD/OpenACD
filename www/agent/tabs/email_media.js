dojo.require("dojox.form.FileUploader");
dojo.require("dojox.widget.Standby");
dojo.require("dojox.html.styles");
dojo.require("dijit.form.Textarea");
dojo.require("dojo.io.iframe");
dojo.requireLocalization("agentUI", "emailPane");

var nodes = dojo.query('.translatecol, .translate', 'emailView');
nodes = nodes.concat(dojo.query('.translatecol, .translate', 'emailReplyDiv'));
var out = [];
for(var i = 0; i < nodes.length; i++){
	var trans = dojo.i18n.getLocalization("agentUI", "emailPane")[nodes[i].innerHTML];
	if(trans){
		if(dojo.hasClass(nodes[i], 'translatecol')){
			trans += ':';
		}
		nodes[i].innerHTML = trans;
	}
}

if(typeof(emailPane) == 'undefined'){

	dojo.create('link', {rel:'stylesheet', href:'/tabs/email_media.css', type:'text/css'}, dojo.query('head')[0]);

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
	};

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
				debug(["load done", res, "emailPane/get_path/" + path]);
				dojo.publish("emailPane/get_path/" + path, [res]);
			},
			error:function(res){
				warning(["err getting path", res]);
			}
		});
	};
	
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
		};
		
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
			for(i = 0; i < skeleton.parts.length; i++){
				tpath = copyPath(path);
				tpath.push(i + 1);
				fetches = emailPane.pathsToFetch(skeleton.parts[i], tpath, fetches);
			}
			
			return fetches;
		}
		
		if(skeleton.type == "message" && skeleton.subtype == "delivery-status"){
			fetches.push({
				'mode':'fetch',
				'path':path,
				'textType':'plain'
			});
			return fetches;
		}
		
		if(skeleton.type == "message"){
			tpath = copyPath(path);
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
	};
	
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
					if(fetchObjs[0].textType == 'html'){
						fetched += html_sanitize(res, emailPane.urlSanitize, emailPane.nameIdSanitize);
					}else{
						res = emailPane.scrubString(res).replace(/\n/g, '<br />');
						fetched += '<span style="font-family:monospace;">' + replaceUrls(res) + '</span>';
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
	};
	
	//scrub &, <, and > so it's displayable via html
	emailPane.scrubString = function(instr){
		return instr.replace(/\&/g, '&amp;').replace(/\</g, '&lt;').replace(/\>/g, '&gt;').replace(/\"/g, '&quot;'); //"
	};
	
	emailPane.urlSanitize = function(url){
		// following 'borrowed' from
		// http://stackoverflow.com/questions/736513/how-do-i-parse-a-url-into-hostname-and-path-in-javascript
		var l = document.createElement("a");
		l.href = url;
		switch(l.protocol){
			case 'cid:':
				return escape(url);
				break;
			case 'mailto:':
				return url;
				break;
			case 'http:':
				if( (l.hostname == window.location.hostname) &&
					(l.port == window.location.port) ){
					return url;
				} else {
					l.protocol = 'scrub';
					return l.href;
				}
				break;
			default:
				l.protocol = 'scrub';
				return l.href;
		}
	}
	
	emailPane.nameIdSanitize = function(name){
		return "santizationPrefix-" + name;
	}
	
	emailPane.getFrom = function(callback){
		dojo.xhrPost({
			url:"/media",
			handleAs:"json",
			content:{
				"command":"get_from",
				"arguments":[],
				"mode":"call"
			},
			load:function(res){
				callback(res);
			},
			error:function(res){
				warning(["err getting from address", res]);
			}
		});
	}
}

emailPane.sub = dojo.subscribe("emailPane/get_skeleton", function(skel){
	debug(skel);
	dojo.unsubscribe(emailPane.sub);
	emailPane.skel = skel;
	
	dojo.byId('attachmentList').clearSub = dojo.subscribe('emailPane/attachment/add', dojo.byId('attachmentList'), function(){
		this.innerHTML = '';
	});
	dojo.byId('emailToSpan').innerHTML = emailPane.scrubString(skel.headers.to);
	dojo.byId('emailFromSpan').innerHTML = emailPane.scrubString(skel.headers.from);
	dojo.byId('emailSubjectSpan').innerHTML = emailPane.scrubString(skel.headers.subject);
	dojo.byId('emailDateSpan').innerHTML = emailPane.scrubString(skel.headers.date);
	dojo.byId('emailDateSpanReply').innerHTML = emailPane.scrubString(skel.headers.date);
	dojo.byId('emailRawHeadersSpan').innerHTML = function(){
		var out = []; //['<pre>'];
		for(var i in skel.headers){
			out.push(emailPane.scrubString(i) + ': ' + emailPane.scrubString(skel.headers[i]));
		}
		//out.push('</pre>');
		out = out.join('</p><p>');
		return '<p>' + out + '</p>';
	}();
	
	dijit.byId('emailSubject').attr('value', 're:  ' + emailPane.scrubString(skel.headers.subject));
	dijit.byId('emailFrom').attr('value', skel.headers.to);
	if(skel.headers['reply-to']){
		dijit.byId('emailTo').attr('value', skel.headers['reply-to']);
	} else{
		dijit.byId('emailTo').attr('value', skel.headers.from);
	}
	
	var paths = emailPane.pathsToFetch(skel);
	var disp = dojo.byId("emailViewDiv");
	disp.sub = dojo.subscribe("emailPane/fetchPaths/done", function(fetched){
		debug(fetched);
		dojo.unsubscribe(disp.sub);
		disp.innerHTML = fetched;
		var nodes = dojo.query('* > img', disp);
		debug(["going through the nodes for images.", nodes]);
		for(var i = 0; i < nodes.length; i++){
			var l = document.createElement('a');
			l.href = nodes[i].src;
			var locationCheck = function(linkNode){
				if(dojo.isSafari){
					return linkNode.hostname == window.location.host;
				}
				
				return linkNode.hostname == window.location.hostname && linkNode.port == window.location.port;
			}
			
			if(l.protocol == 'scrub' || locationCheck(l) ){
				l.protocol = 'http';
				nodes[i].src = l.href;
			} else{
				l.protocol = 'http';
				nodes[i].title = 'Remote image (' + l.href + ') scrubbed; click to load it';
				nodes[i].loadUrl = l.href;
				nodes[i].src = '/images/redx.png';
				dojo.connect(nodes[i], 'onclick', nodes[i], function(){
					this.src = this.loadUrl;
				});
			}
		}
		nodes = dojo.query('* > a', disp);
		for(i = 0; i < nodes.length; i++){
			nodes[i].target = '_blank';
			nodes[i].protocol = 'http';
			//so that successive calls to nodes[i].hostname gets the correct value
			nodes[i].href = nodes[i].href;
		}
		var fetchFromCallback = function(data){
			if(data.success){
				var val = '';
				if(data.data.label){
					val += '"' + data.data.label + '" ';
				}
				val += '<' + data.data.address + '>';
				dijit.byId('emailFrom').attr('value', val);
			} else {
				dijit.byId('emailFrom').attr('value', skel.headers.to.split(',')[0]);
			}
		}
		emailPane.getFrom(fetchFromCallback);
	});
	
	//This could be set up when spying, so disable reply, and allow closability.
	if(agent.state == 'released' || agent.state == 'idle'){
		dijit.byId('email').attr('closable', true);
		dijit.byId('emailReply').destroy();
		emailPane.spyStateListener = dojo.subscribe("agent/state", function(){
			dojo.unsubscribe(emailPane.spyStateListener);
			dojo.unsubscribe(emailPane.tabCloseListener);
			delete emailPane.tabCloseListener;
			delete emailPane.spyStateListener;
			dijit.byId('tabPanel').closeChild(dijit.byId('email'));
		});
		emailPane.tabCloseListener = dojo.subscribe("tabPanel-removeChild", function(child){
			if(child.id == 'email'){
				dojo.unsubscribe(emailPane.tabCloseListener);
				dojo.unsubscribe(emailPane.spyStateListener);
				delete emailPane.tabCloseListener;
				delete emailPane.spyStateListener;
			}
		});
	}
	
	emailPane.fetchPaths(paths);
});

dojo.byId('attachedList').rebuildList = function(filenames){
	while(this.firstChild){
		this.removeChild(this.firstChild);
	}
	if(filenames.length == 0){
		dojo.byId('attachmentListP').style.display = 'none';
		return;
	}
	dojo.byId('attachmentListP').style.display = '';
	for(var i = 0; i < filenames.length; i++){
		var li = this.appendChild(dojo.doc.createElement('li'));
		li.innerHTML += filenames[i];
		li.insertBefore(dojo.doc.createElement('input'), li.firstChild);
		var buttonNode = li.firstChild;
		buttonNode.type = 'image';
		buttonNode.src = '/images/redx.png';
		buttonNode.setAttribute('fileIndex', i);
		buttonNode.setAttribute('filename', filenames[i]);
		li.firstChild.onclick = function(e){
			var index = parseInt(e.target.getAttribute('fileIndex'));
			var nom = e.target.getAttribute('filename');
			dojo.xhrPost({
				url:"/media",
				content:{
					command:'detach',
					mode:'call',
					'arguments':dojo.toJson([index + 1, nom])
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
		};
	}
};

dojo.byId('attachedList').attachListAddSub = dojo.subscribe("emailPane/attachment/add", dojo.byId('attachedList'), function(data){
	if(data.success){
		this.rebuildList(data.filenames);
	}
	else{
		console.log(['attaching failed', data]);
		errMessage(['attaching failed', data.message]);
	}
});

dojo.byId('attachedList').attachListDropSub = dojo.subscribe("emailPane/attachment/drop", dojo.byId('attachedList'), function(data){
	this.rebuildList(data);
});

setTimeout(function(){
	dojo.byId('emailReplyDiv').style.display = 'none';
}, 250);

emailPane.getSkeleton();
