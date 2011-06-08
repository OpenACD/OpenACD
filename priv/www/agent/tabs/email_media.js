dojo.require("dojox.form.FileUploader");
dojo.require("dojox.widget.Standby");
dojo.require("dojox.html.styles");
dojo.require("dijit.form.Textarea");
dojo.require("dojo.io.iframe");
dojo.require("agentUI.emailLib");

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
	
	emailPane = function(){
		throw new Error("lib, can't be instantiated");
	}
}

emailPane.sub = dojo.subscribe("emailLib/get_skeleton", function(skel){
	console.log('got skeleton', skel);
	dojo.unsubscribe(emailPane.sub);
	emailPane.skel = skel;
	
	dojo.byId('attachmentList').clearSub = dojo.subscribe('emailPane/attachment/add', dojo.byId('attachmentList'), function(){
		this.innerHTML = '';
	});
	dojo.byId('emailToSpan').innerHTML = emailLib.scrubString(skel.headers.to);
	dojo.byId('emailFromSpan').innerHTML = emailLib.scrubString(skel.headers.from);
	dojo.byId('emailSubjectSpan').innerHTML = emailLib.scrubString(skel.headers.subject);
	dojo.byId('emailDateSpan').innerHTML = emailLib.scrubString(skel.headers.date);
	dojo.byId('emailDateSpanReply').innerHTML = emailLib.scrubString(skel.headers.date);
	dojo.byId('emailRawHeadersSpan').innerHTML = function(){
		var out = []; //['<pre>'];
		for(var i in skel.headers){
			out.push(emailLib.scrubString(i) + ': ' + emailLib.scrubString(skel.headers[i]));
		}
		//out.push('</pre>');
		out = out.join('</p><p>');
		return '<p>' + out + '</p>';
	}();
	
	dijit.byId('emailSubject').attr('value', 're:  ' + emailLib.scrubString(skel.headers.subject));
	dijit.byId('emailFrom').attr('value', skel.headers.to);
	if(skel.headers['reply-to']){
		dijit.byId('emailTo').attr('value', skel.headers['reply-to']);
	} else{
		dijit.byId('emailTo').attr('value', skel.headers.from);
	}
	
	var paths = emailLib.pathsToFetch(skel);
	var disp = dojo.byId("emailViewDiv");
	disp.sub = dojo.subscribe("emailLib/fetchPaths/done", function(fetched){
		console.log('fetching of paths complete', fetched);
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
		emailLib.getFrom(fetchFromCallback);
	});
	
	//This could be set up when spying, so disable reply, and allow closability.
	if(window.agentConnection.state == 'released' || window.agentConnection.state == 'idle'){
		dijit.byId('email').attr('closable', true);
		dijit.byId('emailReply').destroy();
		emailPane.spyStateListener = dojo.subscribe("OpenACD/Agent/state", function(){
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
	
	emailLib.fetchPaths(paths);
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

emailLib.getSkeleton();
