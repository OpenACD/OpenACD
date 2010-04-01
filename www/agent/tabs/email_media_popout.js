// I do not expect this to be opened more than once in a page's life-time.
// namespacing is just for convience.

dojo.require("agentUI.emailLib");

emailPopout = function(){
	throw new Error("Not for instiation");
}

emailPopout.sub = dojo.subscribe("emailLib/get_skeleton", function(skel){
	debug(skel);
	dojo.unsubscribe(emailPopout.sub);
	emailPane.skel = skel;

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

	var paths = emailLib.pathsToFetch(skel);
	var disp = dojo.byId("emailViewDiv");
	disp.sub = dojo.subscribe("emailLib/fetchPaths/done", function(fetched){
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
		emailLib.getFrom(fetchFromCallback);
	});
	emailLib.fetchPaths(paths);
});


emailLib.getSkeleton();