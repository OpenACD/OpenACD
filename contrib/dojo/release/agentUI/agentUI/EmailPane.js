//>>built
// wrapped by build app
define("agentUI/EmailPane", ["dojo","dijit","dojox","dojo/i18n!agentUI/nls/EmailPane","dojo/require!dojox/form/FileUploader,dojox/widget/Standby,dojox/html/styles,dijit/form/Textarea,dojo/io/iframe,dijit/_Widget,dijit/_Templated,agentUI/emailLib"], function(dojo,dijit,dojox){
dojo.require("dojox.form.FileUploader");
dojo.require("dojox.widget.Standby");
dojo.require("dojox.html.styles");
dojo.require("dijit.form.Textarea");
dojo.require("dojo.io.iframe");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("agentUI.emailLib");

dojo.provide("agentUI.EmailPane");

dojo.requireLocalization("agentUI", "EmailPane");

dojo.create('link', {rel:'stylesheet', href:'/tabs/email_media.css', type:'text/css'}, dojo.query('head')[0]);

dojo.declare("agentUI.EmailPane", [dijit._Widget, dijit._Templated], {
	templateString:"<div style=\"width:100%; height: 100%\">\n\n<a href='/tabs/email_media_popout.html' target='_blank'>\n\t<img src='/images/popout.png' class='popout' />\n</a>\n\n<div dojoAttachPoint=\"emailView\">\n\t<table>\n\t\t<tr>\n\t\t\t<th class=\"translatecol\">TO</th>\n\t\t\t<td dojoAttachPoint=\"emailToSpan\"></td>\n\t\t</tr>\n\t\t<tr>\n\t\t\t<th class=\"translatecol\">FROM</th>\n\t\t\t<td dojoAttachPoint=\"emailFromSpan\"></td>\n\t\t</tr>\n\t\t<tr>\n\t\t\t<th class=\"translatecol\">SUBJECT</th>\n\t\t\t<td dojoAttachPoint=\"emailSubjectSpan\"></td>\n\t\t</tr>\n\t\t<tr>\n\t\t\t<th class=\"translatecol\">DATE</th>\n\t\t\t<td dojoAttachPoint=\"emailDateSpan\"></td>\n\t\t</tr>\n\t\t<tr>\n\t\t\t<th class=\"translatecol\">SHOW_HEADERS</th>\n\t\t\t<td>\n\t\t\t<button label='&rarr;' dojoType='dijit.form.Button' style=\"float:left;\" dojoAttachPoint=\"toggleRawHeaderButton\">\n\t\t\t</button>\n\t\t\t<span dojoAttachPoint='emailRawHeadersSpan' style='display:none'></span>\n\t\t\t</td>\n\t\t</tr>\n\t\t<tr>\n\t\t\t<th>&nbsp;</th>\n\t\t\t<td>\n\t\t\t<button label=\"REPLY\" dojoAttachPoint=\"emailReply\" dojoType=\"dijit.form.Button\">\n\t\t\t</button>\n\t\t\t</td>\n\t\t</tr>\n\t</table>\n\t<div dojoAttachPoint=\"emailViewDiv\" style=\"height: 250px; overflow: auto; border: grey solid 1px;\"></div>\n\t\n</div>\n\n<div dojoAttachPoint=\"emailReplyDiv\">\n\n\t<form dojoType=\"dijit.form.Form\" action=\"javascript:void(0)\" dojoAttachPoint=\"emailReplyForm\">\n\t\t<div style=\"width:59%; display:inline-block;\">\n\t\t\t<label class=\"translatecol narrow\">TO</label>\n\t\t\t<input dojoType=\"dijit.form.TextBox\" dojoAttachPoint=\"emailTo\" style=\"width: 30em;\"/><br/>\n\t\t\t<label class=\"translatecol narrow\">FROM</label>\n\t\t\t<input dojoType=\"dijit.form.TextBox\" dojoAttachPoint=\"emailFrom\" style=\"width: 30em;\"/><br/>\n\t\t\t<label class=\"translatecol narrow\">SUBJECT</label>\n\t\t\t<input dojoType=\"dijit.form.TextBox\" dojoAttachPoint=\"emailSubject\" style=\"width: 30em;\"/><br/>\n\t\t\t<label class=\"translatecol narrow\">CC</label>\n\t\t\t<textarea dojoType=\"dijit.form.Textarea\" dojoAttachPoint=\"emailCC\" name=\"emailCC\" cols=\"20\"></textarea><br/>\n\t\t\t<label class=\"translatecol narrow\">BCC</label>\n\t\t\t<textarea dojoType=\"dijit.form.Textarea\" dojoAttachPoint=\"emailBCC\" name=\"emailBCC\" cols=\"20\"></textarea><br/>\n\t\t\t<label class=\"translatecol narrow\">DATE</label>\n\t\t\t<span dojoAttachPoint=\"emailDateSpanReply\"></span><br/>\n\t\t\t<label class=\"narrow\">&nbsp;</label>\n\t\t\t<span dojoAttachPoint=\"attachmentList\"></span>\n\t\t\t<button label=\"UPLOAD\" dojoType=\"dijit.form.Button\" style=\"display:none\" dojoAttachPoint=\"emailUploadButton\">\n\t\t\t</button>\t\t\n\t\t\t<span dojoAttachPoint=\"attachmentListP\" style=\"display:none\">\n\t\t\t\t<label>&nbsp;</label>\n\t\t\t\t<ul style=\"list-style-type:none; display:inline-block\" dojoAttachPoint=\"attachedList\"></ul>\n\t\t\t</span>\n\t\t</div>\n\n\t\t<div>\n\t\t\t<div dojoAttachPoint=\"emailDisp\" style=\"width:100%;\" />\n\t\t</div>\n\t\t<div style=\"margin-left: auto; margin-right: auto; width: 20%;\">\n\t\t\t<button label=\"SUBMIT\" dojoType=\"dijit.form.Button\" dojoAttachPoint=\"emailSubmit\">\n\t\t\t</button>\n\t\t\t\n\t\t\t<button label=\"CANCEL\" dojoType=\"dijit.form.Button\" dojoAttachPoint=\"emailCancel\">\n\t\t\t</button>\n\t\t</div>\n\t</form>\n</div>\n\n</div>\n",
	widgetsInTemplate: true,

	constructor: function(args, srcNodeRef){
		console.log('email pane construction', args, srcNodeRef);
		dojo.safeMixin(this, args);
		if(args.channelId){
			this.channel = window.agentConnection.channels[args.channelId];
		}
		this.skeletonSub = dojo.subscribe('emailLib/get_skeleton/' + this.channelId, this, function(skel){
			this._handleGetSkeleton(skel);
		});

		this.attachmentAddSub = dojo.subscribe('emailPane/attachment/add/' + this.channelId, this, function(data){
			this._rebuildAttachmentList(data);
		});

		this.attachmentDropSub = dojo.subscribe('emailPane/attachment/drop/' + this.channelId, this, function(data){
			this._rebuildAttachmentList(data);
		});
	},

	postCreate: function(){
		var nodes = dojo.query('.translatecol, .translate', 'emailView', this.domNode);
		var out = [];
		for(var i = 0; i < nodes.length; i++){
			var trans = dojo.i18n.getLocalization("agentUI", "EmailPane")[nodes[i].innerHTML];
			if(trans){
				if(dojo.hasClass(nodes[i], 'translatecol')){
					trans += ':';
				}
			nodes[i].innerHTML = trans;
			}
		}

		var replyDiv = this.emailReplyDiv;
		setTimeout(function(){
			replyDiv.style.display = 'none';
		}, 250);

		dojo.connect(this.toggleRawHeaderButton, 'onClick', this, function(){
			var target = this.emailRawHeadersSpan;
			if(target.style.display == 'none'){
				target.style.display = 'inline-block';
				this.toggleRawHeaderButton.attr('label', '&darr;');
			} else{
				target.style.display = 'none';
				this.toggleRawHeaderButton.attr('label', '&rarr;');
			}
		});

		this.emailReply.attr('label', dojo.i18n.getLocalization("agentUI", "EmailPane").REPLY);

		dojo.connect(this.emailReply, 'onClick', this, function(){
			this.emailView.style.display = 'none';
			this.emailReplyDiv.style.display = 'block';
			var replyBase = dojo.doc.createElement('div');
			//replyBase.id = 'emailReplyEditor';
			replyBase.style.height = '100%';
			this.emailDisp.appendChild(replyBase);
			var widget = new dijit.Editor({height:'300px'}, replyBase);
			this.emailReplyEditor = widget;
			widget.setValue(this.emailViewDiv.innerHTML);
			var div = dojo.create('button', {'class':'attachIcon'}, this.emailReplyEditor.toolbar.domNode);
			var uploadButton = this.emailUploadButton;
			var chanId = this.channelId;
			var attachList = this.attachmentList;
			var button = new dojox.form.FileUploader({
				label:'Attach',
				force:'html',
				htmlFieldName:'attachFiles',
				selectMultipleFiles:false,
				id:'fileUploader',
				selectedList:'attachmentList',
				showProgress:true,
				showLabel:false,
				iconClass:'attachIcon',
				//TODO media? was media
				uploadUrl:'/api',
				hoverClass:'attachIcon',
				activeClass:'attachIcon',
				disabledClass:'btnDisabled',
				/*onClick:function(ev){
					dijit.byId('fileUploader').onClick(ev);
				}*/
				onComplete:function(e){
					uploadButton.domNode.style.display = 'none';
					dojo.publish('emailPane/attachment/add/' + chanId, e);
				},
				onChange:function(data){
					uploadButton.domNode.style.display = '';
					attachList.innerHTML = data.name + ' ' + Math.ceil(data.size * .001) + 'kb';
				}
			}, div);
			button.insideNode.style.backgroundImage = "url('images/paperclip.png')";
		});

		this.emailUploadButton.attr('label', dojo.i18n.getLocalization("agentUI", "EmailPane").UPLOAD);
		dojo.connect(this.emailUploadButton, 'onClick', this, function(){
			this.fileuploader.upload({
				'function':'media_call',
				'channel':this.channelId,
				'command':'attach',
				'args':[{
					'filename':'fileUpload',
					'htmlFieldName':'attachFiles'
				}]
			});
		});

		this.emailSubmit.attr('label', dojo.i18n.getLocalization("agentUI", "EmailPane").SUBMIT);

		dojo.connect(this.emailSubmit, 'onClick', this, function(){
			this.submit();
		});

		this.emailCancel.attr('label', dojo.i18n.getLocalization("agentUI", "EmailPane").CANCEL);

		dojo.connect(this.emailCancel, 'onClick', this, function(){
			this.emailReplyEditor.destroy();
			this.emailView.style.display = 'block';
			this.emailReplyDiv.style.display = 'none';
		});

		this.email = new emailLib.Email(this.channel);
		this.email.getSkeleton();
	},

	submit: function(){
		var coveredNode = this.email.domNode;
		var standby = new dojox.widget.Standby({
			target: coveredNode,
			zIndex:1000
		});
		dojo.doc.body.appendChild(standby.domNode);
		standby.startup();
		standby.show();
		var subscribeChan = 'OpenACD/AgentChannel/' + this.channelId + '/email/mediaevent';
		var sub = dojo.subscribe(subscribeChan, this, function(data){
			if(data.event === 'send_complete'){
				standby.hide();
				if(! data.success){
					errMessage('send mail failed' + data.message);
				} else {
					this.emailReplyEditor.destroy();
					this.emailView.style.display = 'block';
					this.emailReplyDiv.style.display = 'none';
				}
				dojo.unsubscribe(sub);
			}
		});
		var ccs = this.emailCC.attr('value');
		var bccs = this.emailBCC.attr('value');
		// allow split by comma, comma space, comma newline, comma space newline, and newline.
		var regex = /\,\s*|\,|\,?\s*\n/;
		ccs = ccs.split(regex);
		bccs = bccs.split(regex);
		this.agentConnection.agent.agentApi('media_cast', {}, this.channelId, 'send', {
			'to':this.emailTo.attr('value'),
			'from':this.emailFrom.attr('value'),
			'subject':this.emailSubject.attr('value'),
			'cc':ccs,
			'bcc':bccs,
			'body': this.emailReplyEditor.attr('value')
		});
	},

	_handleGetSkeleton: function(skel){
		console.log('got skeleton', skel);
		dojo.unsubscribe(this.skeletonSub);
		this.skeleton = skel;

		this.attachmentList.clearSub = dojo.subscribe('emailPane/attachment/add/' + this.channelId, this.attachmentList, function(){
			this.innerHTML = '';
		});
		
		this.emailToSpan.innerHTML = emailLib.scrubString(skel.headers.to);
		this.emailFromSpan.innerHTML = emailLib.scrubString(skel.headers.from);
		this.emailSubjectSpan.innerHTML = emailLib.scrubString(skel.headers.subject);
		this.emailDateSpan.innerHTML = emailLib.scrubString(skel.headers.date);
		this.emailDateSpanReply.innerHTML = emailLib.scrubString(skel.headers.date);
		this.emailRawHeadersSpan.innerHTML = function(){
			var out = [];
			for(var i in skel.headers){
				out.push(emailLib.scrubString(i) + ': ' + emailLib.scrubString(skel.headers[i]));
			}
			out = out.join('</p><p>');
			return '<p>' + out + '</p>';
		}();
	
		this.emailSubject.attr('value', 're:  ' + emailLib.scrubString(skel.headers.subject));
		this.emailFrom.attr('value', skel.headers.to);
		if(skel.headers['reply-to']){
			this.emailTo.attr('value', skel.headers['reply-to']);
		} else{
			this.emailTo.attr('value', skel.headers.from);
		}
		this._fetchPaths(this.skeleton);
	},

	_locationCheck: function(linkNode){
			if(dojo.isSafari){
				return linkNode.hostname == window.location.host;
			}
			return linkNode.hostname = window.location.hostname && linkNode.port == window.location.port;
	},

	_fetchFromCallback: function(data){
		if(data.success){
			var val = '';
			if(data.data.label){
				val += '"' + data.data.label + '" ';
			}
			val += '<' + data.data.address + '>';
			this.emailFrom.attr('value', val);
		} else {
			this.emailFrom.attr('value', this.skeleton.headers.to.split(',')[0]);
		}
	},

	_fetchPaths: function(skel){
		var paths = emailLib.pathsToFetch(skel);
		var disp = this.emailViewDiv;
		disp.sub = dojo.subscribe("emailLib/fetchPaths/done" + this.channelId, function(fetched){
			console.log('fetching of paths complete', fetched);
			dojo.unsubscribe(disp.sub);
			disp.innerHTML = fetched;
			var nodes = dojo.query('* > img', disp);

			debug(["going through the nodes for images.", nodes]);
			for(var i = 0; i < nodes.length; i++){
				var l = document.createElement('a');
				l.href = nodes[i].src;
			
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
			emailLib.getFrom(_fetchFromCallback);
		});
	},

	_rebuildAttachmentList: function(filenames){
		var listNode = this.attachmentListNode;
		this.filenames = filenames;
		while(listNode.firstChild){
			listNode.removeChild(listNode.firstChild);
		}
		if(filenames.length == 0){
			this.attachmentListP.style.display = 'none';
			return;
		}
		this.attachmentListP.style.display = '';

		for(var i = 0; i < filenames.length; i++){
			var li = listNode.appendChild(dojo.doc.createElement('li'));
			li.innerHTML += filenames[i];
			li.insertBefore(dojo.doc.createElement('input'), li.firstChild);
			var buttonNode = li.firstChild;
			buttonNode.type = 'image';
			buttonNode.src = '/images/redx.png';
			buttonNode.setAttribute('fileIndex', i);
			buttonNode.setAttribute('filename', filenames[i]);
			dojo.connection(li.firstChild, 'onclick', this, function(e){
				var index = parseInt(e.target.getAttribute('fileIndex'));
				var nom = e.target.getAttribute('filename');
				var pubChan = 'emailPane/attachment/drop/' + this.channelId;
				this.channel.webApi('agent', {
					success: function(res){
						dojo.publish(pubChan, [res])
						return true;
					}
				}, this.channelId, 'detach', dojo.toJson([index + 1, nom]))
			});
		}
	}
});

//TODO
	//This could be set up when spying, so disable reply, and allow closability.
/*
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
});*/

});
