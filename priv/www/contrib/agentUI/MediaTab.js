//>>built
// wrapped by build app
define("agentUI/MediaTab", ["dojo","dijit","dojox","dojo/require!dijit/_Widget,dijit/_Templated,dijit/form/Button"], function(dojo,dijit,dojox){
dojo.provide("agentUI.MediaTab");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");

dojo.declare("agentUI.MediaTab", [dijit._Widget, dijit._Templated], {
	templateString:"<div style=\"width:100%; height: 100%\" class=\"mediaTab\">\n<div dojoType=\"dijit.layout.BorderContainer\" design=\"sidbar\" gutters=\"true\" liveSplitters=\"true\" style=\"width:100%;height:100%\" dojoAttachPoint=\"topNode\">\n\t<div dojoType=\"dojox.layout.ContentPane\" region=\"center\" splitter=\"true\" dojoAttachPoint=\"mediaPane\">This is where media data will go.</div>\n\t<div dojoType=\"dijit.layout.ContentPane\" region=\"trailing\" style=\"text-align:right; width:300px\" splitter=\"true\" dojoAttachPoint=\"controlBar\">\n\n\t\t\t<p>\n\t\t\t\t<label for=\"state\" class=\"narrow\">STATE</label>\n\t\t\t\t<span dojoAttachPoint=\"agentStateNode\"></span>\n\t\t\t</p>\n\t\t\t<p>\n\t\t\t\t<label for=\"brand\" class=\"narrow\">BRAND</label>\n\t\t\t\t<span dojoAttachPoint=\"agentBrandNode\"></span>\n\t\t\t</p>\n\t\t\t<p>\n\t\t\t\t<label for=\"callerid\" class=\"narrow\">CALLERID</label>\n\t\t\t\t<span dojoAttachPoint=\"calleridNode\"></span>\n\t\t\t</p>\n\t\t\t<p>\n\t\t\t\t<label for=\"calltype\" class=\"narrow\">MEDIATYPE</label>\n\t\t\t\t<span dojoAttachPoint=\"callTypeNode\"></span>\n\t\t\t</p>\n\t\t<select dojoAttachPoint=\"outboundcallSelect\"></select>\n\n\t\t<div dojoType=\"dijit.form.DropDownButton\" dojoAttachPoint=\"startTransferDropDown\" style=\"display:none\">\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").TRANSFERTOMENU);\n\t\t\t</script>\n\t\t\t<span>TRANSFERTOMENU</span>\n\t\t\t<div dojoType=\"dijit.Menu\" dojoAttachPoint=\"transfertoMenu\">\n\t\t\t\t<div dojoType=\"dijit.PopupMenuItem\" dojoAttachPoint=\"transferToAgentMenu\">\n\t\t\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").AGENT);\n\t\t\t\t\t</script>\n\t\t\t\t\t<script type=\"dojo/connect\" event=\"onMouseEnter\">\n\t\t\t\t\t\t//window.agentConnection.getAvailAgents();\n\t\t\t\t\t</script>\n\t\t\t\t\t<span>AGENT</span>\n\t\t\t\t\t<div dojoType=\"dijit.Menu\" dojoAttachPoint=\"transferToAgentMenuDyn\">\n\t\t\t\t\t</div>\n\t\t\t\t</div>\n\t\t\t\t<div dojoType=\"dijit.PopupMenuItem\" dojoAttachPoint=\"transferToQueueMenu\">\n\t\t\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").QUEUE);\n\t\t\t\t\t</script>\n\t\t\t\t\t<span>QUEUE</span>\n\t\t\t\t\t<div dojoType=\"dijit.Menu\" dojoAttachPoint=\"transferToQueueMenuDyn\">\n\t\t\t\t\t</div>\n\t\t\t\t</div>\n\t\t\t\t<div dojoType=\"dijit.MenuItem\" label=\"Transfer to 3rd Party\">\n\t\t\t\t\t<script type=\"dojo/connect\" event=\"onClick\">\n\t\t\t\t\t//onclick=\"Agent.warmtransfer('2440131');\">\n\t\t\t\t\t\t// TODO das fix\n\t\t\t\t\t\t//dojo.byId('foo').style.display = 'inline';\n\t\t\t\t\t\tdijit.byId('wtcancel').attr('style', 'display:inline');\n\t\t\t\t\t\tdijit.byId('wtdial').attr('style', 'display:inline');\n\t\t\t\t\t\tdijit.byId('btransfer').attr('style', 'display:none');\n\t\t\t\t\t</script>\n\t\t\t\t</div>\n\t\t\t</div>\n\t\t</div>\n\n\t\t<!-- was id=foo \n\t\t<div id=\"foo\" style=\"clear:both;\">-->\n\t\t\t<input type=\"text\" value=\"\" dojoType=\"dijit.form.ValidationTextBox\" \n\t\t\t\tregExp=\"[\\d]+\" invalidMessage=\"Invalid number - no spaces, parentheses or dashes\" purpose=\"dialbox\" />\n\t\t<!--</div>-->\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"warmtransferDialbutton\" style=\"display:none\">\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").DIAL);\n\t\t\t</script>\n\t\t\t<!--<script type=\"dojo/connect\" event=\"onClick\">\n\t\t\t\tdijit.byId('wtcancel').suppressHide = true;\n\t\t\t\twindow.agentConnection.warmtransfer(dijit.byId('dialbox').attr('value'));\n\t\t\t</script>-->\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"warmtransferCompleteButton\" style=\"display:none\">\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").COMPLETE);\n\t\t\t</script>\n\t\t\t<!--<script type=\"dojo/connect\" event=\"onClick\">\n\t\t\t\twindow.agentConnection.warmtransfercomplete();\n\t\t\t</script>-->\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"warmtransferCancelButton\" style=\"display:none\">\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").CANCEL);\n\t\t\t</script>\n\t\t\t<!--<script type=\"dojo/connect\" event=\"onClick\">\n\t\t\t\tdojo.byId('foo').style.display = 'none';\n\t\t\t\tdijit.byId('wtcancel').attr('style', 'display:none');\n\t\t\t\tdijit.byId('wtcomplete').attr('style', 'display:none');\n\t\t\t\tdijit.byId('wtdial').attr('style', 'display:none');\n\t\t\t\tdijit.byId('btransfer').attr('style', 'display:inline');\n\t\t\t\twindow.agentConnection.warmtransfercancel();\n\t\t\t</script>-->\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"dialButton\" style=\"display:none\"><!-- onclick=window.agentConnection.dial()>-->\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").DIAL);\n\t\t\t</script>\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"cancelButton\" style=\"display:none\"><!-- onclick=window.agentConnection.setState(\"idle\")>-->\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").CANCEL);\n\t\t\t</script>\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"answerButton\">\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").ANSWER);\n\t\t\t</script>\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"hangupButton\" style=\"display:none\"><!-- onclick=window.agentConnection.setState(\"wrapup\")>-->\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").HANGUP);\n\t\t\t</script>\n\t\t</button>\n\t\t<button dojoType=\"dijit.form.Button\" dojoAttachPoint=\"endWrapupButton\" style=\"display:none\">\n\t\t\t<script type=\"dojo/method\" event=\"postCreate\">\n\t\t\t\tthis.inherited(\"postCreate\", arguments);\n\t\t\t\tthis.attr('label', dojo.i18n.getLocalization(\"agentUI\", \"labels\").ENDWRAPUP);\n\t\t\t</script>\n\t\t</button>\n\t</div>\n</div>\n</div>\n",
	widgetsInTemplate: true,
	//templateString: "",

	constructor: function(args, srcNodeRef){
		console.log('media tab', args, srcNodeRef);
		dojo.safeMixin(this, args);
		this.title = args.stateData.type + ' - ' + args.channel;

		this._agentSub = dojo.subscribe("OpenACD/AgentChannel", this, this._handleAgentChannelPublish);
		this._agentCommandSubs = {};

		var subChan = 'OpenACD/AgentChannel/' + this.channel + '/mediaload';

		console.log('media load chan', subChan);
		this._agentCommandSubs.mediaload = dojo.subscribe(subChan, this, function(args){
			console.log("loading media", args);
			this.mediaPane.attr('href', "tabs/" + this.stateData.source_module + ".html");
		});
		/*switch(args.state){
			case 'ringing':
				this.answerButton.domNode.style.display = '';
				break;
		}*/
	},


	postCreate: function(){
		dojo.query('label.narrow', this.domNode).forEach(function(node){
			var text = dojo.i18n.getLocalization("agentUI", "labels")[node.innerHTML];
			node.innerHTML = text;
		});
		this.agentStateNode.innerHTML = dojo.i18n.getLocalization("agentUI", "labels")[this.state.toUpperCase()];

		this.agentBrandNode.innerHTML = this.stateData.brandname;
		this.calleridNode.innerHTML = this.stateData.callerid;
		this.callTypeNode.innerHTML = this.stateData.type;
		switch(this.state){
			case "ringing":
				if(this.stateData.ringpath == "outband"){
					this.answerButton.domNode.style.display = 'none';
				} else {
					this.answerButton.domNode.style.display = 'inline';
				}
				break;
			case "oncall":
				if(this.stateData.mediapath == "outband"){
					this.hangupButton.domNode.style.display = 'none';
				} else {
					this.hangupButton.domNode.style.display = 'inline';
				}
				break;
		}

		dojo.connect(this.answerButton, 'onClick', this, function(){
			window.agentConnection.channels[this.channel].setState('oncall');
		});
		dojo.connect(this.hangupButton, 'onClick', this, function(){
			window.agentConnection.channels[this.channel].setState('wrapup');
		});
		dojo.connect(this.endWrapupButton, 'onClick', this, function(){
			window.agentConnection.channels[this.channel].endWrapup();
		});
		dojo.connect(this.mediaPane, 'onDownloadEnd', this, function(){
			console.log('onDownloadEnd start');
			var inits = dojo.query('[init]', this.mediaPane.domNode);
			if(inits.length < 1){
				return;
			}
			var initFunc = inits.attr('init')[0];
			if(typeof(window[initFunc]) == 'function'){
				console.log('init function found');
				try{
					window[initFunc](this.channel, this.mediaPane.domNode);
					console.log('inits function call complete');
				} catch(err) {
					console.error('init failed', err);
				}
			}
		});
	},

	_handleAgentChannelPublish: function(channelId, args){
		if(channelId != this.channel){
			return false;
		}
		console.log('event', this, arguments);
		if(arguments.length > 1){
			var stateText = dojo.i18n.getLocalization("agentUI", "labels")[arguments[1].toUpperCase()];
			if(! stateText){
				stateText = this.agentStateNode.innerHTML;
			}
		} else {
			stateText = this.agentStateNode.innerHTML;
		}
		this.agentStateNode.innerHTML = stateText;
		switch(args){

			case 'oncall':
				this.answerButton.domNode.style.display = 'none';
				if(arguments[2].mediapath == 'inband'){
					this.hangupButton.domNode.style.display = 'inline';
				}
				break;

			case 'wrapup':
				this.answerButton.domNode.style.display = 'none';
				this.hangupButton.domNode.style.display = 'none';
				this.endWrapupButton.domNode.style.display = 'inline';
				break;
		}
	}/*,
	doAnswer: function(opts){
		if(opts.mode == 'href'){
			this.mediaPane.attr('href', opts.content);
		} else {
			this.mediaPane.attr('content', opts.content);
		}
	}*/
});

});
