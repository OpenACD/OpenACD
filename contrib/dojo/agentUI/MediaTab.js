dojo.provide("agentUI.MediaTab");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");

dojo.declare("agentUI.MediaTab", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("agentUI","MediaTab.html"),
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
