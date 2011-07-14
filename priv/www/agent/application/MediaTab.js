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
	},
	_handleAgentChannelPublish: function(channelId, args){
		if(channelId != this.channel){
			return false;
		}
		console.log('event', arguments);
	}/*,
	doAnswer: function(opts){
		if(opts.mode == 'href'){
			this.mediaPane.attr('href', opts.content);
		} else {
			this.mediaPane.attr('content', opts.content);
		}
	}*/
});
