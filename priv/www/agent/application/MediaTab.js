dojo.provide("agentUI.MediaTab");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");

dojo.declare("MediaTab", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("agentUI","MediaTab.html"),
	widgetsInTemplate: true,
	templateString: "",
	constructor: function(args, srcNodeRef){
		dojo.safeMixin(this, args);
	},
	doAnswer: function(opts){
		if(opts.mode == 'href'){
			this.mediaPane.attr('href', opts.content);
		} else {
			this.mediaPane.attr('content', opts.content);
		}
	}
});
