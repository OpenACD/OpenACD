//>>built
define("dojox/form/uploader/plugins/IFrame", [
	"dojo/dom-construct",
	"dojo/_base/declare",
	"dojo/_base/lang",
	"dojo/_base/array",
	"dojo/io/iframe",
	"dojox/form/uploader/plugins/HTML5"
],function(domConstruct, declare, lang, array, ioIframe, formUploaderPluginsHTML5){
	

var pluginsIFrame = declare("dojox.form.uploader.plugins.IFrame", [], {
	//
	// Version: 1.6
	//
	// summary:
	//		A plugin for dojox.form.Uploader that adds Ajax upload capabilities.
	//
	//	description:
	//		Only supported by IE, due to the specifc iFrame hack used. The
	//		formUploaderPluginsHTML5 plugin should be used along with this to add HTML5
	//		capabilities to browsers that support them. Progress events are not supported.
	//		Inherits all properties from dojox.form.Uploader and formUploaderPluginsHTML5.
	//

	force:"",

	postMixInProperties: function(){
		this.inherited(arguments);
		if(!this.supports("multiple") || this.force =="iframe"){
			this.uploadType = "iframe";
			this.upload = this.uploadIFrame;
			this.submit = this.submitIFrame;
		}
	},

	submitIFrame: function(data){
		this.uploadIFrame(data);
	},

	uploadIFrame: function(data){
		// summary:
		//		Internal. You could use this, but you should use upload() or submit();
		//		which can also handle the post data.
		//
		var form, destroyAfter = false;
		if(!this.getForm()){
			form = domConstruct.create('form', {enctype:"multipart/form-data", method:"post"}, this.domNode);
			array.forEach(this._inputs, function(n, i){
				if(n.value) form.appendChild(n);
			}, this);
			destroyAfter = true;
		}else{
			form = this.form;
		}

		var url = this.getUrl();

		var dfd = ioIframe.send({
			url: url,
			form: form,
			handleAs: "json",
			error: lang.hitch(this, function(err){
				if(destroyAfter){ domConstruct.destroy(form); }
				this.onError(err);
			}),
			load: lang.hitch(this, function(data, ioArgs, widgetRef){
				if(destroyAfter){ domConstruct.destroy(form); }
				if(data["ERROR"] || data["error"]){
					this.onError(data);
				}else{
					this.onComplete(data);
				}
			})
		});
	}
});

dojox.form.addUploaderPlugin(pluginsIFrame);

return pluginsIFrame;
});
