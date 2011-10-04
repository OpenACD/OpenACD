//>>built
define("dojox/dtl/_DomTemplated", [
	"dojo/_base/kernel",
	"dojo/dom-construct",
	".",
	"./contrib/dijit",
	"./render/dom",
	"dojo/cache",
	"dijit/_TemplatedMixin"
	], function(dojo,domconstruct,dtl,ddcd,ddrd,cache,TemplatedMixin){
	/*=====
		dtl = dojox.dtl;
		cache = dojo.cache;
		Templated = dijit._Templated
	=====*/
	dtl._DomTemplated = function(){};
	dtl._DomTemplated.prototype = {
		_dijitTemplateCompat: false,
		buildRendering: function(){
			//	summary:
			//		Construct the UI for this widget, setting this.domNode.

			//render needs a domNode to work with
			this.domNode = this.srcNodeRef;

			if(!this._render){
				var old = ddcd.widgetsInTemplate;
				ddcd.widgetsInTemplate = this.widgetsInTemplate;
				this.template = this.template || this._getCachedTemplate(this.templatePath, this.templateString);
				this._render = new ddrd.Render(this.domNode, this.template);
				ddcd.widgetsInTemplate = old;
			}

			var context = this._getContext();
			if(!this._created){
				delete context._getter;
			}
			this.render(context);

			this.domNode = this.template.getRootNode();
			if(this.srcNodeRef && this.srcNodeRef.parentNode){
				domconstruct.destroy(this.srcNodeRef);
				delete this.srcNodeRef;
			}
		},
		setTemplate: function(/*String|dojo._Url*/ template, /*dojox.dtl.Context?*/ context){
			// summary:
			//		Quickly switch between templated by location
			if(dojox.dtl.text._isTemplate(template)){
				this.template = this._getCachedTemplate(null, template);
			}else{
				this.template = this._getCachedTemplate(template);
			}
			this.render(context);
		},
		render: function(/*dojox.dtl.Context?*/ context, /*dojox.dtl.DomTemplate?*/ tpl){
			if(tpl){
				this.template = tpl;
			}
			this._render.render(this._getContext(context), this.template);
		},
		_getContext: function(context){
			if(!(context instanceof dojox.dtl.Context)){
				context = false;
			}
			context = context || new dojox.dtl.Context(this);
			context.setThis(this);
			return context;
		},
		_getCachedTemplate: function(templatePath, templateString){
			if(!this._templates){
				this._templates = {};
			}
			if(!templateString){
				templateString = cache.cache(templatePath, {sanitize: true});
			}
			var key = templateString;
			var tmplts = this._templates;
			if(tmplts[key]){
				return tmplts[key];
			}
			return (tmplts[key] = new dojox.dtl.DomTemplate(
				TemplatedMixin.getCachedTemplate(
					templateString,
					true
				)
			));
		}
	};
	return dojox.dtl._DomTemplated;
});

