//>>built
define("dojox/mobile/Button", [
	"dojo/_base/array",
	"dojo/_base/declare",
	"dojo/dom-class",
	"dojo/dom-construct",
	"dijit/_WidgetBase",
	"dijit/form/_ButtonMixin",
	"dijit/form/_FormWidgetMixin"
],
	function(array, declare, domClass, domConstruct, WidgetBase, ButtonMixin, FormWidgetMixin){

	/*=====
		WidgetBase = dijit._WidgetBase;
		FormWidgetMixin = dijit.form._FormWidgetMixin;
		ButtonMixin = dijit.form._ButtonMixin;
	=====*/
	return declare("dojox.mobile.Button", [WidgetBase, FormWidgetMixin, ButtonMixin], {
		// summary:
		//	Non-templated BUTTON widget with a thin API wrapper for click events and setting the label
		//
		// description:
		//              Buttons can display a label, an icon, or both.
		//              A label should always be specified (through innerHTML) or the label
		//              attribute.  It can be hidden via showLabel=false.
		// example:
		// |    <button dojoType="dijit.form.Button" onClick="...">Hello world</button>

		baseClass: "mblButton",

		// duration: Number
		//	duration of selection, milliseconds or -1 for no post-click CSS styling
		duration: 1000,

		_onClick: function(e){
			var ret = this.inherited(arguments);
			if(ret && this.duration >= 0){ // if its not a button with a state, then emulate press styles
				var button = this.focusNode || this.domNode;
				var newStateClasses = (this.baseClass+' '+this["class"]).split(" ");
				newStateClasses = array.map(newStateClasses, function(c){ return c+"Selected"; });
				domClass.add(button, newStateClasses);
				setTimeout(function(){
					domClass.remove(button, newStateClasses);
				}, this.duration);
			}
			return ret;
		},

		isFocusable: function(){ return false; },

		buildRendering: function(){
			if(!this.srcNodeRef){
				this.srcNodeRef = domConstruct.create("button", {"type": this.type});
			}
			this.inherited(arguments);
			this.focusNode = this.domNode;
		},

		postCreate: function(){
			this.inherited(arguments);
			this.connect(this.domNode, "onclick", "_onClick");
		}
	});

});
