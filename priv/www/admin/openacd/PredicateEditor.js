dojo.provide("openacd.PredicateEditorRow");
dojo.provide("openacd.PredicateEditor");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");

dojo.declare("PredicateEditorRow", [dijit._Widget, dijit._TemplatedMixin, dijit._WidgetsInTemplateMixin], {
	//templatePath: dojo.moduleUrl("openacd","PredicateEditorRow.html"),
	//widgetsInTemplate: true,
	//templateString: "",
	templateString: '<div dojoType="dijit.layout.ContentPane">' + 
	'<input dojoType="dijit.form.FilteringSelect" dojoAttachPoint="propertyField" name="property" ' + 
	'store="${store}" searchAttr="label" query="{type:\'property\'}" ' +
	'dojoAttachEvent=\'onChange: setComparisons\' style="width:${propwidth}">' +
	'</input>' +
	'<input dojoType="dijit.form.FilteringSelect" dojoAttachPoint="comparisonField" name="comparison" store="${store}" searchAttr="label" style="width:${compwidth}" />' +
	'<input dojoType="dijit.form.ValidationTextBox" regExp="" dojoAttachPoint="valueField" name="value" style="width:${valwidth}">' +
	'</input>' +
	'<button dojoType="dijit.form.Button" dojoAttachPoint="dropButton" label="-"></button>' +
	'<button dojotype="dijit.form.Button" dojoAttachPoint="addButton" label="+"></button>' +
	'</div>',
	constructor: function(args){
		this.store = [];
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		this._disabled = false;
		dojo.safeMixin(this, args);
	},
	postCreate: function(){
		if(dojo.isArray(this.store)){
			if(this.store.length > 0 && dojo.isString(this.store[0])){
				this.store = eval(this.store[0]);
			} else {
				this.store = new dojo.store.Memory({data:this.store});
			}
		} else if(dojo.isString(this.store)){
			this.store = eval(this.store);
		}
	},
	setComparisons: function(prop){
		var propData = this.store.get(prop);
		var desiredComps = dojo.map(propData.comparisons, function(instr){
			return 'comp-' + instr;
		});
		var allComps = this.store.query({type:'comparison'});
		var outComps = [];
		for(var i = 0; i < allComps.length; i++){
			if(dojo.indexOf(desiredComps, allComps[i].id) >= 0){
				outComps.push(allComps[i]);
			}
		}
		this.comparisonField.store = new dojo.store.Memory({data:outComps});
		var regex = ".*";
		switch(propData.filter){
			case "integer":
				regex = "[\\d]+";
				break;
			case "number":
				regex = "[\\d]+\\.[\\d]*|[\\d]*\\.[\\d]+";
				break;
			case "regex":
				regex = req.store.getValue(res, "regex");
				break;
			default:
				regex = ".*";
		}
		this.valueField.regExp = regex;
		this.valueField.predFilter = propData.filter;
	},
	getValue: function(){
		var outval = "";
		var filter = this.valueField.predFilter;
		switch(filter){
			case "number":
				outval = parseFloat(this.valueField.value);
				break;
			case "integer":
				outval = parseInt(this.valueField.value, 10);
				break;
			default:
				outval = this.valueField.value;
		}
		out = {
			"property":this.propertyField.value,
			"comparison":this.comparisonField.value,
			"value":outval
		};
		return out;
	},
	setValue: function(obj){
		this.propertyField.set('value', obj.property);
		this.comparisonField.set('value', obj.comparison);
		this.valueField.set('value', obj.value);
	},
	setDisabled: function(bool){
		this._disabled = bool;
		this.propertyField.set('disabled', bool);
		this.comparisonField.set('disabled', bool);
		this.valueField.set('disabled', bool);
		this.addButton.set('disabled',  bool);
		this.dropButton.set('disabled', bool);
	}
});

dojo.declare("PredicateEditor", [dijit._Widget, dijit._TemplatedMixin, dijit._WidgetsInTemplateMixin], {
	//templatePath: dojo.moduleUrl("openacd", "PredicateEditor.html"),
	//widgetsInTemplate:true,
	//templateString:"",
	templateString: '<div dojoAttachPoint="topNode" style="display:table"></div>',
	propwidth : "20em",
	compwidth : "10em",
	valwidth : "20em",
	store : [],
	constructor:function(){
		/*this.store = [];
		this.rows = [];
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";*/
		this.store = [];
		this.rows = [];
		this._disabled = false;
		dojo.safeMixin(this, arguments);
		// because the mixin seems to be fail.
		this.store = arguments[0].store;
	},
	postCreate:function(){
		if(dojo.isArray(this.store)){
			if(this.store.length > 0 && dojo.isString(this.store[0])){
				this.store = eval(this.store[0]);
			} else {
				this.store = new dojo.store.Memory({data:this.store});
			}
		} else if(dojo.isString(this.store)){
			this.store = eval(this.store);
		}
	},
	addRow:function(){
		var ithis = this;
		var row = new PredicateEditorRow({
			store: this.store,
			propwidth: this.propwidth,
			compwidth: this.compwidth,
			valwidth: this.valwidth
		});
		row.addButton.onClick = function(){
			ithis.addRow();
		};
		row.dropButton.onClick = function(){
			ithis.dropRow(row.id);
		};
		this.rows.push(row.id);
		this.topNode.appendChild(row.domNode);
		if(this.rows.length > 1){
			dijit.byId(this.rows[0]).dropButton.set('disabled', false);
		}
	},
	dropRow:function(rowid){
		if(this._disabled){
			return false;
		}
		if(this.rows.length < 2){
			return;
		}
		dijit.byId(rowid).destroy();
		var newrows = [];
		for(var i in this.rows){
			if(this.rows[i] != rowid){
				newrows.push(this.rows[i]);
			}
		}
		this.rows = newrows;
		if(this.rows.length == 1){
			dijit.byId(this.rows[0]).dropButton.set('disabled', true);
		}
	},
	getValue:function(){
		var items = [];
		for(var i = 0; i < this.rows.length; i++){
			items.push(dijit.byId(this.rows[i]).getValue());
		}
		return items;
	},
	setValue:function(list){
		////onsole.log('Predicate editor set value hit');
		for(var i = 0; i < this.rows.length; i++){
			try{
				dijit.byId(this.rows[i]).destroy();
			}
			catch(err){
				//Do nothing w/ the error, just ignore it.
			}
		}
		this.rows = [];
		////onsole.log('adding row');
		for(i = 0; i < list.length; i++){
			this.addRow();
			dijit.byId(this.rows[i]).setValue(list[i]);
		}
	},
	postCreate:function(){
		this.addRow();
	},
	setDisabled:function(bool){
		this._disabled = bool;
		for(var i = 0; i < this.rows.length; i++){
			dijit.byId(this.rows[i]).setDisabled(bool);
		}
	}
});
