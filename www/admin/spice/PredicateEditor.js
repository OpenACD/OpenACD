dojo.provide("spice.PredicateEditorRow");
dojo.provide("spice.PredicateEditor");

dojo.declare("PredicateEditorRow", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice","PredicateEditorRow.html"),
	widgetsInTemplate: true,
	templateString: "",
	constructor: function(args){
		this.store = [];
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		this._disabled = false;
		dojo.safeMixin(this, args);
	},
	setComparisons: function(prop){
		var ithis = this;
		var callback = function(res, req){
			console.log(["setComparisions req", req]);
			var items = [];
			if(res.length < 1){
				return;
			}
			var res = res[0];
			var comparisons = req.store.getValues(res, 'comparisons');
			console.log(["comparisons", comparisons]);
			for(var i = 0; i < comparisons.length; i++){
				items.push({
					'label':req.store.getValue(comparisons[i], 'label'),
					'value':req.store.getValue(comparisons[i], 'value')
				});
			}
			ithis.comparisonField.store = new dojo.data.ItemFileReadStore({
				data:{
					identifier:"value",
					label:"label",
					"items":items
				}
			});
			ithis.valueField.regExp = req.store.getValue(res, 'regExp');
		}
		this.propertyField.store.fetch({
			query:{
				'value':prop,
				'type':'property'
			},
			onComplete:callback
		});
	},
	getValue: function(){
		out = {
			"property":this.propertyField.value,
			"comparison":this.comparisonField.value,
			"value":this.valueField.value
		};
		return out;
	},
	setValue: function(obj){
		this.propertyField.attr('value', obj.property);
		this.comparisonField.attr('value', obj.comparison);
		this.valueField.attr('value', obj.value);
	},
	setDisabled: function(bool){
		this._disabled = bool;
		this.propertyField.attr('disabled', bool);
		this.comparisonField.attr('disabled', bool);
		this.valueField.attr('disabled', bool);
	}
});

dojo.declare("PredicateEditor", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice", "PredicateEditor.html"),
	widgetsInTemplate:true,
	templateString:"",
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
	},
	dropRow:function(rowid){
		if(this._disabled){
			return false
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
	},
	getValue:function(){
		var items = [];
		for(var i in this.rows){
			items.push(dijit.byId(this.rows[i]).getValue());
		}
		return items;
	},
	setValue:function(list){
		for(var i = 0; i < this.rows.length; i++){
			try{
				dijit.byId(this.rows[i]).destroy();
			}
			catch(err){
				//Do nothing w/ the error, just ignore it.
			}
		}
		this.rows = [];
		for(var i = 0; i < list.length; i++){
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
			dijit.byId(this.rows[i]).setDisabled(bool)
		}
	}
});