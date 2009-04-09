dojo.provide("spice.PredicateEditorRow");
dojo.provide("spice.PredicateEditor");

dojo.declare("PredicateEditorRow", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice","PredicateEditorRow.html"),
	widgetsInTemplate: true,
	templateString: "",
	store:[],
	setComparisons: function(prop){
		var ithis = this;
		var callback = function(res, req){
			console.log(res);
			var items = [];
			for(var i in res[0].comparisons){
				items.push({'label':res[0].comparisons[i].label[0]});
			};
			console.log(items);
			ithis.comparisonField.store = new dojo.data.ItemFileReadStore({
				data:{
					identifier:"label",
					label:"label",
					"items":items
				}
			});
			ithis.valueField.regExp = res[0].regExp[0]
		}
		this.propertyField.store.fetch({
			query:{'label':prop},
			onComplete:callback
		});
	},
	getValues: function(){
		out = {
			"property":this.propertyField.value,
			"comparison":this.comparisonField.value,
			"value":this.valueField.value
		};
		return out;
	}
});

dojo.declare("PredicateEditor", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice", "PredicateEditor.html"),
	widgetsInTemplate:true,
	templateString:"",
	store:[],
	rows:[],
	addRow:function(){
		var ithis = this;
		console.log(this);
		console.log(self);
		var row = new PredicateEditorRow({
			store: this.store
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
	getValues:function(){
		var items = [];
		for(var i in this.rows){
			items.push(dijit.byId(this.rows[i]).getValues());
		}
		return items;
	},
	postCreate:function(){
		this.addRow();
	}
});