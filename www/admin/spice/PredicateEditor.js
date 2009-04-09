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
			ithis.comparisonField.store.fetch();
		}
		this.propertyField.store.fetch({
			query:{'label':prop},
			onComplete:callback
		});
	}
});