dojo.require("dojox.form.DropDownSelect");

dojo.provide("spice.RecipeEditorRow");
dojo.provide("spice.RecipeEditor");

dojo.declare("RecipeEditorRow", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice","RecipeEditorRow.html"),
	widgetsInTemplate: true,
	templateString: "",
	propwidth:"20em",
	compwidth:"10em",
	valwidth:"20em",
	conditions:[{
		"property":"ticks",
		"comparison":"=",
		"value":"1"}],
	argsWidget:{
		getValue:function(){return ""},
		setValue:function(){}
	},
	_nullArgsWidget:function(){
		this.argsWidget = {
			setValue:function(){},
			getValue:function(){return ""}
		}
	},
	_buildSelect:function(select){
		select.size = 3;
		select.getValue = function(){
			var out = [];
			var kids = this.childNodes;
			for(var i in kids){
				if(kids[i].tagName == 'OPTGROUP'){
					var opts = kids[i].childNodes;
					for(var j in opts){
						if(opts[j].selected){
							out.push(opts[j].value);
						}
					}
				}
				else{
					if(kids[i].selected){
						out.push(kids[i].value);
					}
				}
			}
			return out;
		}
		var ithis = this;
		select.setValue = function(values){
			if(! values){
				values = [];
			}
			
			if(values.constructor == String){
				values = [values];
			}
			console.log(values);
			var isSelected = function(val){
				for(var i in values){
					if(values[i] == val){
						return true;
					}
				}
				return false;
			}
			
			var kids = this.childNodes;
			console.log(kids);
			for(var i in kids){
				if(kids[i].tagName == 'OPTGROUP'){
					var opts = kids[i].childNodes;
					for(var j in opts){
						opts[j].selected = isSelected(opts[j].value);
					}
				}
			}
		};
		return select;
	},
	setArguments: function(action){
		switch(action){
			case "add_skills":
				var ithis = this;
				var argdiv = this.argumentsDiv;
				var callback = function(select){
					select.size = 3;
					argdiv.setContent(select);
					ithis.argsWidget = ithis._buildSelect(select);
				};
				skills.newSelection(callback, [], [], []);
			break;
			
			case "remove_skills":
				var argdiv = this.argumentsDiv;
				var ithis = this;
				var callback = function(select){
					select.size = 3;
					argdiv.setContent(select);
					ithis.argsWidget = ithis._buildSelect(select);
				}
				skills.newSelection(callback, [], [], []);
			break;
			
			case "set_priority":
				var argsWidget = new dijit.form.ValidationTextBox({
					regExp:"[\\d]+",
					style:"width:5em"
				});
				this.argumentsId = argsWidget.id;
				this.argumentsDiv.setContent(argsWidget.domNode);
				this.argsWidget = argsWidget
			break;
			
			case "prioritize":
				this.argumentsDiv.setContent("");
				this._nullArgsWidget();
			break;
			
			case "deprioritize":
				this.argumentsDiv.setContent("");
				this._nullArgsWidget();
			break;
			
			case "voicemail":
				this.argumentsDiv.setContent("");
				this._nullArgsWidget();
			break;
			
			case "announce":
				var argsWidget = new dijit.form.TextBox({
					style:"width:10em"
				});
				this.argumentsDiv.setContent(argsWidget.domNode);
				this.argsWidget = argsWidget
			 break;
			
			case "add_recipe":
				//TODO : implement real recusive recipe-age.
				this._nullArgsWidget();
			break;
		}
	},
	getValue:function(){
		obj = {
			conditions: this.conditions,
			action: this.actionField.getValue(),
			"arguments": this.argsWidget.getValue(),
			runs: this.runsField.getValue()
		};
		return obj;
	},
	getConditions:function(){
		return this.conditions;
	},
	setValue:function(recipeStep){
		console.log(recipeStep);
		this.conditions = recipeStep.conditions;
		this.actionField.setValue(recipeStep.action);
		this.setArguments(recipeStep.action);
		this.argsWidget.setValue(recipeStep.arguments);
		this.runsField.setValue(recipeStep.runs);
	},
	setConditions:function(conditions){
		this.conditions = conditions;
	},
	postCreate:function(){
		console.log(this.argsWidget);
		this.setArguments("add_skills");
		this.inherited("postCreate", arguments);
	}
});

dojo.declare("RecipeEditor", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice","RecipeEditor.html"),
	widgetsInTemplate: true,
	templateString: "",
	propwidth:"20em",
	compwidth:"10em",
	valwidth:"20em",
	_focusedOn:null,
	rows:[],
	addRow: function(){
		var ithis = this;
		var row = new RecipeEditorRow({
			propwidth: this.propwidth,
			compwidth: this.compwidth,
			valwidth: this.valwidth,
			style:"padding-left:2em;background-color:#ffffff"
		});
		row.addButton.onClick = function(){
			ithis.addRow();
		};
		row.dropButton.onClick = function(){
			ithis.dropRow(row.id);
		}
		row.onFocus = function(){
			if(dijit.byId(ithis._focusedOn)){
				dijit.byId(ithis._focusedOn).setConditions(ithis.conditionsEditor.getValue());
				dijit.byId(ithis._focusedOn).domNode.style.backgroundColor = "#ffffff";
			}
			ithis.conditionsEditor.setValue(row.getConditions());
			ithis._focusedOn = row.id;
			row.domNode.style.backgroundColor = "#ccffff";
		}
		this.rows.push(row.id);
		this.stepsContainer.appendChild(row.domNode);
		this.nullButton.domNode.style.display = "none";
		this.conditionsEditor.domNode.style.display = "table";
	},
	dropRow: function(rowid){
		dijit.byId(rowid).destroy();
		var newrows = [];
		for(var i in this.rows){
			if(this.rows[i] != rowid){
				newrows.push(this.rows[i]);
			}
		};
		this.rows = newrows;
		if(this.rows.length == 0){
			this.nullButton.domNode.style.display = "inline";
			this.conditionsEditor.domNode.style.display = "none";
		}
	},
	postCreate: function(){
		this.addRow();
	},
	getValue:function(){
		var out = [];
		for(var i in this.rows){
			out.push(dijit.byId(this.rows[i]).getValue());
		}
		return out;
	},
	setValue:function(value){
		var cpyrows = this.rows;
		for(var i in cpyrows){
			dijit.byId(cpyrows[i]).destroy();
		}
		this.rows = [];
		for(var i in value){
			this.addRow();
			console.log('setValue');
			console.log(value[i]);
			dijit.byId(this.rows[i]).setValue(value[i]);
		}
		if(this.rows.length == 0){
			 this.nullButton.domNode.style.display = "inline";
			 this.conditionsEditor.domNode.style.display = "none";
		}
	}
});