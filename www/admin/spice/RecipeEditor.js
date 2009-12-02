dojo.require("dijit.form.Select");

dojo.provide("spice.RecipeEditorRow");
dojo.provide("spice.RecipeEditor");

dojo.requireLocalization("admin", "recipeEditor");

dojo.declare("RecipeEditorRow", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice","RecipeEditorRow.html"),
	widgetsInTemplate: true,
	templateString: "",
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
			var isSelected = function(val){
				for(var i in values){
					if(values[i] == val){
						return true;
					}
				}
				return false;
			}
			
			var kids = this.childNodes;
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
		if(this._suppressNextSetArgs){
			delete this._suppressNext;
			return;
		}
		switch(action){
			case "add_skills":
				var ithis = this;
				var argdiv = this.argumentsDiv;
				var callback = function(select){
					select.size = 3;
					argdiv.attr('content', select);
					ithis.argsWidget = ithis._buildSelect(select);
				};
				skills.newSelection(callback, [], [], []);
			break;
			
			case "remove_skills":
				var argdiv = this.argumentsDiv;
				var ithis = this;
				var callback = function(select){
					select.size = 3;
					argdiv.attr('content', select);
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
				this.argumentsDiv.attr('content', argsWidget.domNode);
				this.argsWidget = argsWidget
			break;
			
			case "prioritize":
				this.argumentsDiv.attr('content', "");
				this._nullArgsWidget();
			break;
			
			case "deprioritize":
				this.argumentsDiv.attr('content', "");
				this._nullArgsWidget();
			break;
			
			case "voicemail":
				this.argumentsDiv.attr('content', "");
				this._nullArgsWidget();
			break;
			
			case "announce":
				var argsWidget = new dijit.form.TextBox({
					style:"width:10em"
				});
				this.argumentsDiv.attr('content', argsWidget.domNode);
				this.argsWidget = argsWidget
			 break;
			
			case "add_recipe":
				//TODO : implement real recusive recipe-age.
				this._nullArgsWidget();
			break;
		}
		if(this.argsWidget.attr){
			this.argsWidget.attr('disabled', this._disabled);
		} else if(this.argsWidget.setDisabled) {
			this.argsWidget.setDisabled(this._disabled);
		}
	},
	getValue:function(){
		var args = "";
		if(this.argsWidget.attr){
			args = this.argsWidget.attr('value');
		} else{
			args = this.argsWidget.getValue();
		}
		obj = {
			conditions: this.conditions,
			action: this.actionField.attr('value'),
			"arguments": args,
			runs: this.runsField.getValue()
		};
		return obj;
	},
	getConditions:function(){
		return this.conditions;
	},
	setValue:function(recipeStep){
		this.conditions = recipeStep.conditions;
		this.actionField.attr('value', recipeStep.action);
		this.setArguments(recipeStep.action);
		this._suppressNextSetArgs = true;
		if(this.argsWidget.attr){
			this.argsWidget.attr('value', recipeStep.arguments);
		} else {
			this.argsWidget.setValue(recipeStep.arguments);
		}
		this.runsField.attr('value', recipeStep.runs);
	},
	setConditions:function(conditions){
		this.conditions = conditions;
	},
	constructor:function(){
		this.labels = dojo.i18n.getLocalization("admin", "recipeEditor");
		//this.inherited("constructor", arguments);
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		//labels:[],
		this.conditions = [{
			"property":"ticks",
			"comparison":"=",
			"value":"1"}];
		this.argsWidget = {
			 getValue:function(){return ""},
			 setValue:function(){},
			 attr:function(){}
		};
		this._disabled = false;
	},
	postCreate:function(){
		//this.setArguments("add_skills");
		//this.inherited("postCreate", arguments);
		this.actionField.setArgumentsConn = dojo.connect(this.actionField, 'onChange', this, function(arg){
			this.setArguments(arg)
		});
	},
	setDisabled:function(bool){
		//this.conditions.setDisabled(bool);
		this._disabled = bool;
		this.actionField.attr('disabled', bool);
		if(this.argsWidget.attr){
			this.argsWidget.attr('disabled', bool);
		} else if(this.argsWidget.setDisabled){
			this.argsWidget.setDisabled(bool);
		}
		this.runsField.attr('disabled', bool);
	}
});

dojo.declare("RecipeEditor", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("spice","RecipeEditor.html"),
	widgetsInTemplate: true,
	templateString: "",
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
			ithis.conditionsEditor.setDisabled(ithis._disabled);
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
	constructor: function(arg){
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		this._focusedOn = null;
		this.rows = [];
		this._disabled = false;
	},
	postCreate: function(){
		this.addRow();
	},
	getValue:function(){
		//lock in any changes made to currently selected row conditions.
		if(dijit.byId(this._focusedOn)){
			dijit.byId(this._focusedOn).setConditions(this.conditionsEditor.getValue());
		}
			 
		var out = [];
		for(var i = 0; i < this.rows.length; i++){
			out.push(dijit.byId(this.rows[i]).getValue());
		}
		return out;
	},
	setValue:function(value){
		var cpyrows = this.rows;
		for(var i in cpyrows){
			try{
				dijit.byId(cpyrows[i]).destroy();
			}
			catch(err){
				//destroy failed due to + err.message);
			}
		}
		this.rows = [];
		for(var i = 0; i < value.length; i++){
			this.addRow();
			dijit.byId(this.rows[i]).setValue(value[i]);
		}
		if(this.rows.length == 0){
			 this.nullButton.domNode.style.display = "inline";
			 this.conditionsEditor.domNode.style.display = "none";
		}
	},
	setDisabled:function(bool){
		this._disabled = bool;
		for(var i = 0; i < this.rows.length; i++){
			var dahrow = dijit.byId(this.rows[i]);
			dahrow.setDisabled(bool);
			dahrow.addButton.attr('disabled', bool);
			dahrow.dropButton.attr('disabled', bool);
		}
		this.nullButton.attr('disabled', true);
	}
});