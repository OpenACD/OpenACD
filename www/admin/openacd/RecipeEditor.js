dojo.require("dijit.form.Select");
dojo.require("dijit.form.FilteringSelect");
dojo.require("dojo.data.ItemFileReadStore");
dojo.require("dijit.layout.AccordionContainer");

dojo.provide("openacd.RecipeEditorRow");
dojo.provide("openacd.RecipeEditor");

dojo.requireLocalization("admin", "recipeEditor");

dojo.declare("RecipeEditorAction", [dijit._Widget, dijit._Templated], {
	// actions go in rows, which go in the editor
	widgetsInTemplate: true,
	templateString: '<div><select dojoType="dijit.form.FilteringSelect" dojoAttachPoint="actionField" name="action" style="width:12em">' +
		'<option value="add_skills">ADDSKILLS</option>' +
		'<option value="remove_skills">REMOVESKILLS</option>' +
		'<option value="set_priority">SETPRIORITY</option>' +
		'<option value="prioritize">PRIORITIZE</option>' +
		'<option value="deprioritize">DEPRIORITIZE</option>' +
		'<option value="voicemail">SENDTOVOICEMAIL</option>' +
		'<option value="announce">MEDIAANNOUCE</option>' +
		'<option value="add_recipe">ADDRECIPE</option>' +
	'</select>' +
	'<div dojoType="dijit.layout.ContentPane" dojoAttachPoint="argumentsDiv" style="display:inline"></div>' +
	'<button dojoType="dijit.form.Button" dojoAttachPoint="dropButton" label="DROPSTEP"></button>' +
	'<button dojotype="dijit.form.Button" dojoAttachPoint="addButton" label="ADDSTEP"></button></div>',
	_nullArgsWidget: function(){
		console.log('_nullArgsWidget');
		this.argsWidget = {
			getValue:function(){},
			setValue:function(){ return ""; }
		};
	},
	_buildSelect: function(select){
		console.log('building select')
		select.size = 3;
		select.getValue = function(){
			return select.getValues();
		};
		select.setValue = function(values){
			if(! values){
				values = [];
			}
			if(values.constructor == String){
				values = [values];
			}
			var nodes = dojo.query('> optgroup > option', select);
			for(var i = 0; i < nodes.length; i++){
				console.log(nodes[i]);
				if(openacd.inArray(nodes[i].value, values)){
					nodes[i].selected = true;
				}
			}
		};
	},
	setArguments: function(action, args){
		console.log('setting arguments');
		if(this._suppressNextSetArgs){
			delete this._suppressNextSetArgs;
			return;
		}
		switch(action){
			case "add_skills":
			case "remove_skills":
				var ithis = this;
				var argdiv = this.argumentsDiv;
				var callback = function(select){
					select.size = 3;
					argdiv.attr('content', select);
					ithis.argsWidget = ithis._buildSelect(select);
				};
				var selected = [];
				if(args){
					selected = args;
				}
				skills.createSelect(callback, selected, ['_agent'], ['_profile']);
				break;
			case "set_priority":
				var argsWidget = new dijit.form.ValidationTextBox({
					regExp:"[\\d]+",
					style:"width:5em"
				});
				this.argumentsId = argsWidget.id;
				this.argumentsDiv.attr('content', argsWidget.domNode);
				this.argsWidget = argsWidget;
				break;
			case "prioritize":
			case "deprioritize":
			case "voicemail":
				this.argumentsDiv.attr('content', "");
				this._nullArgsWidget();
				break;
			case "announce":
				var argsWidgetAnnounce = new dijit.form.TextBox({
					style:"width:10em"
				});
				this.argumentsDiv.attr('content', argsWidgetAnnounce.domNode);
				this.argsWidget = argsWidgetAnnounce;
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
			 action: this.actionField.attr('value'),
			 "arguments": args
		};
		return obj;
	},
	setValue:function(actionObj){
		console.log(['in the setValue', this, this.actionField]);
		this.actionField.attr('value', actionObj.action);
		console.log('actionFiled set');
		this.setArguments(actionObj.action, actionObj['arguments']);
		this._suppressNextSetArgs = true;
		if(! openacd.inArray(actionObj.action, ['add_skills', 'remove_skills'])){
			if(this.argsWidget.attr){
				this.argsWidget.attr('value', actionObj['arguments']);
			} else {
				this.argsWidget.setValue(actionObj['arguments']);
			}
		}
	},
	postCreate:function(){
		//this.setArguments("add_skills");
		//this.inherited("postCreate", arguments);
		this.actionField.setArgumentsConn = dojo.connect(this.actionField, 'onChange', this, function(arg){
			this.setArguments(arg);
		});
		var nodes = this.actionField.store.root;
		console.log(['das nodes', nodes]);
		for(var i = 0; i < nodes.childNodes.length; i++){
			if(nodes.childNodes[i].nodeType == 1){
				var untransed = nodes.childNodes[i].innerHTML;
				nodes.childNodes[i].innerHTML = dojo.i18n.getLocalization('admin', 'recipeEditor')[untransed];
			}
		}
		console.log(['relabing buttons', this.addButton, this.dropButton]);
		this.addButton.attr('label', dojo.i18n.getLocalization('admin', 'recipeEditor').ADDSTEP);
		this.dropButton.attr('label', dojo.i18n.getLocalization('admin', 'recipeEditor').DROPSTEP);
		console.log('buttons labeled');
	}
});

dojo.declare("RecipeEditorRow", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("openacd","RecipeEditorRow.html"),
	widgetsInTemplate: true,
	templateString: "",
	addAction:function(index){
		var widget = new RecipeEditorAction();
		if(index >= this.actionsDiv.childNodes.length){
			this.actionsDiv.appendChild(widget.domNode);
		} else {
			this.actionsDiv.insertBefore(widget.domNode, this.actionsDiv.childNodes[index]);
		}
		dojo.connect(widget.addButton, 'onClick', this, function(){
			this.addAction(index + 1);
		});
		dojo.connect(widget.dropButton, 'onClick', this, function(){
			this.dropAction(widget.id);
		});
		console.log('setting value');
		//widget.setValue({action:'prioritze', 'arguments':''});
		console.log('spitting it back');
		this.actions.splice(index, 0, widget.id);
		if(this.actions.length > 1){
			for(var i = 0; i < this.actions.length; i++){
				dijit.byId(this.actions[i]).dropButton.attr('disabled', false);
			}
		}
		return widget;
	},
	dropAction:function(widgetid){
		var i = 0;
		for(i; i < this.actions.length; i++){
			if(this.actions[i] == widgetid){
				break;
			}
		}
		dijit.byId(widgetid).destroy();
		this.actions.splice(i, 1);
		if(this.actions.length == 1){
			dijit.byId(this.actions[0]).dropButton.attr('disabled', true);
		}
	},
	setValue:function(recipeStep){
		console.log(['das setValue', recipeStep]);
		this.conditionsEditor.setValue(recipeStep.conditions);
		console.log('next, destroy kids');
		dojo.empty(this.actionsDiv); //.destroyDescendants();
		for(var i = 0; i < recipeStep.actions.length; i++){
			var widget = this.addAction(i);
			widget.setValue(recipeStep.actions[i]);
		}
		this.runsField.attr('value', recipeStep.runs);
		this.containerNode.attr('title', recipeStep.comment);
	},
	constructor:function(){
		this.labels = dojo.i18n.getLocalization("admin", "recipeEditor");
		//this.inherited("constructor", arguments);
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		//labels:[],
		/*this.argsWidget = {
			 getValue:function(){return ""; },
			 setValue:function(){},
			 attr:function(){}
		};*/
		this.actions = [];
		this._disabled = false;
	},
	postCreate:function(){
		console.log('das postCreate');
		var fakeOpts = {
			conditions:[
				{property:'ticks',
				comparison:'=',
				value:5}
			],
			actions:[
				{action:'prioritize',
				'arguments':''}
			],
			runs:'run_once'
		}
		this.setValue(fakeOpts);
	},
	resize:function(){
		console.log(arguments);
	},
	setDisabled:function(bool){
		this._disabled = bool;
		var actionKids = this.actionsDiv.getChildren();
		for(var i = 0; i < actionKids.length; i++){
			actionKids[i].setDisabled(bool);
		}
		this.conditionsEditor.setDisabled(bool);
		this.runsField.attr('disabled', bool);
	}
});

dojo.declare("RecipeEditor", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("openacd","RecipeEditor.html"),
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
		/*row.addButton.onClick = function(){
			ithis.addRow();
		};
		row.dropButton.onClick = function(){
			ithis.dropRow(row.id);
		};
		row.onFocus = function(){
			if(dijit.byId(ithis._focusedOn)){
				dijit.byId(ithis._focusedOn).setConditions(ithis.conditionsEditor.getValue());
				dijit.byId(ithis._focusedOn).domNode.style.backgroundColor = "#ffffff";
			}
			ithis.conditionsEditor.setValue(row.getConditions());
			ithis.conditionsEditor.setDisabled(ithis._disabled);
			ithis._focusedOn = row.id;
			row.domNode.style.backgroundColor = "#ccffff";
		};*/
		this.rows.push(row.id);
		this.stepsContainer.addChild(row);
		this.nullButton.domNode.style.display = "none";
	},
	dropRow: function(rowid){
		/*dijit.byId(rowid).destroy();
		var newrows = [];
		for(var i in this.rows){
			if(this.rows[i] != rowid){
				newrows.push(this.rows[i]);
			}
		}
		this.rows = newrows;
		if(this.rows.length === 0){
			this.nullButton.domNode.style.display = "inline";
		}*/
	},
	constructor: function(arg){
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		this._focusedOn = null;
		this._disabled = false;
		this.rows = [];
	},
	postCreate: function(){
		this.addRow();
	},
	getValue:function(){
		//lock in any changes made to currently selected row conditions.
		var out = [];
		for(var i = 0; i < this.rows.length; i++){
			out.push(dijit.byId(this.rows[i]).getValue());
		}
		return out;
	},
	setValue:function(value){
		var cpyrows = this.rows;
		for(var i = 0; i < cpyrows.length; i++){
			try{
				dijit.byId(cpyrows[i]).destroy();
			}
			catch(err){
				//destroy failed due to + err.message);
			}
		}
		this.rows = [];
		for(i = 0; i < value.length; i++){
			this.addRow();
			dijit.byId(this.rows[i]).setValue(value[i]);
		}
		if(this.rows.length === 0){
			 this.nullButton.domNode.style.display = "inline";
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

openacd.inArray = function(needle, haystack){
	for(var i = 0; i < haystack.length; i++){
		if(haystack[i] == needle){
			return true;
		}
	}
	return false;
}


openacd.RecipeEditor.recipeConditionsStore = new dojo.data.ItemFileReadStore({
	data:{
		identifier:"value",
		label:"label",
		"items":[
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").TICKINTERVAL,
			"value":"ticks",
			"type":"property",
			"filter":"integer",
			"comparisons":[{_reference:"="}]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").AGENTSAVAILABLE,
			"value":"agents_avail",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPAREEQUAL,
			"value":"=",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPAREGREATERTHAN,
			"value":">",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPARELESSTHAN,
			"value":"<",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").COMPARENOTEQUAL,
			"value":"!=",
			"type":"comparison"},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").AGENTSELIGIBLE,
			"value":"eligible_agents",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").CALLSINQUEUE,
			"value":"calls_queued",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").POSITIONINQUEUE,
			"value":"queue_position",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").CLIENT,
			"value":"client",
			"type":"property",
			"filter":"any",
			"comparisons":[
				{_reference:"="},
				{_reference:"!="}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").HOUR,
			"value":"hour",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").WEEKDAY,
			"value":"weekday",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:"<"},
				{_reference:">"}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").MEDIATYPE,
			"value":"mediatype",
			"type":"property",
			"filter":"any",
			"comparisons":[
				{_reference:"="},
				{_reference:"!="}
			]},
			{"label":dojo.i18n.getLocalization("admin", "recipeEditor").CLIENTCOUNT,
			"value":"client_calls_queued",
			"type":"property",
			"filter":"integer",
			"comparisons":[
				{_reference:"="},
				{_reference:">"},
				{_reference:"<"}
			]}
		]
	}
});
