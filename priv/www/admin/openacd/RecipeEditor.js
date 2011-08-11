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
	templateString: '<div dojoAttachPoint="containerNode"><select dojoType="dijit.form.FilteringSelect" dojoAttachPoint="actionField" name="action" style="width:12em;">' +
		'<option value="add_skills">ADDSKILLS</option>' +
		'<option value="remove_skills">REMOVESKILLS</option>' +
		'<option value="set_priority">SETPRIORITY</option>' +
		'<option value="prioritize">PRIORITIZE</option>' +
		'<option value="deprioritize">DEPRIORITIZE</option>' +
		'<option value="voicemail">SENDTOVOICEMAIL</option>' +
		'<option value="announce">MEDIAANNOUCE</option>' +
		'<option value="add_recipe">ADDRECIPE</option>' +
	'</select>' +
	'<input dojoType="dijit.form.ValidationTextBox" dojoAttachPoint="numberWidget" regExp="[\\d]+" style="width:5em;display:none" />' +
	'<input dojoType="dijit.form.TextBox" dojoAttachPoint="stringWidget" style="width:10em;display:none" />' +
	'<button dojoType="dijit.form.Button" dojoAttachPoint="dropButton" label="DROPSTEP"></button>' +
	'<button dojotype="dijit.form.Button" dojoAttachPoint="addButton" label="ADDSTEP"></button></div>',
	_nullArgsWidget: function(){
		//onsole.log('_nullArgsWidget');
		this.argsWidget = {
			getValue:function(){ return ""; },
			setValue:function(){ return ""; },
			destroy:function(){ },
			setDisabled:function(){ }
		};
	},
	_buildSelect: function(select){
		//onsole.log(['building select', this])
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
				//onsole.log(nodes[i]);
				if(openacd.inArray(nodes[i].value, values)){
					nodes[i].selected = true;
				}
			}
		};
		dojo.place(select, this.argumentsDiv, 'after');
	},
	_insertArgsDiv: function(){
		return dojo.create('div', {'dojoAttachPoint':'argumentsDiv'}, this.containerNode, 'first');
	},
	setArguments: function(action, args){
		//onsole.log(['setting arguments', action, args]);
		/*if(args == undefined){
			console.error('args is undefined for', action);
			console.trace();
		}*/
		/*if(this._suppressNextSetArgs){
			delete this._suppressNextSetArgs;
			return;
		}*/
		this.numberWidget.domNode.style.display = 'none';
		this.stringWidget.domNode.style.display = 'none';
		if(this.argsWidget){
			if(this.argsWidget.destroy){
				this.argsWidget.destroy();
			}
		}
		switch(action){
			case "add_skills":
			case "remove_skills":
				var thisid = this.id;
				//var argdiv = this._insertArgsDiv();
				var callback = function(select){
					select.size = 3;
					dojo.place(select, dijit.byId(thisid).actionField.domNode, 'after');
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
							//onsole.log(nodes[i]);
							if(openacd.inArray(nodes[i].value, values)){
								nodes[i].selected = true;
							}
						}
					};
					select.destroy = function(){
						dijit.byId(thisid).actionField.domNode.parentNode.removeChild(select);
					};
					select.setDisabled = function(bool){
						select.disabled = bool;
					};
					dijit.byId(thisid).argsWidget = select;
				}
				var selected = [];
				if(args){
					selected = args;
				}
				skills.createSelect(callback, selected, ['_agent'], ['_profile']);
				break;
			case "set_priority":
				this.numberWidget.domNode.style.display = '';
				this.argsWidget = 'numberWidget';
				this.numberWidget.set('value', args);
				break;
			case "prioritize":
			case "deprioritize":
			case "voicemail":
				//this.argumentsDiv.set('content', "");
				this._nullArgsWidget();
				break;
			case "announce":
				this.stringWidget.domNode.style.display = '';
				this.argsWidget = 'stringWidget';
				this.stringWidget.set('value', args);
				break;
			case "add_recipe":
				//TODO : implement real recusive recipe-age.
				this._nullArgsWidget();
				break;
		}
		if(this.argsWidget != 'numberWidget' && this.argsWidget != 'stringWidget'){
			 this.argsWidget.setDisabled(this._disabled);
		}
	},
	getValue:function(){
		var args = "";
		if(this.argsWidget == 'numberWidget'){
			args = this.numberWidget.get('value');
		} else if ( this.argsWidget == 'stringWidget') {
			args = this.stringWidget.get('value');
		} else if (this.argsWidget.get){
			args = this.argsWidget.get('value');
		} else{
			args = this.argsWidget.getValue();
		}
		obj = {
			 action: this.actionField.get('value'),
			 "arguments": args
		};
		return obj;
	},
	setValue:function(actionObj){
		//onsole.log(['in the setValue', this, this.actionField]);
		this._suppressNextSetArgs = true;
		this.actionField.set('value', actionObj.action);
		//onsole.log('actionFiled set');
		this.setArguments(actionObj.action, actionObj['arguments']);
		//this._suppressNextSetArgs = true;
		/*if(! openacd.inArray(actionObj.action, ['add_skills', 'remove_skills'])){
			if(this.argsWidget.at tr){
				this.argsWidget.at tr('value', actionObj['arguments']);
			} else if(this.argsWidget.setValue) {
				this.argsWidget.setValue(actionObj['arguments']);
			}
		}*/
	},
	postCreate:function(){
		//this.setArguments("add_skills");
		//this.inherited("postCreate", arguments);
		this.actionField.setArgumentsConn = dojo.connect(this.actionField, 'onChange', this, function(arg){
			if(this._suppressNextSetArgs){
				delete this._suppressNextSetArgs;
				return;
			}
			//onsole.log('onChange smackage!');
			this.setArguments(arg);
		});
		/*this.actionField.meBlur = dojo.connect(this.actionField, 'onBlur', this, function(){
			if(this.argsWidget.focus){
				this.argsWidget.focus();
			}
		});*/
		var nodes = this.actionField.store.root;
		//onsole.log(['das nodes', nodes]);
		for(var i = 0; i < nodes.childNodes.length; i++){
			if(nodes.childNodes[i].nodeType == 1){
				var untransed = nodes.childNodes[i].innerHTML;
				nodes.childNodes[i].innerHTML = dojo.i18n.getLocalization('admin', 'recipeEditor')[untransed];
			}
		}
		//onsole.log(['reloading buttons', this.addButton, this.dropButton]);
		this.addButton.set('label', dojo.i18n.getLocalization('admin', 'recipeEditor').ADDSTEP);
		this.dropButton.set('label', dojo.i18n.getLocalization('admin', 'recipeEditor').DROPSTEP);
		//onsole.log('buttons labeled');
	},
	setDisabled:function(bool){
		this._disabled = bool;
		this.actionField.set('disabled', bool);
		this._nullArgsWidget.set('disabled', bool);
		this.numberWidget.set('disabled', bool);
		this.stringWidget.set('disabled', bool);
	}
});

dojo.declare("RecipeEditorRow", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("openacd","RecipeEditorRow.html"),
	widgetsInTemplate: true,
	templateString: "",
	comment:'',
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
		//onsole.log('setting value');
		widget.setValue({action:'prioritize', 'arguments':''});
		//onsole.log('spitting it back');
		this.actions.splice(index, 0, widget.id);
		if(this.actions.length > 1){
			for(var i = 0; i < this.actions.length; i++){
				dijit.byId(this.actions[i]).dropButton.set('disabled', false);
			}
		} else if(this.actions.length == 1) {
			dijit.byId(this.actions[0]).dropButton.set('disabled', true);
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
			dijit.byId(this.actions[0]).dropButton.set('disabled', true);
		}
	},
	setValue:function(recipeStep){
		//onsole.log(['das setValue', recipeStep]);
		this.conditionsEditor.setValue(recipeStep.conditions);
		//onsole.log('next, destroy kids');
		while(this.actions.length > 0){
			this.dropAction(this.actions[0]);
		}
		for(var i = 0; i < recipeStep.actions.length; i++){
			var widget = this.addAction(i);
			//onsole.log(['setting action args gooober', recipeStep.actions[i]]);
			widget.setValue(recipeStep.actions[i]);
		}
		this.runsField.set('value', recipeStep.runs);
		this.containerNode.set('title', recipeStep.comment);
		this.comment = recipeStep.comment;
	},
	getValue:function(){
		var actionsArray = [];
		for(var i = 0; i < this.actions.length; i++){
			actionsArray.push(dijit.byId(this.actions[i]).getValue());
		}
		out = {
			actions: actionsArray,
			conditions: this.conditionsEditor.getValue(),
			runs:this.runsField.get('value'),
			comment:this.comment
		};
		return out;
	},
	constructor:function(){
		this.labels = dojo.i18n.getLocalization("admin", "recipeEditor");
		this.propwidth = "20em";
		this.compwidth = "10em";
		this.valwidth = "20em";
		this.actions = [];
		this._disabled = false;
	},
	postCreate:function(){
		//onsole.log('das postCreate');
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
			runs:'run_once',
			comment:'New Step'
		}
		this.setValue(fakeOpts);
	},
	resize:function(){
		////onsole.log(arguments);
	},
	setDisabled:function(bool){
		this._disabled = bool;
		for(var i = 0; i < this.actions.length; i++){
			dijit.byId(this.actions[i]).setDisabled(bool);
		}
		this.conditionsEditor.setDisabled(bool);
		this.runsField.set('disabled', bool);
	}
});

dojo.declare("RecipeEditor", [dijit._Widget, dijit._Templated], {
	templatePath: dojo.moduleUrl("openacd","RecipeEditor.html"),
	widgetsInTemplate: true,
	templateString: "",
	addRow: function(){
		this.stepsContainer.set('style', 'visibility:visible');
		var ithis = this;
		var row = new RecipeEditorRow({
			propwidth: this.propwidth,
			compwidth: this.compwidth,
			valwidth: this.valwidth,
			style:"padding-left:2em;background-color:#ffffff",
			onClose:function(){
				//onsole.log('hi!');
			}
		});
		this.rows.push(row.id);
		this.stepsContainer.addChild(row);
		var rowTitleButton = dijit.byId(row.id + '_button');
		//onsole.log(['the row title thang', rowTitleButton]);
		rowTitleButton.set('label', 'New Step');
		rowTitleButton.titleNode.style.display = 'inline';
		row.comment = 'New Step';
		var commentEditor = new dijit.form.TextBox({
			style:'display:none',
			onBlur:function(){
				if(this.get('suppressBlur')){
					this.set('suppressBlur', false);
					return;
				}
				this.set('style', 'display:none');
				rowTitleButton.set('label', this.get('value'));
				row.comment = this.get('value');
			},
			onFocus:function(){
				this.set('suppressBlur', false);
			},
			onClick:function(e){
				//onsole.log('stop onClick');
				e.stopPropagation();
			},
			onKeyPress:function(e){
				//onsole.log('stop onkeypress');
				if(e.which != 13){
					e.stopPropagation();
				}
			},
			onKeyDown:function(e){
				//onsole.log('stop onkeydown');
				if(e.which != 13){
					e.stopPropagation();
				}
			},
			onKeyUp:function(e){
				//onsole.log('stop onkeyup');
				if(e.which != 13){
					e.stopPropagation();
				}
			}
		});
		dojo.place(commentEditor.domNode, rowTitleButton.domNode, 'first');
		row.commentEditor = commentEditor;
		var editCommentButton = new dijit.form.Button({
			label:dojo.i18n.getLocalization("admin", "recipeEditor").EDIT,
			style:'font-size:xx-small;',
			onClick:function(e){
				row.commentEditor.set('suppressBlur', true);
				e.stopPropagation();
				row.commentEditor.set('value', rowTitleButton.get('label'));
				rowTitleButton.set('label', '');
				row.commentEditor.set('style', 'display:inline-block');
				setTimeout(function(){ row.commentEditor.focus(); }, 2);
			}
		});
		dojo.place(editCommentButton.domNode, rowTitleButton.domNode, 'first');
		var addRowButton = new dijit.form.Button({
			label:dojo.i18n.getLocalization("admin", "recipeEditor").ADDSTEP,
			style:'float:right;font-size:xx-small'
		});
		var dropRowButton = new dijit.form.Button({
			label:dojo.i18n.getLocalization("admin", "recipeEditor").DROPSTEP,
			style:'float:right;font-size:xx-small'
		});
		dojo.connect(addRowButton, 'onClick', this, function(){
			this.addRow();
		});
		dojo.connect(dropRowButton, 'onClick', this, function(){
			this.dropRow(row.id);
		});
		dojo.place(dropRowButton.domNode, rowTitleButton.domNode, 'last');
		dojo.place(addRowButton.domNode, rowTitleButton.domNode, 'last');
		row.addRowButton = addRowButton;
		row.dropRowButton = dropRowButton;
		row.editCommentButton = editCommentButton;
		this.nullButton.domNode.style.display = "none";
		this.stepsContainer.resize();
		this.stepsContainer.selectChild(row.id);
	},
	dropRow: function(rowid){
		this.stepsContainer.removeChild(dijit.byId(rowid));
		var newrows = [];
		for(var i in this.rows){
			if(this.rows[i] != rowid){
				newrows.push(this.rows[i]);
			}
		}
		this.rows = newrows;
		if(this.rows.length === 0){
			this.nullButton.domNode.style.display = "inline";
		}
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
		//this.addRow();
	},
	startup: function(){
		this.inherited(arguments);
		//onsole.log(['ooo, startup']);
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
		while(this.rows.length > 0){
			this.dropRow(this.rows[0]);
		}
		for(i = 0; i < value.length; i++){
			this.addRow();
			dijit.byId(this.rows[i] + '_button').set('label', value[i].comment);
			dijit.byId(this.rows[i]).setValue(value[i]);
		}
	},
	setDisabled:function(bool){
		this._disabled = bool;
		for(var i = 0; i < this.rows.length; i++){
			var dahrow = dijit.byId(this.rows[i]);
			dahrow.setDisabled(bool);
			dahrow.addRowButton.set('disabled', bool);
			dahrow.dropRowButton.set('disabled', bool);
			dahrow.editCommentButton.set('disabled', bool);
		}
		this.nullButton.set('disabled', true);
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
			]},
			{"label":dojo.i18n.getLocalization("admin","recipeEditor").CALLERNAME,
			"value":"caller_name",
			"type":"property",
			"filter":"any",
			"comparisons":[
				{_reference:"="},
				{_reference:"!="}
			]},
			{"label":dojo.i18n.getLocalization("admin","recipeEditor").CALLERID,
			"value":"caller_id",
			"type":"property",
			"filter":"any",
			"comparisons":[
				{_reference:"="},
				{_reference:"!="}
			]}
		]
	}
});
