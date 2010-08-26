if(! window.CPXSupervisor){
	window.CPXSupervisor = function(){
		throw new Error("CPXSupervisor is a lib; not meant to be 'newed'");
	}
}

CPXSupervisor.setArchivePath = function(newPath, cb){
	if(! cb){
		cb = function(){ };
	}
	dojo.xhrPost({
		url:"modules/" + modules.activeNode + "/cpx_supervisor/update/archivepath",
		content:{
			value:newPath
		},
		handleAs:'json',
		load:function(res){
			if(res.success){
				cb();
			} else {
				errMessage(res.message);
			}
		},
		error:function(err){
			errMessage(err);
		}
	});
}

CPXSupervisor.setMantisPath = function(newPath, cb){
	if(! cb){
		cb = function(){ };
	}
	dojo.xhrPost({
		url:"modules/" + modules.activeNode + "/cpx_supervisor/update/mantispath",
		content:{
			value: newPath
		},
		handleAs:"json",
		load:function(res){
			if(res.success){
				cb();
			} else {
				console.log("res success false", res.message);
			}
		},
		error:function(err){
			console.log("fail!", err);
		}
	});
}

CPXSupervisor.setTransferPrompt = function(formBits, skills){
	dojo.xhrPost({
		url:"modules/" + modules.activeNode + "/cpx_supervisor/update/transferprompt",
		content:{
			prompts:formBits,
			'skills':skills
		},
		handleAs:'json',
		load:function(res){
			if(res.success){
				//cool
			} else {
				console.log('setTransferPrompt failed', res.message);
			}
		},
		error:function(res){
			console.log('setTransferPrompt errored', res);
		}
	});
}

CPXSupervisor.createTransferPromptOptionsRow = function(rawNode, seedObj){
	var defaultObj = {
		label:'',
		name:'',
		regex:''
	};
	seedObj = dojo.mixin(defaultObj, seedObj);
	for(var i = 0; i < 4; i++){
		rawNode.insertCell().innerHTML = '<div></div>';
	}
	var labelField = new dijit.form.TextBox({name:'label',value:seedObj.label}, rawNode.cells[0].firstChild);
	var nameField = new dijit.form.TextBox({name:'name',value:seedObj.name}, rawNode.cells[1].firstChild);
	var regexField = new dijit.form.TextBox({name:'regex',value:seedObj.regex}, rawNode.cells[2].firstChild);
	var dropButton = new dijit.form.Button({
		label:'-',
		onClick:function(){
			var parentRow = this.domNode.parentNode.parentNode
			var widNodes = dojo.query('*[widgetid]', parentRow);
			dojo.forEach(widNodes, function(node){
				dijit.byId(node.getAttribute('widgetid')).destroy();
			});
			var table = dojo.byId("transferPromptOptions");
			table.deleteRow(parentRow.rowIndex);
		}
	}, rawNode.cells[3].firstChild);
}

dojo.query(".translate, .translatecol", 'cpx_module').forEach(function(node){
	var trans = dojo.i18n.getLocalization('admin', 'cpx_supervisor')[node.innerHTML];
	if(trans){
		node.innerHTML = trans;
	}
	if(node.className == "translatecol"){
		node.innerHTML += ':';
	}
});

dojo.xhrGet({
	url:"modules/" + modules.activeNode + "/cpx_supervisor/get/archivepath",
	handleAs:"json",
	load:function(res){
		if(res.success){
			dijit.byId('archivepath').attr('value', res.result);
		}
		else{
			console.log(["load fail", res.message]);
		}
	},
	error:function(err){
		console.log(["other fail", err]);
	}
});

dojo.xhrGet({
	url:"modules/" + modules.activeNode + "/cpx_supervisor/get/mantispath",
	handleAs:"json",
	load:function(res){
		if(res.success){
			dijit.byId('mantispath').attr('value', res.result);
		} else {
			console.log(["load fail", res.message]);
		}
	},
	error:function(err){
		console.log(["other fail", err]);
	}
});

dojo.xhrGet({
	url:"modules/" + modules.activeNode + "/cpx_supervisor/get/transferprompt",
	handleAs:"json",
	load:function(res){
		var skillCb = function(select){
			select.name = "skills";
			dojo.place(select, dojo.byId("transferPromptSkills"), "only");
		}
		var i;
		for(i = 0; i < res.skills.length; i++){
			var crushedSkill = '';
			if(res.skills[i].expanded){
				crushedSkill = '{' + res.skills[i].atom + ',' + res.skills[i].expanded + '}';
			} else {
				crushedSkill = res.skills[i].atom;
			}
			res.skills[i] = crushedSkill;
		}
		skills.createSelect(skillCb, res.skills, ['_agent','_profile'], ['_profile']);
		var table = dojo.byId("transferPromptOptions");
		for(i = 0; i < res.prompts.length; i++){
			CPXSupervisor.createTransferPromptOptionsRow(table.insertRow(table.rows.length), res.prompts[i]);
		}
	},
	error:function(err){
		console.log(["other fail", err]);
	}
});