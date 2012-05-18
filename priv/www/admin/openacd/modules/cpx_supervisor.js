if(! window.CPXSupervisor){
	window.CPXSupervisor = function(){
		throw new Error("CPXSupervisor is a lib; not meant to be 'newed'");
	}
}

CPXSupervisor.setAgentRingoutLock = function(ringlock){
	dojo.xhrPost({
		url:"modules/" + modules.activeNode + "/cpx_supervisor/update/agent_ringout_lock",
		content:{
			'value':ringlock
		},
		handleAs:'json',
		load:function(res){
			if(res.success){
				return;
			}
			errMessage(res.message);
		},
		error:function(err){
			errMessage(err);
		}
	});
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
				console.warn("res success false", res.message);
			}
		},
		error:function(err){
			console.warn("fail!", err);
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
				console.warn('setTransferPrompt failed', res.message);
			}
		},
		error:function(res){
			console.warn('setTransferPrompt errored', res);
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

CPXSupervisor.setDefaultRingout = function(newVal, callback){
	var contentObj = {};
	if(newVal){
		contentObj.value = newVal;
	}
	dojo.xhrPost({
		url:"/modules/" + modules.activeNode + "/cpx_supervisor/update/default_ringout",
		handleAs:'json',
		content:contentObj,
		load:function(res){
			if(res.success){
				if(callback){
					callback(res);
				}
				return;
			}
			errMessage(["setting default ringout failed", res.message]);
		},
		error:function(res){
			errMessage(['setting default ringout errored', res]);
		}
	});
}

CPXSupervisor.setMaxRingout = function(newVal, callback){
	var contentObj = {};
	if(newVal){
		contentObj.value = newVal;
	}
	console.log('the content sent', contentObj);
	dojo.xhrPost({
		url:"/modules/" + modules.activeNode + "/cpx_supervisor/update/max_ringouts",
		handleAs:'json',
		content:contentObj,
		load:function(res){
			if(res.success){
				if(callback){
					callback(res);
				}
				return;
			}
			errMessage(["setting max_ringout failed", res.message]);
		},
		error:function(res){
			errMessage(["setting max_ringout errored", res]);
		}
	});
}

CPXSupervisor.setPluginDir = function(newVal, callback){
	var contentObj = {};
	if(newVal){
		contentObj.value = newVal;
	}
	dojo.xhrPost({
		url:"/modules/" + modules.activeNode + "/cpx_supervisor/update/plugin_dir",
		handleAs:'json',
		content:contentObj,
		load:function(res){
			if(res.success){
				if(callback){
					callback(res);
				}
				return;
			}
			errMessage(["Setting plugin_dir failed", res.message]);
		},
		error:function(res){
			errMessage(["Setting plugin_dir errored", res]);
		}
	});
}

CPXSupervisor.setExitMaxRingFails = function(newVal){
	var contentObj = {};
	if(newVal){
		contentObj = {
			'value':'true'
		};
	} 
	dojo.xhrPost({
		url:'/modules/' + modules.activeNode + '/cpx_supervisor/update/exit_on_max_ring_fails',
		handleAs:'json',
		content:contentObj,
		load:function(res){
			if(res.success){
				return;
			}
			errMessage(['Setting "exit_on_max_ring_fails" failed', res.message]);
		},
		error:function(res){
			errMessage(['Setting "exit_on_max_ring_fails" errored', res]);
		}
	});
}

CPXSupervisor.addPlugin = function(plugin){
	dojo.xhrPost({
		url:'/modules/' + modules.activeNode + '/cpx_supervisor/update/plugins',
		handleAs:'json',
		content:{
			'plugin':plugin,
			'action':'load',
		},
		load:function(res){
			if(res.success){
				CPXSupervisor.reloadPlugins();
				return;
			}
			errMessage(['adding plugin failed', res.message]);
		},
		error:function(res){
			errMessage(['adding plugin errored', res]);
		}
	});
}

CPXSupervisor.removePlugin = function(plugin){
	dojo.xhrPost({
		url:'/modules/' + modules.activeNode + '/cpx_supervisor/update/plugins',
		handleAs:'json',
		content:{
			'plugin':plugin,
			'action':'unload',
		},
		load:function(res){
			if(res.success){
				CPXSupervisor.reloadPlugins();
				return;
			}
			errMessage(['adding plugin failed', res.message]);
		},
		error:function(res){
			errMessage(['adding plugin errored', res]);
		}
	});
}

CPXSupervisor.reloadPlugins = function(){
	dojo.xhrGet({
		url:"/modules/" + modules.activeNode + "/cpx_supervisor/get/plugins",
		handleAs:'json',
		load:function(res){
			if(res.success){
				var pluginList = dojo.byId("plugins");
				dojo.query(pluginList).empty();
				for(var i in res.value){
					(function(){
						var li = dojo.place('<li>' + i + '</li>', pluginList);
						if(res.value[i] == 'running'){
							li.style.color = 'green';
						} else {
							li.style.color = 'red';
						}
						var button = dojo.place('<button>-</button>', li);
						new dijit.form.Button({
							label:'-',
							onClick:function(){
								CPXSupervisor.removePlugin(i);
							}
						}, button);
					})();
				}
				return;
			}
			errMessage(["get plugins failed", res]);
		},
		error:function(err){
			console.warn(["get plugins exploded", res]);
		}
	});
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
			dijit.byId('archivepath').set('value', res.result);
		}
		else{
			console.warn(["load fail", res.message]);
		}
	},
	error:function(err){
		console.warn(["other fail", err]);
	}
});

dojo.xhrGet({
	url:"modules/" + modules.activeNode + "/cpx_supervisor/get/mantispath",
	handleAs:"json",
	load:function(res){
		if(res.success){
			dijit.byId('mantispath').set('value', res.result);
		} else {
			console.warn(["load fail", res.message]);
		}
	},
	error:function(err){
		console.warn(["other fail", err]);
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
		console.warn(["other fail", err]);
	}
});

dojo.xhrGet({
	url:"/modules/" + modules.activeNode + "/cpx_supervisor/get/default_ringout",
	handleAs:"json",
	load:function(res){
		if(res.success){
			var targetDijit = dijit.byId('defaultRingout');
			targetDijit.set('placeHolder', res['default']);
			if(res.isDefault){
				targetDijit.set('value', '');
			} else {
				targetDijit.set('value', res.value);
			}
			return;
		}
		errMessage(['getting default ringout failed', res.message]);
	},
	error:function(err){
		console.warn('getting default reingout errored', res);
	}
});

dojo.xhrGet({
	url:"/modules/" +modules.activeNode + "/cpx_supervisor/get/max_ringouts",
	handleAs:"json",
	load:function(res){
		if(res.success){
			var targetDij = dijit.byId("maxRingouts");
			targetDij.set('placeHolder', res['default']);
			if(res.isDefault){
				targetDij.set('value', '');
			} else {
				targetDij.set('value', res.value);
			}
			return;
		}
		errMessage(['getting defualt ringout failed', res.message]);
	},
	error:function(err){
		console.warn('getting default ringout errored,', res);
	}
});

dojo.xhrGet({
	url:"/modules/" + modules.activeNode + "/cpx_supervisor/get/exit_on_max_ring_fails",
	handleAs:'json',
	load:function(res){
		if(res.success){
			var targetDij = dijit.byId('exitMaxRingFails');
			targetDij.set('checked', res.value);
			return;
		}
		errMessage(['getting exit_on_max_ring_fails failed', res.message]);
	},
	error:function(err){
		console.warn(['getting exit_on_max_ring_fails errored', res]);
	}
});

dojo.xhrGet({
	url:"/modules/" + modules.activeNode + "/cpx_supervisor/get/agent_ringout_lock",
	handleAs:'json',
	load:function(res){
		if(res.success){
			var targetDij = dijit.byId('agentRingoutLock');
			targetDij.set('value', res['default']);
			if(res.isDefault){
				targetDij.set('value', '');
			} else {
				targetDij.set('value', res.value);
			}
			return;
		}
		errMessage(['getting agent_ringout_lock failed', res.message]);
	},
	error:function(err){
		console.warn(['getting agent_ringout_lock errored', res]);
	}
});

dojo.xhrGet({
	url:"/modules/" + modules.activeNode + "/cpx_supervisor/get/plugin_dir",
	handleAs:'json',
	load:function(res){
		if(res.success){
			var targetDij = dijit.byId('pluginDir');
			targetDij.set('value', res['default']);
			if(res.isDefault){
				targetDij.set('value', '');
			} else {
				targetDij.set('value', res.value);
			}
			return;
		}
		errMessage(['getting pluginDir failed', res.message]);
	},
	error:function(err){
		console.warn(['getting pluginDir errored', res]);
	}
});

CPXSupervisor.reloadPlugins();
