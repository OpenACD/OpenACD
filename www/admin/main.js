/* dependancies */
dojo.require("dijit.Dialog");
dojo.require("dijit.layout.LayoutContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.Form");
dojo.require("dijit.form.TextBox");
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dijit.Tree");
dojo.require("dojo.data.ItemFileReadStore");


currenttab = undefined;

function switchtab(tab) {
	if (currenttab != tab)
		console.log("switched tab to "+tab);
	currenttab = tab;
}

function inspect(obj){
	console.log(obj);
	for(var i in obj){
		console.log("  " + i + ": " + obj[i])
	}
}

function selectskill(item) {
	if (item.type == "skill") {
		dijit.byId("editSkillGroupPane").domNode.style.display="none";
		dijit.byId("editSkillPane").domNode.style.display="block";
		dijit.byId("skillAtom").setValue(item.atom);
		dijit.byId("skillName").setValue(item.name);
		dijit.byId("skillDesc").setValue(item.description);
		dijit.byId("skillAtom").setDisabled(true);
		dijit.byId("skillName").setDisabled(item.protected == "true");
		dijit.byId("skillDesc").setDisabled(item.protected == "true");
		dijit.byId("wtf").setDisabled(item.protected == "true");
		dijit.byId("skillPane").refresh();
	} else if (item.type == "group") {
		dijit.byId("editSkillGroupPane").domNode.style.display="block";
		dijit.byId("editSkillPane").domNode.style.display="none";
		dijit.byId("skillPane").refresh();
	}
}

function selectqueue(item) {
	if (item.type == "queue") {
		dijit.byId("editQueueGroupPane").domNode.style.display="none";
		dijit.byId("editQueueGeneralPane").domNode.style.display="none";
		dijit.byId("editQueuePane").domNode.style.display="block";
		dijit.byId("queuePane").refresh();
	} else if (item.type == "group") {
		dijit.byId("editQueueGroupPane").domNode.style.display="block";
		dijit.byId("editQueueGeneralPane").domNode.style.display="none";
		dijit.byId("editQueuePane").domNode.style.display="none";
		dijit.byId("queuePane").refresh();
	}
}


dojo.addOnLoad(function() {
	var tabbar = dijit.byId("mainTabContainer");
	currenttab = dijit.byId("agentsTab");
	dojo.connect(tabbar,'selectChild','switchtab');
	dojo.connect(dijit.byId('itemTree'), 'onClick', 'selectskill');
	dojo.connect(dijit.byId('queueTree'), 'onClick', 'selectqueue');
	dojo.connect(dijit.byId("wtf"), 'onClick', function(foo) {
		console.log(foo.target)
	});
	dojo.connect(dijit.byId("generalQueueSettings"), 'onClick', function(foo) {
		dijit.byId("editQueueGroupPane").domNode.style.display="none";
		dijit.byId("editQueueGeneralPane").domNode.style.display="block";
		dijit.byId("editQueuePane").domNode.style.display="none";
		dijit.byId("queuePane").refresh();
	});
});

function setSkill(e){
	console.log("what is e " + e)
	// prevent the form from actually submitting
	e.preventDefault(); 
	// submit the form in the background	
	dojo.xhrPost({
		url: "setskill",
		form: "editSkillForm",
		handleAs: "json",
		content:{
			"skillatom":dijit.byId("skillAtom").getValue(),
			"action" : "set"
		},
		handle: function(data,args){
			if(typeof data == "error"){
				console.warn("error!",args);
			}else{
				// show our response 
				console.log("success!"),
				inspect(data);
			}
		}
	});
};
dojo.addOnLoad(function(){
			   var theForm = dojo.byId("editSkillPane");
			   // another dojo.connect syntax: call a function directly	
			   dojo.connect(theForm,"onsubmit",setSkill);
			   }); 
