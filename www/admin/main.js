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


dojo.addOnLoad(function() {
	var tabbar = dijit.byId("mainTabContainer");
	currenttab = dijit.byId("agentsTab");
	dojo.connect(tabbar,'selectChild','switchtab');
	var tree = dijit.byId("itemTree");
	dojo.connect(tree, 'onClick', function(item) {
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
	});
});

