/* dependancies */
dojo.require("dijit.Dialog");
dojo.require("dijit.layout.LayoutContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.form.Button");
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
	dojo.connect(tree, 'onClick', function(X) {
		console.log("tree item selected" + X);
	});
});

