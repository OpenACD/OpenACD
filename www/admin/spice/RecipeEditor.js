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
	setArguments: function(action){
		console.log(action);
		switch(action){
			case "add_skills":
				
			break;
			
			case "remove_skills":
			
			break;
			
			case "set_priority":
				var argsWidget = new dijit.form.ValidationTextBox({
					regExp:"[\\d]+"
				});
				this.argumentsId = argsWidget.id;
				this.argumentsDiv.setContent(argsWidget.domNode);
			break;
			
			case "prioritize":
				this.argumentsDiv.setContent("");
			break;
			
			case "deprioritize":
				this.argumentsDiv.setContent("");
			break;
			
			case "voicemail":
				this.argumentsDiv.setContent("");
			break;
			
			case "announce":
				var argsWidget = new dijit.form.TextBox({});
				this.argumentsDiv.setContent(argsWidget.domNode);
			 break;
			
			case "add_recipe":
			
			break;
		}
	},
	getValues:function(){
		
	},
	setValues:function(recipeStep){
		
	}
});