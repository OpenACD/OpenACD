dependencies = {
	action:'clean,release',
	releaseName:'agentUI',
	layers:[{
		name:'../agentUI/base.js',
		dependencies:[
			'agentUI.base'
		]
	}],

	prefixes: [
		['dijit','../dijit'],
		['dojox','../dojox'],
		['agentUI','../agentUI']
	]
};

