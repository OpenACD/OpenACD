window.queueDashboardViewKillListen = dojo.subscribe('tabPanel-removeChild', function(child){
	if(child.id == 'queueDashboardTab'){
		dropTab('queueDashboardTab');
		dojo.unsubscribe(window.queueDashboardTabViewKillListen);
		delete window.queueDashboardTabViewKillListen;
	}
});

storeTab('qeueueDashboardTab');