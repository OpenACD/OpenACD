dojo.query('#endpointsMain .translate, #endpointsMain .translatecol').forEach(function(item, index, list){
	var transKey;
	var trans;
	var applyTrans = function(){ };

	if(! item.innerText){
		var dij = dijit.byNode(item);
		if(! dij){
			console.log('could not event find dijit', item);
			return;
		}
		transKey = dij.get('label');
		applyTrans = function(){
			diji.set('label', trans);
		}
	} else {
		transKey = item.innerText;
		applyTrans = function(){
			item.innerText = trans;
		}
	}
	var trans = dojo.i18n.getLocalization('agentUI', 'labels')[transKey];

	console.log(transKey, trans, dij);
	if(! trans){
		return;
	}
	if(dojo.hasClass(item, 'translatecol')){
		trans = trans + ":";
	}
	applyTrans();
});
