if(! window.OpenACD){
	function OpenACD(){
		this.createdBy = 'OpenACDAgent';
		throw new Error('lib; not for new');
	}
}

OpenACD.loadWidget = function(openacdUrl, frameId){
	if(! frameId){
		frameId = 'OpenACDWidget';
	}
	if(document.getElementById(frameId)){
		throw new Error('element id already used');
	}
	var frame = document.createElement('iframe');
	frame.id = frameId;
	frame.name = frameId;
	frame.src = openacdUrl + '/widget.html';
	frame.style.width = '0';
	frame.style.height = '0';
	frame.style.display = 'none';
	document.body.appendChild(frame);
	return frame.contentWindow;
}
