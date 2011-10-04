//>>built
// AMD-ID "dojox/encoding/crypto/_base"
define("dojox/encoding/crypto/_base", ["dojo/_base/kernel"], function(dojo) {
	
	dojo.getObject("encoding.crypto", true, dojox);

	var c = dojox.encoding.crypto;
	c.cipherModes = {
		// summary:
		//	Enumeration for various cipher modes.
		ECB:0, CBC:1, PCBC:2, CFB:3, OFB:4, CTR:5
	};
	c.outputTypes = {
		// summary:
		//	Enumeration for input and output encodings.
		Base64:0, Hex:1, String:2, Raw:3
	};
	
	return dojox.encoding.crypto;
});
