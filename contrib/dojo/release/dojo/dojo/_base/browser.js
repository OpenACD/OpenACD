/*
	Copyright (c) 2004-2011, The Dojo Foundation All Rights Reserved.
	Available via Academic Free License >= 2.1 OR the modified BSD license.
	see: http://dojotoolkit.org/license for details
*/

//>>built
if(require.has){
	require.has.add("config-selectorEngine", "acme");
}
define("dojo/_base/browser", [
	"../ready",
	"./kernel",
	"./connect", // until we decide if connect is going back into non-browser environments
	"./unload",
	"./window",
	"./event",
	"./html",
	"./NodeList",
	"../query",
	"./xhr",
	"./fx"], function(dojo) {
	// module:
	//		dojo/_base/browser
	// summary:
	//		This module causes the browser-only base modules to be loaded.
	return dojo;
});
