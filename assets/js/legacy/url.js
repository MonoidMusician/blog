/**
 * From http://papermashup.com/read-url-get-variables-withjavascript/
**/
function getGET() {
	var vars = {};
	var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
		vars[key] = value;
	});
	return vars;
}
