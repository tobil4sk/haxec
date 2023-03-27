final FILE = "bin/main.js";

function main() {
	final args = Sys.args();

	switch args[0] {
		case "--empty":
			checkForEmptySourceHeader(FILE);
		case expected:
			checkSourceHeader(FILE, expected);
	}
}

function checkForEmptySourceHeader(path:String) {
	final content = getJsSourceContent(path);

	if (StringTools.startsWith(content, "// "))
		throw "File has a source header when none was expected: " + content.split("\n")[0];
}

function checkSourceHeader(path:String, expected:String) {
	final content = getJsSourceContent(path);

	if (!StringTools.startsWith(content, "// " + expected))
		throw "File source header does not start with expected: // " + expected +
			"\nSource header: " + content.split("\n")[0];
}

function getJsSourceContent(path:String) {
	return sys.io.File.getContent(path);
}
