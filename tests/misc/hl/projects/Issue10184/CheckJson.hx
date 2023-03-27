function main() {
	final json = haxe.Json.parse(sys.io.File.getContent("bin/hlc.json"));

	final defines:haxe.DynamicAccess<String> = json.defines;

	for (define in defines.keys()) {
		if (StringTools.contains(define, "-")) {
			throw 'Generated `hlc.json` contains raw version of define flag: $define';
		}
	}
}
