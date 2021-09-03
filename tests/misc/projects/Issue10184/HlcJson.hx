import haxe.DynamicAccess;
import haxe.Json;
import sys.io.File;
import sys.FileSystem;

function deleteDirectory(path:String) {
	if (!FileSystem.isDirectory(path))
		return FileSystem.deleteFile(path);

	for (item in FileSystem.readDirectory(path))
		deleteDirectory('$path/$item');

	FileSystem.deleteDirectory(path);
}

function main() {
	Tools.runHaxe(["Tools", "--hl", "out/main.c"]);

	final json = Json.parse(File.getContent("out/hlc.json"));

	final defines:DynamicAccess<String> = json.defines;

	for (define in defines.keys())
		if (StringTools.contains(define, "-"))
			Tools.error("Generated `hlc.json` contains raw version of define flag: " + define);

	deleteDirectory("out");
}
