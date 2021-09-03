function error(msg, code = 1) {
	Sys.stderr().writeString(msg + "\n");
	Sys.exit(code);
}

function runHaxe(args:Array<String>):sys.io.Process {
	final process = new sys.io.Process("haxe", args);
	final exitCode = process.exitCode();
	if (exitCode != 0)
		throw "Unable to compile: " + process.stdout.readAll().toString() + process.stderr.readAll().toString() + "\n";
	return process;
}

macro function getDefines():haxe.macro.Expr {
	final defines:Map<String, String> = haxe.macro.Context.getDefines();
	final map:Array<haxe.macro.Expr> = [];
	for (key in defines.keys())
		map.push(macro $v{key} => $v{Std.string(defines.get(key))});
	return macro $a{map};
}
