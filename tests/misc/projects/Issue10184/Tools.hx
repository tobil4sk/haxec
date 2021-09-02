macro function getDefines():haxe.macro.Expr {
	final defines:Map<String, String> = haxe.macro.Context.getDefines();
	final map:Array<haxe.macro.Expr> = [];
	for (key in defines.keys()) {
		map.push(macro $v{key} => $v{Std.string(defines.get(key))});
	}
	return macro $a{map};
}
