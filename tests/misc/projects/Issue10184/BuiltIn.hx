function main() {
	// test that built in defines can be accessed by macros
	// either with a dash or underscore
	final defines = Tools.getDefines();

	if (!defines.exists("haxe-ver"))
		throw "`haxe-ver` flag is missing";

	if (!defines.exists("haxe_ver"))
		throw "`haxe_ver` flag is missing";
}
