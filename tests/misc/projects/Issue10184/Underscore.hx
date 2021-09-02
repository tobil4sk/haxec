function main() {
	// -D variable_a
	final defines = Tools.getDefines();

	if (!defines.exists("variable_a"))
		throw "`variable_a` flag is missing";
	if (defines.exists("variable-a"))
		throw "`variable-a` flag should not be present";
}
