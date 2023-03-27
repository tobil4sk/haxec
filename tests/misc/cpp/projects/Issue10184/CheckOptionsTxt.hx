function main() {
	final definePairs = sys.io.File.getContent("bin/Options.txt").split("\n");

	for (definePair in definePairs)
		for (char in StringTools.iterator(definePair)) {
			if (char == "=".code) break;
			if (char == "-".code){
				throw 'Generated `Options.txt` contains raw version of define flag: $definePair';
			}
		}
}
