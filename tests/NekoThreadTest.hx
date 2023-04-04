function main() {
	sys.thread.Thread.create(function() {
		trace("Hello world");
		Sys.sleep(0.2);
	});
	trace("Hi world");
	Sys.sleep(0.5);
}