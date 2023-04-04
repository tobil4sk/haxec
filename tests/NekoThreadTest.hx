import sys.thread.Lock;

function main() {
	final lock = new Lock();

	sys.thread.Thread.create(function() {
		trace("Hello world");
		Sys.sleep(0.2);
		lock.release();
	});
	trace("Hi world");
	lock.wait();
}
