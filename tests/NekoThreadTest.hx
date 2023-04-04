import sys.thread.Lock;
import sys.thread.Thread;

function main() {
	final p = new sys.io.Process("git", ["subodule", "update"]);
	// just in case process hangs waiting for stdin
	p.stdin.close();

	final streamsLock = new Lock();
	function readFrom(stream:haxe.io.Input, to: {value: String}) {
		to.value = stream.readAll().toString();
		streamsLock.release();
	}

	final out = {value: ""};
	final err = {value: ""};
	Thread.create(readFrom.bind(p.stdout, out));
	Thread.create(readFrom.bind(p.stderr, err));

	final code = p.exitCode();
	for (_ in 0...2) {
		// wait until we finish reading from both streams
		streamsLock.wait();
	}

	p.close();
}
