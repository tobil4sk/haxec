package io;

import sys.io.Process;
import utest.Assert;

class TestProcess extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		var p = new Process(cmd, args);
		var exitCode = p.exitCode();
		runInfo = {
			out: p.stdout.readAll().toString(),
			err: p.stderr.readAll().toString(),
		};
		p.close();
		return exitCode;
	}

	function testNonExistentCodeOutput() {
		var bin = "totally_nonexistent_command_12345";
		run(bin);
		Assert.equals("", runInfo.out);
		Assert.notEquals("", runInfo.err);

		var exitCode = run(bin, ["-v"]);
		Assert.equals("", runInfo.out);
		Assert.notEquals("", runInfo.err);
	}
}
