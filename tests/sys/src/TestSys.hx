import utest.Assert;

class TestSys extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		return Sys.command(cmd, args);
	}

	function testEnvironment() {
		var env = Sys.environment();
		// EXISTS should be set manually via the command line
		Assert.notNull(env.get("EXISTS"));
		Assert.isNull(env.get("doesn't exist"));

		final nonExistent = "NON_EXISTENT";
		env.set(nonExistent, "1");
		// new copies should not be affected
		Assert.isNull(Sys.environment()[nonExistent]);

		#if !jvm
		// env should not update when environment updates
		final toUpdate = "TO_UPDATE";

		Sys.putEnv(toUpdate, "1");
		Assert.isNull(env.get(toUpdate));

		// new copy should have the variable
		Assert.equals("1", Sys.environment()[toUpdate]);

		// environment should not update if env updates
		env.set(toUpdate, "2");
		Assert.equals("1", Sys.getEnv(toUpdate));

		// variables set via target specific api should exist
		#if python
		final toSetNatively = "SET_NATIVELY";
		python.lib.Os.environ.set(toSetNatively, "1");
		Assert.equals("1", Sys.environment()[toSetNatively]);
		#end
		#end
	}

	function existsInSubProcess(variable:String, value:String) {
		#if js
		return UtilityProcess.runUtilityAsCommand(["checkEnv", variable, value]) == 0;
		#else
		return UtilityProcess.runUtility(["checkEnv", variable, value]).exitCode == 0;
		#end
	}

	function testGetEnv() {
		// EXISTS should be set manually via the command line
		Assert.notNull(Sys.getEnv("EXISTS"));

		// on windows, Sys.getEnv should be case insensitive
		if (Sys.systemName() == "Windows")
			Assert.notNull(Sys.getEnv("exists"));

		Assert.isNull(Sys.getEnv("doesn't exist"));
	}

	#if !jvm
	function testPutEnv() {
		Sys.putEnv("FOO", "value");
		Assert.equals("value", Sys.getEnv("FOO"));

		Assert.equals("value", Sys.environment().get("FOO"));

		Assert.isTrue(existsInSubProcess("FOO", "value"));

		#if python
		// the variable should also be visible through python's api
		Assert.equals("value", python.lib.Os.environ.get("FOO"));
		#end

		// null
		Sys.putEnv("FOO", null);
		Assert.isNull(Sys.getEnv("FOO"));

		Assert.isFalse(Sys.environment().exists("FOO"));

		Assert.isFalse(existsInSubProcess("FOO", "value"));

		#if python
		// the variable should also be gone when checking through python's api
		Assert.isFalse(python.lib.Os.environ.hasKey("FOO"));
		#end

		Assert.isTrue(try {
			Sys.putEnv("NON_EXISTENT", null);
			true;
		} catch (e) {
			trace(e);
			false;
		});
	}
	#end

	function testProgramPath() {
		var p = Sys.programPath();

		Assert.isTrue(haxe.io.Path.isAbsolute(p));
		Assert.isTrue(sys.FileSystem.exists(p));

		trace(p);
		Assert.isTrue(StringTools.endsWith(p,
		#if interp
			"Main.hx"
		#elseif neko
			 "sys.n"
		#elseif cpp
			#if cppia
				// "Main.cppia"
				"Host-debug" + (Sys.systemName() == "Windows" ? ".exe" : "")
			#else
				"Main-debug" + (Sys.systemName() == "Windows" ? ".exe" : "")
			#end
		#elseif jvm
			"sys.jar"
		#elseif python
			"sys.py"
		#elseif php
			"index.php"
		#elseif lua
			"sys.lua"
		#elseif js
			"sys.js"
		#elseif hl
			#if hlc
				"sys.exe"
			#else
				"sys.hl"
			#end
		#end
		));
	}

	function testGetCwd() {
		final current = Sys.getCwd();
		// ensure it has a trailing slash
		Assert.notEquals(current, haxe.io.Path.removeTrailingSlashes(current));
	}

	#if !jvm
	function testSetCwd() {
		var cur = Sys.getCwd();
		Sys.setCwd("../");
		var newCwd = haxe.io.Path.join([cur, "../"]);
		function normalize(path) {
			return haxe.io.Path.addTrailingSlash(haxe.io.Path.normalize(path));
		}
		Assert.equals(normalize(newCwd), normalize(Sys.getCwd()));
		Sys.setCwd(cur);
	}
	#end
}
