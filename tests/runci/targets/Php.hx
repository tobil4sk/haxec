package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Php {
	static var miscPhpDir(get,never):String;
	static inline function get_miscPhpDir() return miscDir + 'php/';

	static function modifyIniWindows() {
		final configPath = {
			final output = new sys.io.Process("php --ini | findstr 'Loaded Configuration File:'").stdout.readAll();
			Sys.command('echo $output');
			StringTools.trim(output.toString().split(":")[1]);
		}
		Sys.command('echo $configPath');
		Sys.println(configPath);
		var content = sys.io.File.getContent(configPath);

		final toUncomment = ["extension=php_pdo.dll", "extension=php_sqlite.dll", "extension=php_sockets.dll"];
		for (ext in toUncomment)
			content = StringTools.replace(content, ';$ext', ext);

		sys.io.File.saveContent(configPath, content);
	}

	static public function getPhpDependencies() {
		var phpCmd = commandResult("php", ["-v"]);
		var phpVerReg = ~/PHP ([0-9]+\.[0-9]+)/i;
		var phpVer = if (phpVerReg.match(phpCmd.stdout))
			Std.parseFloat(phpVerReg.matched(1));
		else
			null;

		if (phpCmd.exitCode == 0 && phpVer != null && phpVer >= 7.0) {
			switch systemName {
				case "Linux":
					var phpInfo = commandResult("php", ["-i"]);
					if(phpInfo.stdout.indexOf("mbstring => enabled") < 0) {
						Linux.requireAptPackages(["php-mbstring"]);
					}
				case "Windows":
					modifyIniWindows();
				case _:
			}
			infoMsg('php $phpVer has already been installed.');
			return;
		}
		switch systemName {
			case "Linux":
				Linux.requireAptPackages(["php-cli", "php-mbstring"]);
			case "Mac":
				runCommand("brew", ["install", "php"], true);
			case "Windows":
				runCommand("cinst", ["php", "-version", "7.1.8", "-y"], true);
				modifyIniWindows();
			case _:
				throw 'unknown system: $systemName';
		}
		runCommand("php", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getPhpDependencies();

		changeDirectory(miscPhpDir);
		runCommand("haxe", ["run.hxml"]);

		var binDir = "bin/php";

		var prefixes = [[]];
		if(isCi()) {
			prefixes.push(['-D', 'php-prefix=haxe']);
			prefixes.push(['-D', 'php-prefix=my.pack']);
		}

		for(prefix in prefixes) {
			changeDirectory(unitDir);
			if(isCi())
				deleteDirectoryRecursively(binDir);

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runThroughPhpVersions(runCommand.bind(_, [binDir + "/index.php"]));

			changeDirectory(sysDir);
			if(isCi())
				deleteDirectoryRecursively(binDir);

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runThroughPhpVersions( function(cmd:String) {
				runCommand(setSysVar + cmd + " bin/php/Main/index.php");
			});
		}
	}

	static function runThroughPhpVersions(fn:(phpCmd:String)->Void) {
		switch [ci, systemName] {
			case [GithubActions, "Linux"]:
				for(version in ['7.4', '8.0']) {
					fn('php$version');
				}
			case _:
				fn('php');
		}
	}
}
