// Issue12447
class Test {
	static macro function foo() {
			var arr = [1, 3, 4, 5];

			return macro [$a{[for (i in arr) macro $v{i}]}];
	}

	#if !macro
	static function main() {
			trace([1, 3, 4, 5]);
			trace(foo());
	}
	#end
}
