It is inspired by different kaleidoscope toy compiler implementations:
https://llvm.org/docs/tutorial/
https://github.com/jauhien/iron-kaleidoscope
https://github.com/sdiehl/kaleidoscope

It is tested on llvm 6.0.0 and gcc 4. 

This example generates a human readable IR representation instead of a bitcode based one.

It uses parseback as parser (both lexical and syntax ones): https://github.com/djspiewak/parseback 

To run type in console: sbt pack && ./compileAndRun.sh "def run(a) sub(a);def sub(a) 3.0*sqrt(fn2(a)); extern sqrt(a); def fn2(a) a*2.0"

After run build directory will contain result.ll and ast.png. Last one is parsed code represented as an AST tree. 