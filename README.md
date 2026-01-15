# SMC Language

An assembler that supports various MineCraft ISAs. Currently just an assembler, but will be extended to a full programming language in the future.

## Installation

Have cargo installed on your system first, you can install it from [here](https://rust-lang.org/tools/install/).

```bash
cargo install smc-assembler
```

## Why use this over [BatPU-2](https://github.com/mattbatwings/BatPU-2)

SMC assembly is a compatible alternative to BatPU-2, but still offers better a better development experience.

* Verbose and helpful error message
* Impossible to compile invalid code
* No Python VM needed
* Fast compilation
* Easy to use CLI
* Supports multiple MineCraft ISAs other than the BatPU-2

### Error Messages

Here are some examples of SMC error messages:

```
Compilation failed with 3 error(s):

    --> ./crates/assembler/tests/programs/2048.smc:780:22
    |
780 |   LDI r14 0b11111111 whoops
    |                      ^^^^^^
    | Semantic Error: Expected operation, but recieved Identifier("whoops")

    --> ./crates/assembler/tests/programs/2048.smc:787:3
    |
787 |   STR r13 r14 1
    |   ^^^
    | Semantic Error: Expected immediate (number or define), but recieved Keyword(LoweredOperation(Str))

    --> ./crates/assembler/tests/programs/2048.smc:860:11
    |
860 |   LDI r14 + 0b11100101
    |           ^
    | Syntax Error: Unexpected character `+`

Error: Compilation failed
```

The key thing to notice is that the error messages are verbose and helpful.
They show you the location of the error. Additionally every error that is possible
will display all at once. SMC will not compile invalid code.

## Usage

```
Usage: smc-assembler <COMMAND>

Commands:
  compile  Compiles the given source file
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

### Compiling

With the compile command, the ext given will determine the output format.

```bash
smc-assembler compile --target batpu2-mattbatwings-none ./minesweeper.smc ./minesweeper.schem
```

## Extensions

If you want to extend SMC, feel free to create an issue, and a new tailored SMC version for your ISA could exist!
