# SMC Language

Currently just an assembler, but will be extended to a full programming language.

## Installation

Have cargo installed on your system first, you can install it from [here](https://rust-lang.org/tools/install/).

```bash
cargo install --git https://github.com/BjornTheProgrammer/smc-assembler
```

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
compile ./minesweeper.smc ./minesweeper.schem
```
