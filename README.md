# SMC Language

An assembler and emulator for various MineCraft ISAs. Currently just an assembler, but will be extended to a full programming language in the future.

## Installation

Download the latest release from [here](https://github.com/BjornTheProgrammer/smc-language/releases). Each release has installers for:

* `smc-assembler`, the assembler CLI
* `smc-emu`, the emulator
* `smc-lsp`, the language server

## Supported ISAs

| Target | Source extension | Output formats |
| --- | --- | --- |
| `batpu2-mattbatwings-none` | `.smc` | `.mc`, or a schematic (`.schem`, `.litematic`, `.nbt`) |
| `tau-analyzers-none` | `.tasm` | `.tau`, or a schematic |
| `lyminal-uni-none` | `.lasm` | `.lym`, or a schematic |

## Why use this over [BatPU-2](https://github.com/mattbatwings/BatPU-2)

SMC assembly is a compatible alternative to BatPU-2, but offers a better development experience.

* Verbose and helpful error message
* Impossible to compile invalid code
* No Python VM needed
* Fast compilation
* Easy to use CLI
* Supports multiple MineCraft ISAs other than the BatPU-2
* An emulator to run and debug programs before building them in MineCraft
* [LSP support](https://github.com/BjornTheProgrammer/smc-zed-lsp)
* (WIP) support for other instruction data formats

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

The target is picked from the input's extension, and the output's extension picks the format. Pass `--target` to override the target.

```bash
smc-assembler compile ./minesweeper.smc ./minesweeper.mc
```

Schematics also need the layout of the instruction memory they're pasted into:

```bash
smc-assembler compile --format batpu2-instruction-memory ./minesweeper.smc ./minesweeper.schem
```

## Emulator

`smc-emu` runs Lyminal programs, either assembly (`.lasm`) or assembled binaries (`.lym`). Open one with LOAD, or pass it on the command line:

```bash
smc-emu ./hello-world.lasm
```

Saving the loaded file reloads it, so you can edit in your usual editor and watch the result. A save that doesn't assemble keeps the last working version and shows the error.

* RUN, STEP and RESET control the CPU, and the clock can be set from 1 Hz to 1 MHz
* The screen, gamepad, debugger, memory and console are panels. Pin them to any edge, stack them, resize them, or close them and reopen them from the toolbar. The layout is remembered between runs
* While paused, click a byte in the memory panel to edit ROM, RAM or registers in hex. Shift-click selects a range to copy or paste
* The console sends text to the ASCII port and shows what the program writes back

| Gamepad | Keys |
| --- | --- |
| D-pad | WASD or arrow keys |
| A | X or K |
| B | Z or J |
| Start | Enter |
| Select | Backspace |

BatPU-2 and Tau can be assembled, but not emulated yet.

## Extensions

If you want to extend SMC, feel free to create an issue, and a new tailored SMC version for your ISA could exist!
