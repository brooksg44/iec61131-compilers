# IEC 61131-3 Compilers

This project provides Clojure-based compilers for the IEC 61131-3 PLC programming languages:

- LD (Ladder Diagram)
- ST (Structured Text)
- IL (Instruction List)

## Prerequisites

- [Clojure CLI tools](https://clojure.org/guides/install_clojure)
- Java JDK 8 or higher

## Project Structure

```
iec61131-compilers/
├── deps.edn                       # Dependency configuration
├── README.md                      # Project documentation
├── resources/                     # Resource files
│   └── examples/                  # Example programs
└── src/                           # Source code
    └── iec61131/                  # Source files by language
```

## Usage

### Setup

1. Clone this repository:
   ```bash
   git clone https://your-repository-url/iec61131-compilers.git
   cd iec61131-compilers
   ```

2. Ensure your LD source file is available (examples are in `resources/examples/`)

### Running the Compilers

To run the LD (Ladder Diagram) compiler:

```bash
# Compile a Ladder Diagram to Instruction List
clj -M:run-ld-compiler -i path/to/your/program.ld

# Compile to C
clj -M:run-ld-compiler -i path/to/your/program.ld -t c

# Compile to JavaScript
clj -M:run-ld-compiler -i path/to/your/program.ld -t js

# Output to a file
clj -M:run-ld-compiler -i path/to/your/program.ld -o output.il
```

### Building an Executable JAR

To build a standalone JAR file:

```bash
clj -X:uberjar
```

This creates `iec61131-compilers.jar` which can be run with:

```bash
java -jar iec61131-compilers.jar -i path/to/your/program.ld
```

## Examples

### Ladder Diagram (LD) Example

```
PROGRAM Example
VAR
  Input1, Input2 : BOOL;
  Output1 : BOOL;
END_VAR

NETWORK 1: Main Logic
|--| |--Input1--|/|--Input2--|(  )--Output1--|
|
```

### Compiling the Example

```bash
# Assuming the example is saved as resources/examples/example.ld
clj -M:run-ld-compiler -i resources/examples/example.ld
```

This will output the compiled IL code to the console.

## License

[Specify your license here]