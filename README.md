# go-deadlock-checker
A static deadlock detection tool for (limited) Go programs. Written in Haskell as part of a bachelor thesis.

## Overview
This tool analyzes pairs of Go-like functions to detect potential deadlocks by:
- Converting functions to session types
- Testing for behavioral equivalence (do both functions behave the same?)
- Testing for duality (are the functions complementary/can they interact without deadlocks?)

The analysis is based on session type theory and uses SMT(Z3) solving for constraint satisfaction.

## Installation & Build

### Prerequisites
- GHC >= 9.6
- Cabal >= 3.0

### Building
```bash
cabal update
cabal build
```

### Running
```bash
cabal run go-deadlock-checker [input-file]
```
If no input file is specified, it defaults to `inputs.txt`.

## Input Format
The input file contains test cases separated by `---`. Each test case has:
1. Two function definitions
2. Variable declarations 
3. Two function calls

Example:
```go
func sender (c chan int) {
    c <- 42
    close(c)
}
func receiver (c chan int) {
    <- c
}

var channel chan int
sender(channel)
receiver(channel)
```

## Output Explanation
For each test case, the tool outputs:
- Session Types: The behavioral representation of each function
- **A ~ B?**: Equivalence test result (True = functions behave identically)
- **A ^ B?**: Duality test result (True = functions can interact without deadlocks) and the rules that were applied to get the stated result

## Example Results
- `equiv FALSE`: Functions have different behaviors (e.g., different send order)
- `dual TRUE`: Functions are complementary and won't deadlock
- `dual FALSE`: Potential deadlock detected

## Limitations
- Only supports basic Go constructs (channels, conditionals, assignments)
- No support for loops, complex data structures, or dynamic channel creation in expressions
- Float expressions in conditions may cause runtime errors
- Limited to two-function analysis

## Formal grammar
```
Program     ::= Function Function {Declare} Functioncall Functioncall;

Function    ::= 'func' identifier '(' [Param {',' Param}] ')' Block;
Param       ::= identifier Type;

Functioncall::= identifier '(' [identifier {',' identifier}] ')';

Block       ::= '{' {Statement} '}';
Statement   ::= Declare | Send | End | If | Skip | Assign | Expr;

Type        ::= 'int' | 'bool' | 'chan int' | 'chan bool';
ChannelType ::= 'chan int' | 'chan bool';

Declare     ::= 'var' identifier Type;
Assign      ::= identifier ('=') Expr;

Send        ::= identifier '<-' Expr;
End         ::= 'close' '(' identifier ')';

If          ::= 'if' Expr 'then' Block 'else' Block;

Skip        ::= 'skip';
Expr        ::= BinOp | Var | bool | int | '<-' Expr | 'make' '(' ChannelType ')';
BinOp       ::= Expr Op Expr;
Op          ::= ('+' | '-' | '*' | '/' | '%' | '>=' | '<=' | 
                '==' | '!=' | '>' | '<' | '&&' | '||');
Var         ::= identifier;
bool        ::= 'true' | 'false';
int         ::= {'0'..'9'};
identifier  ::= ( 'a'..'z' | '_' ) { 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' }; 

Comments    := '//' | '/*' ... '*/';
```
In the code, Statement has two seperate constructors for make and receive instead of being defined in the Expression, since it made more sense to have
the statement as similar to the definition of a session type as possible.