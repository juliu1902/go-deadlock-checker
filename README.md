# go-deadlock-checker
A static deadlock detection tool for (limited) Go programs. Written in Haskell as part of a bachelorproject.

# supported GO-Syntax
```
Program        := { Declaration } { Statement } ;
Declaration    := var x int|bool|chan int|chan bool ;
Statement      := Send | Receive | End | If | For | Skip ;
New Channel    := make (chan int|bool) ;
Skip           := 'skip' ;
Send           := identifier '<-' Expr ;
Receive        := identifier '= <-' identifier ;
End            := 'close' identifier ;

If             := 'if' Expr 'then' ( Block | Statement )
                  'else' ( Block | Statement ) ; // a single statement doesn't need to be wrapped in {}

For            := 'for' Expr Block ; // single statement needs to be wrapped in {}

Block          := '{' Statement '}' ;

Expr           := Atom { Op Atom } ;  // left asssociative and no * before + etc.
Atom           := Bool | Number | identifier ;
Bool           := 'true' | 'false' ;
Op             := '+' | '-' | '*' | '/' | '%' | '>=' | '<=' | '==' | '!=' | '>' | '<' | '&&' ;

identifier     := ( 'a'..'z' | '_' ) { 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' } ; 
number         := signed-integer | signed-float ;
Comments       := '//' | '/*' ... '*/' ; // allowed everywhere
Assignments    := identifier ':=' ( number | identifier | Expr ) ; // allowed everywgere
```

##### Notes:

- `{}`= zero or more times

- `|`= 'or'

- Weird expressions like `true + false`,`1<false`, `42 && x` allowed.
- whitespaces and tabs are allowed everywhere as long 
- assignments allowed between statements

- Skip is technically parsed and allowed where a Statement is also allowed, but it's only functional inside an If.

# Channels
### Naming
Every channel gets its name from the identifier of `var id chan int`
### Internal Representation
In our variable environment every channel gets represented as an `Achan id` out of our Datatype `AbstractValue`. That's if `id`
is referencing to the channel with name `id` by `id ::= make (chan int)`.

If a variable, let's call it `c`, refers to different channels based on a condition b, then its represented by `Aif cond v1 v2`.
Every operation with this `c` (f.e. we want to send on c) creates a `if cond then ... else ...` process.

examples:
```
var c1 chan int
c1 ::= make (chan int)
```
Type of c: `TChan CInt`
Value of c: `Achan "c1"`
```
var c1 chan int
var c2 chan int
var c chan int
c1 ::= make (chan int)
c2 ::= make (chan int)
if b then { c = c1 } else { c = c2 }
```
Type of c: `TChan CInt`
Value of c: `Aif b (Achan "c1") (Achan "c2")`

```
var x int
x ::= 0
```
Type of x: `TInt`
Value of x: `Aterm (EInt 0)`

# functions overview

### parseStatement
- **Input:** Go source code as a string
- **Output:** Statement
- **Purpose:** Parses a Go program into our internal `Statement` datatype using Megaparsec. Can be executes with `runParser`

### stmtToST
- **Input:** Statement
- **Output:** Session Type as string
- **Purpose:** represents parsed Statements as Session Types/processes
  

