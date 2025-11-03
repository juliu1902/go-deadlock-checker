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
Every channel has a unique "internal" name, `id<n>` with n being an increasing integer starting with 0. The Channel name has type ChannelID and can
only be given internally. A fresh Channel is generated with `var c chan int` or `var c chan bool`
### Internal Representation
In our variable environment every channel gets represented as an `AChan id` out of our Datatype `AbstractValue`. That's if `id` is created
with `var id chan int`

If a variable, let's call it `c`, refers to different channels based on a condition b, then its represented by `AIf cond v1 v2`.
Every operation with this `c` (f.e. we want to send on c) creates a `if cond then ... else ...` process.

examples:
```
var c1 chan int
c1 ::= make (chan int)
```
Type of c: `TChan CInt`
Value of c: `Achan "c1"`
```
var b bool
var c1 chan int
var c2 chan int
var c chan int
if b then { c = c1 } else { c = c2 }
```
Type of c: `TChan CInt`
Value of c: `AIf b (AChan "id0") (AChan "id1")`

```
var x int
x ::= 0
```
Type of x: `TInt`
Value of x: `ATerm (EInt 0)`
  

