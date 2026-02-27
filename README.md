# go-deadlock-checker
A static deadlock detection tool for (limited) Go programs. Written in Haskell as part of a bachelor thesis.

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
