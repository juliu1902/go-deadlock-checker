# go-deadlock-checker
A static deadlock detection tool for (limited) Go programs. Written in Haskell as part of a bachelorproject.

# supported GO-Syntax
```
Program        := { Statement } ;

Statement      := Send | Receive | End | If | For | Skip ;

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



# functions overview

### parseStatement
- **Input:** Go source code as a string
- **Output:** Statement
- **Purpose:** Parses a Go program into our internal `Statement` datatype using Megaparsec. Can be executes with `runParser`
  
### assignChannelsToStmts
- **Input:** Statement parsed by `parseStatement`
- **Output:** ChannelBehavior 
- **Purpose:** Tracks all Channels used in the Statement and assigns its Statement (turns a global statement into a "per-channel-view")
- **How it works:** the helper function chronologically breaks the Statement down into small (Channel, Statement)-pairs 
  which `assignChannelsToStmts` foldls, preserving the program order of the go operations.
  If/For blocks are allowed to mention more than one channel.

#### insertStmt
- **Input**: `ChannelBehavior`, `(Channel, Statement)`
- **Output**: Updated `ChannelBehavior`
- **Purpose**: Inserts a new action into the behavior of a channel, appending it via `Sequence` if the channel already exists.

