# go-deadlock-checker
A static deadlock detection tool for (limited) Go programs. Written in Haskell as part of a bachelorproject.

## functions overview

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
  If/For blocks are only allowed to mention exactly one channel at the moment, otherwise it rejects with an error. Skip is attached to the
  last seen channel.

#### trackLastChannel
- **Input**: `Statement`
- **Output**: `Maybe Channel`
- **Purpose**: Determines the last channel mentioned in a statement. Used mainly to associate `skip` statements with the correct channel.

#### insertStmt
- **Input**: `ChannelBehavior`, `(Channel, Statement)`
- **Output**: Updated `ChannelBehavior`
- **Purpose**: Inserts a new action into the behavior of a channel, appending it via `Sequence` if the channel already exists.

