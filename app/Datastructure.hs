module Datastructure where

data Statement = Skip | Send Channel | Receive Channel | End Channel | Sequence Statement Statement | If Expr Statement Statement | For Expr Statement
type Expr = String
type Channel = String
newtype ChannelBehavior = ChannelBehavior [(Channel, Statement)]

instance Show Statement where
  show x = case x of
    Skip -> "skip"
    Send _ -> "!"
    Receive _ -> "?"
    End _ -> "end"
    Sequence s1 s2 ->  show s1 ++ ";" ++ show s2
    If e s1 s2 -> show s1 ++ " if " ++ e ++ " else " ++ show s2
    For e s-> "for " ++ e ++ " " ++ show s

instance Show ChannelBehavior where
    show (ChannelBehavior []) = ""
    show (ChannelBehavior(x:xs)) = fst x ++ " |-> " ++ show (snd x) ++ "\n" ++ show (ChannelBehavior xs)

-- Reihenfolge so richtig, damit trackLastChannel priorisiert wird in der helper
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y  = y

-- inserts new Channel Statement pair in given ChannelBehavior or adds it with Sequence to already existing Channel
insertStmt :: ChannelBehavior -> (Channel, Statement) -> ChannelBehavior
insertStmt  (ChannelBehavior [])(ch, st) = ChannelBehavior [(ch, st)]
insertStmt  (ChannelBehavior (x:xs))(ch, st)
    | ch == fst x = ChannelBehavior ((ch, Sequence (snd x) st):xs)
    | otherwise =
        let ChannelBehavior rest = insertStmt (ChannelBehavior xs)(ch, st) 
        in ChannelBehavior (x : rest)

assignChannelsToStmts :: Statement -> ChannelBehavior
assignChannelsToStmts stmt =
    let tuples = helper stmt Nothing
    in foldl insertStmt (ChannelBehavior[]) tuples -- merging multiple (Channel, Statement)-pairs with the same channelname to one single (Channel, Statement)
    where
        -- helper function takes a Statement and the channel of the last detected action (mainly necessary for skip)
        -- and creates its many (Channel,Statement) pairs (!! without grouping the same channels into one single (Channel, Statement) pair at first!!)
        helper :: Statement -> Maybe Channel -> [(Channel, Statement)]
        helper s currentChannel = case s of
            Send ch      -> [(ch, s)]
            Receive ch   -> [(ch, s)]
            End ch       -> [(ch, s)]
            Skip         -> case currentChannel of
                Just ch -> [(ch, s)]
                Nothing -> []  -- sollte nicht auftreten
            Sequence s1 s2 ->
                let res1 = helper s1 currentChannel
                    lastChannel = trackLastChannel s1 `orElse` currentChannel
                    res2 = helper s2 lastChannel
                in res1 ++ res2
            If cond s1 s2 ->
                let grouped1 = foldl insertStmt (ChannelBehavior[]) (helper s1 currentChannel)
                    grouped2 = foldl insertStmt (ChannelBehavior[]) (helper s2 currentChannel)
                in case (grouped1, grouped2) of
                    ((ChannelBehavior[(ch1, st1)]), (ChannelBehavior[(ch2, st2)])) | ch1 == ch2 ->
                        [(ch1, If cond st1 st2)]
                    _ -> error "If-Branching über mehrere Channels hinweg nicht behandelt"
            For expr s1 ->
                let grouped = foldl insertStmt (ChannelBehavior[]) (helper s1 currentChannel)
                in case grouped of
                    (ChannelBehavior [(ch, st)]) -> [(ch, For expr st)]
                    _ -> error "For-Schleife über mehrere Channels hinweg nicht behandelt"

trackLastChannel :: Statement -> Maybe Channel
trackLastChannel stmt = case stmt of
    Skip               -> Nothing
    Send ch            -> Just ch
    Receive ch         -> Just ch
    End ch             -> Just ch
    Sequence s1 s2     ->
        case trackLastChannel s2 of
            Just ch -> Just ch
            Nothing -> trackLastChannel s1
    If _ s1 s2         ->
        case (trackLastChannel s1, trackLastChannel s2) of
            (Just c1, Just _) -> Just c1 -- IMPORTANT: Case "more than one channel is mentioned in one If" is not properly implemented! TODO
            (Just c1, Nothing) -> Just c1
            (Nothing, Just c2) -> Just c2
            (Nothing, Nothing) -> Nothing
    For _ s -> trackLastChannel s