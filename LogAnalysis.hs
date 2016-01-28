module Main where

import           Log

main :: IO ()
main = undefined

parseMessageMessageType :: [String] -> (Maybe MessageType , String)
parseMessageMessageType (x:n:xs) = if x == "I"
						then (Just Info , unwords (n:xs))
						else if x == "E"
						then (Just (Error (read n)) , unwords xs) 
						else if x == "W"
						then (Just Warning , unwords (n:xs))
						else (Nothing, unwords xs)

parseMessageTimeStamp :: [String] -> (Maybe MessageType , TimeStamp, String)
parseMessageTimeStamp (as) = (m, (read w), unwords ws) where
	(m,xs) = parseMessageMessageType (as)
	(w:ws) = words xs

parseMessage :: String -> LogMessage
parseMessage (errorLine) = let (m,t,s) = parseMessageTimeStamp (words errorLine) in 
	case m of 
		Nothing -> Unknown errorLine
		Just mt -> LogMessage mt t s 
	

parse :: String -> [LogMessage]
parse (errorlog) = map (parseMessage) (lines errorlog)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown s1) x = x
insert lMessage Leaf = Node Leaf lMessage Leaf
insert lMessage (Node l lm r) = if lm > lMessage
								then Node (insert lMessage l) lm r
								else Node l lm (insert lMessage r)



instance Ord LogMessage where
	LogMessage mt1 ts1 s1 <= LogMessage mt2 ts2 s2 = ts1 <= ts2 
	Unknown s1 <= LogMessage mt2 ts2 s2 = False


  
