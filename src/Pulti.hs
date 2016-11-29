module Pulti
where
import Data.Char
import Data.List
import Network.HTTP.Client
 

message :: String
message = "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi0e1:yi1ee1:2d1:v1:x1:xi1e1:yi0eee"

move :: String -> Maybe (Int, Int, Char)
move str = calculteMove (pirmasZingsnis str)

calculteMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
calculteMove moves = findMove moves

parseMove :: Maybe (Int, Int, Char) -> Maybe String
parseMove Nothing = Nothing
parseMove (Just (x,y,z)) = Just ("d1:v1:x1:xi" ++ [(intToDigit x)] ++  "e1:yi" ++ [(intToDigit y)] ++"eee")

turn :: String -> Maybe String
turn msg = parseMove $ move msg 

findMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
findMove moves  
    | (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[] = Just (digitToInt '1',digitToInt '1','x')
    | (((length(moves))==2) && ((filter(\(a,b,c)-> a=='0' && b=='0') moves)==[])) = Just (digitToInt '0',digitToInt '0','x')
    | (((length(moves))==2) && ((filter(\(a,b,c)-> a=='2' && b=='0') moves)==[])) = Just (digitToInt '2',digitToInt '0','x')
    -- Kai x = 0
    | (((length(filter(\(a,b,c)-> a=='0' && c=='o') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='0') moves)==[])) = Just (digitToInt '0',digitToInt '0','x')
    | (((length(filter(\(a,b,c)-> a=='0' && c=='o') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='1') moves)==[])) = Just (digitToInt '0',digitToInt '1','x')
    | (((length(filter(\(a,b,c)-> a=='0' && c=='o') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='2') moves)==[])) = Just (digitToInt '0',digitToInt '2','x')
    -- Kai x = 2
    | (((length(filter(\(a,b,c)-> a=='2' && c=='o') moves))==2) && ((filter(\(a,b,c)-> a=='2' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='0') moves)==[])) = Just (digitToInt '2',digitToInt '0','x')
    | (((length(filter(\(a,b,c)-> a=='2' && c=='o') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='1') moves)==[])) = Just (digitToInt '2',digitToInt '1','x')
    | (((length(filter(\(a,b,c)-> a=='2' && c=='o') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='2') moves)==[])) = Just (digitToInt '2',digitToInt '2','x')
    -- Kai y = 0
    | (((length(filter(\(a,b,c)-> b=='0' && c=='o') moves))==2) && ((filter(\(a,b,c)-> b=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='0') moves)==[])) = Just (digitToInt '0',digitToInt '0','x')
    | (((length(filter(\(a,b,c)-> b=='0' && c=='o') moves))==2) && ((filter(\(a,b,c)-> b=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='1' && b=='0') moves)==[])) = Just (digitToInt '1',digitToInt '0','x')
    | (((length(filter(\(a,b,c)-> b=='0' && c=='o') moves))==2) && ((filter(\(a,b,c)-> b=='0' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='0') moves)==[])) = Just (digitToInt '2',digitToInt '0','x')
    -- Kai y = 2
    | (((length(filter(\(a,b,c)-> b=='2' && c=='o') moves))==2) && ((filter(\(a,b,c)-> b=='2' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='2') moves)==[])) = Just (digitToInt '0',digitToInt '2','x')
    | (((length(filter(\(a,b,c)-> b=='2' && c=='o') moves))==2) && ((filter(\(a,b,c)-> b=='2' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='1' && b=='2') moves)==[])) = Just (digitToInt '1',digitToInt '2','x')
    | (((length(filter(\(a,b,c)-> b=='2' && c=='o') moves))==2) && ((filter(\(a,b,c)-> b=='2' && c=='x') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='2') moves)==[])) = Just (digitToInt '2',digitToInt '2','x')
    -- Tikrinami visi like atvejai
    | (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[] = Just (digitToInt '0',digitToInt '0','x')
    | (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[] = Just (digitToInt '2',digitToInt '0','x')
    | (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[] = Just (digitToInt '0',digitToInt '2','x')
    | (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[] = Just (digitToInt '2',digitToInt '2','x')
    | (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[] = Just (digitToInt '0',digitToInt '1','x')
    | (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[] = Just (digitToInt '1',digitToInt '0','x')
    | (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[] = Just (digitToInt '1',digitToInt '2','x')
    | (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[] = Just (digitToInt '2',digitToInt '1','x')
    | otherwise = Nothing

pirmasZingsnis :: String -> [(Char,Char,Char)]
pirmasZingsnis "" = error "no list"
pirmasZingsnis ('d' :rest) = antrasZingsnis (take ((length rest) -1) rest)
pirmasZingsnis _ = error "netinkamas tipas"

antrasZingsnis :: String -> [(Char,Char,Char)]
antrasZingsnis "" = []
antrasZingsnis a = treciasZingsnis a []

treciasZingsnis :: String -> [(Char,Char,Char)] -> [(Char,Char,Char)]
treciasZingsnis a [] = 
    let
        v = head (drop 9 a)
        x = head (drop 14 a)
        y = head (drop 20  a)
        rest = drop 23 a
    in treciasZingsnis rest ((x,y,v) : [])
treciasZingsnis ('1' :a) acc =
    let
        v = head (drop 8 a)
        x = head (drop 13 a)
        y = head (drop 19  a)
        rest = drop 22 a
    in treciasZingsnis rest ((x,y,v):acc)
treciasZingsnis "" acc = acc