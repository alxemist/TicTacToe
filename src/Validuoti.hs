module Validuoti
where
message :: String
message = "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi0e1:yi1ee1:2d1:v1:x1:xi1e1:yi0eee"

validate :: String -> Bool
validate str = validateMoves (dropDictionaryEncode str)

validateMoves :: [(Char,Char,Char)] -> Bool
validateMoves moves
    -- Tikrina ar tai nebuvo paskutinis ejimas  
    | (length(moves)) == 9 = True
    -- Tikrina stulpelius ar nelaimeta
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='1' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='2' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==3)) = True
    -- Tikrina eilutes ar nelaimeta
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='1' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='2' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==3)) = True
    -- Tikrina istrizaines ar nelaimeta
    | (((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves))==1)) = True
    | (((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves))==1)) = True
    | otherwise = False

dropDictionaryEncode :: String -> [(Char,Char,Char)]
dropDictionaryEncode "" = error "Message is empty"
dropDictionaryEncode ('d' :rest) = parseMoves (take ((length rest) -1) rest) []
dropDictionaryEncode _ = error "Wrong message used"

parseMoves :: String -> [(Char,Char,Char)] -> [(Char,Char,Char)]
parseMoves a [] = 
    let
        v = head (drop 9 a)
        x = head (drop 14 a)
        y = head (drop 20  a)
        rest = drop 23 a
    in parseMoves rest ((x,y,v) : [])
parseMoves ('1' :a) listOfMoves =
    let
        v = head (drop 8 a)
        x = head (drop 13 a)
        y = head (drop 19  a)
        rest = drop 22 a
    in parseMoves rest ((x,y,v):listOfMoves)
parseMoves "" listOfMoves = listOfMoves