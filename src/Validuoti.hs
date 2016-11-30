module Validuoti
where
import Atkodavimas
message :: String
message = "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi0e1:yi1ee1:2d1:v1:x1:xi1e1:yi0eee"

validate :: String -> Bool
validate str = validateMoves (pirmasZingsnis str)

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