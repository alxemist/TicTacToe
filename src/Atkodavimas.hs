module Atkodavimas
where
import Data.Char
import Data.List
 
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