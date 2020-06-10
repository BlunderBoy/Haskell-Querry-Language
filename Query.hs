module Query where

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1
--transform inputul initial in lista de stringuri pentru intrari si un string pentru schema
splitBy :: Char -> String -> [String]
splitBy c = foldr op [[]] 
            where op x (y:ys)
                    | x /= c = (x:y):ys
                    | otherwise = []:(y:ys)

getRawSchemaTabel separatorLinie tabelString = head $ splitBy separatorLinie tabelString
getTableSchema :: ColSeparator -> LnSeparator -> String -> TableSchema
getTableSchema separatorColoana separatorLinie tabelString = splitBy separatorColoana (getRawSchemaTabel separatorLinie tabelString)

getRawIntrariTabel tabelString separatorLinie = tail $ splitBy separatorLinie tabelString
getIntrariTabel :: ColSeparator -> LnSeparator -> String -> [Entry]
getIntrariTabel separatorColoana separatorLinie tabelString = map (splitBy separatorColoana) (filter (\x -> x /= "") (getRawIntrariTabel tabelString separatorLinie)) 

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table separatorColoana separatorLinie tabelString = Table (getTableSchema separatorColoana separatorLinie tabelString)
                                                               (getIntrariTabel separatorColoana separatorLinie tabelString)

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str
-- TODO 2
whitespace :: Integer -> String
whitespace 0 = ""
whitespace numar = " " ++ (whitespace (numar-1))

showElem :: Integer -> Field -> String
showElem lungimeaColoanei string 
    | (toInteger (length string)) == lungimeaColoanei = string
    | otherwise = string ++ (whitespace (lungimeaColoanei - (toInteger (length string))))

printEntry :: [Integer] -> [String] -> String
printEntry _ [] = "|\n"
printEntry (x:xs) (y:ys) = "|" ++ (showElem x y) ++ (printEntry xs ys) 

printEntries :: [Integer] -> [Entry] -> String 
printEntries _ [] = ""
printEntries listaCuMaxime (x:xs) = (printEntry listaCuMaxime x) ++ (printEntries listaCuMaxime xs)

getNthElement :: [a] -> Integer -> a
getNthElement list n = list!!(fromIntegral n)

prepareBanner :: Integer -> String
prepareBanner 0 = "-\n"
prepareBanner n = "-" ++ (prepareBanner (n - 1))

lungimeBanner :: Table -> Integer
lungimeBanner (Table a b) = sum (listaMaxima (Table a b)) + (toInteger (length a))

showBanner (Table a b) = prepareBanner (lungimeBanner (Table a b))

numarColoane :: Table -> Integer
numarColoane (Table header intrari) = toInteger (length header)

coloanaN :: Table -> Int -> [String]
coloanaN (Table header []) index = [] 
coloanaN (Table header (x:xs)) index = if ((length x) > 1) then ((x !! index) : (coloanaN (Table header xs) index)) else []

listaDeColoaneH :: Table -> Int -> Integer -> [[String]]
listaDeColoaneH (Table header intrari) indexCurent cateColoane
    | ((toInteger indexCurent) < cateColoane ) = (coloanaN (Table header intrari) indexCurent) : (listaDeColoaneH (Table header intrari) (indexCurent + 1) cateColoane)
    | otherwise = []

listaDeColoane (Table header intrari ) = listaDeColoaneH (Table header intrari) 0 (numarColoane (Table header intrari))

lungimeMaximaDinLista :: [String] -> Integer
lungimeMaximaDinLista [] = 0
lungimeMaximaDinLista (x:xs) = toInteger $ max (length x) (fromIntegral (lungimeMaximaDinLista xs))

listaCuMaxime :: [[String]] -> [Integer]
listaCuMaxime (x:[]) = (lungimeMaximaDinLista x) : []
listaCuMaxime (x:xs) = (lungimeMaximaDinLista x) : (listaCuMaxime xs)

updatedMax :: Table -> [Integer] -> [Integer]
updatedMax (Table (y:[]) intrari) (x:[]) = if ((toInteger (length y)) > x) then [(toInteger (length y))] else [x]
updatedMax (Table (y:ys) intrari) (x:xs) = if ((toInteger (length y)) > x) then (toInteger (length y)) : (updatedMax (Table (ys) intrari) (xs)) else x : (updatedMax (Table (ys) intrari) (xs))

listaMaxima :: Table -> [Integer]
listaMaxima (Table header intrari) = updatedMax (Table header intrari) (listaCuMaxime (listaDeColoane (Table header intrari)))

instance Show Table where
    show (Table header entries) = (showTable (Table header entries)) where
        showTable (Table header entries) = (showBanner (Table header entries)) ++ (printEntry (listaMaxima (Table header entries)) header) ++ (showBanner (Table header entries)) ++ (printEntries (listaMaxima (Table header entries)) entries) ++ (showBanner (Table header entries))

-- select
indexOf :: String -> [String] -> Integer
indexOf element [] = -1
indexOf element (x:xs) = if ( element /= x ) then ( 1 + (indexOf element xs)) else 0

nume2IndexColoana :: [String] -> [String] -> [Integer]
nume2IndexColoana (x:[]) undeCaut = [indexOf x undeCaut]
nume2IndexColoana (x:xs) undeCaut = (indexOf x undeCaut) : (nume2IndexColoana xs undeCaut)

indexColoana2Coloane :: Table -> [Integer] -> [[String]]
indexColoana2Coloane tabel (x:[]) = [coloanaN tabel (fromIntegral x)]
indexColoana2Coloane tabel (x:xs) = (coloanaN tabel (fromIntegral x)) : (indexColoana2Coloane tabel xs)

rezultatSelectRaw :: [String] -> Table -> [[String]]
rezultatSelectRaw searches (Table header intrari) = indexColoana2Coloane (Table header intrari) (nume2IndexColoana searches header) 

--transpose btw
backToEntries:: [[String]]->[[String]]
backToEntries ([]:_) = []
backToEntries x = (map head x) : backToEntries (map tail x)

rezultatSelect :: [String] -> Table -> Table
rezultatSelect searches tabel = (Table searches (backToEntries (rezultatSelectRaw searches tabel)))

--select limit XD
rezultatSelectLimit :: [String] -> Table -> Integer -> Table
rezultatSelectLimit searches tabel limit = (Table searches (take (fromIntegral limit) (backToEntries (rezultatSelectRaw searches tabel))))

--filter
aCataColoana :: FilterCondition -> TableSchema -> Integer
aCataColoana (Lt camp _) schema = indexOf camp schema
aCataColoana (Eq camp _) schema = indexOf camp schema
aCataColoana (In camp _) schema = indexOf camp schema

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field numar) schema = \x -> ((read (x !! (fromIntegral (aCataColoana (Lt field numar) schema)))::Integer) < numar)
getFilter (Eq field string) schema = \x -> ((x !! (fromIntegral (aCataColoana (Eq field string) schema))) == string)
getFilter (In field range) schema = \x -> (elem (x !! (fromIntegral (aCataColoana (In field range) schema))) range)
getFilter (Not cond) schema = \x -> (not ((getFilter cond schema) x))

filtru :: (Entry -> Bool) -> Table -> Table
filtru functie (Table header intrari) = Table header (filter functie intrari)

merge :: Table -> Table -> Table
merge (Table header1 intrari1) (Table header2 intrari2) = (Table header1 (intrari1 ++ intrari2))

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom tabel) = tabel 
eval (Select cautari query) = rezultatSelect cautari (eval query)
eval (SelectLimit cautari limit query) = rezultatSelectLimit cautari (eval query) limit
eval (Filter conditie (Atom (Table schema intrari))) = filtru (getFilter conditie schema) (eval (Atom (Table schema intrari)))
eval (Filter conditie query) = eval (Filter conditie (Atom (eval query)))
eval (query1 :|| query2) = merge (eval query1) (eval query2)
-- TODO 5
getQuery :: String -> Table
getQuery string = eval (Filter (Eq "user_id" string) (Atom user_info))

getZone :: Table -> String
getZone (Table header intrari) = getNthElement (head intrari) (indexOf "zone" (head intrari))

same_zone :: String -> Query
same_zone string = Filter (Not (Eq "user_id" string)) $
                   Select ["user_id", "occupation"] $
                   Filter (Eq "zone" (getZone (getQuery string))) $
                   (Atom user_info)
------------------------
integerList :: Integer -> Integer -> [String]
integerList a b 
    | a < b = [show a] ++ (integerList (a+1) b)
    | otherwise = []

male_within_age :: Integer -> Integer -> Query
male_within_age a b = Select ["occupation", "zone"] $
                      Filter (In "age" (tail (integerList a b))) $
                      Filter (Eq "sex" "M") $
                      (Atom user_info)
                      

mixed :: [String] -> [String] -> Int -> Query
mixed zone ocupatii treshold = Select ["user_id"] $
                              Filter (In "zone" zone) $
                              Filter (In "occupation" ocupatii) $
                              Filter (Lt "age" (toInteger treshold)) $
                              (Atom user_info)
