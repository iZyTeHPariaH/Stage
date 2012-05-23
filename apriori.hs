import Data.List

type Transaction a = [a]
type Itemset a = [a]



-- Détermine si un ensemble d'item est présent dans une transaction
memberp                     :: (Eq a) => Itemset a -> Transaction a -> Bool
memberp itemset transaction =  foldl (\a e -> a && (e `elem` transaction)) True itemset

-- Supprime les doublons d'une liste de listes d'items (liste d'ensembles fréquents)
clean liste = foldl (\a e -> if or (map (e `memberp`) a) then a else (e:a)) [] liste

-- Calcule le support d'un ensemble d'items dans une base de transactions
support :: (Eq a) => Itemset a -> [Transaction a] -> Int
support itemset transactions = length [t | t <- transactions, itemset  `memberp` t]

-- Récupère la liste des éléments présents dans une liste
getContent l = getContent' l []
getContent' [] acc = acc
getContent' (x:xs) acc = getContent' xs (foldl f acc x)
    where f a e = if e `elem` a then a else (e:a)


-- Ensembles fréquents de taille inférieure ou égale à n (ensembles fréquents pour chaque n)
ensemblesFreq 1 transactions itemset minsupp = [[[item] | item <- itemset, support [item] transactions >= minsupp]] 
ensemblesFreq n transactions itemset minsupp = ((clean ensemblesN):ensemblesN_1)
    where ensemblesN_1 = ensemblesFreq (n-1) transactions itemset minsupp
          ensemblesN = [ret | i <- candidats,
                              e <- head ensemblesN_1,
                              not (i `elem` e),
                              let ret = (i:e),
                              support ret transactions >= minsupp]
                              
          candidats = getContent $  head ensemblesN_1
          
          
          
db :: [Transaction Char]          
db = ["facdgimp",
      "abcflmo",
      "bfhjo",
      "bcksp",
      "afcelpmn"]
     
