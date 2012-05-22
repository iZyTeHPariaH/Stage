

type Transaction a = [a]


-- Modifier pour calculer le support des motifs (et pas uniquement des items)
support :: (Eq a) => a -> [Transaction a] -> Int
support item transactions = length [t | t <- transactions, item  `elem` t]


getContent l = getContent' l []
getContent' [] acc = acc
getContent' (x:xs) acc = getContent' xs (foldl f acc x)
    where f a e = if e `elem` a then a else (e:a)


-- Ensembles fréquents de taille inférieure ou égale à n (ensembles fréquents pour chaque n)
ensemblesFreq 1 transactions itemset minsupp = [[[item] | item <- itemset, support item >= minsupp]] 
ensemblesFreq n transactions itemset minsupp = ([e ++ [i] | e <- head ensemblesN_1,
                                                           i <- candidats,
                                                           not (i `elem` e)]:ensemblesN_1)
    where ensemblesN_1 = ensemblesFreq (n-1) transactions itemset minsupp
          candidats = getContent $  head ensemblesN_1