
import Data.Array
import Control.Monad.State
import Control.Monad.Writer


-- Un arbre de préfixes est soit vide, soit une valeur et une liste de branches
data FPTree a = FPTree a (Array Integer (FPTree a)) 
              | Node a
              deriving (Show,Eq)


getVal (FPTree x _) = x
getVal (Node x) = x

{- Accesseurs et modificateurs d'états -}
getS :: State a a
getS = state $ \s -> (s,s)

putS :: a -> State a ()
putS s = state $ \s' -> ((),s)



{-Modification de la valeur courante du noeud-}
setNodeVal :: a -> State (FPTree a) ()
setNodeVal val = do
  s <- getS
  case s of 
    Node _ -> putS $ Node val
    FPTree _ l -> putS $ FPTree val l
    
getNodeVal :: State (FPTree a) a
getNodeVal = do
  s <- getS
  case s of
    Node x -> return x
    FPTree x _ -> return x
    

-- Supprime le noeud comportant le symbole spécifié.
removeNode :: (Eq a) => a -> State (FPTree a) Bool
removeNode sym = do 
  currentTree <- getS
  case currentTree of
    Node _ -> return False
    FPTree val tr -> let (l,u) = bounds (tr)
                         new  = [arbre | arbre <- elems tr, getVal arbre /= sym] in
                     do putS $ FPTree val (listArray (l,u-1) new)
                        return True

{- Action représentant un déplacement dans un arbre en spécifiant la valeur d'un noeud.
   La fonction renvoie Just () si elle réussit, et Nothing si le noeud n'est pas trouvé 
-}
loadNode ::(Eq a) =>  a -> State (FPTree a) (Maybe ())
loadNode dir = do
  s <- getS
  case s of
    Node _ -> return Nothing
    FPTree _ l -> let rlist = [t | t@(FPTree val _) <- elems l, val == dir ] in
                    if null rlist then return Nothing else putS (head rlist) >> return (Just ())

{- Une direction est un symbole (celui des noeuds où on est passé)
-}
type Direction a = a
{- Pour se rappeller de chaque déplacement, on dispose des marques qui contiennent
   la direction qu'on a prise et le reste de l'arbre que l'on a pas exploré 
-}
type Mark a = (Direction a,FPTree a)
{- L'endroit où nous sommes dans l'arbre est en réalité un couple (Arbre, [Marques précédentes]).
   Ainsi, à chaque position, on connait non seulement le sous arbres où nous sommes, mais également
   l'ensemble des noeuds par les (lesquels on est passé, et enfin l'ensemble des sous arbres que nous n'avons
   pas exploré (et donc en somme, on connait la totalité de l'arbre)
-}

{-Permet de se déplacer dans l'arbre :
  avance d'un noeud, et rajoute le résultat à la liste des marques existantes -}
goToNextNode          :: (Eq a) => a -> [Mark a] -> State (FPTree a) ([Mark a])
goToNextNode sym path =  do   
  currentState <- getS    
  removeNode sym
  newState <- getS
  val <- getNodeVal
  putS currentState
  return $ (val,newState):path

{-Permet de se déplacer dans l'arbre en spécifiant
  une liste de symboles-}
goToNode         :: (Eq a) => [a] -> State (FPTree a) [Mark a]
goToNode symlist =  foldM (flip goToNextNode) [] symlist    

back1 :: (Eq a) => [Mark a] -> State (FPTree a) [Mark a]
back1 marks = do
  currentTree <- getS
  let (sym,FPTree val ct) = head marks 
  putS $ FPTree sym (listArray)