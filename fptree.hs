
import Data.Array
import Control.Monad.State
import Control.Monad.Writer


-- Un arbre de préfixes est soit vide, soit une valeur et une liste de branches
data FPTree a = Empty 
              | FPTree a (Array Integer (FPTree a)) 
                deriving Show


type Walker a = WriterT Integer (State (FPTree a))




seDeplacer i Empty=  return Empty
seDeplacer i (FPTree a tr)  =   do 
  tell i 
  return (tr ! i)
                                 

seDeplacer' i = do  
  s <- lift gets
  case s of
    (FPTree a tr) -> do tell i
                        lift (put (tr ! i))
    otherwise -> return ()
    