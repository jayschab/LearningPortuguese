{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell, StandaloneDeriving
  , GeneralizedNewtypeDeriving, TypeFamilies #-}

import Lang.Portuguese
import System.Random
import System.IO
import Control.Applicative ( (<$>), (<*>) )
import System.Posix.Unistd(usleep)
import Control.Monad.State
import Control.Monad (sequence_)
import Data.Char (toLower)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import System.Directory
import System.FilePath
import System (system)
import Data.List (intercalate)
import Control.Monad.Trans.Reader

-- AcidState/IxSet imports
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy,SafeCopy )
import Data.Data            ( Data, Typeable )
import Data.IxSet           (Indexable(..), IxSet(..), (@=), (@*), Proxy(..), getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.Ratio
import qualified Control.Monad.Reader as Q
import Control.Monad.State  (get, put)
import Control.Exception    (bracket)



deriving instance Data Tense
deriving instance Data Subject

deriving instance Typeable Tense
deriving instance Typeable Subject

deriving instance Ord Tense
deriving instance Ord Subject

$(deriveSafeCopy 0 'base ''Tense)
$(deriveSafeCopy 0 'base ''Subject)


tense :: (Tense,Subject,String) -> Tense
tense (a,_,_) = a

subject :: (Tense,Subject,String) -> Subject
subject (_,b,_) = b

verb :: (Tense,Subject,String) -> String
verb (_,_,c) = c


data VerbFreq = VerbFreq
    { combo    :: (Tense,Subject,String)
    , trials   :: [Bool]
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''VerbFreq)

newtype Stats = Stats (IxSet VerbFreq) deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''Stats)



sumB :: [Bool] -> Int
sumB = length . filter (==True)

newtype AskedCount  = AskedCount Int    deriving (Eq, Ord, Data, Typeable, SafeCopy)
--newtype WordCount = WordCount Int deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable VerbFreq where
    empty = ixSet [ ixFun $ \vf -> [combo vf]
		  , ixFun $ \vf -> [ tense $ combo vf ]
		  , ixFun $ \vf -> [ subject $ combo vf ]
		  , ixFun $ \vf -> [ verb $ combo vf ]
                  , ixFun $ \vf -> [ AskedCount (length $ trials vf) ]
                  , ixFun $ \vf -> [ let bs = trials vf in (sumB bs % length bs) ]
                  ]

updateStats :: (Tense,Subject,String) -> Bool -> Update Stats  ()
updateStats (t,s,v) pass = 
	do (Stats vfs) <- get
	   let newVf = maybe (VerbFreq (t,s,v) [pass]) (\vf->VerbFreq (t,s,v) (pass:(trials vf))) $ getOne $ vfs @= (t,s,v)
	   put . Stats $ IxSet.updateIx (t,s,v)  newVf vfs 

tenseStats ::  Query Stats  [(Tense,(Int,Int))]
tenseStats = 
	do (Stats vfs) <- Q.ask
 	   return $ map (\t -> (t, counts . getTrials vfs $ t)) enumAll
	   where getTrials vs = join . map trials . IxSet.toList . (vs @=)
		 counts bs = (sumB bs, length bs)

$(makeAcidic ''Stats ['updateStats, 'tenseStats])

main =  do bracket (openLocalState $ Stats empty)
               (createCheckpointAndClose)
               (mainLoop)

-------------------------
theDelay :: Int
theDelay = 4

type Verb = String

data Params = Params { pHiTen :: Tense
                     , pNumVerbs :: Int
                     , pVerbs :: [(Verb,String)]
                     , pAcid :: AcidState Stats
                     }

mainLoop :: AcidState Stats -> IO ()
mainLoop acid = do 
	args <- getArgs
	let hiTen = case args of {[] -> maxBound ; (x:_) -> toEnum . read $ x}
        verbs <- map (splitOnFirst '\t') . lines <$> readFile "./verbs.txt"
        let len = length verbs
        seed <- newStdGen
        --loop seed verbs (hiTen,len) acid
        runReaderT (loop seed) $ Params hiTen len verbs acid


--loop :: StdGen -> [(String,String)] -> (Tense,Int) -> AcidState Stats -> IO ()
loop :: StdGen -> ReaderT Params IO ()
loop seed = do 
	liftIO $ system "clear"
        ((tense,subj,(verb,def)),newSeed) <- getProblem seed
	liftIO $ displayProblem (tense,subj,verb)
        liftIO $ delay theDelay >> putStrLn ""
        liftIO $ putStrLn $ conjugateR tense subj verb 
        showOptions tense subj verb def newSeed 
         where showOptions tense subj verb def seed = do
                                dispLine $ tab2spaces 4 "[S]how All\t[D]efinition\t[C]orrect\t[W]rong\t[G]et Stats\t[Q]uit"
                                x <- toLower . head <$> liftIO getLine
                                case x of
                                   's' -> (dispLine $ show $ conjs PresentInd verb) >> showOptions tense subj verb def seed
                                   'd' -> dispLine def >> showOptions tense subj verb def seed
                                   'c' -> ask >>= (\p -> update' (pAcid p) $ UpdateStats (tense,subj,verb) True)  >> loop seed
                                   'w' -> ask >>= (\p -> update' (pAcid p) $ UpdateStats (tense,subj,verb) False)  >> loop seed
                                   'g' -> ask >>= (\p -> query' (pAcid p) TenseStats) >>= dispLine . show >> return ()
                                   'q' -> return ()
                  where dispLine = liftIO . putStrLn


tab2spaces n = intercalate (replicate n ' ') . splitOn "\t"

displayProblem :: (Tense,Subject,Verb) -> IO ()
displayProblem (tense,subj,verb) = do
                    putStrLn $ show tense ++ "\t\t" ++ verb
                    putStrLn $ (show subj)

getProblem seed = 
             do p <- ask
                let hiTen = pHiTen p
                let num = pNumVerbs p
                let ((t,s,idx),newSeed) = getRands seed hiTen num
                let (v,d) = (pVerbs p) !! idx
                return $ ((t,s,(v,d)),newSeed)
                                   
delay :: Int -> IO ()
delay n = sequence_ $ map (\x->putStr (show x) >> hFlush stdout >> dots ) [1..n]
        where putDot = sleep 333 >> putStr "." >> hFlush stdout 
              dots = sequence_ [putDot,putDot,putDot]
              sleep i = usleep (i*1000) --for Windows comment this out

getRands :: StdGen -> Tense -> Int -> ((Tense,Subject,Int),StdGen)
getRands seed hiTen hi = flip runState seed $ (,,) <$> getRandomR (minBound,hiTen) <*> getRandom <*> getRandomR (0,hi)
        
getRandomR :: (Random a) => (a,a) -> State StdGen a
getRandomR (lo,hi) = do seed <- get
                        let (val,newSeed) = randomR (lo,hi) seed
                        put newSeed
                        return val

getRandom :: (Random a, Bounded a) => State StdGen a
getRandom = getRandomR (minBound,maxBound)
                        
splitOnFirst :: (Eq a) => a -> [a] -> ([a],[a])
splitOnFirst chr xs = helper xs []
    where helper (c:cs) ys = if c == chr then (reverse ys,cs) else helper cs (c:ys)
          helper [] ys = (reverse ys,[])
