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

-- AcidState/IxSet imports
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy,SafeCopy )
import Data.Data            ( Data, Typeable )
import Data.IxSet           (Indexable(..), IxSet(..), (@=), (@*), Proxy(..), getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.Ratio
import Control.Monad.Reader (ask)
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
	do (Stats vfs) <- ask
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

mainLoop :: AcidState Stats -> IO ()
mainLoop acid = do 
	args <- getArgs
	let hiTen = case args of {[] -> maxBound ; (x:_) -> toEnum . read $ x}
        verbs <- map (splitOnFirst '\t') . lines <$> readFile "./verbs.txt"
        let len = length verbs
        seed <- newStdGen
        loop seed verbs (hiTen,len) acid

loop :: StdGen -> [(String,String)] -> (Tense,Int) -> AcidState Stats -> IO ()
loop seed verbs (hiTen,len) acid = do 
			system "clear"
			displayProblem (tense,subj,verb)
                        delay theDelay >> putStrLn ""
                        putStrLn $ conjugateR tense subj verb 
                        showOptions
        where ((tense,subj,idx),newSeed) = getRands seed (hiTen,len-1)
              (verb,def) = verbs !! idx
              showOptions = do
                                putStrLn $ tab2spaces 4 "[S]how All\t[D]efinition\t[C]orrect\t[W]rong\t[G]et Stats\t[Q]uit"
                                x <- toLower . head <$> getLine
                                case x of
                                   's' -> (putStrLn $ show $ conjs PresentInd verb) >> showOptions
                                   'd' -> (putStrLn def) >> showOptions
                                   'c' -> update' acid (UpdateStats (tense,subj,verb) True)
					  >> loop newSeed verbs (hiTen,len) acid
                                   'w' -> update' acid (UpdateStats (tense,subj,verb) False)
					  >> loop newSeed verbs (hiTen,len) acid
                                   'g' -> query' acid TenseStats >>= putStrLn . show
                                   'q' -> return ()


tab2spaces n = intercalate (replicate n ' ') . splitOn "\t"
displayProblem :: (Tense,Subject,String) -> IO ()
displayProblem (tense,subj,verb) = do
                    putStrLn $ show tense ++ "\t\t" ++ verb
                    putStrLn $ (show subj)
		    --putStrLn $ verb

loadIrregulars :: FilePath -> IO [(String, [[String]])]
loadIrregulars dir = do
                       allFiles <- filter (not . flip elem [".",".."]) <$> getDirectoryContents dir
                       let list = map (\f -> do  conjs <- map (splitOn "\t") . lines <$> readFile (dir </> f)
                                                 return (dropExtension f,conjs)
                                      ) allFiles
                       sequence list
                       
                                   
delay :: Int -> IO ()
delay n = sequence_ $ map (\x->putStr (show x) >> hFlush stdout >> dots ) [1..n]
        where putDot = sleep 333 >> putStr "." >> hFlush stdout 
              dots = sequence_ [putDot,putDot,putDot]
              sleep i = usleep (i*1000)
getRands :: StdGen -> (Tense,Int) -> ((Tense,Subject,Int),StdGen)
getRands seed (hiTen,hi) = flip runState seed $ (,,) <$> getRandomR (minBound,hiTen) <*> getRandom <*> getRandomR (0,hi)
        
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
