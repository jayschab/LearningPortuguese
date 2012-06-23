module Lang.Portuguese where

import Data.List (splitAt)
import System.Random

conjugate :: Tense -> Subject -> String -> String 
conjugate ten sub str = maybe (error "conjugate") id $ lookup sub $ conjs ten str

conjs :: Tense -> String -> [(Subject,String)] 
conjs PresentInd str = zip enumAll $ zipWith (++) stems endings
    where (stem,end) = stemSplit str
          stems = repeat 4 stem
          endings = case end of
                        "ar" -> ["o","a","amos","am"] 
                        "er" -> ["o","e","emos","em"]
                        "ir" -> ["o","e","imos","em"]
conjs ImperfectInd str = zip enumAll $ zipWith (++) stems endings
    where (stem,end) = stemSplit str
          stems = repeat 4 stem
          endings = case end of
                        "ar" -> ["ava","ava","ávamos","avam"] 
                        _    -> ["ia","ia","ímos","iam"]
conjs PastSimpleInd str = zip enumAll $ zipWith (++) stems endings
    where (stem,end) = stemSplit str
          stems = if (end=="ar") && elem (last stem) estresses then ((init stem)++(maybe "" id $ lookup (last stem) estresses))++(repeat 3 stem)
                                                               else repeat 4 stem
          endings = case end of
                        "ar" -> ["ei","ou","amos","aram"] 
                        "er" -> [ "i","eu","emos","eram"]
                        "ir" -> [ "i","iu","imos","iram"]
conjs PluperfectInd str = [(FirstSingular,"testEu")]
conjs FutureInd str = [(FirstSingular,"testEu")]
conjs Conditional str = [(FirstSingular,"testEu")]
conjs PresentSub str = [(FirstSingular,"testEu")]
conjs ImperfectSub str = [(FirstSingular,"testEu")]
conjs FutureSub str = [(FirstSingular,"testEu")]
conjs PersonalInfinitive str = [(FirstSingular,"testEu")]
conjs Imperative  str = [(FirstSingular,"testEu")]

estresses = [("ç","c"),("c","qu"),("g","gu")]

stemSplit :: String -> (String,String)
stemSplit s = splitAt (len-2) s
    where len = length s

participle :: String -> String
participle verb = let (stem,end) = stemSplit verb in stem++(head end)++"ndo"    
    
enumAll :: (Enum a,Bounded a) => [a]
enumAll = enumFromTo minBound maxBound    

data Tense = PresentInd
           | ImperfectInd
           | PastSimpleInd
           | PluperfectInd
           | FutureInd
           | Conditional
           | PresentSub
           | ImperfectSub
           | FutureSub
           | PersonalInfinitive
           | Imperative 
           deriving (Eq,Enum,Bounded)
           
instance Show Tense where
    showsPrec _ ImperfectInd = showString "Imperfect Indicative"
    showsPrec _ PastSimpleInd = showString "Past Simple Indicative"
    showsPrec _ PluperfectInd = showString "Pluperfect Indicative"
    showsPrec _ FutureInd = showString "Future Indicative"
    showsPrec _ Conditional = showString "Conditional"
    showsPrec _ PresentSub = showString "Present Subjunctive"
    showsPrec _ ImperfectSub = showString "Imperfect Subjunctive"
    showsPrec _ FutureSub = showString "Future Subjunctive"
    showsPrec _ PersonalInfinitive = showString "Present Infinitive"
    showsPrec _ Imperative  = showString "Imperative"
 
instance Random Tense where
  randomR (a,b) g = applyFst toEnum $ randomR ( (fromEnum a) + 1, fromEnum b) g
  random g        = randomR (minBound,maxBound) g

applyFst f (a,b) = (f a,b)  
 
data Subject = FirstSingular
             | ThirdSingular
             | FirstPlural
             | ThirdPlural
             deriving (Eq,Enum,Bounded)
           
instance Show Subject where
    showsPrec _ FirstSingular = showString "Eu"
    showsPrec _ ThirdSingular = showString "Ele/Ela/Você"
    showsPrec _ FirstPlural = showString "Nós"
    showsPrec _ ThirdPlural = showString "Eles/Elas/Vocês"
       
instance Random Subject where
  randomR (a,b) g = applyFst toEnum $ randomR ( (fromEnum a) + 1, fromEnum b) g
  random g        = randomR (minBound,maxBound) g       