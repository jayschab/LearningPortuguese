module Lang.Portuguese where

import Data.List (splitAt)
import System.Random

conjugate :: Tense -> Subject -> String -> String 
conjugate ten sub str = maybe (error "conjugate") id $ lookup sub $ conjs ten str

conjugateR :: Tense -> Subject -> String -> String 
conjugateR ten sub str = maybe (error "conjugate") id $ lookup sub $ conjsR ten str

conjsR :: Tense -> String -> [(Subject,String)] 
conjsR tense verb
    | tense `elem` [PresentProg,PastProg] = conjs tense verb
    | otherwise = maybe (conjs tense verb) (zip enumAll . (!! (tenseToIdx tense))) $ lookup verb irregulars

conjs :: Tense -> String -> [(Subject,String)] 
conjs PresentInd str = zip enumAll $ zipWith (++) stems endings
    where (stem,end) = stemSplit str
          stems = replicate 4 stem
          endings = case end of
                        "ar" -> ["o","a","amos","am"] 
                        "er" -> ["o","e","emos","em"]
                        "ir" -> ["o","e","imos","em"]
conjs ImperfectInd str = zip enumAll $ zipWith (++) stems endings
    where (stem,end) = stemSplit str
          stems = replicate 4 stem
          endings = case end of
                        "ar" -> ["ava","ava","ávamos","avam"] 
                        _    -> ["ia","ia","ímos","iam"]
conjs Preterite str = zip enumAll $ zipWith (++) stems endings
    where (stem,end) = stemSplit str
          stems = if (end=="ar") && elem (last stem) (map fst estresses) 
                     then ((init stem)++(maybe "" id $ lookup (last stem) estresses)):(replicate 3 stem)
                     else replicate 4 stem
          endings = case end of
                        "ar" -> ["ei","ou","amos","aram"] 
                        "er" -> [ "i","eu","emos","eram"]
                        "ir" -> [ "i","iu","imos","iram"]
conjs PluperfectInd str = zipWith (\(s,x) y -> (s,x++" "++y)) (conjsR ImperfectInd "ter") $ repeat (pastParticiple str)
conjs FutureInd str = zip enumAll $ zipWith (++) stems endings
    where stems = replicate 4 $ maybe str id $ lookup str special
          endings = ["ei","á","emos","aõ"] 
          special = [("dizer","dir"),("fazer","fir"),("trazer","trar")]
conjs Conditional str = zip enumAll $ zipWith (++) stems endings
    where stems = replicate 4 $ maybe str id $ lookup str special
          endings = ["ia","ia","íamos","iam"] 
          special = [("dizer","dir"),("fazer","fir"),("trazer","trar")]
conjs PresentSub str = zip enumAll $ zipWith (++) stems endings
    where (_,end) = stemSplit str
          stem = init $ conjugateR PresentInd FirstSingular str
          stems = if (end=="ar") && elem (last stem) (map fst estresses) 
                     then ((init stem)++(maybe "" id $ lookup (last stem) estresses)):(replicate 3 stem)
                     else replicate 4 stem
          endings = case end of
                        "ar" -> ["e","e","emos","em"] 
                        _    -> [ "a","a","amos","am"]
conjs ImperfectSub str = zip enumAll $ zipWith (++) stems endings
    where stems = replicate 4 $ take (length pretThird - 3) pretThird
          pretThird = conjugateR Preterite ThirdPlural str
          endings = ["sse","sse","ssemos","ssem"]
          --special spelling changes          
conjs FutureSub str = zip enumAll $ zipWith (++) stems endings
    where stems = replicate 4 $ take (length pretThird - 3) pretThird
          pretThird = conjugateR Preterite ThirdPlural str
          endings = ["r","r","rmos","rem"] 
conjs PersonalInfinitive str = zip enumAll $ repeat "not implemented"
conjs Imperative  str = zip enumAll $ repeat "not implemented"
conjs PresentProg str = zipWith (\(s,x) y -> (s,x++" "++y)) (conjsR PresentInd "estar") $ repeat (presentParticiple str)
conjs PastProg str = zipWith (\(s,x) y -> (s,x++" "++y)) (conjsR ImperfectInd "estar") $ repeat (presentParticiple str)

estresses = [('ç',"c"),('c',"qu"),('g',"gu")]

stemSplit :: String -> (String,String)
stemSplit s = splitAt (len-2) s
    where len = length s

presentParticiple :: String -> String
presentParticiple verb = let (stem,end) = stemSplit verb in stem++(head end:"ndo")
    
pastParticiple :: String -> String
pastParticiple verb = let (stem,end) = stemSplit verb in stem++((if head end == 'a' then 'a' else 'i'):"do")    
    
enumAll :: (Enum a,Bounded a) => [a]
enumAll = enumFromTo minBound maxBound    

data Tense = PresentInd
           | ImperfectInd
           | Preterite
           | PresentProg
           | PastProg
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
    showsPrec _ PresentInd = showString "Present"
    showsPrec _ ImperfectInd = showString "Imperfect"
    showsPrec _ Preterite = showString "Preterite"
    showsPrec _ PresentProg = showString "Present Progressive"
    showsPrec _ PastProg = showString "Past Progressive" 
    showsPrec _ PluperfectInd = showString "Pluperfect Indicative"
    showsPrec _ FutureInd = showString "Future Indicative"
    showsPrec _ Conditional = showString "Conditional"
    showsPrec _ PresentSub = showString "Present Subjunctive"
    showsPrec _ ImperfectSub = showString "Imperfect Subjunctive"
    showsPrec _ FutureSub = showString "Future Subjunctive"
    showsPrec _ PersonalInfinitive = showString "Present Infinitive"
    showsPrec _ Imperative  = showString "Imperative"
 
instance Random Tense where
  randomR (a,b) g = applyFst toEnum $ randomR (fromEnum a, fromEnum b) g
  random g        = randomR (minBound,maxBound) g

tenseToIdx PresentInd = 0
tenseToIdx ImperfectInd = 1
tenseToIdx Preterite = 2
tenseToIdx PluperfectInd = 3
tenseToIdx FutureInd = 4
tenseToIdx Conditional = 5
tenseToIdx PresentSub = 6
tenseToIdx ImperfectSub = 7
tenseToIdx FutureSub = 8
tenseToIdx PersonalInfinitive = 9 
tenseToIdx Imperative = 10

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
  randomR (a,b) g = applyFst toEnum $ randomR (fromEnum a, fromEnum b) g
  random g        = randomR (minBound,maxBound) g       
  
  
irregulars = [
        ("vir"
        ,[
             ["venho","vem","vimos","v\234m"]
            ,["vinha","vinha","v\237nhamos","vinham"]
            ,["vim","veio","viemos","vieram"]
            ,["viera","viera","vi\233ramos","vieram"]
            ,["virei","vir\225","viremos","vir\227o"]
            ,["viria","viria","vir\237amos","viriam"]
            ,["venha","venha","venhamos","venham"]
            ,["viesse","viesse","vi\233ssemos","viessem"]
            ,["vier","vier","viermos","vierem"]
            ,["vir","vir","virmos","virem"]
            ,["--","venha","venhamos","venham"]]
        )
        ,("ver"
        ,[
             ["vejo","v\234","vemos","v\234em"]
            ,["via","via","v\237amos","viam"]
            ,["vi","viu","vimos","viram"]
            ,["vira","vira","v\237ramos","viram"]
            ,["verei","ver\225","veremos","ver\227o"]
            ,["veria","veria","ver\237amos","veriam"]
            ,["veja","veja","vejamos","vejam"]
            ,["visse","visse","v\237ssemos","vissem"]
            ,["vir","vir","virmos","virem"]
            ,["ver","ver","vermos","verem"]
            ,["--","veja","vejamos","vejam"]])
        ,("valer"
            ,[["valho","vale","valemos","valem"]
            ,["vali","valeu","valemos","valeram"]
            ,["valia","valia","val\237amos","valiam"]
            ,["valera","valera","val\234ramos","valeram"]
            ,["valerei","valer\225","valeremos","valer\227o"]
            ,["valeria","valeria","valer\237amos","valeriam"]
            ,["valha","valha","valhamos","valham"]
            ,["valesse","valesse","val\234ssemos","valessem"]
            ,["valer","valer","valermos","valerem"]
            ,["valer","valer","valermos","valerem"]
            ,["--","valha","valhamos","valham"]])
        ,("trazer"
            ,[["trago","traz","trazemos","trazem"]
            ,["trazia","trazia","traz\237amos","traziam"]
            ,["trouxe","trouxe","trouxemos","trouxeram"]
            ,["trouxera","trouxera","troux\233ramos","trouxeram"]
            ,["trarei","trar\225","traremos","trar\227o"]
            ,["traria","traria","trar\237amos","trariam"]
            ,["traga","traga","tragamos","tragam"]
            ,["trouxesse","trouxesse","troux\233ssemos","trouxessem"]
            ,["trouxer","trouxer","trouxermos","trouxerem"]
            ,["trazer","trazer","trazermos","trazerem"]
            ,["--","traga","tragamos","tragam"]])
        ,("ter"
            ,[["tenho","tem","temos","t\234m"]
            ,["tinha","tinha","t\237nhamos","tinham"]
            ,["tive","teve","tivemos","tiveram"]
            ,["tivera","tivera","tiv\233ramos","tiveram"]
            ,["terei","ter\225","teremos","ter\227o"]
            ,["teria","teria","ter\237amos","teriam"]
            ,["tenha","tenha","tenhamos","tenham"]
            ,["tivesse","tivesse","tiv\233ssemos","tivessem"]
            ,["tiver","tiver","tivermos","tiverem"]
            ,["ter","ter","termos","terem"]
            ,["--","tenha","tenhamos","tenham"]])
        ,("ser"
            ,[["sou","\233","somos","s\227o"]
            ,["era","era","\233ramos","eram"]
            ,["fui","foi","fomos","foram"]
            ,["fora","fora","f\244ramos","foram"]
            ,["serei","ser\225","seremos","ser\227o"]
            ,["seria","seria","ser\237amos","seriam"]
            ,["seja","seja","sejamos","sejam"]
            ,["fosse","fosse","f\244ssemos","fossem"]
            ,["for","for","formos","forem"]
            ,["ser","ser","sermos","serem"]
            ,["--","seja","sejamos","sejam"]])
        ,("saber"
            ,[["sei","sabe","sabemos","sabem"]
            ,["sabia","sabia","sab\237amos","sabiam"]
            ,["soube","soube","soubemos","souberam"]
            ,["soubera","soubera","soub\233ramos","souberam"]
            ,["saberei","saber\225","saberemos","saber\227o"]
            ,["saberia","saberia","saber\237amos","saberiam"]
            ,["saiba","saiba","saibamos","saibam"]
            ,["soubesse","soubesse","soub\233ssemos","soubessem"]
            ,["souber","souber","soubermos","souberem"]
            ,["saber","saber","sabermos","saberem"]
            ,["--","saiba","saibamos","saibam"]])
        ,("rir"
            ,[["rio","ri","rimos","riem"]
            ,["ria","ria","r\237amos","riam"]
            ,["ri","riu","rimos","riram"]
            ,["rira","rira","r\237ramos","riram"]
            ,["rirei","rir\225","riremos","rir\227o"]
            ,["riria","riria","rir\237amos","ririam"]
            ,["ria","ria","riamos","riam"]
            ,["risse","risse","r\237ssemos","rissem"]
            ,["rir","rir","rirmos","rirem"]
            ,["rir","rir","rirmos","rirem"]
            ,["--","ria","riamos","riam"]])
        ,("remir"
            ,[["redimo","redime","remimos","redimem"]
            ,["remi","remiu","remimos","remiram"]
            ,["remia","remia","rem\237amos","remiam"]
            ,["remira","remira","rem\237ramos","remiram"]
            ,["remirei","remir\225","remiremos","remir\227o"]
            ,["remiria","remiria","remir\237amos","remiriam"]
            ,["redima","redima","redimamos","redimam"]
            ,["remisse","remisse","rem\237ssemos","remissem"]
            ,["remir","remir","remirmos","remirem"]
            ,["remir","remir","remirmos","remirem"]
            ,["--","redima","redimamos","redimam"]])
        ,("querer"
            ,[["quero","quer","queremos","querem"]
            ,["queria","queria","quer\237amos","queriam"]
            ,["quis","quis","quisemos","quiseram"]
            ,["quisera","quisera","quis\233ramos","quiseram"]
            ,["quererei","querer\225","quereremos","querer\227o"]
            ,["quereria","quereria","querer\237amos","quereriam"]
            ,["queira","queira","queiramos","queiram"]
            ,["quisesse","quisesse","quis\233ssemos","quisessem"]
            ,["quiser","quiser","quisermos","quiserem"]
            ,["querer","querer","querermos","quererem"]
            ,["--","queira","queiramos","queiram"]])
        ,("por"
            ,[["ponho","p\245e","pomos","p\245em"]
            ,["punha","punha","p\250nhamos","punham"]
            ,["pus","p\244s","pusemos","puseram"]
            ,["pusera","pusera","pus\233ramos","puseram"]
            ,["porei","por\225","poremos","por\227o"]
            ,["poria","poria","por\237amos","poriam"]
            ,["ponha","ponha","ponhamos","ponham"]
            ,["pusesse","pusesse","pus\233ssemos","pusessem"]
            ,["puser","puser","pusermos","puserem"]
            ,["p\244r","p\244r","pormos","porem"]
            ,["--","ponha","ponhamos","ponham"]])
        ,("poder"
            ,[["posso","pode","podemos","podem"]
            ,["podia","podia","pod\237amos","podiam"]
            ,["pude","p\244de","pudemos","puderam"]
            ,["pudera","pudera","pud\233ramos","puderam"]
            ,["poderei","poder\225","poderemos","poder\227o"]
            ,["poderia","poderia","poder\237amos","poderiam"]
            ,["possa","possa","possamos","possam"]
            ,["pudesse","pudesse","pud\233ssemos","pudessem"]
            ,["puder","puder","pudermos","puderem"]
            ,["poder","poder","podermos","poderem"]
            ,["--","possa","possamos","possam"]])
        ,("perder"
            ,[["perco","perde","perdemos","perdem"]
            ,["perdia","perdia","perd\237amos","perdiam"]
            ,["perdi","perdeu","perdemos","perderam"]
            ,["perdera","perdera","perd\234ramos","perderam"]
            ,["perderei","perder\225","perderemos","perder\227o"]
            ,["perderia","perderia","perder\237amos","perderiam"]
            ,["perca","perca","percamos","percam"]
            ,["perdesse","perdesse","perd\234ssemos","perder"]
            ,["perdessem","perderes","perder","perderdes"]
            ,["perderem","perder","perdermos","perderem"]
            ,["--","perca","percamos","percam"]])
        ,("ouvir"
            ,[["ou\231o / oi\231o ","ouve","ouvimos","ouvem"]
            ,["ouvia","ouvia","ouv\237amos","ouviam"]
            ,["ouvi","ouviu","ouvimos","ouviram"]
            ,["ouvira","ouvira","ouv\237ramos","ouviram"]
            ,["ouvirei","ouvir\225","ouviremos","ouvir\227o"]
            ,["ouviria","ouviria","ouvir\237amos","ouviriam"]
            ,["ou\231a","ou\231a","ou\231amos","ou\231am"]
            ,["oi\231a","oi\231a","oi\231amos","oi\231am"]
            ,["ouvisse","ouvisse","ouv\237ssemos","ouvissem"]
            ,["ouvir","ouvir","ouvirmos","ouvirem"]
            ,["--","oi\231a","oi\231amos","oi\231am"]])
        ,("medir"
            ,[["me\231o","mede","medimos","medem"]
            ,["medi","mediu","medimos","mediram"]
            ,["media","media","med\237amos","mediam"]
            ,["medira","medira","med\237ramos","mediram"]
            ,["medirei","medir\225","mediremos","medir\227o"]
            ,["mediria","mediria","medir\237amos","mediriam"]
            ,["me\231a","me\231a","me\231amos","me\231am"]
            ,["medisse","medisse","med\237ssemos","medissem"]
            ,["medir","medir","medirmos","medirem"]
            ,["medir","medir","medirmos","medirem"]
            ,["--","me\231a","me\231amos","me\231am"]])
        ,("ler"
            ,[["leio","l\234","lemos","l\234em"]
            ,["lia","lia","l\237amos","liam"]
            ,["li","leu","lemos","leram"]
            ,["lera","lera","l\234ramos","leram"]
            ,["lerei","ler\225","leremos","ler\227o"]
            ,["leria","leria","ler\237amos","leriam"]
            ,["leia","leia","leiamos","leiam"]
            ,["lesse","lesse","l\234ssemos","lessem"]
            ,["ler","ler","lermos","lerem"]
            ,["ler","ler","lermos","lerem"]
            ,["--","leia","leiamos","leiam"]])
        ,("ir"
            ,[["vou","vai","vamos","v\227o"]
            ,["ia","ia","\237amos","iam"]
            ,["fui","foi","fomos","foram"]
            ,["fora","fora","f\244ramos","foram"]
            ,["irei","ir\225","iremos","ir\227o"]
            ,["iria","iria","ir\237amos","iriam"]
            ,["v\225","v\225","vamos","v\227o"]
            ,["fosse","fosse","f\244ssemos","fossem"]
            ,["for","for","formos","forem"]
            ,["ir","ir","irmos","irem"]
            ,["--","v\225","vamos","v\227o"]])
        ,("haver"
            ,[["hei","h\225","havemos","h\227o"]
            ,["havia","havia","hav\237amos","haviam"]
            ,["houve","houve","houvemos","houveram"]
            ,["houvera","houvera","houv\233ramos","houveram"]
            ,["haverei","haver\225","haveremos","haver\227o"]
            ,["haveria","haveria","haver\237amos","haveriam"]
            ,["haja","haja","hajamos","hajam"]
            ,["houvesse","houvesse","houv\233ssemos","houvessem"]
            ,["houver","houver","houvermos","houverem"]
            ,["haver","haver","havermos","haverem"]
            ,["--","haja","hajamos","hajam"]])
        ,("fazer"
            ,[["fa\231o","faz","fazemos","fazem"]
            ,["fazia","fazia","faz\237amos","faziam"]
            ,["fiz","fez","fizemos","fizeram"]
            ,["fizera","fizera","fiz\233ramos","fizeram"]
            ,["farei","far\225","faremos","far\227o"]
            ,["faria","faria","far\237amos","fariam"]
            ,["fa\231a","fa\231a","fa\231amos","fa\231am"]
            ,["fizesse","fizesse","fiz\233ssemos","fizessem"]
            ,["fizer","fizer","fizermos","fizerem"]
            ,["fazer","fazer","fazermos","fazerem"]
            ,["--","fa\231a","fa\231amos","fa\231am"]])
        ,("estar"
            ,[["estou","est\225","estamos","est\227o"]
            ,["estava","estava","est\225vamos","estavam"]
            ,["estive","esteve","estivemos","estiveram"]
            ,["estivera","estivera","estiv\233ramos","estiveram"]
            ,["estarei","estar\225","estaremos","estar\227o"]
            ,["estaria","estaria","estar\237amos","estariam"]
            ,["esteja","esteja","estejamos","estejam"]
            ,["estivesse","estivesse","estiv\233ssemos","estivessem"]
            ,["estiver","estiver","estivermos","estiverem"]
            ,["estar","estar","estarmos","estarem"]
            ,["--","esteja","estejamos","estejam"]])
        ,("dizer"
            ,[["digo","diz","dizemos","dizem"]
            ,["dizia","dizia","diz\237amos","diziam"]
            ,["disse","disse","dissemos","disseram"]
            ,["dissera","dissera","diss\233ramos","disseram"]
            ,["direi","dir\225","diremos","dir\227o"]
            ,["diria","diria","dir\237amos","diriam"]
            ,["diga","diga","digamos","digam"]
            ,["dissesse","dissesse","diss\233ssemos","dissessem"]
            ,["disser","disser","dissermos","disserem"]
            ,["dizer","dizer","dizermos","dizerem"]
            ,["--","diga","digamos","digam"]])
        ,("dar"
            ,[["dou","d\225","damos","d\227o"]
            ,["dava","dava","d\225vamos","davam"]
            ,["dei","deu","demos","deram"]
            ,["dera","dera","d\233ramos","deram"]
            ,["darei","dar\225","daremos","dar\227o"]
            ,["daria","daria","dar\237amos","dariam"]
            ,["d\234","d\234","d\234mos","d\234em"]
            ,["desse","desse","d\233ssemos","dessem"]
            ,["der","der","dermos","derem"]
            ,["dar","dar","darmos","darem"]
            ,["--","d\234","d\234mos","d\234em"]])
        ,("crer"
            ,[["creio","cr\234","cremos","cr\234em"]
            ,["cri","creu","cremos","creram"]
            ,["cria","cria","cr\237amos","criam"]
            ,["crera","crera","cr\234ramos","creram"]
            ,["crerei","crer\225","creremos","crer\227o"]
            ,["creria","creria","crer\237amos","creriam"]
            ,["creia","creia","creiamos","creiam"]
            ,["cresse","cresse","cr\234ssemos","cressem"]
            ,["crer","crer","crermos","crerem"]
            ,["crer","crer","crermos","crerem"]
            ,["--","creia","creiamos","creiam"]])
        ,("caber"
            ,[["caibo","cabe","cabemos","cabem"]
            ,["cabia","cabia","cab\237amos","cabiam"]
            ,["coube","coube","coubemos","couberam"]
            ,["coubera","coubera","coub\233ramos","couberam"]
            ,["caberei","caber\225","caberemos","caber\227o"]
            ,["caberia","caberia","caber\237amos","caberiam"]
            ,["caiba","caiba","caibamos","caibam"]
            ,["coubesse","coubesse","coub\233ssemos","coubessem"]
            ,["couber","couber","coubermos","couberem"]
            ,["caber","caber","cabermos","caberem"]
            ,["--","caba","cabamos","cabam"]]
        )
    ]  
