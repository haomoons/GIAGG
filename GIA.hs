--Authors: Hao Zeng <hz110@le.ac.uk>
-- 
--

module GIA where

import Prelude hiding (succ)
import Data.Set as S
import Data.List as L
import Data.Tuple as T
--import Misc
--import Data.Foldable as F
import Data.Map.Strict as M
--import DotStuff

--
type Ptp     = String
type Message = String
--type Id      = Int
--type P       = Map Id Ptp
type State   = String
--type Cond    = String
data Dir     = Send
             | Receive
             | Synch
               deriving (Eq,Ord,Show)

type Channel = ( Ptp, Ptp )
type Interface  = ( Channel, Dir, Message )

data Event = Interf Interface 
           | Tau
             deriving (Eq,Ord,Show)  
             
type Tran  = (State, Event, State)
type ExTran = (State, [Event], State)

type GIA  = (Set State, State, Set Ptp, Set Interface, Set Tran)

isSend :: Interface -> Bool
isSend (_, d, _) = (d == Send)

isSendEvent :: Event -> Bool
isSendEvent e = case e of 
                  Interf inter -> isSend inter
                  Tau          -> False

isReceive :: Interface -> Bool
isReceive (_, d, _) = (d == Receive)

isReceiveEvent :: Event -> Bool
isReceiveEvent e = case e of 
                  Interf inter -> isReceive inter
                  Tau          -> False

isSynch :: Interface -> Bool
isSynch (_, d, _) = (d == Synch)

isNotSynch :: Interface -> Bool
isNotSynch (_, d, _) = (d /= Synch)

change2Synch :: Interface -> Interface
change2Synch (ch, d, msg) = (ch, Synch, msg)

emptyGia :: GIA
emptyGia = ( S.empty, "", S.empty, S.empty, S.empty)

stateNumber :: GIA -> Int
stateNumber ( sts, _, _, _, _ ) = S.size sts

statesOf :: GIA -> Set State
statesOf ( sts, _, _, _, _ ) = sts

initialOf :: GIA -> State
initialOf ( _, v0, _, _, _ ) = v0

groupOf :: GIA -> Set Ptp
groupOf ( _, _, group, _, _ ) = group

interfacesOf :: GIA -> Set Interface
interfacesOf ( _, _, _, interfaces, _ ) = interfaces

transitionsOf :: GIA -> Set Tran
transitionsOf ( _, _, _, _, trxs ) = trxs

dualInterface :: Interface -> Interface
dualInterface (ch, d, msg) = case d of
                              Send    -> ( ch, Receive, msg )
                              Receive -> ( ch, Send, msg )
                              _       -> ( ch, d, msg )

dualEvent :: Event -> Event
dualEvent e =case e of
              Interf inter    -> Interf (dualInterface inter)
              _               -> Tau  
              
msgOf :: Interface -> Message
msgOf ( _, _, msg) = msg

subjectOf :: Interface -> Ptp
subjectOf ( ( s, r ), d, _) = case d of
                                Send    -> s
                                Receive -> r
                                Synch   -> s

objectOf :: Interface -> Ptp
objectOf ( ( s, r ), d, _) = case d of
                                Send    -> r
                                Receive -> s
                                Synch   -> r

isSubjectP :: Ptp -> Event -> Bool
isSubjectP p e = (p==subjectOf (event2Interface e))

isObjectP :: Ptp -> Event -> Bool
isObjectP p e = (p==objectOf (event2Interface e))

isSobjectPQ :: Ptp -> Ptp -> Event -> Bool
isSobjectPQ p q e = ((p==subjectOf (event2Interface e)) && (q==objectOf (event2Interface e)))

isNotSobjectPQ :: Ptp -> Ptp -> Event -> Bool
isNotSobjectPQ p q e = ((p/=subjectOf (event2Interface e)) || (q/=objectOf (event2Interface e)))

isSameSubject :: Interface -> Interface -> Bool
isSameSubject inter inter'= (subjectOf inter == subjectOf inter')

isSameObject :: Interface -> Interface -> Bool
isSameObject inter inter'= (objectOf inter == objectOf inter')

eventOf :: Tran -> Event
eventOf ( _, e, _ ) = e

dual :: Interface -> Interface -> Bool
dual inter inter' = ( inter == (dualInterface inter') )

-- replaceState v v' m replaces v with v' in m
replaceState :: State -> State -> GIA -> GIA
replaceState v v' m@(vtxs, v0, g, acts, trxs) =
  if v == v'
  then m
  else (S.insert v' (S.delete v vtxs), (aux v0), g, acts, trxs')
    where trxs'  = S.map (\( v_, x, v_' ) -> ( aux v_, x, aux v_' )) trxs
          aux v_ = if v_ == v then v' else v_

replaceStates :: (State -> Bool) -> State -> GIA -> GIA
replaceStates cond v m@( vtxs, _, _, _, _ ) =
  aux (S.toList $ S.filter cond vtxs) m
  where aux l m' =
          case l of
           []    -> m'
           v':vs -> aux vs (replaceState v' v m')

isComposable :: GIA -> GIA -> Bool
isComposable (_, _, g', _, _) (_, _, g'', _, _) = (S.empty==S.intersection g' g'')

--sharedInput :: Set Interface -> Set Interface -> Set Interface
--sharedInput xs ys = S.filter isReceive (S.union s1 s2)
--  where s1=S.intersection xs (S.map dualInterface ys)
--        s2=S.intersection ys (S.map dualInterface xs)
        
sharedInput :: Set Interface -> Set Interface -> Set Interface
sharedInput xs ys = S.union s1 s2
  where s1=S.fromList [x| x <- (S.toList xs), isReceive x, S.member (dualInterface x) ys]
        s2=S.fromList [y| y <- (S.toList ys), isReceive y, S.member (dualInterface y) xs]
        
--sharedOutput :: Set Interface -> Set Interface -> Set Interface
--sharedOutput xs ys = S.filter isSend (S.union s1 s2)
--  where s1=S.intersection xs (S.map dualInterface ys)
--        s2=S.intersection ys (S.map dualInterface xs)
        
sharedOutput :: Set Interface -> Set Interface -> Set Interface
sharedOutput xs ys = S.union s1 s2
  where s1=S.fromList [x| x <- (S.toList xs), isSend x, S.member (dualInterface x) ys]
        s2=S.fromList [y| y <- (S.toList ys), isSend y, S.member (dualInterface y) xs]

isSharedOutput :: Interface -> Set Interface -> Set Interface -> Bool
isSharedOutput inter xs ys = S.member inter (sharedOutput xs ys)


--sharedInternal :: Set Interface -> Set Interface -> Set Interface
--sharedInternal xs ys = S.map change2Synch (S.filter isNotSynch (S.union s1 s2))
--  where s1=S.intersection xs (S.map dualInterface ys)
--        s2=S.intersection ys (S.map dualInterface xs)
        
sharedInternal :: Set Interface -> Set Interface -> Set Interface
sharedInternal xs ys = S.map change2Synch (S.union s1 s2)
  where s1=S.fromList [x| x <- (S.toList xs), isReceive x, S.member (dualInterface x) ys]
        s2=S.fromList [y| y <- (S.toList ys), isReceive y, S.member (dualInterface y) xs]
        

cartProdSet :: Set State -> Set State -> Set State 
cartProdSet xs ys = S.fromList [x++","++y | x <- (S.toList xs), y <- (S.toList ys)]

change2SynchE :: Event -> Event
change2SynchE e = case e of
                     Tau                         -> Tau
                     Interf (ch, Send, msg)      -> Interf (ch, Synch, msg)
                     Interf (ch, Receive, msg)   -> Interf (ch, Synch, msg)
                     _                           -> e

interface2Event :: Interface -> Event
interface2Event (ch, d, msg) = Interf (ch, d, msg)

isInterface :: Event -> Bool
isInterface e = case e of
                  Interf (_, _, _)    -> True
                  Tau                 -> False 

event2Interface :: Event -> Interface
event2Interface e = case e of
                      Interf (ch, d, msg)    -> (ch, d, msg)
                      Tau                    -> (("Tau", "Tau"), Synch, "Tau") 

--remove all unreachable states and transitions
remUnreach :: GIA -> GIA
remUnreach m@(vtxs, v0, g, acts, trxs) = (vtxs', v0, g, acts, trxs')
  where vtxs'= S.fromList [ vtx | vtx <- S.toList vtxs, not (L.null (langTran v0 vtx trxs)) ]
        trxs'= S.fromList [ trx | trx <- S.toList trxs, S.member (beginState trx) vtxs', S.member (targetState trx) vtxs']

otimesProd :: GIA -> GIA -> GIA
otimesProd m@(vtxsA, vA, gA, actsA, trxsA) m'@(vtxsB, vB, gB, actsB, trxsB) 
  = if (isComposable m m') /=True
    then error "They are not Composable"
    else remUnreach (vtxs, v, g, acts, trxs)
    where vtxs = cartProdSet vtxsA vtxsB
          v = vA++","++vB
          g = S.union gA gB
          acts = S.unions [actsI, actsO, actsH]
          actsI = S.difference ( S.union (S.filter isReceive actsA) (S.filter isReceive actsB) ) (sharedInput actsA actsB)
          actsO = S.difference ( S.union (S.filter isSend actsA) (S.filter isSend actsB) ) (sharedOutput actsA actsB) 
          actsH = S.unions [ (S.filter isSynch actsA), (S.filter isSynch actsB), (sharedInternal actsA actsB) ]
          trxs = S.unions [trxs1, trxs2, trxs3, trxs4]
          trxs1 = S.fromList [(v'++","++v'', a, u'++","++v'') | (v', a, u') <- (S.toList trxsA), v'' <- S.toList vtxsB, not ( S.member a ( S.map interface2Event ( S.union (sharedInput actsA actsB) (sharedOutput actsA actsB) ) ) ) ]
          trxs2 = S.fromList [(v'++","++v'', b, v'++","++u'') | (v'', b, u'') <- (S.toList trxsB), v' <- S.toList vtxsA, not ( S.member b ( S.map interface2Event ( S.union (sharedInput actsA actsB) (sharedOutput actsA actsB) ) ) ) ]
          trxs3 = S.fromList [(v'++","++v'', change2SynchE c, u'++","++u'') | (v', c, u') <- (S.toList trxsA), (v'', d, u'') <- (S.toList trxsB), isInterface c, isInterface d, event2Interface c == dualInterface ( event2Interface d ), isNotSynch ( event2Interface c ) ]
          trxs4 = S.fromList [(v'++","++v'', change2SynchE e, u'++","++u'') | (v'', e, u'') <- (S.toList trxsB), (v', f, u') <- (S.toList trxsA),  isInterface e, isInterface f, event2Interface e == dualInterface ( event2Interface f ), isNotSynch ( event2Interface e ) ]

beginState :: Tran -> State
beginState (vtx, _, _) = vtx

targetState :: Tran -> State
targetState (_, _, vtx) = vtx

finalStates :: Set Tran -> Set State 
finalStates trxs = S.fromList [ v | (u, e1 ,v) <- (S.toList trxs), not (S.member v (S.map beginState (S.difference trxs (S.fromList [(u, e1, v)])))) ]


--find the target state and event
findEnd :: State -> Set Tran -> [(Event, State)]
findEnd vtx trxs = [ (e, v') | (v, e, v') <- S.toList trxs, v==vtx]

--find the route between two states
route :: State -> State -> Set Tran -> [[Tran]]
route vtx vtx' trxs
    | vtx == vtx'    = [[]]
    | otherwise      = [(vtx, e, t):path | (e, t) <- findEnd vtx trxs, path <- route t vtx' trxs]

--find the route between two states
route' :: State -> Set State -> Set Tran -> [[[Tran]]]
route' vtx vtxs trxs = [ route vtx vtx' trxs| vtx' <- S.toList vtxs]


--find the target state and event with specific interface
findEndWithInterface :: Interface -> Set Tran -> Set State
findEndWithInterface inter trxs = S.fromList [ v' | (v, e, v') <- S.toList trxs, eventOf (v, e, v')==( interface2Event inter ) ]

isTheInterface :: Interface -> Tran -> Bool
isTheInterface inter trx = (interface2Event inter == eventOf trx)

mapIsTheInterface :: Interface -> [Tran] ->[Bool]
mapIsTheInterface inter trxs = L.map (isTheInterface inter) trxs

mapMapIsTheInterface :: Interface -> [[Tran]] -> [[Bool]]
mapMapIsTheInterface inter trxss = L.map (mapIsTheInterface inter) trxss

mapMapMapIsTheInterface :: Interface -> [[[Tran]]] -> [[[Bool]]]
mapMapMapIsTheInterface inter trxsss = L.map (mapMapIsTheInterface inter) trxsss

indexPosition :: Eq a => a -> [a] -> Int 
indexPosition x [] = -1
indexPosition x xs 
     | x== (L.head xs)    = 0
     | otherwise          = 1+indexPosition x (L.tail xs)

deleteFromN :: Eq a => Int -> [a] ->[a]
deleteFromN _ []     = []
deleteFromN i (x:xs)
   | i == 0    = []
   | otherwise = x : deleteFromN (i-1) xs

mapEvent :: [Tran] -> [Event]
mapEvent trxs = L.map eventOf trxs 

mapMapEvent :: [[Tran]] -> [[Event]]
mapMapEvent trxss = L.map mapEvent trxss

mapMapMapEvent :: [[[Tran]]] -> [[[Event]]]
mapMapMapEvent trxsss = L.map mapMapEvent trxsss

--retain the first shared input, remove the path after meeting first shared input
refineRoute :: Event -> [[[Tran]]] -> [[[Tran]]]
refineRoute e trxsss = [[ deleteFromN (indexPosition e (mapEvent trxs)) trxs]  |trxss <- trxsss, trxs<- trxss]


string2List :: (Char -> Bool) -> State -> [State]
string2List p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : string2List p s''
                            where (w, s'') = break p s'
--string2List (==',') "break,this,string,at,commas"


isErrState1 :: State -> State -> GIA -> GIA -> Bool
isErrState1 v v' (vtxs, v0, g, acts, trxs) (vtxs', v0', g', acts', trxs') = 
    let sharedoutp = [ (e, vi) |  (e, vi) <- (findEnd v trxs), isSharedOutput (event2Interface e) acts acts']
        xsss = mapMapMapEvent (route' v' (finalStates trxs') trxs')
    in if L.null sharedoutp==True
       then False
       else L.foldl' (&&) True [if a == True then False else True | xss<-xsss, xs<-xss, (e, vi)<-sharedoutp, let a= (S.member (dualEvent e) (S.fromList xs) )] 


isErrState2 :: State -> State -> GIA -> GIA -> Bool
isErrState2 v v' (vtxs, v0, g, acts, trxs) (vtxs', v0', g', acts', trxs') = 
    let sharedoutp = [ (e, vi) |  (e, vi) <- (findEnd v trxs), isSharedOutput (event2Interface e) acts acts']
    in if L.null sharedoutp==True
      then False
      else L.foldl' (&&) True [if  S.null s == True then False else True | (e, vi)<-sharedoutp,  let tsss= refineRoute (dualEvent e) (route' v' (findEndWithInterface (dualInterface (event2Interface e)) trxs') trxs'), let esss= mapMapMapEvent tsss, ess<-esss, es<-ess, let s= S.intersection (S.fromList es) (S.filter (isSobjectPQ (objectOf (event2Interface e)) (subjectOf (event2Interface e))) (S.map interface2Event (S.union (sharedOutput acts acts') (sharedInput acts acts') ) ) ) ]

isErrState3 :: State -> State -> GIA -> GIA -> Bool
isErrState3 v v' (vtxs, v0, g, acts, trxs) (vtxs', v0', g', acts', trxs') = 
    let sharedoutp = [ (e, vi) |  (e, vi) <- (findEnd v trxs), isSharedOutput (event2Interface e) acts acts']
        con = L.foldl' (&&) True [ b |(e, vi)<-sharedoutp, let rsss= mapMapMapEvent (refineRoute (dualEvent e) (route' v' (findEndWithInterface (dualInterface (event2Interface e)) trxs') trxs') ), rss<-rsss, rs<-rss, let b=L.null rs]
    in if L.null sharedoutp==True
       then False
       else if con ==True
       then False
       else L.foldl' (||) False [if S.null sa == True then False else if S.null sb == True then False else True | (e, vi)<-sharedoutp,  let lsss= mapMapMapEvent (route' vi (finalStates trxs) trxs), lss<-lsss, ls<-lss, let rsss= mapMapMapEvent (refineRoute (dualEvent e) (route' v' (findEndWithInterface (dualInterface (event2Interface e)) trxs') trxs') ), rss<-rsss, rs<-rss, let sa= S.intersection (S.fromList rs) (S.filter (isNotSobjectPQ (objectOf (event2Interface e)) (subjectOf (event2Interface e))) (S.map interface2Event (S.union (sharedOutput acts acts') (sharedInput acts acts') ) ) ), let sb = S.intersection (S.delete Tau (S.fromList ls)) (S.map (dualEvent) sa) ]

isErrStateCon1 :: State -> State -> GIA -> GIA -> Bool
isErrStateCon1 v v' m m' = ((isErrState1 v v' m m') || (isErrState1 v' v m' m))

isErrStateCon2 :: State -> State -> GIA -> GIA -> Bool
isErrStateCon2 v v' m m' = ((isErrState2 v v' m m') || (isErrState2 v' v m' m))

isErrStateCon3 :: State -> State -> GIA -> GIA -> Bool
isErrStateCon3 v v' m m' = ((isErrState3 v v' m m') || (isErrState3 v' v m' m))

isErrState :: State -> State -> GIA -> GIA -> Bool
isErrState v v' m m' = (isErrStateCon1 v v' m m') || (isErrStateCon2 v v' m m') || (isErrStateCon3 v v' m m')

findErrState :: GIA -> GIA -> [(State, Bool)]
findErrState m m' = [(q, b)|v<-S.toList (statesOf m), v'<-S.toList (statesOf m'), let q=v++","++v', S.member q (statesOf (otimesProd m m')), let b=isErrState v v' m m' ] 

--find the route between two states by add Tau
langTran :: State -> State -> Set Tran -> [[Tran]]
langTran vtx vtx' trxs
    | vtx == vtx'    = [[(vtx, Tau, vtx)]]
    | otherwise      = [(vtx, e, t):path | (e, t) <- findEnd vtx trxs, path <- route t vtx' trxs]

--find the route between two states by add Tau
langTran' :: State -> Set State -> Set Tran -> [[[Tran]]]
langTran' vtx vtxs trxs = [ langTran vtx vtx' trxs| vtx' <- S.toList vtxs]

isNotTauTran :: Tran -> Bool
isNotTauTran (_, e, _) = (e/=Tau)

isNotTauEvent :: Event -> Bool
isNotTauEvent e = (e/=Tau)

filterTau :: [Event] -> [Event]
filterTau es = L.filter isNotTauEvent es

mapFilterTau :: [[Event]] -> [[Event]]
mapFilterTau ess = L.map filterTau ess

mapMapFilterTau :: [[[Event]]] -> [[[Event]]]
mapMapFilterTau esss = L.map mapFilterTau esss


pickFstEvent :: [[[Event]]] -> [Event]
pickFstEvent trxsss =  [if L.null trxs==True then Tau else L.head trxs |trxss<-trxsss, trxs<-trxss ]

isRemTau :: Tran -> Set Tran ->  Bool
isRemTau (v, e, vtau) trxs = 
    let vtsss =langTran' vtau (finalStates trxs) trxs 
        vsss =langTran' v (finalStates trxs) trxs  
        etsss= mapMapFilterTau (mapMapMapEvent vtsss) 
        esss= mapMapFilterTau (mapMapMapEvent vsss)
        etsss'= S.fromList [ets | etss<-etsss, ets<-etss]
        esss' = S.fromList [es | ess<-esss, es<-ess]
        picvt = S.delete Tau (S.fromList (pickFstEvent etsss))
        picv  = S.delete Tau (S.fromList (pickFstEvent esss))
        sdcon = L.foldl' (&&) True (L.map isSendEvent (S.toList (S.union picvt picv)))
        recon = L.foldl' (&&) True (L.map isReceiveEvent (S.toList (S.union picvt picv)))
    in if e /= Tau 
       then False
       else if etsss'==esss'
       then True
       else if S.null (S.union picvt picv)==True
       then True
       else if S.null picvt == True
       then False
       else if (sdcon || recon)==True
       then True
       else False

--obtain all removeable tau transitions
findRemTau :: GIA -> [(Tran, Bool)]
findRemTau m@(vtxs, v0, g, acts, trxs) = [(trx, b) | trx<-S.toList trxs, Tau==eventOf trx, let b= isRemTau trx trxs]

--
--deine some function for project from gg to GIA
--
xtimesProd :: GIA -> GIA -> GIA
xtimesProd (vtxsA, vA, gA, actsA, trxsA) (vtxsB, vB, gB, actsB, trxsB)=(vtxs, v0, g, acts, trxs) 
  where vtxs = cartProdSet vtxsA vtxsB
        v0 = vA++","++vB
        g = S.union gA gB
        acts = S.union actsA actsB
        trxs = S.union (S.fromList [(v++","++v', e, u++","++u') | (v, e, u) <- S.toList trxsA, v'<-S.toList vtxsB, u'<-S.toList vtxsB, v'==u']) (S.fromList [(v++","++v', e, u++","++u') | (v', e, u') <- S.toList trxsB, v<-S.toList vtxsA, u<-S.toList vtxsA, v==u])

giaProd :: [GIA] -> GIA
giaProd [] = emptyGia
giaProd (m:ms) = if ms==[]
                  then m
                  else xtimesProd m (giaProd ms)    

stateProd :: State -> State -> State
stateProd q q' = if q' == "" then q else q ++ "," ++ q'

--
--define removable tau closure
--
tauEq :: State -> Set Tran -> [[Tran]] -> Set State
tauEq vtx strxs ltrxss= S.union (S.fromList [vtx]) (S.fromList [ if bs==vtx then ts else if ts==vtx then bs else vtx | ltrxs <- ltrxss, trx<-ltrxs, eventOf trx==Tau, isRemTau trx strxs, let bs=beginState trx,  let ts=targetState trx])

tauEqs :: GIA -> [(State, Set State)]
tauEqs m@(vtxs, v0, g, acts, trxs) = [(vtx, s) | vtx<-S.toList vtxs, let rsss = route' v0 (finalStates trxs) trxs, let tss = [ rs | rss<-rsss, rs<-rss, not (L.null rs)], let s=tauEq vtx trxs tss]

tauEqState :: [(State, Set State)] -> Set (Set State)
tauEqState ts = S.delete (S.empty) (S.fromList [ if S.size (snd t)==1 then (snd t) else if isSubsetOf (snd t) (snd t') then (snd t') else S.empty | t<-ts, t'<-ts, t/=t'])

--creat dictionary of equ class of set and new state
--dicOfState :: Set (Set State) -> Set ( [State], Set State)
--dicOfState ss = S.fromList [(S.toList s, s)|s<-S.toList ss]

--lookUp :: [State] -> Set ( [State], Set State) -> Set State
--lookUp k mp = L.filter (\e -> e/="") [if l==(fst m) then (snd m) else ""|m<-mp]

list2String :: [State] -> State
list2String ls = L.foldl' (++) "" ls

--check s is a substring of l 
check :: Eq a => [a]->[a]->Bool
check l s = check' l s True where
    check' _ [] h          = True
    check' [] _ h          = False
    check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)

--remove all removable tau by using substring that mean have some bug
refineGia' :: GIA -> GIA
refineGia' m@(vtxs, v0, g, acts, trxs) = (vtxs', v0', g', acts', trxs')
  where vtxs'= S.fromList [ list2String l | let ls=[ S.toList e | e <- S.toList (tauEqState (tauEqs m))], l<-ls ]
        v0'= L.head (L.filter (\e -> e/="") [if (fst ts)==v0 then list2String (S.toList (snd ts)) else "" |ts <- tauEqs m])
        g'=g
        acts'=acts
        trxs'= S.filter (\(s, e, t) -> (s/=t) )  ( S.delete ("",Tau,"") (S.fromList [ if (check v' v) && (check u' u) then (v', e, u') else ("", Tau, "")  | v'<-S.toList vtxs', u'<- S.toList vtxs', (v, e, u) <- S.toList trxs]) )

--remove all removable tau by using element
refineGia :: GIA -> GIA
refineGia m@(vtxs, v0, g, acts, trxs) = (vtxs', v0', g', acts', trxs')
  where vtxs'= S.fromList [ list2String l | let ls=[S.toList e | e <- S.toList (tauEqState (tauEqs m))], l<-ls ]
        v0'= L.head (L.filter (\e -> e/="") [if S.member v0 ts then list2String (S.toList ts) else "" |ts <- S.toList ( tauEqState (tauEqs m) )])
        g'=g
        acts'=acts
        trxs'= S.filter (\(s, e, t) -> (s/=t) )  ( S.delete ("",Tau,"") (S.fromList [ if (S.member v x') && (S.member u y') then (list2String (S.toList x'), e, list2String (S.toList y')) else ("", Tau, "")  | let ds=tauEqState (tauEqs m), x'<-S.toList ds, y'<-S.toList ds, (v, e, u) <- S.toList trxs]) ) 
    
