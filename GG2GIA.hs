--
-- Authors: Hao Zeng <hz110@le.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--

--
-- This module contains function to project a syntactic choreography,
-- to calculate its semantics, and to output it in dot format.
--

module GG2GIA where

import Data.Set as S
import Data.List as L
--import Data.Tuple as T
import Data.Map.Strict as M
--import Data.Char
import Data.Ord as O
--import Misc
--import CFSM
--import DotStuff
import GIA

-- A syntactic global graph is a set of nodes, a source, a sink, and a
-- set of edges We assume that cp's will be automatically generated
-- (uniquely) during parsing
data GG = Emp
        | Act Channel Message
        | Par [GG]
        | Bra (Set GG)
        | Seq [GG]
        | Rep GG Ptp
        deriving (Eq, Ord, Show)

-- type Endpoint = String

normGG :: GG -> GG
--
-- Syntactic global graphs can be normalised by flattening nested |
-- and + the name normGG is misleading.
--
-- The function is based on the following structural rules:
-- (o) + (o) = (o)
-- ( GG, |, (o) ) abelian monoid
-- ( GG, ;, (o) ) monoid
-- G + G' = G' + G
-- G;(G1 + G2) = G;G1 + G;G2
-- (G1 | G2) ; (G1' | G2') = G1;G1' | G2;G2'   if ptps(G2) \cap ptps(G1') = ptps(G1) \cap ptps(G2') = {}
--
-- the last equation is not applied yet
--
normGG gg =
  case gg of
    Seq ggs   -> let ngs = [g | g <- (L.map normGG ggs), g /= Emp]
                 in (if ngs ==[] then Emp else Seq ngs)
    Rep gg' p -> Rep (normGG gg') p
    Par ggs   -> let ngs = [g | g <- (normPar ggs), g /= Emp]
                 in (case ngs of
                        [] -> Emp
                        [g] -> g
                        _   -> Par $ L.sort ngs
                    )
    Bra ggs   -> let nb = S.filter (\g -> g /= Emp) (normBra $ S.toList ggs)
                 in (case S.size nb of
                        0 -> Emp
                        1 -> head $ S.toList nb
                        _ -> Bra nb
                    )
    _         -> gg
  where normPar gs = case gs of
                       []   -> []
                       [_]  -> gs
                       g:l' -> let ng = normGG g
                               in (case ng of
                                      Par ggs' -> normPar (ggs' ++ l')
                                      _        -> [ng] ++ (normPar l')
                                  )
        normBra gs = case gs of
                       []   -> S.empty
                       [g]  -> let ng = normGG g
                               in (if ng == Emp then S.empty else S.singleton ng)
                       g:l' -> let ng = normGG g
                               in (case ng of
                                      Bra ggs' -> normBra ((S.toList ggs') ++ l')
                                      _        -> S.union (S.singleton ng) (normBra l')
                                  )

startGG :: GG -> GG -> Bool
--
-- start g g' checks if g is a prefix of g'
--
startGG g g' = let ng = normGG g
                   ng' = normGG g'
               in (case ng' of
                      Seq ggs -> startGG ng (head ggs)
                      Bra ggs -> L.all (startGG ng) l
                        where l = S.toList ggs
                      _       -> False
                  )


factorise :: GG -> GG
--
-- factorise gg rewrites a GG in normal form by factorising the common
-- parts of branches
-- PRE: gg in normal form
-- POST: application of the congruenze law g;g1 + g;g2 = g;(g1+g2) from left to right
--
factorise gg = case gg of
                Emp       -> Emp
                Act _ _   -> gg
                Par ggs   -> Par (L.map factorise ggs)
                Bra ggs   -> if S.null ggs
                             then Emp
                             else let prefix     = prefOf (S.elemAt 0 ggs)
                                      part       = S.partition (startGG prefix) ggs
                                      prefOf gg' = case gg' of
                                                      Seq ggs' -> if L.null ggs' then error $ show (S.elemAt 0 ggs) else head ggs'
                                                      _        -> gg'
                                      ggSet      = fst part
                                  in normGG (Bra (S.union (fact prefix ggSet) (rest $ snd part)))
                                       where fact prefix ggSet = if S.size ggSet == 1
                                                                 then ggSet
                                                                 else S.singleton (Seq [prefix, factorise (Bra (S.map suffOf ggSet))])
                                             suffOf gg'        = case gg' of
                                                                  Seq ggs' -> if L.length ggs' == 1 then Emp else Seq (tail ggs')
                                                                  _        -> Emp
                                             rest ggSet'       = if S.size ggSet' == 1
                                                                 then ggSet'
                                                                 else S.singleton $ factorise (Bra ggSet')
                Seq ggs   -> Seq (L.map factorise ggs)
                Rep gg' p -> Rep (factorise gg') p


wb :: Set GG -> Bool
--
-- PRE: the input ggs are factorised
-- POST: returns True iff th list is well-branched
--        
wb ggs =
  let ps             = S.toList $ ggptp S.empty (Bra ggs)
      disjoint (x, y)= S.null $ S.intersection x y
--      same (x, y)    = x == y
      firstActs p gg = case sevAt p gg of
                        Emp               -> (S.empty, S.empty)
                        act@(Act (s,_) _) -> if s == p
                                             then (S.singleton act, S.empty)
                                             else (S.empty, S.singleton act)
                        Par ggs''         -> L.foldr aux (S.empty, S.empty) ggs''
                            where aux gg_ (fA,fP) = let (fA',fP') = firstActs p gg_ in
                                                    (S.union fA fA', S.union fP fP')
                        Bra ggs''         -> S.foldr aux (S.empty, S.empty) ggs''
                            where aux = \gg_ (fA,fP) -> let (fA',fP') = firstActs p gg_ in
                                                        ((S.union fA fA'), (S.union fP fP'))
                        Seq ggs''         -> case ggs'' of
                                              []    -> (S.empty, S.empty)
                                              gg':_ -> if res == (S.empty, S.empty)
                                                       then firstActs p (Seq (tail ggs''))
                                                       else res
                                                           where res = firstActs p gg'
                        Rep gg' _        -> firstActs p gg'
      matrix         = M.fromList [(p, L.map (firstActs p) (S.toList ggs)) | p <- ps ]
      check prop f p = L.all (\pairs -> prop (f pairs)) (matrix!p)
      active         = S.fromList ([ p | p <- ps, check (\x -> not (S.null x)) fst p ])
      passive        = S.fromList ([ p | p <- ps, check (\x -> not (S.null x)) snd p ])
      getpairs l res = case l of
                        []   -> res
                        e:l' -> getpairs l' (res ++ [(e,e') | e' <- l'])
  in S.null ggs || (
                    L.all (\p -> check (\x -> S.null x) fst p || check (\x -> not (S.null x)) fst p) ps &&
                    L.all (\p -> check (\x -> S.null x) snd p || check (\x -> not (S.null x)) snd p) ps &&
                    (S.size active == 1) && (disjoint (active, passive)) &&
                    L.all disjoint (getpairs (L.map fst (matrix!(S.elemAt 0 active))) []) &&
                    L.all (\p -> L.all disjoint (getpairs (L.map snd (matrix!p)) [])) (S.toList passive)
                   )

sevAt :: Ptp -> GG -> GG
sevAt p gg = case gg of
               Emp               -> Emp
               act@(Act (s,r) _) -> if (s==p || r==p) then act else Emp
               Par ggs           -> Par (L.map (sevAt p) ggs)
               Bra ggs           -> Bra (S.map (sevAt p) ggs)
               Seq ggs           -> Seq (L.map (sevAt p) ggs)
               Rep gg' p'        -> Rep (sevAt p gg') p'
                      
ggptp :: Set Ptp -> GG -> Set Ptp
--
-- ggptp computes the set of participants of a global graph
--
ggptp ptps g = case g of
                Emp         -> ptps
                Act (s,r) _ -> S.union ptps (S.fromList [s,r])
                Par gs      -> S.union ptps (S.unions $ L.map (ggptp S.empty) gs)
                Bra gs      -> S.union ptps (S.unions $ S.toList (S.map (ggptp S.empty) gs))
                Seq gs      -> S.union ptps (S.unions (L.map (ggptp S.empty) gs))
                Rep g' p    -> S.union ptps (ggptp (S.singleton p) g')

(€) :: (Eq a) => a -> [a] -> Bool
x € y = L.elem x y


proj2Gia' :: GG -> Ptp -> State -> State -> Int -> (GIA, State)
--
-- PRE:  actions are well formed (wffActions) ^ q0 /= qe ^ p is a participant of gg
-- POST: the non-minimised projection of GG wrt p and a unique exiting state (it must always exist!)
-- n is a counter for fresh state generation
-- q0 and qe correspond to the entry and exit state, respectively
--
proj2Gia' gg p q0 qe n =
  let suf  = show n
      --taul = ((p,p), Tau, "")
      dm   = ( (S.fromList [q0], q0, S.fromList [p], S.empty, S.empty), q0 )
      taudm   = ( (S.fromList [q0, qe], q0, S.fromList [p], S.empty, S.fromList [(q0, Tau, qe)]), qe )
      --tautrx q1 q2 = if q1==q2 then S.empty else S.singleton (q1, taul, q2)
  in case gg of
      Emp         -> dm
      Act (s,r) m -> if (p/=s && p/=r)
                     then taudm
                     else ( ((S.fromList [q0, qe]), q0, S.fromList [p], S.singleton inter, (S.singleton (q0, Interf inter, qe))) , qe )
        where inter = if (p == s) then ((p,r),Send,m) else ((s,p),Receive,m)
      Par ggs     -> ( replaceState (initialOf m) q0 ( S.union (S.singleton qe) (statesOf m ) , initialOf m , S.fromList [p], (interfacesOf m), (transitionsOf m)), qe )
        where m   = replaceState qe' qe (giaProd $ L.map fst mps)
              qe' = L.foldr stateProd "" (L.map snd mps)
              mps = L.map (\g -> proj2Gia' g p q0 qe n) ggs
      Bra ggs     -> ( replaceStates (\q -> q € [q0 ++ (show i) | i <- [1 .. (length mps)]]) q0 (states, q0, S.fromList [p], acts, trxs), qe )
        where (states, acts, trxs) = L.foldl
                (\(x,y,z) m -> ( S.union x (statesOf m) ,
                                 S.union y (interfacesOf m) ,
                                 S.union z (transitionsOf m) )
                )
                (S.singleton qe, S.fromList [], S.empty)
                ms
              ggs'    = L.zip (S.toList ggs) [1..S.size ggs]
              mps     = L.map (\(g,i) -> proj2Gia' g p (q0 ++ (show i)) qe n) ggs'
              (ms, _) = (L.map fst mps, L.map snd mps)
      Seq ggs     -> ( replaceState qe' qe (states, q0, S.fromList [p], acts, trxs) , qe )
        where (_, qe', states, acts, trxs) =
                L.foldl
                  (\( i , qi , x , y , z ) g ->
                    let ( m , qf' ) = proj2Gia' g p qi (qe ++ (show i)) n in
                     (i + 1 ,
                      qf' ,
                      S.union x (statesOf m) ,
                      S.union y (interfacesOf m) ,
                      S.union z (transitionsOf m)
                     )
                  )
                  (0, q0, S.empty, S.empty, S.empty)
                  ggs


proj2Gia :: GG -> Ptp -> State -> State -> Int -> (GIA, State)
--
-- PRE:  actions are well formed (wffActions) ^ q0 /= qe ^ p is a participant of gg
-- POST: the non-minimised projection of GG wrt p and a unique exiting state (it must always exist!)
-- n is a counter for fresh state generation
-- q0 and qe correspond to the entry and exit state, respectively
--
proj2Gia gg p q0 qe n =
  let suf  = show n
      --taul = ((p,p), Tau, "")
      dm   = ( (S.fromList [q0], q0, S.fromList [p], S.empty, S.empty), q0 )
      taudm   = ( (S.fromList [q0, qe], q0, S.fromList [p], S.empty, S.fromList [(q0, Tau, qe)]), qe )
      --tautrx q1 q2 = if q1==q2 then S.empty else S.singleton (q1, taul, q2)
  in case gg of
      Emp         -> dm
      Act (s,r) m -> if (p/=s && p/=r)
                     then taudm
                     else ( ((S.fromList [q0, qe]), q0, S.fromList [p], S.singleton inter, (S.singleton (q0, Interf inter, qe))) , qe )
        where inter = if (p == s) then ((p,r),Send,m) else ((s,p),Receive,m)
      Par ggs     -> ( replaceState (initialOf m) q0 ( S.union (S.singleton qe) (statesOf m ) , initialOf m , S.fromList [p], (interfacesOf m), (transitionsOf m)), qe )
        where m   = replaceState qe' qe (giaProd $ L.map fst mps)
              qe' = L.foldr stateProd "" (L.map snd mps)
              mps = L.map (\g -> proj2Gia g p q0 qe n) ggs
      Bra ggs     -> (replaceStates (\q -> q € [qe ++ (show i) | i <- [1 .. (length mps)]]) qe (replaceStates (\q -> q € [q0 ++ (show i) | i <- [1 .. (length mps)]]) q0 (states, q0, S.fromList [p], acts, trxs)), qe)
        where (states, acts, trxs) = L.foldl
                (\(x,y,z) m -> (S.union x (statesOf m),
                                S.union y (interfacesOf m),
                                S.union z (transitionsOf m) )
                )
                (S.singleton qe, S.fromList [], S.empty)
                (L.map fst mps)
              ggs' = L.zip (S.toList ggs) [1 .. S.size ggs]
              mps  = L.map (\(g,i) -> proj2Gia g p (q0 ++ (show i)) (qe ++ (show i)) n) ggs'
      Seq ggs     -> ( replaceState qe' qe (states, q0, S.fromList [p], acts, trxs) , qe )
        where (_, qe', states, acts, trxs) =
                L.foldl
                  (\( i , qi , x , y , z ) g ->
                    let ( m , qf' ) = proj2Gia g p qi (qe ++ (show i)) n in
                     (i + 1 ,
                      qf' ,
                      S.union x (statesOf m) ,
                      S.union y (interfacesOf m) ,
                      S.union z (transitionsOf m)
                     )
                  )
                  (0, q0, S.empty, S.empty, S.empty)
                  ggs


