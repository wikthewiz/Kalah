module Main where
import Data.Sequence



main::IO()
main = do   
      _ <- print $ getStart
      return ()
-- [a | a <- xs, a <= x]


doOneAHead (xs,ys) = best [getDrawSeq (getNextDraw d) | d <- allDraws]
  where
    allDraws = doAllPossibleDraws (xs,ys)
    getNextDraw d
      | hasMoreDraws d = addAllToD d (doAllPossibleDraws (getBoard d))
      | otherwise = d


doAllAHead (xs,ys) = best [ getDrawSeq $ getAllNext d | d <- allDraws ]
  where 
    allDraws = doAllPossibleDraws (xs,ys)
    getAllNext d
      | hasMoreDraws d = addAllToD d [ getAllNext d' | d' <- (doAllPossibleDraws (getBoard d))]
      | otherwise = d


doNAHead (xs,ys) n = best [ getDrawSeq $ get_n_next d n | d <- allDraws ]
  where 
    allDraws = doAllPossibleDraws (xs,ys)
    get_n_next d n'
      | n' == 0 = d
      | hasMoreDraws d = addAllToD d [ get_n_next d' (n' - 1) | d' <- (doAllPossibleDraws (getBoard d))]
      | otherwise = d    
    
    
best :: [[(Int,Int)]] -> [(Int,Int)]
best ([]) = []
best (xs:[]) = xs
best (xs:ys:[]) = best' xs ys
best (xs:ys:xss) = best' (best' xs ys) (best xss)


best' :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
best' xs [] = xs
best' [] ys = ys
best' xs ys
  | bestX' (last xs) (last ys) = xs
  | otherwise = ys
  where
    bestX' (_, x) (_, y) = x > y


data Draws = Empty | Draw (Int, (Seq Int, Seq Int, Bool),[Draws]) deriving (Show, Eq)


getDrawSeq :: Draws -> [(Int, Int)]
getDrawSeq d =
  case d of
    Empty -> []
    Draw(i, (_, _, _), []) -> [(i, getBaseCount d)] ++ []
    Draw(i, (_, _, _), [Empty]) -> [(i, getBaseCount d)] ++ []
    Draw(i, (_, _, _), draws) -> [(i, getBaseCount d)] ++ (best [ getDrawSeq d' | d' <- draws])


doAllPossibleDraws :: (Seq Int, Seq Int) -> [Draws] 
doAllPossibleDraws (xs, ys) = [ doDraw (xs, ys) i | i <- draws]
  where
    draws = getPossibleDraws (xs, ys)


addAllToD :: Draws -> [Draws] -> Draws
addAllToD _ [] = Empty
addAllToD d (x:[]) = addD d x
addAllToD d (x:xs) = addAllToD (addD d x) xs


hasMoreDraws :: Draws -> Bool
hasMoreDraws d = 
  case d of
    Empty -> False
    Draw (_, (_, _, hasMore), _) -> hasMore 
    
    
getBoard :: Draws -> (Seq Int, Seq Int)
getBoard d =
  case d of
    Empty -> (fromList[0,0,0,0,0,0],fromList[0,0,0,0,0,0])
    Draw (_, (xs, ys, _), _) -> (xs, ys)


getI :: Draws -> Int
getI d =
  case d of
    Empty -> -1
    Draw (i, (_, _, _), _) -> i


addD::Draws -> Draws -> Draws 
addD dx dy =
  case dx of
    Empty -> dy
    Draw (i, (xs, ys, hasMore), []) -> Draw (i, (xs, ys, hasMore), [dy])
    Draw (i, (xs, ys, hasMore), ds) -> Draw (i, (xs, ys, hasMore), dy:ds)


getBaseCount :: Draws -> Int
getBaseCount d =
  case d of
    Empty -> 0
    Draw (_, (xs, _, _), _) -> index xs 6


createDraw :: (Seq Int, Seq Int, Bool) -> Int -> Draws
createDraw (xs, ys, hasMore) i   = Draw (i, (xs, ys, hasMore), [Empty])


doDraw :: (Seq Int, Seq Int) -> Int -> Draws
doDraw (xs, ys) i = createDraw (addDraw (newxs, newys) (i + 1) balls) i
  where
   (balls, (newxs, newys)) = startDraw (xs, ys) i


doMove :: (Seq Int, Seq Int) -> Int -> (Seq Int, Seq Int, Bool)
doMove (xs, ys) i = addDraw (newxs, newys) (i + 1) balls
  where
   (balls, (newxs, newys)) = startDraw (xs, ys) i


startDraw :: (Seq Int, Seq Int) -> Int -> (Int, (Seq Int, Seq Int)) 
startDraw (xs,ys) i
  | i < 6 = (getBalls xs i, (setZero xs i, ys))
  | i > 6 = (getBalls ys (getYIndex i), (xs, setZero ys (getYIndex i)))
  | otherwise = (0, (fromList([0]), fromList([0])))
  where 
    getBalls xs' i' = index xs' i'
    setZero xs' i' = update i' 0 xs'


addDraw :: (Seq Int, Seq Int) -> Int -> Int -> (Seq Int, Seq Int, Bool)
addDraw (xs,ys) i b 
  | b == 0 = (fromList [], fromList [], True)
  | isLastBall && isHomeBoard && ballCount' > 0 = doMove (makeDraw xs i, ys) i
  | isLastBall && isAdversaryBoard && ballCount' > 0 = doMove (xs, makeDraw ys (getYIndex i)) i
  | isLastBall && isHomeBoard && ballCount' == 0 = (makeDraw xs i, ys, False)
  | isLastBall && isAdversaryBoard && ballCount' == 0 = (xs, makeDraw ys (getYIndex i), False)
  | isLastBall && isHomeBase = (makeDraw xs i, ys, True)
  | not isLastBall && (isHomeBoard || isHomeBase) = addDraw (makeDraw xs i, ys ) (i + 1) (b - 1) 
  | not isLastBall && isAdversaryBoard = addDraw (xs, ( makeDraw ys (getYIndex i) )) (i + 1) (b - 1)
  | i == 13 = addDraw (xs, ys) 0 b
  | otherwise = (fromList [], fromList [], False)
  where 
    makeDraw xs' i' = update i' (( index xs' i') + 1) xs'
    isHomeBase = i == 6
    isHomeBoard = i < 6
    isAdversaryBoard = i > 6 && i < 13
    isLastBall = b == 1
    ballCount' = ballCount xs ys i


getPossibleDraws (xs, ys)
  | hasHomeDraws = getHomeBoardDraws 
  | hasAdversaryDraws = getAdversaryDraws
  where
    hasHomeDraws = Data.Sequence.length (Data.Sequence.filter (\x -> x > 0) xs) > 0
    hasAdversaryDraws = Data.Sequence.length (Data.Sequence.filter (\x -> x > 0) ys) > 0 
    getHomeBoardDraws = Prelude.filter (\a -> a /= 6) (findIndicesL (\a -> a > 0) xs)
    getAdversaryDraws = map (\a -> a + 7) (Prelude.filter (\a -> a /= 6) (findIndicesL (\a -> a > 0) ys))


getBestDraw xs = maximum xs


ballCount xs ys i 
  | i < 6 = index xs i
  | otherwise = index ys $ getYIndex i


getYIndex i = mod i 7

getStart :: Seq Int
getStart = fromList [5,5,5,5,5,5,0]

getNBoard :: Int -> (Seq Int, Seq Int)
getNBoard n = (fromList[n,n,n,n,n,n,0], fromList[n,n,n,n,n,n,0])

getStartBoard :: (Seq Int, Seq Int)
getStartBoard = (getStart, getStart)

getTestBoard = (update 5 13 getStart, getStart)

