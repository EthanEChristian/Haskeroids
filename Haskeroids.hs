{-# LANGUAGE OverloadedStrings #-}
import Graphics.Blank                    
import Control.Concurrent
import Data.Char
import Data.Text
import System.Random
import Data.Map as Map

main = blankCanvas 3000 { events = ["keydown","keyup"] }$ run

type Coords = (Double,Double,Double)    -- x and y , r to represent amount of rotation
type MovementDict = Map (Maybe Int) Bool
type ScreenDemension = (Double,Double)
type Size = Int
type Hits = Int
type Direction = Int -- 0 - 360 Degrees, 0 being straight up
type Speed = Double
type Asteroid = (Coords,Size,Hits,Direction,Speed)
data GameState = GameState { getCoords :: Coords, getMovDict :: MovementDict, getAsteroids :: [Asteroid], getScore :: Int}


run :: DeviceContext -> IO ()
run ctx =do
	let canvasHeight = height ctx
	let canvasWidth  = width ctx
	astroids <- createXLargeAsteroid (canvasWidth,canvasHeight)
	loop (GameState (200,200,0) Map.empty [astroids] 0)ctx
	


loop :: GameState -> DeviceContext -> IO()
loop gState context = do 
		let preKeys = getMovDict gState

		let coords = getCoords gState

		let astroids = getAsteroids gState

		let score = getScore gState

		let screenDim = (width context,height context)
		

		
		
		let collisions = Prelude.map (detectAsteroidCollision coords) astroids  
		if (or collisions)
			then
				do
					print "Collision"
					send context $ do
						printDeath screenDim
		else
			do
				send context $ do
					clearCanvas
					printScore score screenDim
					sequence_ $ Prelude.map printSimpleAsteroid astroids
					printShip coords

				

				threadDelay (2 * 1000)		

				newAsts <- repopulateAsteroids astroids screenDim
				
				let newAsteroids = moveAstroids newAsts screenDim

				ch <- flush context

				

				if ((Prelude.null ch) && (Map.null preKeys )) 
					then loop (GameState coords preKeys newAsteroids score) context
					else 
						do
							print ch
							let dict = Prelude.foldl reduceKeys preKeys ch
							let charList = keys dict
							let newLoc = (Prelude.foldl (movement.(fixPosition screenDim))  coords charList) 
						
							loop (GameState newLoc dict newAsteroids score) context

printDeath :: ScreenDemension -> Canvas ()
printDeath (maxX,maxY) = do
	fillStyle "black"
	font "bold 60px Arial"
	fillText( "You Died",(maxX /2 ), (maxY /2))

repopulateAsteroids :: [Asteroid] -> ScreenDemension -> IO([Asteroid])
repopulateAsteroids as dim= do
	if (Prelude.length as) < 1
		then
			do 
				newAst <- createXLargeAsteroid dim
				repopulateAsteroids (as ++ [newAst]) dim
	else
		return as

detectAsteroidCollision :: Coords -> Asteroid -> Bool
detectAsteroidCollision (x,y, r) (crds, sz, _, _, _)  = (fromIntegral radAst) > distToShip where
	radAst = 10 * sz
	deltaX = abs ((fst3 crds) - x)
	deltaY = abs ((snd3 crds) - (y+10))
	distToShip = sqrt((deltaX ** 2) + (deltaY ** 2))


createXLargeAsteroid :: ScreenDemension -> IO(Asteroid)
createXLargeAsteroid (maxX,maxY) = do
	x <- randomRIO(0,maxX)
	y <- randomRIO(0,maxY)
	r <- randomRIO(0 , 360)
	dir <- randomRIO(0, 360)
	return ((x,y,r),4,0,dir,0.25)

moveAstroids :: [Asteroid] -> ScreenDemension -> [Asteroid]
moveAstroids as maxWindow = Prelude.map (moveAstroid maxWindow) as

moveAstroid :: ScreenDemension -> Asteroid -> Asteroid
moveAstroid maxWindow ((x,y,r), sz, hts, dir, spd) = (crds ,sz,hts,dir,spd) where
	x' = (x - (spd * sin(degreeToRad $ fromIntegral(dir))))
	y' = (y - (spd * cos(degreeToRad $ fromIntegral(dir))))
	crds = fixPosition maxWindow (x',y',r)

printSimpleAsteroid :: Asteroid -> Canvas ()
printSimpleAsteroid ((x, y, _), size, hits, dir, spd)= do
	beginPath()
	arc(x, y, (fromIntegral(size) * 10), 0, pi*2, False)
	fillStyle "gray"
	closePath()
	fill()

printScore :: Int -> ScreenDemension -> Canvas ()
printScore x (maxX, maxY) = do 
	fillStyle "black"
	font "bold 36px Arial"
	fillText((pack(show x)),(maxX -150), 40)

printShip :: Coords -> Canvas ()
printShip (x,y,r) =  do
	save()
	let rot = degreeToRad (360 - r)

	translate(x,(y + 10))
	rotate(rot)
	translate(-x,-(y + 10))


	beginPath()
	moveTo(x,y)
	lineTo( x +5, y +20)
	lineTo(x -5,y +20)
	closePath()

	fillStyle "black"
	fill()
	restore()

reduceKeys :: MovementDict -> Event -> MovementDict
reduceKeys m ev = do
	if (eType ev) == "keydown"
		then (insert (eWhich ev) True m)
	else (delete (eWhich ev) m)

movement :: Coords -> Maybe Int ->  Coords
movement (x, y, r) (Just w) | 87  == w  = ((x - (0.75 * sin(degreeToRad r))) ,(y - (0.75 * cos (degreeToRad  r))) ,r)  -- Foward 'w'
movement (x, y, r) (Just d) | 65  == d  = (x , y, (fromIntegral(((ceiling r) + 1) `mod` 360::Int))) 				 -- Right  'd'
movement (x, y, r) (Just s) | 83  == s  = ((x + (0.75 * sin(degreeToRad r))) ,(y + (0.75 * cos (degreeToRad r))) ,r)	 -- Back   's'
movement (x, y, r) (Just a) | 68  == a  = (x, y, (fixDegrees(r-1)) ) 											 -- Left   'a'
movement (x, y, r)  _  = (x,y,r)

fixDegrees :: Double -> Double
fixDegrees 360 = 0
fixDegrees (-1) = 359
fixDegrees x = x

fixPosition ::ScreenDemension -> Coords ->  Coords
fixPosition (maxX,maxY) (x,y,r)  = (maxValFix x maxX, maxValFix y maxY,r)


degreeToRad :: Double -> Double
degreeToRad x = (2 * pi / 360) * x

maxValFix :: Double -> Double -> Double
maxValFix val maxVal | val > (maxVal+20)= -20
maxValFix val maxVal | val < -20 = maxVal +20 
maxValFix val maxVal = val
		
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, y, _) = y

thr3 :: (a,b,c) -> c
thr3 (_, _, z) = z