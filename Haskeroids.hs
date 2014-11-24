{-# LANGUAGE OverloadedStrings #-}
import Graphics.Blank                    
import Control.Concurrent
import Data.Char
import Data.Map as Map

main = blankCanvas 3000 { events = ["keydown","keyup"] }$ (loop (GameState (200,200,0) Map.empty [])) -- start blank canvas on port 3000

type Coords = (Double,Double,Double)
type MovementDict = Map (Maybe Int) Bool
type Size = Int
type Hits = Int
type Direction = Int
type Speed = Int
type Asteroid = (Coords,Size,Hits,Direction,Speed)
data GameState = GameState { getCoords :: Coords, getMovDict :: MovementDict, getAsteroids :: [Asteroid]}




loop :: GameState -> DeviceContext -> IO(a)
loop gState context = do 
		let preKeys = getMovDict gState
		let coords = getCoords gState
		send context $ do
			clearCanvas
			printShip coords

			--printSimpleAsteroid (100,100,0) 4 0 0 0

		threadDelay (2 * 100)		

		ch <- flush context

		let canvasHeight = height context
		let canvasWidth  = width context

		if ((Prelude.null ch) && (Map.null preKeys )) 
			then loop gState context
			else 
				do
					print ch
					let dict = Prelude.foldl reduceKeys preKeys ch
					let charList = keys dict
					let newLoc = (Prelude.foldl (movement.(fixPosition (canvasWidth, canvasHeight)))  coords charList) 
				
					loop (GameState (newLoc) dict []) context

printSimpleAsteroid :: Asteroid -> Canvas ()
printSimpleAsteroid ((x, y, _), size, hits, dir, spd)= do
	beginPath()
	arc(x, y, size * 25, 0, pi*2, False)
	fillStyle "black"
    closePath()
    fill()

printShip :: Coords -> Canvas ()
printShip (x,y,r) =  do-- send commands to this specific context
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

fixPosition ::(Double,Double) -> Coords ->  Coords
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