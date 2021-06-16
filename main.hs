--T1 Haskell
--Nome:Robson Daniel Marchesan

import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)
type Triangle  = (Point, Point, Point)


paletaCor :: String -> [(Int,Int,Int)]
paletaCor choice 
      |choice == "verde" = greenPalette 20
      |choice == "vermelho" = redPalette 20
      |choice == "azul" = bluePalette 20
      |otherwise = bluePalette 20


redPalette :: Int -> [(Int,Int,Int)]
redPalette n = [(80+i*10, 0, 0) | i <- [0..n] ]

greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

bluePalette :: Int -> [(Int,Int,Int)]
bluePalette n = [(0, 0, 80+i*10) | i <- [0..n] ]

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

svgCircle :: Circle -> String -> String 
svgCircle ((x,y),r) style = 
  printf "<circle cx='%.3f' cy='%.3f' r='%.2f' style='%s' />\n" x y r style

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

svgTriangle :: Triangle -> String -> String
svgTriangle ((x1,x2), (y1,y2), (z1,z2)) style = 
  printf "<polygon points='%.3f,%.3f %.3f,%.3f %.3f,%.3f' style='%s' />\n" x1 x2 y1 y2 z1 z2 style


--------------------------------------------------------------------------------------
linhaCircle :: [Circle]
linhaCircle = [((x*(gap+2*r) + 2*r, 100), r) | x <- [0..fromIntegral(4)]]
  where r = 100
        gap = 30

linhaRect :: [Rect]
linhaRect = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (4)]]
  where (w,h) = (400,400)
        gap = 30

linhaTriangle :: [Triangle]
linhaTriangle = [((m*gap+200,100),(m*gap+250,200),(m*gap+150,200)) | m <- [0..fromIntegral (4)]]
  where 
        gap = 100


--------------------------------------------------------------------------------------
rectCircle :: [Circle]
rectCircle = a ++ b ++ c ++ d
    where a = [((x*(gap+2*r) + 2*r, 100), r)| x <- [0..fromIntegral(4)]]
          b = [((x*(gap+2*r) + 2*r, 1200), r)| x <- [0..fromIntegral(4)]]
          c = [((100, x*(gap+2*r) + 2*r), r)| x <- [0..fromIntegral(4)]]
          d = [((1200, x*(gap+2*r) + 2*r), r)| x <- [0..fromIntegral(4)]]
          r = 100
          gap = 30

rectRect :: [Rect]
rectRect = a ++ b ++ c ++ d
    where a = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (4)]]
          b = [((m*(w+gap), 1100), w, h) | m <- [0..fromIntegral (4)]]
          c = [((0.0, m*(w+gap)), w, h) | m <- [0..fromIntegral (4)]]
          d = [((1100, m*(w+gap)), w, h) | m <- [0..fromIntegral (4)]]
          (w,h) = (250,250)
          gap = 30

rectTriangle :: [Triangle]
rectTriangle = a ++ b ++ c ++ d
    where a = [((m*gap+200,100),(m*gap+250,200),(m*gap+150,200)) | m <- [0..fromIntegral (4)]]
          b = [((m*gap+200,100+500),(m*gap+250,200+500),(m*gap+150,200+500)) | m <- [0..fromIntegral (4)]]
          c = [((200,m*gap+100),(250,m*gap+200),(150,m*gap+200)) | m <- [0..fromIntegral (4)]]
          d = [((200+390,m*gap+100),(250+390,m*gap+200),(150+390,m*gap+200)) | m <- [0..fromIntegral (4)]]
          gap = 100


--------------------------------------------------------------------------------------
triangleCircle :: [Circle]
triangleCircle = a ++ b ++ c
    where a = [((x*(gap+2*r) + 2*r, 100+900), r) | x <- [0..fromIntegral(4)]]
          b = [((x*(gap+2*r) + 2*r, x*(gap+2*r) + 100) , r) | x <- [0..fromIntegral(4)]]
          c = [((2*r, x*(gap+2*r) + 100), r) | x <- [0..fromIntegral(4)]]
          r = 100
          gap = 30

triangleRect :: [Rect]
triangleRect = a ++ b ++ c
    where a = [((m*(w+gap), 1120), w, h) | m <- [0..fromIntegral (4)]]
          b = [((m*(w+gap), m*(w+gap)), w, h) | m <- [0..fromIntegral (4)]]
          c = [((0.0, m*(w+gap)), w, h) | m <- [0..fromIntegral (4)]]
          (w,h) = (250,250)
          gap = 30

triangleTriangle ::[Triangle]
triangleTriangle = a ++ b ++ c
    where a = [((m*gap+200,100+400),(m*gap+250,200+400),(m*gap+150,200+400)) | m <- [0..fromIntegral (4)]]
          b = [((m*gap+200,m*gap+100),(m*gap+250,m*gap+200),(m*gap+150,m*gap+200)) | m <- [0..fromIntegral (4)]]
          c = [((200,m*gap+100),(250,m*gap+200),(150,m*gap+200)) | m <- [0..fromIntegral (4)]]
          gap = 100

--------------------------------------------------------------------------------------
circleCircle :: [Circle]
circleCircle = [((centerX + centerR * cos (m),centerY + centerR * sin(m)), r)| m <- [0,(pi/4)..fromIntegral (8)]]
    where (centerX,centerY) = (1000,1000)
          centerR = 300
          r = 100.0

          
circleRect :: [Rect]
circleRect = [((centerX + centerR * cos (m),centerY + centerR * sin(m)), x,y)| m <- [0,(pi/4)..fromIntegral (8)]]
    where (centerX,centerY) = (1000,1000)
          centerR = 300
          (x,y) = (200,200)

circleTriangle :: [Triangle]
circleTriangle = [((x1 + centerX + centerR * cos (m),y1+centerY + centerR * sin(m)),(x2+centerX + centerR * cos (m),y2+centerY + centerR * sin(m)),(x3+centerX + centerR * cos (m),y3+centerY + centerR * sin(m)))| m <- [0,(pi/4)..fromIntegral (8)]]
    where (centerX,centerY) = (1000,1000)
          centerR = 300
          ((x1,y1),(x2,y2),(x3,y3)) = ((200,100),(250,200),(150,200))

--------------------------------------------------------------------------------------
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d)" r g b

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

escolha :: String -> String -> String -> String
escolha esc esc2 cor
        |esc == "linha" &&  esc2 == "circulo"     = svgElements svgCircle (linhaCircle) (map svgStyle (paletaCor cor))
        |esc == "linha" && esc2 == "retangulo"    = svgElements svgRect (linhaRect) (map svgStyle (paletaCor cor)) 
        |esc == "linha" && esc2 == "triangulo"    = svgElements svgTriangle (linhaTriangle) (map svgStyle (paletaCor cor)) 
        |esc == "retangulo" && esc2 == "circulo"  = svgElements svgCircle (rectCircle) (map svgStyle (paletaCor cor))
        |esc == "retangulo" && esc2 == "retangulo"= svgElements svgRect (rectRect) (map svgStyle (paletaCor cor))
        |esc == "retangulo" && esc2 == "triangulo"= svgElements svgTriangle (rectTriangle) (map svgStyle (paletaCor cor))
        |esc == "triangulo" && esc2 == "circulo"  = svgElements svgCircle (triangleCircle) (map svgStyle (paletaCor cor))
        |esc == "triangulo" && esc2 == "retangulo"= svgElements svgRect (triangleRect) (map svgStyle (paletaCor cor))
        |esc == "triangulo" && esc2 == "triangulo"= svgElements svgTriangle (triangleTriangle) (map svgStyle (paletaCor cor))
        |esc == "circulo" && esc2 == "circulo"    = svgElements svgCircle (circleCircle) (map svgStyle (paletaCor cor))
        |esc == "circulo" && esc2 == "retangulo"  = svgElements svgRect (circleRect) (map svgStyle (paletaCor cor))
        |esc == "circulo" && esc2 == "triangulo"  = svgElements svgTriangle (circleTriangle) (map svgStyle (paletaCor cor))
        |otherwise = svgElements svgCircle (linhaCircle) (map svgStyle (paletaCor cor))

desenho :: String -> String -> String -> String
desenho forma forma2 cor  = 
  svgBegin 2000 2000 ++ 
  (escolha forma forma2 cor)++
  svgEnd 

main :: IO ()
main = do
  putStrLn "Digite a Cor:"
  cor <- getLine
  putStrLn "Digite a Forma principal:"
  forma <- getLine
  putStrLn "Digite a Forma secundaria:"
  forma2 <- getLine
  writeFile "teste.svg" $ desenho forma forma2 cor