module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List
--import System.Random


type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Float
imageWidth = 350

imageHeight :: Float
imageHeight = 350

cons :: Float
cons = (imageWidth + imageHeight) / 10000 * 0.9

-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)		
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Float -> Float -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++ concat (gera dataset) ++ "</svg>\n"


spiral :: Float -> Point
spiral a = (x+a*(cos a), y+a*(sin a))
	where
	x = imageWidth/2
	y = imageHeight/2
	

intersecta :: Circle -> [Circle] -> Bool
intersecta ((x2,y2),r2) [] = False
intersecta ((x2,y2),r2) (((x1,y1),r1):lc)
	| d <= a = True
	| d > a  = intersecta ((x2,y2),r2) lc
	where 
	d = sqrt ((y2 - y1)^2 + (x2 - x1)^2) - 0.5 -- -0.5 = 0.5 espaçamento de tolerancia pra nao parecerem circulos grudados
	a = r1 + r2

calculaRaio :: Int -> Float
calculaRaio d = (sqrt (fromIntegral d)) * 1.5 + 1

geraLista :: [Int] -> [Circle]
geraLista [] = []
geraLista (d:[]) = [(spiral 0, calculaRaio d)] -- Primeiro elemento
geraLista ds = (circulosAnteriores ++ [novoCirc circulosAnteriores r 0]) -- Cria a lista a partir dos ultimos elementos pro primeiro.
	where
	circulosAnteriores = geraLista (init ds)
	r = calculaRaio (last ds)

novoCirc :: [Circle] -> Float -> Float -> Circle
novoCirc lc r2 a
	| intersects == True = novoCirc lc r2 (a + cons) -- Se tem interseção cria um novo circulo
	| intersects == False = ((newPos), r2) -- Se não tem retorna o ponto criado
	where
	newPos = spiral (a + cons)
	intersects = intersecta (newPos, r2) lc

gera :: [Int] -> [String]
gera [] = []
gera d = svgBubbleGen (geraLista d)

svgBubbleGen:: [Circle] -> [String]
svgBubbleGen [] = []
svgBubbleGen (a:b) = svgCircle (fst a, snd a) ((floor (foldr (*) 3 (take 1 (iterate (1.1*) (snd a))))),(floor (foldr (/) 5 (take 4 (iterate (2.5*) (snd a))))),(floor (foldr (/) 2.2 (take 3 (iterate (2.5*) (snd a)))))) : svgBubbleGen b

	
-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> (Int, Int, Int) -> String
svgCircle ((x,y),d) (r,g,b) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y d t s v
	where
	t = b + g
	s = g + r
	v = r + b

	{-
	| r > 5.00 && r <= 5.02 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(153,225,153)\" />\n" x y a
	| r > 5.02 && r <= 5.04 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(229,204,255)\" />\n" x y a
	| r > 5.04 && r <= 5.06 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,229,204)\" />\n" x y a
	| r > 5.06 && r <= 5.08 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(102,178,225)\" />\n" x y a
	| r > 5.08 && r <= 5.05 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(192,192,192)\" />\n" x y a
	| r > 5.08 && r <= 5.1 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(204,204,0)\" />\n" x y a
	| r > 5.1 && r <= 5.3 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(176,224,230)\" />\n" x y a
	| r > 5.3 && r <= 5.6 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(245,245,220)\" />\n" x y a
	| r > 5.6 && r <= 5.8 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,102,102)\" />\n" x y a
	| r > 5.6 && r <= 6.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(138,43,226)\" />\n" x y a
	| r > 6.0 && r <= 7.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(188,143,143)\" />\n" x y a
	| r > 7.0 && r <= 8.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(0,224,44)\" />\n" x y a
	| r > 8.0 && r <= 9.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(212,0,224)\" />\n" x y a
	| r > 9 && r <= 10 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(33,146,224)\" />\n" x y a
	| r > 10 && r <= 15 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(109,224,0)\" />\n" x y a
	| r > 15 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(240,128,128)\" />" x y a
	where 
	r = a - 2
	-}
	
	
-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Float -> Float -> String
svgViewBox w h =
        printf  "<svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%f\" height=\"%f\" style=\"fill:white;\"/>\n" w h