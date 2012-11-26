module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List
import System.Random
import Test.QuickCheck.Gen -- ungen/choose

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Float
imageWidth = 350

imageHeight :: Float
imageHeight = 350

cons :: Float
cons = (imageWidth + imageHeight) / 10000 * 0.8

-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let
		pairs = map (span (/= ' ')) (lines strcontent)
		freqs = readInts (map snd pairs)
		palavras = map fst pairs
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs palavras)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss



-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Float -> Float -> [Int] -> [String] -> String
svgCloudGen w h dataset palavras = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++ concat (gera dataset palavras) ++ "</svg>\n"


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
calculaRaio d = (sqrt (fromIntegral d)) * 1.6

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

gera :: [Int] -> [String] -> [String]
gera [] [] = []
gera d p = svgBubbleGen (geraLista d) p

svgBubbleGen:: [Circle] -> [String] -> [String]
svgBubbleGen [] [] = []
svgBubbleGen (a:b) (h:p) = svgCircle (fst a, snd a) ((rd (fst (fst a))),(rd (snd a)),(rd (snd (fst a)))) h : svgBubbleGen b p


rd :: Float -> Int
rd a = unGen (choose (0::Int, 255::Int)) (mkStdGen (floor a)) (floor a)
  
  
-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> (Int, Int, Int) -> String -> String
svgCircle ((x,y),d) (r,g,b) str
	| d > 15 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n<text x=\"%f\" y=\"%f\" style=\"font-family:Verdana;font-weight:bold; font-size:7;\" text-anchor=\"middle\" fill=\"white\"><tspan>%s</tspan><tspan dx=\"-%d\" dy=\"10\">[%d]</tspan></text>" x y d r g b x1 y1 p y2 s
	| otherwise = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />" x y d r b g
	where
	--if str > 5
	p = take 5 str ++ ".."
	--p = if (length str) < 6 then x else str
	--else p = str
	s = round ((d / 1.6) ^ 2) :: Int
	x1 = x --(round x) - (length (show s))
	y1 = y -- 6.0
	y2 = 18 + (length p)
	
	
	{-	
	| a > 1.00 && a <= 3.02 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(153,225,153)\" />\n" x y a
	| a > 3.02 && a <= 6.04 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(229,24,255)\" />\n" x y a
	| a > 6.04 && a <= 8.06 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(25,229,104)\" />\n" x y a
	| a > 8.06 && a <= 9.08 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(102,178,225)\" />\n" x y a
	| a > 9.08 && a <= 11.05 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(192,192,192)\" />\n" x y a
	| a > 11.05 && a <= 13.1 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(204,204,0)\" />\n" x y a
	| a > 13.1 && a <= 15.3 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(176,224,230)\" />\n" x y a
	| a > 15.3 && a <= 17.6 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(25,245,220)\" />\n" x y a
	| a > 17.6 && a <= 20.8 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,102,102)\" />\n" x y a
	| a > 20.8 && a <= 25.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(138,43,226)\" />\n" x y a
	| a > 25.0 && a <= 30.0 = printf "<circle cx="%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(188,143,143)\" />\n" x y a
	| a > 30.0 && a <= 40.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(0,224,44)\" />\n" x y a
	| a > 40.0 && a <= 50.0 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(212,0,224)\" />\n" x y a
	| a > 50 && a <= 60 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(33,146,224)\" />\n" x y a
	| a > 60 && a <= 70 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(109,224,0)\" />\n" x y a
	| a > 70 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(240,128,128)\" />" x y a
    -}
	
	
	
-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Float -> Float -> String
svgViewBox w h =
        printf  "<svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%f\" height=\"%f\" style=\"fill:white;\"/>\n" w h