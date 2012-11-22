{--
        Esqueleto de programa para geração de bubble cloud em Haskell.
        Mais informações em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 350

imageHeight :: Int
imageHeight = 350


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
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (gera w h 0 dataset)) ++ "</svg>\n"


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.

gera :: Int -> Int -> Int -> [Int] -> [String]
gera w h ad [] = []
gera w h ad (d:s) = svgBubbleGen w h d ad : gera w h (head s) s
	
spiralw :: Int -> Int -> Float
spiralw x r = fromIntegral r/2

spiralh :: Int -> Int -> Float
spiralh y r = fromIntegral r/2

svgBubbleGen:: Int -> Int -> Int -> Int -> String
svgBubbleGen w h d ad 
	| d >= 82 = svgCircle (( (spiralw w ad), (spiralh h ad)), fromIntegral 90)
	| otherwise = svgCircle (( (spiralw w ad), (spiralh h ad)), fromIntegral d/10+5)

-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> String
svgCircle ((x,y),r)	
	| r <= 5.5 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(44,122,245)\" />\n" x y r
	| r > 5.5 && r <= 6 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(12,222,45)\" />\n" x y r
	| r > 6 && r <= 11.5 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(222,156,45)\" />\n" x y r
	| r > 11.5 && r <= 17 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(0,252,177)\" />\n" x y r
	| r > 17 && r <= 22 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(2,22,45)\" />\n" x y r
	| r > 22 && r <= 27 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(15,202,55)\" />\n" x y r
	| r > 27 && r <= 32 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(51,102,255)\" />\n" x y r
	| r > 32 && r <= 37 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,102,0)\" />\n" x y r
	| r > 37 && r <= 42 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,152,120)\" />\n" x y r
    | r > 42 && r <= 47 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(2,22,45)\" />\n" x y r
	| r > 47 && r <= 52 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(211,22,245)\" />\n" x y r
	| r > 52 && r <= 57 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(211,222,245)\" />\n" x y r
	| r > 57 && r <= 62 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(11,22,45)\" />\n" x y r
	| r > 62 && r <= 67 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(111,122,145)\" />\n" x y r
	| r > 67 && r <= 72 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(55,122,205)\" />\n" x y r
	| r > 72 && r <= 77 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(1,222,245)\" />\n" x y r
	| r > 77 && r <= 82 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(167,212,45)\" />\n" x y r
	| r > 82 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"90.1\" fill=\"rgb(16,12,65)\" />\n" x y -- Maior raio = 90 
	
-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:black;\"/>\n" w h