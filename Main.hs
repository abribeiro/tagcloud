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
imageWidth = 360

imageHeight :: Int
imageHeight = 360


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
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h d = [svgCircle ((fromIntegral w/2, fromIntegral h/2), fromIntegral (head d)/10)] 

-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> String
svgCircle ((x,y),r)
	| r <= 0.5 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(44,122,245)\" />\n" x y r
	| r > 0.5 && r <= 1 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(12,222,45)\" />\n" x y r
	| r > 1 && r <= 1.5 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(222,156,45)\" />\n" x y r
	| r > 1.5 && r <= 3 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(0,252,177)\" />\n" x y r
	| r > 3 && r <= 5 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(2,22,45)\" />\n" x y r
	| r > 5 && r <= 7 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(15,202,55)\" />\n" x y r
	| r > 7 && r <= 10 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(51,102,255)\" />\n" x y r
	| r > 10 && r <= 13 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,102,0)\" />\n" x y r
	| r > 13 && r <= 16 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(255,152,120)\" />\n" x y r
    | r > 16 && r <= 20 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(2,22,45)\" />\n" x y r
	| r > 20 && r <= 25 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(211,22,245)\" />\n" x y r
	| r > 25 && r <= 30 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(211,222,245)\" />\n" x y r
	| r > 30 && r <= 35 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(11,22,45)\" />\n" x y r
	| r > 35 && r <= 40 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(111,122,145)\" />\n" x y r
	| r > 40 && r <= 45 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(55,122,205)\" />\n" x y r
	| r > 45 && r <= 50 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(1,222,245)\" />\n" x y r
	| r > 50 = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(167,212,45)\" />\n" x y r

-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h