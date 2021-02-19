module Task2 where
import Data.Word
import Data.List.Split

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)


--a
grayscale :: Image -> Image
grayscale img = Image (width img) (height img) (convertContent (content img))

convertContent :: [[Rgb]] -> [[Rgb]]
convertContent content = map(\ x -> convertRow x) content


convertRow::[Rgb] -> [Rgb]
convertRow l = map (\elem -> Rgb (truncate ((fromIntegral (red elem) * 0.30 + (fromIntegral (green elem)) * 0.59 + ((fromIntegral (blue elem)) * 0.11))))
                                 (truncate ((fromIntegral (red elem) * 0.30 + (fromIntegral (green elem)) * 0.59 + ((fromIntegral (blue elem)) * 0.11))))
                                 (truncate ((fromIntegral (red elem) * 0.30 + (fromIntegral (green elem)) * 0.59 + ((fromIntegral (blue elem)) * 0.11))))) l

-- image {content = (map (map convertRow) content)}

--b

--matrix calculation

gX :: [[Float]]
gY :: [[Float]]
gX = [[1,0,-1],[2,0,-2],[1,0,-1]]
gY = [[1,2,1],[0,0,0],[-1,-2,-1]] 

calcEl :: [[Float]] -> [[Float]] -> Int
calcEl m1 m2 = truncate (sum (map (\row -> sum row) (map (\(l1,l2) -> zipWith (*) l1 (reverse l2)) (zip m1 (reverse m2)))))

calcElForRgb :: [[Float]] -> Int
calcElForRgb m
   | res <= 0 = 0
   | res >= 255 = 255 
   | otherwise = res
   where res = truncate (sqrt (fromIntegral (((calcEl m gX) ^ 2) + ((calcEl m gY) ^ 2)))) 

extractMatrixFromContent content = map (\row -> (map (\ (Rgb {red = x, green = y, blue = z}) -> (fromIntegral x)) row)) content

generateBigMatrix :: [[Float]] -> [[Float]]
generateBigMatrix input = [last(last(input)) : last(input) ++ [head(last(input))]] ++ map (\row -> last(row): row ++ [head(row)]) input ++ [last(head(input)) : head(input) ++ [head(head(input))]]

generate3_3Matrix input i j =  [drop (i-1) (take (i+2) (input!!(j-1))), drop (i-1) (take (i+2) (input!!j)), drop (i-1) (take (i+2) (input!!(j+1)))]  

edgeDetect :: Image -> Image
edgeDetect img = Image {width = (width img), height = (height img), content = (convertMatrixToImage (generateRGB (height img) (extractMatrixFromContent (content img))))}

generateRGB::Int -> [[Float]] -> [[Int]]
generateRGB height originalMatrix
   | (height - 1) == 0 = [map (\row -> [calcElForRgb (generate3_3Matrix bigMatrix 1 height)] ++ [calcElForRgb (generate3_3Matrix bigMatrix 2 height)] ++ [calcElForRgb (generate3_3Matrix bigMatrix 3 height)]) originalMatrix!!(height-1)]
   | otherwise = generateRGB (height-1) originalMatrix ++ [map (\row -> [calcElForRgb (generate3_3Matrix bigMatrix 1 height)] ++ [calcElForRgb (generate3_3Matrix bigMatrix 2 height)] ++ [calcElForRgb (generate3_3Matrix bigMatrix 3 height)]) originalMatrix!!(height-1)]
   where bigMatrix = generateBigMatrix originalMatrix   

convertMatrixToImage :: [[Int]] -> [[Rgb]]
convertMatrixToImage matrix = map (\ row -> (map (\x -> Rgb {red = (fromIntegral x), green = (fromIntegral x), blue = (fromIntegral x)}) row)) matrix


instance Eq Image where
  (==) Image {width = w1, height = h1, content = c1} Image {width = w2, height = h2, content = c2} = (w1 == w2) && (h1 == h2) && (extractMatrixFromContent c1 == extractMatrixFromContent c2) 



--c

instance Eq Rgb where
  (==) Rgb {red = r1, green = g1, blue = b1} Rgb {red = r2, green = g2, blue = b2} = (r1 == r2) && (g1 == g2) && (b1 == b2)

floodFill :: Rgb -> Int -> Int -> Image -> Image
floodFill color x y img = Image (width img) (height img) (flood x y ((content img)!!x!!y) color (content img) (height img) (width img))

flood i j target_color color content height width
    | i < 0 || (j < 0) || (i >= height) || (j >= width) = content
    | content!!i!!j /= target_color = content
    | content!!i!!j == color = content
    | otherwise = flood i (j-1) target_color color (flood i (j+1) target_color color (flood (i+1) j target_color color (flood (i-1) j target_color color (changeEl i j content color) height width) height width) height width) height width


changeEl i j content new = take i content ++ [(take j (content!!i)) ++ [new] ++ (drop (j+1) (content!!i))] ++ (drop (i+1) content)


--d

saveImage:: FilePath -> Image -> IO()
saveImage path img = do 
    let line0 = "P3\n"
        line1 = show (width img) ++ " " ++ (show (height img)) ++ "\n"
        line2 = show 255 ++ "\n"
        contentLines = convertToLines (content img)
    writeFile path line0
    appendFile path line1
    appendFile path line2
    appendFile path (unlines contentLines)


convertToLines::[[Rgb]] -> [String]
convertToLines rgbMatrix = foldl (++) [] (map (\row -> (map (\r -> (show (fromIntegral (red r)) ++ " " ++ show (fromIntegral (green r)) ++ " " ++ show (fromIntegral(blue r)))) row)) rgbMatrix)



--e
loadImage :: String -> IO Image
loadImage path = do
    fileContent <- readFile path
    let imagePpm = lines fileContent
        size = parse (imagePpm!!1)
        width = read (size!!0)::Int
        height = read (size!!1)::Int
        content = convertToContent (parseToRgbMatrix (map (\x -> parseToWord8List (parse x)) (drop 3 imagePpm))) width
    return (Image width height content)

parse str =  split (dropDelims(dropBlanks (onSublist " "))) str
parseToWord8List = map (\x -> read x::Word8) 
parseToRgbMatrix = map (\x -> Rgb {red = (x!!0), green = (x!!1), blue = (x!!2)})

convertToContent [] w = []
convertToContent l w = [(take w l)] ++ convertToContent (drop w l) w
    







----- for manual test purposes
testM :: [[Float]]
testM = [[1,2,3,4],[5,2,1,3],[1,1,1,0],[4,3,1,1]]


test = Image 3 2 [[Rgb 155 128 0, Rgb 155 128 0,   Rgb 255 255 0], [Rgb 0 255 0, Rgb 155 128 0, Rgb 128 255 128]]




    


