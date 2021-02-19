module Task2_Test where
import Task2
import Test.HUnit


--a
testImage = Image 3 2 [[Rgb 155 128 0, Rgb 155 128 0,   Rgb 255 255 0], [Rgb 0 255 0, Rgb 155 128 0, Rgb 128 255 128]]
test0 = "graysacle works just fine" ~: Image 3 2 [[Rgb 122 122 122, Rgb 122 122 122,Rgb 226 226 226],[Rgb 150 150 150,Rgb 122 122 122,Rgb 202 202 202]] ~=? grayscale testImage
test1 = "grayscale works for empty content" ~: Image 0 0 [[]] ~=? Image 0 0 [[]] 
--b
gXtest :: [[Float]]
gYtest :: [[Float]]
gXtest = [[1,0,-1],[2,0,-2],[1,0,-1]]
gYtest = [[1,2,1],[0,0,0],[-1,-2,-1]]
matrixAtest :: [[Float]]
matrixAtest =  [[2.0,1.0,1.0],[1.0,1.0,1.0],[1.0,1.0,1.0]]


test10 = "calcEl works" ~: -1 ~=? calcEl matrixAtest gXtest

test11 = "calcElForRgb works" ~: 255 ~=? calcElForRgb [[76, 122, 226], [150,255,202], [100, 100, 200]]
test12 = "calcElForRgb works" ~: 0 ~=? calcElForRgb [[-1, -1, -1], [-1,-1,-1], [-1,-1,-1]]
test13 = "calcElForRgb works" ~: 182 ~=? calcElForRgb [[76, 1,1], [150,102,105], [0,0,0]]

contentTest = [[Rgb {red = 76, green = 76, blue = 76},Rgb {red = 122, green = 122, blue = 122},Rgb {red = 226, green = 226, blue = 226}],[Rgb {red = 150, green = 150, blue = 150},Rgb {red = 255, green = 255, blue = 255},Rgb {red = 202, green = 202, blue = 202}]]
test14 = "extractMatrixFromContent works" ~: [[76, 122, 226], [150,255,202]] ~=? extractMatrixFromContent contentTest

bigMatrixRes::[[Float]]
bigMatrixRes = [[9,7,8,9,7],[3,1,2,3,1],[6,4,5,6,4],[9,7,8,9,7],[3,1,2,3,1]]
test15 = "generateBigMatrix works" ~:  bigMatrixRes ~=? generateBigMatrix [[1,2,3],[4,5,6],[7,8,9]]

test3_3MatrixRes::[[Float]]
test3_3MatrixRes = [[1,2,3],[4,5,6],[7,8,9]]
test16 = "generate3_3Matrix works" ~: test3_3MatrixRes ~=? generate3_3Matrix bigMatrixRes 2 2

edgeDetectTestImageExpected = Image {width = 3
                             , height = 2
                             , content = [[Rgb {red = 54, green = 54, blue = 54}
                                           , Rgb {red = 255, green = 255, blue = 255}
                                           , Rgb {red = 255, green = 255, blue = 255}],
                                          [Rgb {red = 54, green = 54, blue = 54}
                                           , Rgb {red = 255, green = 255, blue = 255}
                                           , Rgb {red = 255, green = 255, blue = 255}]]}
edgeDetectTestImageInput = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0], [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]


test17 = "edgeDetect works" ~: edgeDetectTestImageExpected ~=? edgeDetect edgeDetectTestImageInput

--c
floodFillImageExpected = Image 3 2 [[Rgb 155 155 155, Rgb 155 155 155, Rgb 255 255 0],[Rgb 0 255 0, Rgb 155 155 155, Rgb 128 255 128]]
fllodFillAlgorithmInput = Image 3 2 [[Rgb 155 128 0, Rgb 155 128 0, Rgb 255 255 0], [Rgb 0 255 0, Rgb 155 128 0, Rgb 128 255 128]]
test20 = "floodFill works fine " ~: floodFillImageExpected ~=? floodFill (Rgb 155 155 155) 0 1 fllodFillAlgorithmInput

input21 = Image 3 3 [[Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
expected21 = Image 3 3 [[Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
test21 = "floodFill works for a lone pixel " ~: expected21 ~=? floodFill (Rgb 155 155 155) 0 1 input21 

input22 = Image 3 3 [[Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0]]
expected22 = Image 3 3 [[Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0]]
test22 = "floodFill works for a lower pixel " ~: expected22 ~=? floodFill (Rgb 155 155 155) 0 1 input22 

input23 = Image 3 3 [[Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0]]
expected23 = Image 3 3 [[Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0]]
test23 = "floodFill works for upper pixel " ~: expected23 ~=? floodFill (Rgb 155 155 155) 2 1 input23

input24 = Image 3 3 [[Rgb 255 1 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
expected24 = Image 3 3 [[Rgb 155 155 155, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
test24 = "floodFill works for left pixel " ~: expected24 ~=? floodFill (Rgb 155 155 155) 0 1 input24

input25 = Image 3 3 [[Rgb 255 1 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
expected25 = Image 3 3 [[Rgb 155 155 155, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
test25 = "floodFill works for right pixel " ~: expected25 ~=? floodFill (Rgb 155 155 155) 0 0 input25

input26 = Image 3 3 [[Rgb 255 1 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 1 0]]
expected26 = Image 3 3 [[Rgb 155 155 155, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 155 155 155]]
test26 = "floodFill works recursively" ~: expected26 ~=? floodFill (Rgb 155 155 155) 0 1 input26

input27 = Image 3 3 [[Rgb 255 1 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 1 0]]
expected27 = Image 3 3 [[Rgb 155 155 155, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 155 155 155, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
test27 = "floodFill works recursively and includes the expected neighbours" ~: expected27 ~=? floodFill (Rgb 155 155 155) 0 1 input27

input28 = Image 3 3 [[Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0], [Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0]]
expected28 = Image 3 3 [[Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0], [Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0], [Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0]]
test28 = "floodFill fills the whole image" ~: expected28 ~=? floodFill (Rgb 0 0 0) 0 1 input28

input29 = Image 3 3 [[Rgb 255 0 0, Rgb 255 1 0, Rgb 255 1 0], [Rgb 255 1 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 1 0, Rgb 255 0 0, Rgb 255 0 0]]
expected29 = Image 3 3 [[Rgb 255 0 0, Rgb 0 0 0, Rgb 0 0 0], [Rgb 0 0 0, Rgb 0 0 0, Rgb 255 0 0], [Rgb 0 0 0, Rgb 255 0 0, Rgb 255 0 0]]
test29 = "floodFill from the last row to the first one" ~: expected29 ~=? floodFill (Rgb 0 0 0) 2 0 input29

input30 = Image 3 3 [[Rgb 255 0 0, Rgb 255 1 0, Rgb 255 1 0], [Rgb 255 1 0, Rgb 255 1 0, Rgb 255 0 0], [Rgb 255 1 0, Rgb 255 0 0, Rgb 255 0 0]]
expected30 = Image 3 3 [[Rgb 255 0 0, Rgb 0 0 0, Rgb 0 0 0], [Rgb 0 0 0, Rgb 0 0 0, Rgb 255 0 0], [Rgb 0 0 0, Rgb 255 0 0, Rgb 255 0 0]]
test30 = "floodFill check for the opposite directions" ~: expected30 ~=? floodFill (Rgb 0 0 0) 0 2 input30

input31 = Image 3 3 [[Rgb 255 1 0, Rgb 255 1 0, Rgb 255 1 0], [Rgb 255 1 0, Rgb 255 0 0, Rgb 255 1 0], [Rgb 255 1 0, Rgb 255 0 0, Rgb 255 1 0]]
expected31 = Image 3 3 [[Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0], [Rgb 0 0 0, Rgb 255 0 0, Rgb 0 0 0], [Rgb 0 0 0, Rgb 255 0 0, Rgb 0 0 0]]
test31 = "floodFill fills the whole image" ~: expected31 ~=? floodFill (Rgb 0 0 0) 2 2 input31


tl = TestList [test0, test1, test10, test11, test12, test13, test14, test15, test16, test17, test20, test21, test22, test23, test24, test25, test26, test27, test28, test29, test30, test31]
