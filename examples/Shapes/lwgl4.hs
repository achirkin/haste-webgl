{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Bits ((.|.))
import Geometry.Space
import Haste
import Haste.Foreign
import Haste.Graphics.WebGL
import Data.Word

import Shapes.Drawable




createCube :: WorldContext -> IO SimpleTriangleMesh
createCube w = createSTIdxMesh w coords colors indices
    where coords = [Vector3 (-1) (-1) 1, Vector3 1 (-1) 1, Vector3 1 1 1, Vector3 (-1) 1 1,
                    Vector3 (-1) (-1) (-1), Vector3 (-1) 1 (-1), Vector3 1 1 (-1), Vector3 1 (-1) (-1),
                    Vector3 (-1) 1 (-1), Vector3 (-1) 1 1, Vector3 1 1 1, Vector3 1 1 (-1),
                    Vector3 (-1) (-1) (-1), Vector3 1 (-1) (-1), Vector3 1 (-1) 1, Vector3 (-1) (-1) 1,
                    Vector3 1 (-1) (-1), Vector3 1 1 (-1), Vector3 1 1 1, Vector3 1 (-1) 1,
                    Vector3 (-1) (-1) (-1), Vector3 (-1) (-1) 1, Vector3 (-1) 1 1, Vector3 (-1) 1 (-1)
                   ]::[Vector3 Float]
          colors = [Vector3 255  0   0,
                    Vector3 255 255  0,
                    Vector3  0  255  0,
                    Vector3  0  130 130,
                    Vector3 255  0  255,
                    Vector3  0   0  255] >>= replicate 4 :: [Vector3 Word8]
          indices = [0, 1, 2, 0, 2, 3,
                     4, 5, 6, 4, 6, 7,
                     8, 9, 10, 8, 10, 11,
                     12, 13, 14, 12, 14, 15,
                     16, 17, 18, 16, 18, 19,
                     20, 21, 22, 20, 22, 23]::[Word16]
    
    
createPyramid :: WorldContext -> IO SimpleTriangleMesh
createPyramid w = createSTMesh w coords colors
    where coords = [Vector3 0 1 0, Vector3 (-1) (-1)   1 , Vector3   1  (-1)   1,
                    Vector3 0 1 0, Vector3   1  (-1)   1 , Vector3   1  (-1) (-1),
                    Vector3 0 1 0, Vector3   1  (-1) (-1), Vector3 (-1) (-1) (-1),
                    Vector3 0 1 0, Vector3 (-1) (-1) (-1), Vector3 (-1) (-1)   1]::[Vector3 Float]
          colors = [Vector3 255 0 0, Vector3 0 255 0, Vector3 0 0 255,
                    Vector3 255 0 0, Vector3 0 0 255, Vector3 0 255 0,
                    Vector3 255 0 0, Vector3 0 255 0, Vector3 0 0 255,
                    Vector3 255 0 0, Vector3 0 0 255, Vector3 0 255 0] ::[Vector3 Word8]


fragmentShaderText :: String
fragmentShaderText = unlines [
    "precision mediump float;",
    "varying vec4 vColor;",
    "void main(void) {",
    "    gl_FragColor = vColor;",
    "}"]

vertexShaderText :: String
vertexShaderText = unlines [
    "attribute vec3 aVertexPosition;",
    "attribute vec4 aVertexColor;",
    "uniform mat4 uMVMatrix;",
--    "uniform mat4 uPMatrix;",
    "varying vec4 vColor;",
    "void main(void) {",
--    "  gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);",
    "  gl_Position = uMVMatrix * vec4(aVertexPosition, 1.0);",
    "  vColor = aVertexColor;",
    "}"]

getShader::Context->ShaderType->String->IO Shader
getShader gl typ src = do
    result <- createShader gl typ
    shaderSource gl result src
    compileShader gl result
    return result

initShaders::Context->IO (UniformLocation, AttribLocation, AttribLocation)
initShaders gl = do
    fragmentShader <- getShader gl FragmentShader fragmentShaderText
    vertexShader <- getShader gl VertexShader vertexShaderText
    shaderProgram <- createProgram gl
    attachShader gl shaderProgram vertexShader
    attachShader gl shaderProgram fragmentShader
    linkProgram gl shaderProgram

    useProgram gl shaderProgram

    vertPos <- getAttribLocation gl shaderProgram "aVertexPosition"
    enableVertexAttribArray gl vertPos
    vertCol <- getAttribLocation gl shaderProgram "aVertexColor"
    enableVertexAttribArray gl vertCol

    mvmatUniform <- getUniformLocation gl shaderProgram "uMVMatrix"
    return (mvmatUniform, vertPos, vertCol)


drawScene :: Drawable (s Float SimpleTriangleMesh)
          => WorldContext
          -> [s Float SimpleTriangleMesh]
          -> IO ()
drawScene w drawables = do
    clear (wgl w) (ColorBufferBit .|. DepthBufferBit)
    mapM_ (draw w) drawables
    

forever :: Int -> IO a -> IO ()
forever delay m = m >> setTimeout delay (Main.forever delay m)

main :: IO ()
main = setTimeout 100 $ do
    Just canvas <- elemById "glcanvas"
    cwidth <- liftM read (getProp canvas "offsetWidth")
    cheight <- liftM read (getProp canvas "offsetHeight")
    let f (x, y) | x > 1680 = f (quot x 2, quot y 2)
                 | otherwise = (x, y)
        (width, height) = f (cwidth, cheight)
        aratio = (fromIntegral cwidth / fromIntegral cheight) :: Float
    setAttr canvas "width" $ show width
    setAttr canvas "height" $ show height

    gl <- getContext canvas "webgl"
    (mvUni, posAttr, colAttr) <- initShaders gl
    
    enable gl DepthTest
    viewport gl 0 0 width height
    clearColor gl 0 0 0 0
    
    camera <- initCamera aratio mvUni
    let world = World {
            wgl = gl,
            wcamera = camera,
            wtime = 0,
            wAttrLocs = (posAttr, colAttr)
        }
    cube <- liftM wrap (createCube world)
    pyramid <- liftM wrap (createPyramid world)
    Main.forever (floor (1000/120 :: Float)) $ do
        t <- ffi "(function() {return (new Date().getTime());})" :: IO Int
        let rPyramid = - fromIntegral t / 2000 :: Float
            rCube = fromIntegral t / 1000  :: Float
            rCamera = fromIntegral t / 5000 :: Float
            viewmat = lookAtMatrix (Vector3 0 1 0) (Vector3 (-3.5 + 13 * cos rCamera) 5 (13*sin rCamera)) (Vector3 (-3.5) 0 0)
        drawScene world{ wcamera = camera >>= transformM4 viewmat, wtime = t}
            ([cube >>= translate (Vector3 1.5 0 0) >>= rotate (unit ones) rCube
            , pyramid >>= translate (Vector3 (-3.5) 0 0) >>= rotateY rPyramid] :: [QTransform Float SimpleTriangleMesh])
    return ()
