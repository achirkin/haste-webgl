{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (foldr, foldl, foldr1, foldl1)
--import Control.Monad
--import Data.IORef
import Data.Bits ((.|.))
import Geometry.Space
--import qualified Geometry.Space as S
import Data.Foldable ( Foldable(..) )
--import Data.Word
import Haste
import Haste.Foreign
--import Haste.DOM
import Haste.Graphics.WebGL
import qualified Haste.Graphics.WebGL as WGL
import Foreign (Ptr, castPtr, mallocArray, newArray, withArray)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word

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
    "uniform mat4 uPMatrix;",
    "varying vec4 vColor;",
    "void main(void) {",
    "  gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);",
    "  vColor = aVertexColor;",
    "}"]

getShader::Context->ShaderType->String->IO Shader
getShader gl typ src = do
    result <- createShader gl typ
    shaderSource gl result src
    compileShader gl result
    return result

initShaders::Context->IO (UniformLocation, UniformLocation, AttribLocation, AttribLocation)
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

    pmatUniform <- getUniformLocation gl shaderProgram "uPMatrix"
    mvmatUniform <- getUniformLocation gl shaderProgram "uMVMatrix"

    return (pmatUniform, mvmatUniform, vertPos, vertCol)

setMatrixUniforms::Context->(UniformLocation, UniformLocation)->(Matrix4x4 Float, Matrix4x4 Float)->IO ()
setMatrixUniforms gl (pIdx, mvIdx) (pMat, mvMat) = do
    ptrp <- malloc :: IO (Ptr (Matrix4x4 Float))
    poke ptrp pMat
    uniformMatrix4fv gl pIdx ptrp
    ptrm <- malloc :: IO (Ptr (Matrix4x4 Float))
    poke ptrm mvMat
    uniformMatrix4fv gl mvIdx ptrm
--    arrp <- newSizedArray 16
--    store arrp pMat >> uniformMatrix4fv gl pIdx arrp
--    arrmv <- newSizedArray 16
--    store arrmv mvMat >> uniformMatrix4fv gl mvIdx arrmv

initBuffers::Context->IO (Buffer, Buffer, Buffer, Buffer, Buffer)
initBuffers gl = do
    pyramidVertsBuffer <- createBuffer gl
    bindBuffer gl ArrayBufferTarget pyramidVertsBuffer

    let pyramidVerts = [0, 1, 0, -1, -1, 1, 1, -1, 1,
                      0, 1, 0, 1, -1, 1, 1, -1, -1,
                      0, 1, 0, 1, -1, -1, -1, -1, -1,
                      0, 1, 0, -1, -1, -1, -1, -1, 1]::[Float]

    newArray pyramidVerts >>= bufferData' gl ArrayBufferTarget StaticDraw

    pyramidColorBuffer <- createBuffer gl
    bindBuffer gl ArrayBufferTarget pyramidColorBuffer

    let pyramidColors = [1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1,
                       1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1,
                       1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1,
                       1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1]::[Float]

    newArray pyramidColors >>= bufferData' gl ArrayBufferTarget StaticDraw

    cubeVertsBuffer <- createBuffer gl
    bindBuffer gl ArrayBufferTarget cubeVertsBuffer

    let cubeVerts = [-1, -1, 1, 1, -1, 1, 1, 1, 1, -1, 1, 1,
                   -1, -1, -1, -1, 1, -1, 1, 1, -1, 1, -1, -1,
                   -1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, -1,
                   -1, -1, -1, 1, -1, -1, 1, -1, 1, -1, -1, 1,
                   1, -1, -1, 1, 1, -1, 1, 1, 1, 1, -1, 1,
                   -1, -1, -1, -1, -1, 1, -1, 1, 1, -1, 1, -1]::[Float]
    newArray cubeVerts >>= bufferData' gl ArrayBufferTarget StaticDraw

    cubeColorBuffer <- createBuffer gl
    bindBuffer gl ArrayBufferTarget cubeColorBuffer

    let faceColors = [[1, 0, 0, 1], [1, 1, 0, 1],
                    [0, 1, 0, 1], [1, 0.5, 0.5, 1],
                    [1, 0, 1, 1], [0, 0, 1, 1]]::[[Float]]
        cubeColors = concat . concatMap (Prelude.replicate 4) $ faceColors
    newArray cubeColors >>= bufferData' gl ArrayBufferTarget StaticDraw

    cubeIndexBuffer <- createBuffer gl
    bindBuffer gl ElementArrayBufferTarget cubeIndexBuffer

    let cubeIndices = [0, 1, 2, 0, 2, 3,
                     4, 5, 6, 4, 6, 7,
                     8, 9, 10, 8, 10, 11,
                     12, 13, 14, 12, 14, 15,
                     16, 17, 18, 16, 18, 19,
                     20, 21, 22, 20, 22, 23]::[Word16]
    newArray cubeIndices >>= bufferData' gl ElementArrayBufferTarget StaticDraw

    return (pyramidVertsBuffer, pyramidColorBuffer, cubeVertsBuffer, cubeColorBuffer, cubeIndexBuffer)

drawScene::Context->(UniformLocation, UniformLocation)->Int->
           (Buffer, Buffer, Buffer, Buffer, Buffer)->(AttribLocation, AttribLocation) -> (Int,Int)->IO ()
drawScene gl (pIdx, mvIdx) time buffers attribs (wwidth,wheight) = do
    let (pyramidVertsBuffer, pyramidColorBuffer, cubeVertsBuffer, cubeColorBuffer, cubeIndexBuffer) = buffers
        (posAttrib, colorAttrib) = attribs
        rPyramid = - fromIntegral time / 2000 :: Float
        rCube = fromIntegral time/ 1000  :: Float
        rCamera = fromIntegral time / 5000 :: Float

    viewport gl 0 0 wwidth wheight
    clear gl (ColorBufferBit .|. DepthBufferBit)
  
    let projmat = perspective 0.1 100 (pi/4) (fromIntegral wwidth / fromIntegral wheight)
        viewmat = lookAtMatrix (Vector3 0 1 0) (Vector3 (-3.5 + 13 * cos rCamera) 5 (13*sin rCamera)) (Vector3 (-3.5) 0 0)
        -- prod (rotationY rCamera) $ translation (Vector3 0 0 (-8::Float))

    -- let pyramidMat = translateM (Vector3 (-3.5) 0 0) `prod` rotateYM rPyramid
    let MTransform pMat _ = translate (Vector3 (-3.5) 0 0) () >>= rotateY rPyramid

    bindBuffer gl ArrayBufferTarget pyramidVertsBuffer
    vertexAttribPointer gl posAttrib 3 FloatVAType False 0 0

    bindBuffer gl ArrayBufferTarget pyramidColorBuffer
    vertexAttribPointer gl colorAttrib 4 FloatVAType False 0 0

    setMatrixUniforms gl (pIdx, mvIdx) (projmat, prod viewmat pMat)
    drawArrays gl Triangles 0 (4*3)

    let MTransform cubeMat _ = translate (Vector3 1.5 0 0) () >>= rotate (unit ones) rCube

    bindBuffer gl ArrayBufferTarget cubeVertsBuffer
    vertexAttribPointer gl posAttrib 3 FloatVAType False 0 0

    bindBuffer gl ArrayBufferTarget cubeColorBuffer
    vertexAttribPointer gl colorAttrib 4 FloatVAType False 0 0

    bindBuffer gl ElementArrayBufferTarget cubeIndexBuffer

    setMatrixUniforms gl (pIdx, mvIdx) (projmat, prod viewmat cubeMat)
    drawElements gl Triangles 36 EltUnsignedShort 0

forever :: Int -> IO a -> IO ()
forever delay m = m >> setTimeout delay (Main.forever delay m)

main :: IO ()
main = setTimeout 100 $ do
    let root = documentBody
    canvas <- newElem "canvas"
    wwidth <- ffi "window.innerWidth" :: IO Int
    wheight <- ffi "window.innerHeight" :: IO Int
    setAttr canvas "width" $ show wwidth
    setAttr canvas "height" $ show wheight
    addChild canvas root

    gl <- getContext canvas "webgl"
    (pmUni, mvUni, posAttr, colAttr) <- initShaders gl
    buffers <- initBuffers gl

    enable gl DepthTest

    ns <- mallocArray 20 :: IO (Ptr Int)
    pokeElemOff ns 0 3
    pokeElemOff ns 1 2
    p <- peekElemOff ns 0
    -- ref <- newIORef (0, 0)::IO (IORef (Float, Float))

    Main.forever (floor (1000/60 :: Float)) $ do
        clearColor gl 0 0 0 0
        t <- ffi "(function() {return (new Date().getTime());})" :: IO Int
        drawScene gl (pmUni, mvUni) (t+p) buffers (posAttr, colAttr) (wwidth,wheight)

    return ()
