{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Shapes.Drawable where

import Foreign
import Haste.Graphics.WebGL
import Geometry.Space
import Control.Applicative

data WorldContext = World {
    wgl       :: Context,
    wcamera   :: Camera,
    wtime     :: Int,
    wAttrLocs :: (AttribLocation, AttribLocation)
    }

-- | Camera transform
type Camera = MTransform Float (UniformLocation, Ptr (Matrix4x4 Float))

-- | Create a camera transform
initCamera :: Float
           -> UniformLocation
           -> IO Camera
initCamera aratio matloc = do
    let projmat = perspective 0.1 100 (pi/4) aratio
    mptr <- malloc :: IO (Ptr (Matrix4x4 Float))
    return $ MTransform projmat (matloc, mptr)


-- | Apply current transform of an object (including perspective) and save shader uniforms
applyTransform :: (SpaceTransform s, TransformInterop MTransform s, Monad (s Float), Applicative (s Float))
               => WorldContext -> s Float a -> IO a
applyTransform World {wcamera = (MTransform projmat (matloc, mptr)), wgl = gl} tr = do
        let MTransform matrix x = transform (MTransform projmat id) tr
        poke mptr matrix
        uniformMatrix4fv gl matloc mptr
        return x

-- | Most primitive triangle mesh
data SimpleTriangleMesh
    = STIdxMesh Int (Ptr Word8) (Ptr Word16) Buffer Buffer (AttribLocation, AttribLocation)
    | STMesh Int (Ptr Word8) Buffer (AttribLocation, AttribLocation)


-- | create a simple mesh for indexed drawing
createSTIdxMesh :: WorldContext
                -> [Vector3 Float] -- ^ points of the mesh
                -> [Vector3 Word8] -- ^ colors of the mesh
                -> [Word16] -- ^ indices of triangles in the mesh
                -> IO SimpleTriangleMesh
createSTIdxMesh World{wgl = gl, wAttrLocs = locs} points colors indices = do
    (_,ptr,buf) <- createPackedBuf gl points colors
    iptr <- newArray indices
    ibuf <- createBuffer gl
    bindBuffer gl ElementArrayBufferTarget ibuf
    bufferData gl ElementArrayBufferTarget iptr StaticDraw
    return $ STIdxMesh (length indices) ptr iptr buf ibuf locs

-- | create a simple mesh for array drawing
createSTMesh :: WorldContext
                -> [Vector3 Float] -- ^ points of the mesh
                -> [Vector3 Word8] -- ^ colors of the mesh
                -> IO SimpleTriangleMesh
createSTMesh World{wgl = gl, wAttrLocs = locs} points colors = do
    (size,ptr,buf) <- createPackedBuf gl points colors
    return $ STMesh size ptr buf locs




-- | render triangle mesh on screen
showSTMesh :: WorldContext -> SimpleTriangleMesh -> IO ()
showSTMesh World{wgl = gl} (STIdxMesh size _ _ buf ibuf ulocs) = do
    setupPackedBuf gl buf ulocs
    bindBuffer gl ElementArrayBufferTarget ibuf
    drawElements gl Triangles size EltUnsignedShort 0
showSTMesh World{wgl = gl} (STMesh size _ buf ulocs) = do
    setupPackedBuf gl buf ulocs
    drawArrays gl Triangles 0 size

-- | setup vertex attribute pointers according to our packing scheme (in packVertColors)
setupPackedBuf :: Context -> Buffer -> (AttribLocation, AttribLocation) -> IO ()
setupPackedBuf gl buf (vloc, cloc) =  do
    bindBuffer gl ArrayBufferTarget buf
    vertexAttribPointer gl vloc 3 FLOAT False 16 0
    vertexAttribPointer gl cloc 4 UNSIGNED_BYTE True 16 12 

-- | pack points and colors tightly in one buffer
packVertColors :: [Vector3 Float] -> [Vector3 Word8] -> Ptr Word8 -> IO ()
packVertColors _ [] _ = return ()
packVertColors [] _ _ = return ()
packVertColors (Vector3 x y z : pnts) (Vector3 r g b : cls) p = do
            pokeByteOff p 0 x
            pokeByteOff p 4 y
            pokeByteOff p 8 z
            pokeByteOff p 12 r
            pokeByteOff p 13 g
            pokeByteOff p 14 b
            pokeByteOff p 15 (255 :: Word8)
            packVertColors pnts cls (plusPtr p 16)
            
-- | create an array and a buffer, and fill them using point and color lists
createPackedBuf :: Context -> [Vector3 Float] -> [Vector3 Word8] -> IO (Int, Ptr Word8, Buffer)
createPackedBuf gl points colors = do
    let size = length points
    ptr <- mallocArray (size * 16) :: IO (Ptr Word8)
    packVertColors points colors ptr
    buf <- createBuffer gl
    bindBuffer gl ArrayBufferTarget buf
    bufferData gl ArrayBufferTarget ptr StaticDraw
    return (size,ptr,buf)

-- | our meshes together with transforms could be drawn - so they implement this interface
class Drawable a where
    draw :: WorldContext -> a -> IO ()

instance Drawable SimpleTriangleMesh where
    draw w m = applyTransform w (MTransform eye m) >>= showSTMesh w

instance ( SpaceTransform s
         , TransformInterop MTransform s
         , Monad (s Float)
         , Applicative (s Float)
         ) => Drawable (s Float SimpleTriangleMesh) where
    draw w s = applyTransform w s >>= showSTMesh w
