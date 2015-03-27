{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Haste.Graphics.WebGL.UniformsAttributes where

--import Haste.DOM
import Haste.Foreign
import Haste.Prim

--import Haste.JSArray.Typed
import Haste.Graphics.WebGL.ProgramsShaders
import Haste.Graphics.WebGL.Types

import qualified Foreign as F

import Geometry.Space


instance Unpack (F.Ptr Int)
instance Unpack (F.Ptr Float)
instance Unpack (F.Ptr (Vector2 Float))
instance Unpack (F.Ptr (Vector3 Float))
instance Unpack (F.Ptr (Vector4 Float))
instance Unpack (F.Ptr (Vector2 Int))
instance Unpack (F.Ptr (Vector3 Int))
instance Unpack (F.Ptr (Vector4 Int))
instance Unpack (F.Ptr (Matrix2x2 Float))
instance Unpack (F.Ptr (Matrix3x3 Float))
instance Unpack (F.Ptr (Matrix4x4 Float))
instance Unpack (F.Ptr (Matrix2x2 Int))
instance Unpack (F.Ptr (Matrix3x3 Int))
instance Unpack (F.Ptr (Matrix4x4 Int))


newtype AttribInfo = AttribInfo JSAny deriving (Pack, Unpack)
newtype AttribLocation = AttribLocation JSAny deriving (Pack, Unpack)

newtype UniformInfo = UniformInfo JSAny deriving (Pack, Unpack)
newtype UniformLocation = UniformLocation JSAny deriving (Pack, Unpack)



-- webgl doesn't support fixed
data VertexAttribType = BYTE | UNSIGNED_BYTE | SHORT | UNSIGNED_SHORT | INT | UNSIGNED_INT | FLOAT 

instance Enum VertexAttribType where
    fromEnum BYTE           = 0x1400
    fromEnum UNSIGNED_BYTE  = 0x1401
    fromEnum SHORT          = 0x1402
    fromEnum UNSIGNED_SHORT = 0x1403
    fromEnum INT            = 0x1404
    fromEnum UNSIGNED_INT   = 0x1405
    fromEnum FLOAT          = 0x1406
    
    
    toEnum 0x1400 = BYTE
    toEnum 0x1401 = UNSIGNED_BYTE
    toEnum 0x1402 = SHORT
    toEnum 0x1403 = UNSIGNED_SHORT
    toEnum 0x1404 = INT
    toEnum 0x1405 = UNSIGNED_INT
    toEnum 0x1406 = FLOAT
    toEnum _ = undefined

instance Pack VertexAttribType where
  pack = toEnum . pack

instance Unpack VertexAttribType where
  unpack = unpack . fromEnum

data VertexAttrPName = CurrentVertexAttrib | VertexAttribArrayBufferBinding |
                       VertexAttribArrayEnabled | VertexAttribArraySize |
                       VertexAttribArrayStride | VertexAttribArrayType |
                       VertexAttribArrayNormalized

instance Enum VertexAttrPName where
  fromEnum CurrentVertexAttrib = 0x8626
  fromEnum VertexAttribArrayBufferBinding = 0x889f
  fromEnum VertexAttribArrayEnabled = 0x8622
  fromEnum VertexAttribArraySize = 0x8623
  fromEnum VertexAttribArrayStride = 0x8624
  fromEnum VertexAttribArrayType = 0x8625
  fromEnum VertexAttribArrayNormalized = 0x886a

  toEnum 0x8626 = CurrentVertexAttrib
  toEnum 0x889f = VertexAttribArrayBufferBinding
  toEnum 0x8622 = VertexAttribArrayEnabled
  toEnum 0x8623 = VertexAttribArraySize
  toEnum 0x8624 = VertexAttribArrayStride
  toEnum 0x8625 = VertexAttribArrayType
  toEnum 0x886a = VertexAttribArrayNormalized
  toEnum _ = undefined

instance Pack VertexAttrPName where
  pack = toEnum . pack

instance Unpack VertexAttrPName where
  unpack = unpack . fromEnum

disableVertexAttribArray::Context->AttribLocation->IO ()
disableVertexAttribArray = ffi "(function(ctx, index) {ctx.disableVertexAttribArray(index);})"

enableVertexAttribArray::Context->AttribLocation->IO ()
enableVertexAttribArray = ffi "(function(ctx, index) {ctx.enableVertexAttribArray(index);})"

getActiveAttrib::Context->Program->AttribLocation->IO AttribInfo
getActiveAttrib = ffi "(function(ctx, program, index) {return ctx.getActiveAttrib(program, index);})"

getActiveUniform::Context->Program->UniformLocation->IO UniformInfo
getActiveUniform = ffi "(function(ctx, program, index) {return ctx.getActiveUniform(program, index);})"

getAttribLocation::Context->Program->String->IO AttribLocation
getAttribLocation = ffi "(function(ctx, program, name) {return ctx.getAttribLocation(program, name);})"

getUniform::Context->Program->UniformLocation->IO JSAny
getUniform = ffi "(function(ctx, program, index) {return ctx.getUniform(program, index);})"

getUniformLocation::Context->Program->String->IO UniformLocation
getUniformLocation = ffi "(function(ctx, program, name) {return ctx.getUniformLocation(program, name);})"

getVertexAttrib::Context->AttribLocation->VertexAttrPName->IO JSAny
getVertexAttrib = ffi "(function(ctx, index, pname) {return ctx.getVertexAttrib(index, pname);})"

getVertexAttribOffset::Context->AttribLocation->VertexAttrPName->IO Int
getVertexAttribOffset = ffi "(function(ctx, index, pname) {return ctx.getVertexAttribOffset(index, pname);})"

uniform1f::Context->UniformLocation->Double->IO ()
uniform1f = ffi "(function(ctx, uniform, val) {ctx.uniform1f(uniform, val);})"

uniform2f::Context->UniformLocation->Double->Double->IO ()
uniform2f = ffi "(function(ctx, uniform, x, y) {ctx.uniform2f(uniform, x, y);})"

uniform3f::Context->UniformLocation->Double->Double->Double->IO ()
uniform3f = ffi "(function(ctx, uniform, x, y, z) {ctx.uniform3f(uniform, x, y, z);})"

uniform4f::Context->UniformLocation->Double->Double->Double->Double->IO ()
uniform4f = ffi "(function(ctx, uniform, x, y, z, w) {ctx.uniform4f(uniform, x, y, z, w);})"

uniform1i::Context->UniformLocation->Int->IO ()
uniform1i = ffi "(function(ctx, uniform, val) {ctx.uniform1i(uniform, val);})"

uniform2i::Context->UniformLocation->Int->Int->IO ()
uniform2i = ffi "(function(ctx, uniform, x, y) {ctx.uniform2i(uniform, x, y);})"

uniform3i::Context->UniformLocation->Int->Int->Int->IO ()
uniform3i = ffi "(function(ctx, uniform, x, y, z) {ctx.uniform3i(uniform, x, y, z);})"

uniform4i::Context->UniformLocation->Int->Int->Int->Int->IO ()
uniform4i = ffi "(function(ctx, uniform, x, y, z, w) {ctx.uniform4i(uniform, x, y, z, w);})"

uniform1fv::Context->UniformLocation->F.Ptr Float->IO ()
uniform1fv = ffi "(function(ctx, uniform, arr) {ctx.uniform1fv(uniform, arr['v']['f32']);})"

uniform2fv::Context->UniformLocation->F.Ptr (Vector2 Float)->IO ()
uniform2fv = ffi "(function(ctx, uniform, arr) {ctx.uniform2fv(uniform, arr['v']['f32']);})"

uniform3fv::Context->UniformLocation->F.Ptr (Vector3 Float)->IO ()
uniform3fv = ffi "(function(ctx, uniform, arr) {ctx.uniform3fv(uniform, arr['v']['f32']);})"

uniform4fv::Context->UniformLocation->F.Ptr (Vector4 Float)->IO ()
uniform4fv = ffi "(function(ctx, uniform, arr) {ctx.uniform4fv(uniform, arr['v']['f32']);})"

uniform1iv::Context->UniformLocation->F.Ptr Int->IO ()
uniform1iv = ffi "(function(ctx, uniform, arr) {ctx.uniform1iv(uniform, arr['v']['i32']);})"

uniform2iv::Context->UniformLocation->F.Ptr (Vector2 Int)->IO ()
uniform2iv = ffi "(function(ctx, uniform, arr) {ctx.uniform2iv(uniform, arr['v']['i32']);})"

uniform3iv::Context->UniformLocation->F.Ptr (Vector3 Int)->IO ()
uniform3iv = ffi "(function(ctx, uniform, arr) {ctx.uniform3iv(uniform, arr['v']['i32']);})"

uniform4iv::Context->UniformLocation->F.Ptr (Vector4 Int)->IO ()
uniform4iv = ffi "(function(ctx, uniform, arr) {ctx.uniform4iv(uniform, arr['v']['i32']);})"

uniformMatrix2fv::Context->UniformLocation->F.Ptr (Matrix2x2 Float)->IO ()
uniformMatrix2fv = ffi "(function(ctx, uniform, arr) {ctx.uniformMatrix2fv(uniform, ctx.FALSE, arr['v']['f32']);})"

uniformMatrix3fv::Context->UniformLocation->F.Ptr (Matrix3x3 Float)->IO ()
uniformMatrix3fv = ffi "(function(ctx, uniform, arr) {ctx.uniformMatrix3fv(uniform, ctx.FALSE, arr['v']['f32']);})"

uniformMatrix4fv::Context->UniformLocation-> F.Ptr (Matrix4x4 Float)->IO ()
uniformMatrix4fv = ffi "(function(ctx, uniform, arr) {ctx.uniformMatrix4fv(uniform, ctx.FALSE, arr['v']['f32']);})"


vertexAttrib1f::Context->AttribLocation->Double->IO ()
vertexAttrib1f = ffi "(function(ctx, attrib, val) {ctx.vertexAttrib1f(attrib, val);})"

vertexAttrib2f::Context->AttribLocation->Double->Double->IO ()
vertexAttrib2f = ffi "(function(ctx, attrib, x, y) {ctx.vertexAttrib2f(attrib, x, y);})"

vertexAttrib3f::Context->AttribLocation->Double->Double->Double->IO ()
vertexAttrib3f = ffi "(function(ctx, attrib, x, y, z) {ctx.vertexAttrib3f(attrib, x, y, z);})"

vertexAttrib4f::Context->AttribLocation->Double->Double->Double->Double->IO ()
vertexAttrib4f = ffi "(function(ctx, attrib, x, y, z, w) {ctx.vertexAttrib4f(attrib, x, y, z, w);})"

vertexAttrib1fv::Context->AttribLocation-> F.Ptr Float ->IO ()
vertexAttrib1fv = ffi "(function(ctx, attrib, arr) {ctx.vertexAttrib1fv(attrib, arr['v']['f32']);})"

vertexAttrib2fv::Context->AttribLocation-> F.Ptr (Vector2 Float) ->IO ()
vertexAttrib2fv = ffi "(function(ctx, attrib, arr) {ctx.vertexAttrib2fv(attrib, arr['v']['f32']);})"

vertexAttrib3fv::Context->AttribLocation-> F.Ptr (Vector3 Float)->IO ()
vertexAttrib3fv = ffi "(function(ctx, attrib, arr) {ctx.vertexAttrib3fv(attrib, arr['v']['f32']);})"

vertexAttrib4fv::Context->AttribLocation->F.Ptr (Vector4 Float)->IO ()
vertexAttrib4fv = ffi "(function(ctx, attrib, arr) {ctx.vertexAttrib4fv(attrib, arr['v']['f32']);})"

vertexAttribPointer::Context->AttribLocation->Int->VertexAttribType->Bool->Int->Int->IO ()
vertexAttribPointer = ffi "(function(ctx, index, size, type, normalized, stride, offset) {ctx.vertexAttribPointer(index, size, type, normalized, stride, offset);})"
