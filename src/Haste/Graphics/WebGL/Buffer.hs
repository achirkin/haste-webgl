{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Haste.Graphics.WebGL.Buffer where

import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

import qualified Foreign as F

instance Unpack (F.Ptr F.Word8)

data BufferTarget = ArrayBufferTarget | ElementArrayBufferTarget

instance Enum BufferTarget where
  fromEnum ArrayBufferTarget = 0x8892
  fromEnum ElementArrayBufferTarget = 0x8893

  toEnum 0x8892 = ArrayBufferTarget
  toEnum 0x8893 = ElementArrayBufferTarget
  toEnum _ = undefined

instance Pack BufferTarget where
  pack = toEnum . pack

instance Unpack BufferTarget where
  unpack = unpack . fromEnum

newtype Buffer = Buffer JSAny deriving (Pack, Unpack)

data BufferUsage = StaticDraw | StreamDraw | DynamicDraw

instance Enum BufferUsage where
  fromEnum StaticDraw = 0x88e4
  fromEnum StreamDraw = 0x88e0
  fromEnum DynamicDraw = 0x88e8

  toEnum 0x88e4 = StaticDraw
  toEnum 0x88e0 = StreamDraw
  toEnum 0x88e8 = DynamicDraw
  toEnum _ = undefined

instance Pack BufferUsage where
  pack = toEnum . pack

instance Unpack BufferUsage where
  unpack = unpack . fromEnum

data BufferPName = BufferSize | BufferUsage

instance Enum BufferPName where
  fromEnum BufferSize = 0x8764
  fromEnum BufferUsage = 0x8765

  toEnum 0x8764 = BufferSize
  toEnum 0x8765 = BufferUsage
  toEnum _ = undefined

instance Pack BufferPName where
  pack = toEnum . pack

instance Unpack BufferPName where
  unpack = unpack . fromEnum

bindBuffer::Context->BufferTarget->Buffer->IO ()
bindBuffer = ffi "(function(ctx, target, buffer) {ctx.bindBuffer(target, buffer);})"

bufferDataSized::Context->BufferTarget->Int->BufferUsage->IO ()
bufferDataSized = ffi "(function(ctx, target, size, usage) {ctx.bufferData(target, size, usage);})"

-- really I think we can pass in a plain ArrayBuffer as well, but...
bufferData::Context->BufferTarget->F.Ptr a->BufferUsage->IO ()
bufferData ctx bt ptr =
    ffi "(function(ctx, target, data, usage) {ctx.bufferData(target, data['b'], usage);})" ctx bt ptr'
        where ptr' = F.castPtr ptr :: F.Ptr F.Word8

bufferData'::Context->BufferTarget->BufferUsage->F.Ptr a->IO ()
bufferData' ctx targ usage dat = bufferData ctx targ dat usage

bufferSubData::Context->BufferTarget->Int->F.Ptr a->IO ()
bufferSubData ctx bt i ptr =
    ffi "(function(ctx, target, offset, data) {ctx.bufferSubData(target, offset, data['b']);})" ctx bt i ptr'
        where ptr' = F.castPtr ptr :: F.Ptr F.Word8

createBuffer::Context->IO Buffer
createBuffer = ffi "(function(ctx) {return ctx.createBuffer();})"

deleteBuffer::Context->Buffer->IO ()
deleteBuffer = ffi "(function(ctx, buffer) {return ctx.deleteBuffer(buffer);})"

getBufferParameter::Context->BufferTarget->BufferPName->IO Int
getBufferParameter = ffi "(function(ctx, target, pname) {return ctx.getBufferParameter(target, pname);})"

isBuffer::Context->Buffer->IO Bool
isBuffer = ffi "(function(ctx, buffer) {return ctx.isBuffer(buffer);})"
