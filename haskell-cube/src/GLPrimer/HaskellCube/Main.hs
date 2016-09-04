{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module GLPrimer.HaskellCube.Main
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Foreign as T
import qualified Data.Vector as V

import Control.Monad (void, when, unless, liftM2, (<=<))
import Control.Applicative ((<$>), (<*>), pure)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Bits ((.|.))

import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Data.Vector.Storable as SV
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr (Ptr, nullPtr, castPtr, wordPtrToPtr)

import qualified Graphics.UI.GLFW as GLFW
  ( WindowHint(..)
  , ClientAPI(..)
  , OpenGLProfile(..)
  , windowHint
  , init
  , createWindow
  , makeContextCurrent
  , setWindowCloseCallback
  , getWindowSize
  , setWindowSizeCallback
  , setWindowCloseCallback
  , swapBuffers
  , pollEvents
  , terminate
  , getTime
  , setTime
  )
import Graphics.GL
  ( GLfloat
  , GLint
  , GLuint
  , GLsizei
  , GLenum
  , GLchar
  , GLboolean
  , GLbitfield
  , glActiveTexture
  , glAttachShader
  , glLinkProgram
  , glBindBuffer
  , glBindTexture
  , glBindVertexArray
  , glBufferData
  , glClear
  , glClearColor
  , glClearDepth
  , glCompileShader
  , glCreateProgram
  , glCreateShader
  , glCullFace
  , glEnable
  , glEnableVertexAttribArray
  , glDepthFunc
  , glDeleteProgram
  , glDeleteTextures
  , glDeleteShader
  , glDeleteBuffers
  , glDeleteVertexArrays
  , glDrawElements
  , glFlush
  , glGetError
  , glGenBuffers
  , glGenTextures
  , glGetShaderiv
  , glGetProgramiv
  , glGenVertexArrays
  , glGetShaderInfoLog
  , glGetProgramInfoLog
  , glGetAttribLocation
  , glGetUniformLocation
  , glShaderSource
  , glTexImage2D
  , glTexParameteri
  , glUniform1i
  , glUniform1f
  , glUniform3f
  , glUniform4f
  , glUniformMatrix3fv
  , glUniformMatrix4fv
  , glUseProgram
  , glVertexAttribPointer
  , glViewport
  , pattern GL_ARRAY_BUFFER
  , pattern GL_ELEMENT_ARRAY_BUFFER
  , pattern GL_COLOR_BUFFER_BIT
  , pattern GL_DEPTH_BUFFER_BIT
  , pattern GL_NO_ERROR
  , pattern GL_INVALID_ENUM
  , pattern GL_INVALID_VALUE
  , pattern GL_INVALID_OPERATION
  , pattern GL_INFO_LOG_LENGTH
  , pattern GL_INVALID_FRAMEBUFFER_OPERATION
  , pattern GL_UNSIGNED_INT
  , pattern GL_LINEAR
  , pattern GL_TRIANGLES
  , pattern GL_TRUE
  , pattern GL_FALSE
  , pattern GL_TEXTURE0
  , pattern GL_TEXTURE_2D
  , pattern GL_FLOAT
  , pattern GL_STATIC_DRAW
  , pattern GL_LINK_STATUS
  , pattern GL_DEPTH_TEST
  , pattern GL_LEQUAL
  , pattern GL_BACK
  , pattern GL_OUT_OF_MEMORY
  , pattern GL_STACK_UNDERFLOW
  , pattern GL_STACK_OVERFLOW
  , pattern GL_RGBA
  , pattern GL_UNSIGNED_BYTE
  , pattern GL_FRAGMENT_SHADER
  , pattern GL_VERTEX_SHADER
  , pattern GL_COMPILE_STATUS
  , pattern GL_CLAMP_TO_EDGE
  , pattern GL_TEXTURE_WRAP_S
  , pattern GL_TEXTURE_WRAP_T
  , pattern GL_TEXTURE_MAG_FILTER
  , pattern GL_TEXTURE_MIN_FILTER
  )
import Linear
  ( V2(..)
  , V3(..)
  , M44
  , M33
  , Quaternion
  , perspective
  , lookAt
  , axisAngle
  , mkTransformation
  , (!*!)
  , inv33
  , column
  , _xyz
  , negated
  , identity
  )
import Data.Distributive (distribute)
import Control.Lens (view)
import Codec.Picture (readPng, Image(Image), DynamicImage(ImageRGBA8), PixelRGBA8)

data Mesh = Mesh
  { _meshVBO :: GLuint
  , _meshIBO :: GLuint
  , _meshVAO :: GLuint
  , _meshIndexCount :: GLsizei
  }

data Shader = Shader
  { _shaderProgram :: GLuint
  , _positions :: GLuint
  , _colors :: GLuint
  , _normals :: GLuint
  , _uvs :: GLuint
  , _pvmMatrix :: GLint
  , _viewModelMatrix :: GLint
  , _normalMatrix :: GLint
  , _diffuseColour :: GLint
  , _ambientColour :: GLint
  , _specularColour :: GLint
  , _shininess :: GLint
  , _lightDirection :: GLint
  , _diffuseMap :: GLint
  }

type TextureID = GLuint

data Resources = Resources
  { _mesh :: Mesh
  , _texture :: TextureID
  , _shader :: Shader
  }

data MeshSpec = MeshSpec
  { _specPositions :: V.Vector (V3 GLfloat)
  , _speccolors :: V.Vector (V3 GLfloat)
  , _specNormals :: V.Vector (V3 GLfloat)
  , _specUVs :: V.Vector (V2 GLfloat)
  , _specIndices :: V.Vector (GLuint, GLuint, GLuint)
  }

data MeshData = MeshData
  { _vertexData :: V.Vector GLfloat
  , _indexData :: V.Vector GLuint
  }

data DemoState = DemoState
  { _cubeRotation :: Quaternion GLfloat
  , _cameraPosition :: V3 GLfloat
  }

getErrors :: IO [GLuint]
getErrors = do
  err <- getError
  if err == GL_NO_ERROR
     then return []
     else do
       errs <- getErrors
       return $ err : errs

showError :: GLuint -> String
showError GL_INVALID_ENUM = "GL_INVALID_ENUM"
showError GL_INVALID_VALUE = "GL_INVALID_VALUE"
showError GL_INVALID_OPERATION = "GL_INVALID_OPERATION"
showError GL_INVALID_FRAMEBUFFER_OPERATION = "GL_INVALID_FRAMEBUFFER_OPERATION"
showError GL_OUT_OF_MEMORY = "GL_OUT_OF_MEMORY"
showError GL_STACK_UNDERFLOW = "GL_STACK_UNDERFLOW"
showError GL_STACK_OVERFLOW = "GL_STACK_OVERFLOW"
showError x = "GL Error " ++ show x

printErrors :: String -> IO ()
printErrors prefix = do
  es <- map showError <$> getErrors
  when (not $ null es) $ error (prefix ++ ": " ++ show es)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  success <- GLFW.init
  if not success
  then void $ putStrLn "Failed to initialise GLFW"
  else do

    let windowHints :: [GLFW.WindowHint]
        windowHints =
          [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
          , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
          , GLFW.WindowHint'OpenGLForwardCompat True
          , GLFW.WindowHint'ContextVersionMajor 3
          , GLFW.WindowHint'ContextVersionMinor 2
          ]

    mapM_ GLFW.windowHint windowHints

    w <- GLFW.createWindow 480 320 "Haskell GL" Nothing Nothing

    case w of
      Nothing -> putStrLn "Failed to create window"
      Just win -> do
        GLFW.makeContextCurrent w

        closed <- newIORef False
        GLFW.setWindowCloseCallback win $ Just (const $ writeIORef closed True)

        dims <- GLFW.getWindowSize win
        projectionMatrix <- newIORef $ calculateProjectionMatrix dims

        GLFW.setWindowSizeCallback win $ Just (const $ resize projectionMatrix)

        let swapper = GLFW.swapBuffers win >> GLFW.pollEvents

        maybeResources <- initialise
        case maybeResources of
          Just resources -> runDemo closed projectionMatrix swapper resources
          Nothing -> return ()
        GLFW.terminate

getDeltaTime :: IO GLfloat
getDeltaTime = do
  t <- GLFW.getTime
  GLFW.setTime 0
  return $ maybe 0 (fromRational . toRational) t

v3 :: (a, a, a) -> V3 a
v3 (x, y, z) = V3 x y z

cuboid :: GLfloat -> GLfloat -> GLfloat -> MeshSpec
cuboid l' h' d' = MeshSpec positions colors normals uvs indices
  where
    l, d, h :: GLfloat
    l = l' * 0.5
    d = d' * 0.5
    h = h' * 0.5

    positions =
      [ v3 ( l, h, d), v3 ( l,-h, d), v3 ( l,-h,-d), v3 ( l, h,-d)
      , v3 ( l, h,-d), v3 (-l, h,-d), v3 (-l, h, d), v3 ( l, h, d)
      , v3 (-l, h, d), v3 (-l,-h, d), v3 ( l,-h, d), v3 ( l, h, d)
      , v3 (-l, h,-d), v3 (-l,-h,-d), v3 (-l,-h, d), v3 (-l, h, d)
      , v3 ( l,-h,-d), v3 ( l,-h, d), v3 (-l,-h, d), v3 (-l,-h,-d)
      , v3 ( l, h,-d), v3 ( l,-h,-d), v3 (-l,-h,-d), v3 (-l, h,-d)
      ]

    colors = V.map ((/ V3 l' h' d') . (+ V3 l h d)) positions

    normals = V.concat . map (V.replicate 4) $ ns ++ negated ns
      where ns = [V3 1 0 0, V3 0 1 0, V3 0 0 1]

    uvs = V.concat . replicate 6 $ [V2 0 0, V2 0 1, V2 1 1, V2 1 0]

    indices = quads . V.zipWith forFace [0..] . V.replicate 6 $ (0, 1, 2, 3)
      where
        forFace i (a, b, c, d) = (a + i * 4, b + i * 4, c + i * 4, d + i * 4)
        quads = V.concatMap triangulate
        triangulate :: (GLuint, GLuint, GLuint, GLuint) -> V.Vector (GLuint, GLuint, GLuint)
        triangulate (a, b, c, d)   =   [(a, b, c), (c, d, a)]

unpackIndices :: V.Vector (GLuint, GLuint, GLuint) -> V.Vector GLuint
unpackIndices = V.concatMap unpack
  where
    unpack (a, b, c) = [a, b, c]

interleave :: V.Vector (V3 GLfloat) -> V.Vector (V3 GLfloat) -> V.Vector (V3 GLfloat) -> V.Vector (V2 GLfloat) -> V.Vector GLfloat
interleave positions colors normals uvs = V.foldr (V.++) V.empty $ V.zipWith4 combine positions colors normals uvs
  where
    combine (V3 x y z) (V3 r g b) (V3 nx ny nz) (V2 u v) = [x, y, z, r, g, b, nx, ny, nz, u, v]

fromMeshSpec :: MeshSpec -> MeshData
fromMeshSpec spec = MeshData
  { _vertexData = interleave (_specPositions spec) (_speccolors spec) (_specNormals spec) (_specUVs spec)
  , _indexData = unpackIndices (_specIndices spec)
  }

readPngAsImageMay :: MonadIO m => FilePath -> m (Maybe (Image PixelRGBA8))
readPngAsImageMay path = do
  png <- liftIO (readPng path)
  case png of
    (Right (ImageRGBA8 i)) -> return $ Just i
    (Left s) -> liftIO (print s) >> return Nothing
    _ -> return Nothing

fillWith :: (MonadIO m, SV.Storable a) => (Ptr a -> IO b) -> m a
fillWith f = liftIO . alloca $ liftM2 (>>) f peek

peekAlloc :: (MonadIO m, SV.Storable a) => (Ptr a -> IO ()) -> m a
peekAlloc f = liftIO . alloca $ \ptr -> do
    f ptr
    peek ptr

initialise :: IO (Maybe Resources)
initialise = runMaybeT $ do
  (Image texWidth texHeight texData) <- MaybeT $ readPngAsImageMay "data/haskell.png"

  textureID <- peekAlloc (glGenTextures 1)

  bindTexture GL_TEXTURE_2D textureID
  let (w, h) = (fromIntegral texWidth, fromIntegral texHeight)

  liftIO . SV.unsafeWith texData $ texImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr

  texParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  texParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  texParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  texParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE

  let loadAndCompileShader :: GLenum -> FilePath -> IO (Maybe GLuint)
      loadAndCompileShader shaderType filename = do
        shaderID <- createShader shaderType

        shaderCode <- T.readFile filename
        T.withCStringLen shaderCode $
          \(str, len) -> with str $
            \strPtr -> with (fromIntegral len) $
              \lenPtr -> shaderSource shaderID 1 strPtr lenPtr

        compileShader shaderID
        compileStatus <- getShaderiv shaderID GL_COMPILE_STATUS

        when (compileStatus == GL_FALSE) $ do
          infoLogLength <- getShaderiv shaderID GL_INFO_LOG_LENGTH
          let infoLogLength' = fromIntegral infoLogLength
          getShaderInfoLog shaderID infoLogLength nullPtr (T.putStr <=< T.peekCStringLen)

        return $ if compileStatus == GL_TRUE
          then Just shaderID
          else Nothing

  vs <- MaybeT $ loadAndCompileShader GL_VERTEX_SHADER "data/vertexShader.glsl"
  fs <- MaybeT $ loadAndCompileShader GL_FRAGMENT_SHADER "data/fragmentShader.glsl"

  programID <- createProgram
  attachShader programID vs
  attachShader programID fs
  linkProgram  programID
  linkStatus <- getProgramiv programID GL_LINK_STATUS

  when (linkStatus == GL_FALSE) . MaybeT $ do
    infoLogLength <- getProgramiv programID GL_INFO_LOG_LENGTH
    getProgramInfoLog programID infoLogLength nullPtr (T.putStr <=< T.peekCStringLen)

    return Nothing

  deleteShader vs
  deleteShader fs

  let unsign :: Integral a => GLint -> Maybe a
      unsign x
        | x < 0 = Nothing
        | otherwise = Just $ fromIntegral x

  let forString :: Integral a => (GLuint -> Ptr GLchar -> IO GLint) -> T.Text -> MaybeT (ContT r IO) a
      forString f x = do
        (str, _) <- lift $ ContT (T.withCStringLen $ T.concat [x, "\0"])
        loc <- liftIO $ f programID str
        MaybeT . return $ unsign loc

  shader <- MaybeT . evalContT . runMaybeT $ Shader
    <$> pure programID
    <*> getAttribLocation `forString` "position"
    <*> getAttribLocation `forString` "colour"
    <*> getAttribLocation `forString` "normal"
    <*> getAttribLocation `forString` "uv"
    <*> getUniformLocation `forString` "pvmMatrix"
    <*> getUniformLocation `forString` "viewModelMatrix"
    <*> getUniformLocation `forString` "normalMatrix"
    <*> getUniformLocation `forString` "diffuseColour"
    <*> getUniformLocation `forString` "ambientColour"
    <*> getUniformLocation `forString` "specularColour"
    <*> getUniformLocation `forString` "shininess"
    <*> getUniformLocation `forString` "lightDirection"
    <*> getUniformLocation `forString` "diffuseMap"

  let cube = fromMeshSpec $ cuboid 1 1 1

  [vbo, ibo] <- liftIO . allocaArray 2 $ \buffers -> do
    glGenBuffers 2 buffers
    peekArray 2 buffers

  bindBuffer GL_ARRAY_BUFFER vbo
  let vertices = _vertexData cube
  let vertexBufSize   = sizeOf (V.head vertices) * V.length vertices
  liftIO . SV.unsafeWith (SV.convert vertices) $ \vsPtr ->
    bufferData GL_ARRAY_BUFFER (fromIntegral vertexBufSize) (castPtr vsPtr) GL_STATIC_DRAW

  bindBuffer GL_ARRAY_BUFFER 0

  bindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
  let indices = _indexData cube
  let indexBufSize   = sizeOf (V.head indices) * V.length indices
  liftIO . SV.unsafeWith (SV.convert indices) $ \isPtr ->
    bufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral indexBufSize) (castPtr isPtr) GL_STATIC_DRAW
  bindBuffer GL_ELEMENT_ARRAY_BUFFER 0

  vao <- peekAlloc (glGenVertexArrays 1)
  bindVertexArray vao

  bindBuffer GL_ARRAY_BUFFER vbo
  bindBuffer GL_ELEMENT_ARRAY_BUFFER ibo

  enableVertexAttribArray (_positions shader)
  enableVertexAttribArray (_colors shader)
  enableVertexAttribArray (_normals shader)
  enableVertexAttribArray (_uvs shader)

  let floatSize = sizeOf (undefined :: GLfloat)
  let offset x = wordPtrToPtr $ x * fromIntegral floatSize
  let stride = fromIntegral floatSize * 11

  vertexAttribPointer (_positions shader) 3 GL_FLOAT GL_FALSE stride (offset 0)
  vertexAttribPointer (_colors shader) 3 GL_FLOAT GL_FALSE stride (offset 3)
  vertexAttribPointer (_normals shader) 3 GL_FLOAT GL_FALSE stride (offset 6)
  vertexAttribPointer (_uvs shader) 2 GL_FLOAT GL_FALSE stride (offset 9)

  let mesh = Mesh vbo ibo vao (fromIntegral $ V.length indices)

  liftIO initGL
  return Resources
    { _mesh = mesh
    , _texture = textureID
    , _shader = shader
    }

initGL :: IO ()
initGL = do
  clearColor 0.96 0.96 0.96 1
  clearDepth 1
  enable GL_DEPTH_TEST
  depthFunc GL_LEQUAL
  cullFace GL_BACK

resize :: IORef (M44 GLfloat) -> Int -> Int -> IO ()
resize projectionMatrix w h = do
  viewport 0 0 (fromIntegral w) (fromIntegral h)
  writeIORef projectionMatrix $ calculateProjectionMatrix (w, h)

calculateProjectionMatrix :: Integral a => (a, a) -> M44 GLfloat
calculateProjectionMatrix (w, h) =
  perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100

defaultState :: DemoState
defaultState = DemoState
  { _cubeRotation = axisAngle (V3 0 1 0) 0
  , _cameraPosition = V3 0 1 (-2)
  }

update :: DemoState -> GLfloat -> DemoState
update s dt = s { _cubeRotation = cubeRotatedBy (rotationSpeed * dt) }
  where
    cubeRotatedBy θ = _cubeRotation s * axisAngle (V3 0 1 0) θ
    rotationSpeed = pi / 2

runDemo :: IORef Bool ->   IORef (M44 GLfloat) -> IO () -> Resources -> IO ()
runDemo closed projectionMatrix swapBuffers res = do
  loop defaultState
  cleanup res
  where
    loop :: DemoState -> IO ()
    loop s = do
      c <- readIORef closed
      unless c (runFrame s)

    runFrame :: DemoState -> IO ()
    runFrame s = do
      draw res s =<< readIORef projectionMatrix
      flush >> swapBuffers
      dt <- getDeltaTime
      loop $ update s dt

draw :: Resources -> DemoState -> M44 GLfloat -> IO ()
draw res state projectionMatrix = do
  let viewMat = lookAt (_cameraPosition state) (V3 0 0 0) (V3 0 1 0)
  let modelMat = mkTransformation (_cubeRotation state) (V3 0 0 0)
  let viewModelMat = viewMat !*! modelMat
  let pvmMat = projectionMatrix !*! viewModelMat
  let viewModelMat33 = view (_xyz . column _xyz) viewModelMat
  let inverseMat = Just $ inv33 viewModelMat33
  let normalMat = maybe identity distribute inverseMat

  clear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  useProgram (_shaderProgram (_shader res))
  bindVertexArray (_meshVAO (_mesh res))
  activeTexture GL_TEXTURE0
  bindTexture GL_TEXTURE_2D (_texture res)

  uniformMatrix4fv pvmMat (_pvmMatrix (_shader res)) 1 GL_TRUE
  uniformMatrix4fv viewModelMat (_viewModelMatrix (_shader res)) 1 GL_TRUE
  uniformMatrix3fv normalMat (_normalMatrix (_shader res)) 1 GL_TRUE

  uniform4f (_diffuseColour $ _shader res) 0.6 0.6 0.6 1
  uniform4f (_ambientColour $ _shader res) 0.1 0.1 0.1 1
  uniform4f (_specularColour $ _shader res) 0.7 0.7 0.7 1
  uniform1f (_shininess $ _shader res) 0.4
  uniform3f (_lightDirection $ _shader res) 0 0 1
  uniform1i (_diffuseMap $ _shader res) 0

  drawElements GL_TRIANGLES (_meshIndexCount $ _mesh res) GL_UNSIGNED_INT (wordPtrToPtr 0)

cleanup :: Resources -> IO ()
cleanup (Resources m t s) = do
  deleteVertexArrays 1 (_meshVAO m)
  deleteBuffers 1 (_meshVBO m)
  deleteBuffers 1 (_meshIBO m)
  deleteTextures 1 t
  deleteProgram (_shaderProgram s)

-- GL wrappers

clear :: GLbitfield -> IO ()
clear = glClear

useProgram :: GLuint -> IO ()
useProgram = glUseProgram

bindVertexArray :: MonadIO m => GLuint -> m ()
bindVertexArray = glBindVertexArray

activeTexture :: GLenum -> IO ()
activeTexture = glActiveTexture

uniformMatrix4fv :: M44 GLfloat -> GLint -> GLsizei -> GLboolean -> IO ()
uniformMatrix4fv mat matLoc count transpose = with mat $ glUniformMatrix4fv matLoc count transpose . castPtr

uniformMatrix3fv :: M33 GLfloat -> GLint -> GLsizei -> GLboolean -> IO ()
uniformMatrix3fv mat matLoc count transpose = with mat $ glUniformMatrix4fv matLoc count transpose . castPtr

getError = glGetError

bindTexture :: MonadIO m => GLenum -> GLuint -> m ()
bindTexture = glBindTexture

createProgram = glCreateProgram

attachShader = glAttachShader

linkProgram = glLinkProgram

getProgramiv programId params = fillWith (glGetProgramiv programId params)

uniform1i = glUniform1i

uniform1f = glUniform1f

uniform3f = glUniform3f

uniform4f = glUniform4f

drawElements = glDrawElements

deleteVertexArrays count val = with val (glDeleteVertexArrays count)

deleteBuffers count val = with val (glDeleteBuffers count)

deleteTextures count val =  with val (glDeleteTextures count)

deleteProgram = glDeleteProgram

texImage2D = glTexImage2D

texParameteri = glTexParameteri

createShader = glCreateShader

shaderSource = glShaderSource

compileShader = glCompileShader

getShaderiv shaderId compileStatus = fillWith $ glGetShaderiv shaderId compileStatus

getShaderInfoLog shaderId logLength ptr f = allocaBytes logLength' $ \buffer -> do
  glGetShaderInfoLog shaderId logLength ptr buffer
  f (buffer, logLength')
  where
    logLength' = fromIntegral logLength

getProgramInfoLog programId logLength ptr f = allocaBytes logLength' $ \buffer -> do
  glGetProgramInfoLog programId logLength ptr buffer
  f (buffer, logLength')
  where
    logLength' = fromIntegral logLength

deleteShader = glDeleteShader

getAttribLocation = glGetAttribLocation

getUniformLocation = glGetUniformLocation

bindBuffer = glBindBuffer

bufferData = glBufferData

enableVertexAttribArray = glEnableVertexAttribArray

vertexAttribPointer = glVertexAttribPointer

clearColor = glClearColor

clearDepth = glClearDepth

enable = glEnable

depthFunc = glDepthFunc

cullFace = glCullFace

flush = glFlush

viewport = glViewport
