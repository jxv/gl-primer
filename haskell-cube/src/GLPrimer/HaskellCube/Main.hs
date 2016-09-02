{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module GLPrimer.HaskellCube.Main
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Foreign as T
import qualified Data.Vector as V

import Control.Monad (void, when, unless, liftM2)
import Control.Applicative ((<$>), (<*>), pure)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Bits ((.|.))

import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

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
import Codec.Picture (readPng, Image(Image), DynamicImage(ImageRGBA8))

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
  err <- glGetError
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

        initialise >>= runDemo closed projectionMatrix swapper
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


initialise :: IO (Maybe Resources)
initialise = runMaybeT $ do
  png <- liftIO $ readPng "data/haskell.png"
  (Image texWidth texHeight texData) <- MaybeT $ case png of
    (Right (ImageRGBA8 i)) -> return $ Just i
    (Left s) -> liftIO (print s) >> return Nothing
    _ -> return Nothing

  textureID <- liftIO . alloca $ \texIDPtr -> do
    glGenTextures 1 texIDPtr
    peek texIDPtr

  let fillWith f = liftIO . alloca $ liftM2 (>>) f peek

  glBindTexture GL_TEXTURE_2D textureID
  let (w, h) = (fromIntegral texWidth, fromIntegral texHeight)

  liftIO . SV.unsafeWith texData $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE

  let loadAndCompileShader :: GLenum -> FilePath -> IO (Maybe GLuint)
      loadAndCompileShader shaderType filename = do

        shaderID <- glCreateShader shaderType

        shaderCode <- T.readFile filename
        T.withCStringLen shaderCode $
          \(str, len) -> with str $
            \strPtr -> with (fromIntegral len) $
              \lenPtr -> glShaderSource shaderID 1 strPtr lenPtr

        glCompileShader shaderID
        compileStatus <- fillWith $
          glGetShaderiv shaderID GL_COMPILE_STATUS

        when (compileStatus == GL_FALSE) $ do
          infoLogLength <- fillWith $
            glGetShaderiv shaderID GL_INFO_LOG_LENGTH
          let infoLogLength' = fromIntegral infoLogLength
          allocaBytes infoLogLength' $ \infoBuffer -> do
              glGetShaderInfoLog   shaderID infoLogLength
                                   nullPtr infoBuffer
              T.putStr =<<   T.peekCStringLen (infoBuffer, infoLogLength')

        return $ if compileStatus == GL_TRUE
          then Just shaderID
          else Nothing

  vs <- MaybeT $ loadAndCompileShader GL_VERTEX_SHADER "data/vertexShader.glsl"
  fs <- MaybeT $ loadAndCompileShader GL_FRAGMENT_SHADER "data/fragmentShader.glsl"

  programID <- glCreateProgram
  glAttachShader programID vs
  glAttachShader programID fs
  glLinkProgram  programID
  linkStatus <- fillWith $ glGetProgramiv programID GL_LINK_STATUS

  when (linkStatus == GL_FALSE) . MaybeT $ do
    infoLogLength <- fillWith $ glGetProgramiv programID GL_INFO_LOG_LENGTH

    let infoLogLength' = fromIntegral infoLogLength
    allocaBytes infoLogLength' $ \infoBuffer -> do
      glGetProgramInfoLog programID infoLogLength nullPtr infoBuffer

      T.putStr =<< T.peekCStringLen (infoBuffer, infoLogLength')

    return Nothing

  glDeleteShader vs
  glDeleteShader fs

  let unsign :: Integral a => GLint -> Maybe a
      unsign x
        | x < 0 = Nothing
        | otherwise = Just $ fromIntegral x

  let forString :: Integral a => (GLuint -> Ptr GLchar -> IO GLint) -> T.Text -> MaybeT (ContT r IO) a
      forString f x = do
        (str, _) <- lift $ ContT (T.withCStringLen $ T.concat [x, "\0"])
        loc <- liftIO $ f programID str
        MaybeT . return $ unsign loc

  glShader <- MaybeT . evalContT . runMaybeT $ Shader
    <$> pure programID
    <*> glGetAttribLocation `forString` "position"
    <*> glGetAttribLocation `forString` "colour"
    <*> glGetAttribLocation `forString` "normal"
    <*> glGetAttribLocation `forString` "uv"
    <*> glGetUniformLocation `forString` "pvmMatrix"
    <*> glGetUniformLocation `forString` "viewModelMatrix"
    <*> glGetUniformLocation `forString` "normalMatrix"
    <*> glGetUniformLocation `forString` "diffuseColour"
    <*> glGetUniformLocation `forString` "ambientColour"
    <*> glGetUniformLocation `forString` "specularColour"
    <*> glGetUniformLocation `forString` "shininess"
    <*> glGetUniformLocation `forString` "lightDirection"
    <*> glGetUniformLocation `forString` "diffuseMap"

  let cube = fromMeshSpec $ cuboid 1 1 1

  [vbo, ibo] <- liftIO . allocaArray 2 $ \buffers -> do
    glGenBuffers 2 buffers
    peekArray 2 buffers

  glBindBuffer GL_ARRAY_BUFFER vbo
  let vertices = _vertexData cube
  let vertexBufSize   = sizeOf (V.head vertices) * V.length vertices
  liftIO . SV.unsafeWith (SV.convert vertices) $ \vsPtr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral vertexBufSize) (castPtr vsPtr) GL_STATIC_DRAW

  glBindBuffer GL_ARRAY_BUFFER 0

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
  let indices = _indexData cube
  let indexBufSize   = sizeOf (V.head indices) * V.length indices
  liftIO . SV.unsafeWith (SV.convert indices) $ \isPtr ->
    glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral indexBufSize) (castPtr isPtr) GL_STATIC_DRAW
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0

  vao <- liftIO . alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr
  glBindVertexArray vao

  glBindBuffer GL_ARRAY_BUFFER vbo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo

  glEnableVertexAttribArray (_positions glShader)
  glEnableVertexAttribArray (_colors glShader)
  glEnableVertexAttribArray (_normals glShader)
  glEnableVertexAttribArray (_uvs glShader)

  let floatSize = sizeOf (undefined :: GLfloat)
  let offset x = wordPtrToPtr $ x * fromIntegral floatSize
  let stride = fromIntegral floatSize * 11

  glVertexAttribPointer (_positions glShader) 3 GL_FLOAT GL_FALSE stride (offset 0)
  glVertexAttribPointer (_colors glShader) 3 GL_FLOAT GL_FALSE stride (offset 3)
  glVertexAttribPointer (_normals glShader) 3 GL_FLOAT GL_FALSE stride (offset 6)
  glVertexAttribPointer (_uvs glShader) 2 GL_FLOAT GL_FALSE stride (offset 9)

  let glMesh = Mesh vbo ibo vao (fromIntegral $ V.length indices)

  liftIO initGL >> return (Resources glMesh textureID glShader)

initGL :: IO ()
initGL = do
  glClearColor 0.96 0.96 0.96 1
  glClearDepth 1
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glCullFace GL_BACK

resize :: IORef (M44 GLfloat) -> Int -> Int -> IO ()
resize projectionMatrix w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  writeIORef projectionMatrix $ calculateProjectionMatrix (w, h)

calculateProjectionMatrix :: Integral a => (a, a) -> M44 GLfloat
calculateProjectionMatrix (w, h) =
  perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100

defaultState :: DemoState
defaultState = DemoState
  { _cubeRotation     =   axisAngle (V3 0 1 0) 0
  , _cameraPosition   =   V3 0 1 (-2)
  }

update :: DemoState -> GLfloat -> DemoState
update s dt = s { _cubeRotation = cubeRotatedBy (rotationSpeed * dt) }
  where
    cubeRotatedBy θ = _cubeRotation s * axisAngle (V3 0 1 0) θ
    rotationSpeed = pi / 2

runDemo :: IORef Bool ->   IORef (M44 GLfloat) -> IO () -> Maybe Resources -> IO ()
runDemo _ _ _ Nothing = return ()
runDemo closed projectionMatrix swapBuffers (Just res) = do
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
      glFlush >> swapBuffers
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

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  glUseProgram . _shaderProgram $ _shader res
  glBindVertexArray . _meshVAO $ _mesh res
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ _texture res

  with pvmMat $ glUniformMatrix4fv (_pvmMatrix $ _shader res) 1 GL_TRUE . castPtr

  with viewModelMat $ glUniformMatrix4fv (_viewModelMatrix $ _shader res) 1 GL_TRUE . castPtr

  with normalMat $ glUniformMatrix3fv (_normalMatrix $ _shader res) 1 GL_TRUE . castPtr

  glUniform4f (_diffuseColour $ _shader res) 0.6 0.6 0.6 1
  glUniform4f (_ambientColour $ _shader res) 0.1 0.1 0.1 1
  glUniform4f (_specularColour $ _shader res) 0.7 0.7 0.7 1
  glUniform1f (_shininess $ _shader res) 0.4
  glUniform3f (_lightDirection $ _shader res) 0 0 1
  glUniform1i (_diffuseMap $ _shader res)0

  glDrawElements GL_TRIANGLES (_meshIndexCount $ _mesh res) GL_UNSIGNED_INT (wordPtrToPtr 0)

cleanup :: Resources -> IO ()
cleanup (Resources m t s) = do
  with (_meshVAO m) (glDeleteVertexArrays 1)
  with (_meshVBO m) (glDeleteBuffers 1)
  with (_meshIBO m) (glDeleteBuffers 1)
  with t (glDeleteTextures 1)
  glDeleteProgram (_shaderProgram s)
