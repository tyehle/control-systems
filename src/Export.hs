module Export where


-- in response to https://www.reddit.com/r/haskell/comments/3u5s4e/is_there_a_way_to_write_the_frames_of_a_gloss/

import Codec.Picture.Types (Image(..), PixelRGBA8)
import Codec.Picture.Png (writePng)
import Control.Monad (forM_)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
import Graphics.GL -- as gl*
import qualified Graphics.UI.GLFW as GLFW
import Foreign (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Text.Printf (printf)


windowWidth, windowHeight :: Num a => a
windowWidth = 10
windowHeight = 10

-- let GLFW bother with the OpenGL initialization
initOpenGL :: IO ()
initOpenGL = do
    True <- GLFW.init
    Just w <- GLFW.createWindow
                windowWidth windowHeight
                "gloss-to-file demo"
                Nothing Nothing
    GLFW.makeContextCurrent (Just w)

drawFrame :: Gloss.State -> Gloss.Picture -> IO ()
drawFrame s p = Gloss.withClearBuffer Gloss.black
            $ Gloss.withModelview (windowWidth, windowHeight)
            $ do
    glColor3f 1 1 1
    Gloss.renderPicture s 1 p

initialize :: IO Gloss.State
initialize = do
    s <- Gloss.initState
    initOpenGL
    return s

saveFrameImpl :: Gloss.State -> FilePath -> Gloss.Picture -> IO ()
saveFrameImpl s f p = do
    glDrawBuffer GL_BACK
    drawFrame s p
    glReadBuffer GL_BACK
    imageData <- mallocArray (windowWidth * windowHeight * 4)
    glReadPixels 0 0 windowWidth windowHeight GL_RGBA GL_UNSIGNED_BYTE imageData

    -- save the result
    foreignPtr <- newForeignPtr_ imageData
    let vector = unsafeFromForeignPtr0 foreignPtr (windowWidth * windowHeight * 4)
    let image :: Image PixelRGBA8
        image = Image windowWidth windowHeight vector
    writePng f (Image windowWidth windowHeight vector :: Image PixelRGBA8)

    free imageData


saveFrame :: FilePath -> Gloss.Picture -> IO ()
saveFrame f p = do
    s <- initialize
    saveFrameImpl s f p


type Animation = Float -> Gloss.Picture

-- FilePath must contain "%d", will be replaced by frame number
saveFrames :: FilePath -> Animation -> [Float] -> IO ()
saveFrames f anim ts = do
    s <- initialize
    forM_ (zip [1..] ts) $ \(n, t) -> do
      let filename = printf f (n :: Int)
      let picture = anim t
      saveFrameImpl s filename picture


exportExample :: IO ()
exportExample = do
    saveFrame "circle.png" (Gloss.circle 5)
    saveFrames "growing_circle%d.png" Gloss.circle [0,1..5]