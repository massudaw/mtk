import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Renderable
import Graphics.UI.Gtk.Misc.DrawingArea
import qualified Graphics.UI.Gtk as G
import Pipes
import Pipes.Concurrent
import Pipes.Tutorial as P
import Control.Monad
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import Data.List (isPrefixOf)
import Data.IORef
import Control.Monad(when)
import System.IO.Unsafe(unsafePerformIO)


anyKey :: (Monad m) => m a -> GE.Event -> m Bool
anyKey m (GE.Key {GE.eventKeyName=key})
    | any (`isPrefixOf` key) ignores = return True
    | otherwise                      = m >> return True
  where ignores = ["Shift","Control","Alt",
                   "Super","Meta","Hyper"]

fromList =  mapM_  yield 

chartDraw :: Input (Renderable a) -> Int -> Int -> IO () 
chartDraw i windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    G.widgetSetSizeRequest window windowWidth windowHeight
    forkIO (do  
        let canvasUpdate = do 
            chart <- await 
            liftIO $  G.onExpose canvas $ const (updateCanvas chart canvas)
        runEffect $ fromInput i >-> forever canvasUpdate
        return ())
    G.set window [G.containerChild G.:= canvas]
    G.onDestroy window G.mainQuit
    G.widgetShowAll window
    G.mainGUI

main = do
    G.unsafeInitGUIForThreadedRTS
    (input,output) <- spawn  (Bounded 10)
    forkIO $ runEffect $ fromList (replicate 10 emptyRenderable) >-> toOutput input 
    chartDraw output 400 400
