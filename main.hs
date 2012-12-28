import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Data.Monoid
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFW as GLFW -- Note: this is GLFW-b

import System.Environment(getArgs)
import System.Exit



data GraphObject = GraphObject { graphImage :: Draw.Image Any, graphXMaxMin :: Draw.R2, graphYMaxMin :: Draw.R2, graphData :: [Draw.R2] }

data ApplicationState = ApplicationState { shutDown :: Bool
                                         , bgGrid :: Draw.Image Any
                                         , dragGraph :: Maybe (Int, Int)
                                         , graphTranslate :: Draw.R2
                                         , graphScale :: (Draw.R, Draw.R)
                                         , currentGraph :: GraphObject }

fi = fromIntegral

resX, resY :: Int
resX = 640
resY = 480

-------------
-------------

axisColor = Draw.Color 1 1 1 (0.8)

initScreen :: IO ()
initScreen = do
  True <- GLFW.initialize
  True <- GLFW.openWindow GLFW.defaultDisplayOptions {
    GLFW.displayOptions_width = resX,
    GLFW.displayOptions_height = resY
    }
  return ()

sinStuff :: [Draw.R2]
sinStuff = [((x*pi/180), sin(x*pi/180)) | x <- [(-500)..1000]]

axes :: Draw.Image Any
axes = Draw.tint axisColor $ (Draw.line (0, -2) (0, 2)) `mappend` (Draw.line (2, 0) (-2, 0))

graphDrawing :: [Draw.R2] -> Draw.Image Any
graphDrawing graphData = Draw.tint (Draw.Color 1 0 1 1) (mconcat [Draw.line x x1 | (x, x1) <- zip graphData (tail graphData)])

generateApplicationState :: [Draw.R2] -> ApplicationState
generateApplicationState gData = ApplicationState False axes Nothing ((-1)-(xMin * globalScale), 0) (globalScale, globalScale) $ GraphObject gImage gXmaxmin gYmaxmin gData
    where
        (xData, yData) = (map fst gData, map snd gData)
        (xMax, xMin)   = (maximum xData, minimum xData)
        (yMax, yMin)   = (maximum yData, minimum yData)
        gXmaxmin       = (xMax, xMin)
        gYmaxmin       = (yMax, yMin)
        yScale         = 2 / (yMax - yMin)
        xScale         = 2 / (xMax - xMin)
        globalScale    = min yScale xScale
        gImage         = graphDrawing gData

keepBetween :: Ord a => a -> a -> a -> a
keepBetween lowerBound upperBound num = min upperBound $ max lowerBound num

r2KeepBetween :: Draw.R2 -> Draw.R2 -> Draw.R2 -> Draw.R2
r2KeepBetween yBounds xBounds (x, y) = ((uncurry keepBetween xBounds) x, (uncurry keepBetween yBounds) y) 

setupHandlers :: TVar ApplicationState -> IO ()
setupHandlers appStateTVar = do

    GLFW.setWindowCloseCallback $ do
        atomically $ modifyTVar (appStateTVar) (\x -> x {shutDown = True})
        return True

    GLFW.setMouseWheelCallback $ \mwv -> do
            when (mwv /= 0) $ do
                GLFW.setMouseWheel 0
                appState <- readTVarIO appStateTVar
                let (scalex, scaley) = graphScale appState
                    (newx, newy)     = (scalex * (1-((fi mwv)/10)), scaley * (1-((fi mwv)/10)))
                atomically $ modifyTVar (appStateTVar) (\x -> x { graphScale = (newx, newy) })

    GLFW.setMousePositionCallback $ \mx my -> do
        mb0_Down <- GLFW.mouseButtonIsPressed GLFW.MouseButton0
        when mb0_Down $ do
            (wx, wy) <- GLFW.getWindowDimensions
            appState <- readTVarIO appStateTVar
            let newTrans = case dragGraph appState of
                    Just (oldx, oldy) ->
                        let graph = currentGraph appState
                            (oldTx, oldTy) = graphTranslate appState
                            xRatio = ( (fi $ 2 * (mx - oldx)) / (fi wx))
                            yRatio = ( (fi $ 2 * (oldy - my)) / (fi wy))
                            (xMax, xMin) = graphXMaxMin graph
                            (yMax, yMin) = graphYMaxMin graph
                            (xScale, yScale) = graphScale appState
                            xBounds = keepBetween ((0) - (xMax * xScale)) (0 - (xMin * xScale))
                            yBounds = keepBetween ((0) - (yMax * yScale)) (0 - (yMin * yScale))
                        in (xBounds $ oldTx + xRatio, yBounds $ oldTy + yRatio)
                    Nothing -> (graphTranslate appState)
            atomically $ writeTVar (appStateTVar) (appState { graphTranslate = newTrans, dragGraph = Just (mx, my) })

    GLFW.setMouseButtonCallback $ \mb upDown-> do
        case mb of
            GLFW.MouseButton0 -> if upDown
                    then return ()
                    else atomically $ modifyTVar (appStateTVar) (\x -> x { dragGraph = Nothing })


mainIO :: TVar ApplicationState -> IO ()
mainIO appStateTVar = waitClose >> GLFW.terminate
    where
        waitClose = do
            appState <- readTVarIO appStateTVar
            unless (shutDown appState) $ do
                let curIm = graphImage $ currentGraph appState
                    (scalex, scaley) = graphScale appState

                Draw.clearRender $ (Draw.translate (r2KeepBetween (-1, 1) (-1, 1) (graphTranslate appState)) %% bgGrid appState) `mappend` ( Draw.translate (graphTranslate appState) %% Draw.scale scalex scaley %% curIm)

                GLFW.swapBuffers

                waitClose

main = do
    initScreen
    appStateTVar <- atomically $ newTVar $ (generateApplicationState sinStuff)
    args <- getArgs

    setupHandlers appStateTVar
    mainIO appStateTVar