{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JS where 


import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM, JSVal, obj, jss, MakeObject, js, js1, js2, js3, jsg1, jsf, fun, JSM
                                   , ghcjsPure, isUndefined, array
                                   , valToBool, valToNumber, valToJSON, valToStr, fromJSVal, ToJSVal, jsg2, jsg, (<#), syncPoint, new
                                   , objSetPropertyByName, makeObject)
import JSDOM (currentWindowUnchecked, currentDocumentUnchecked)
import Control.Lens ((^.))

import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe (isJust, fromJust) 

import Foreign.JavaScript.Utils (jsonDecode)
  
data MediaRecorder = MediaRecorder JSVal
data UserMediaStream = UserMediaStream JSVal 

setVideoSrcObject :: MonadJSM m => m () 
setVideoSrcObject = liftJSM $ do

  constraintObject <- obj 
  constraintObject ^. jss "video" "true"
  constraintObject ^. jss "audio" "true"

  window <- currentWindowUnchecked

  stream <- window ^. js "navigator" ^. js "mediaDevices" ^. js1 "getUserMedia" constraintObject

  streamUndefined <- ghcjsPure $ isUndefined stream

  case streamUndefined of
    True -> pure ()
    False -> do
      stream <- handlePromise (UnsafeToPromise stream) (pure . UserMediaStream) (const $ pure "Navigator request denied")
      case stream of
        Left err -> pure ()
        Right (UserMediaStream stream') -> do 
          window ^. js "document" ^. js1 "getElementById" "myVideo" ^. jss "srcObject" stream'  
          pure () 
  
          doLoad 
 

  pure () 


doLoad :: MonadJSM m => m ()
doLoad = liftJSM $ do
  doc <- currentDocumentUnchecked
  videoEl <- doc ^. js1 "getElementById" "myVideo"
  canvasEl <- doc ^. js1 "getElementById" "myCanvas"

  ctx <- canvasEl ^. js1 "getContext" "2d"

  videoEl ^. js3 "addEventListener" "play" (fun $ \_ _ _ -> do
                                               timerCallback (Video videoEl) (CanvasContext ctx) 
                                           ) "false"
    

  pure () 
 
newtype Video = Video JSVal
newtype CanvasContext = CanvasContext JSVal 

timerCallback :: MonadJSM m => Video -> CanvasContext -> m ()
timerCallback vid@(Video videoEl) ctx'@(CanvasContext ctx) = liftJSM $ do
  isPaused :: JSVal <- videoEl ^. js "paused"
  isPaused' <- valToBool isPaused
  case isPaused' of
    True -> pure ()
    False -> do
      computeFrame vid ctx'
--      pure ()

      jsg1 "requestAnimationFrame" (fun $ \_ _ _ -> timerCallback vid ctx') 
      pure () 
  
  --isEnded <- videoEl ^. js "ended" 
  

  
  pure () 



-- valid :: Show s => [s]
-- valid = [1, "String", User 1 1 "galen"] 


-- testFunc :: MonadJSM m => JSVal -> JSVal -> m ()
-- testFunc _ 0 = pure () 
-- testFunc array length = do
--   --x <- array !! length
  --grey <- array !! (length  * 4 + 0) + array !! (length * 4 + 1) + array 
  

-- testFunc data' l = do
--   let add3 :: Int -> Int -> Int -> Int
--       add3 a b c = a + b + c 
--   flip mapM_ [0..l] $ \i -> do 
--     grey <- add3 <$> (data' !! (i * 4 + 0)) <*> (data' !! (i * 4 + 1)) <*> (data' !! (i * 4 + 2))
--     -- build new array and set the data attribute
--     pure ()

-- The length should always be a multiple of 4 
greyScale :: [Int] -> [Int]
greyScale [] = [] 
greyScale (r:g:b:alpha:xs) =
  let
    grey = (r + g + b) `div` 3
  in
    grey : grey : grey : alpha : (greyScale xs) 

clog :: ToJSVal a => a -> JSM ()
clog a = do
  _ <- jsg "console" ^. js1 "log" a --- "console.log(a)"
  pure ()

computeFrame :: MonadJSM m => Video -> CanvasContext -> m ()
computeFrame (Video videoEl) (CanvasContext ctx) = liftJSM $ do
  width <- videoEl ^. js "width" 
  height <- videoEl ^. js "height"
  ctx ^. jsf "drawImage" (videoEl,"0","0",width, height)

  frame <- ctx ^. jsf "getImageData" ("0","0", width, height)

  -- MENTION !!!
  -- length' :: Double <- valToNumber =<< frame ^. js "data" ^. js "length"
  --let length'' = length' // 4
  data' :: Maybe [Int] <- fromJSVal =<< frame ^. js "data" 

  -- should always be groups of 4 repeating RGBA
  case data' of
    Nothing -> clog "Could not read array"
    Just rgbas -> case rem (length rgbas) 4 == 0 of
      False -> clog "Improperly formatted array" 
      True -> do
        let newData = greyScale rgbas

        clog newData
        --newData' <- array newData
        --clog newData'
        clampedData <- new (jsg "Uint8ClampedArray") newData
        clog clampedData
        imageData <- new (jsg "ImageData") (clampedData, width, height) 
        clog imageData
--         clog frame
--         clog "=----="
--         frame' <- makeObject frame
--         objSetPropertyByName frame' "data" newData 
--        frame ^. jss "data" newData
--         liftIO $ print $ take 10 newData
--         clog $ frame' ^. js "data"
--         syncPoint
--       frame ^. jss "bullshit" "Helllo"
--         --clog $ frame ^. js "bullshit"
-- --        (frame <# "data") newData 
  --      clog frame
        ctx ^. js3 "putImageData" imageData "0" "0"
        pure ()

  
  --testFunc
  -- let
  --   dataList :: Maybe [Double]
  --   dataList = jsonDecode data'

  --liftIO $ print $ () data'
  --liftIO $ print data'
  --let length'' = length' / 4

  -- recurseSetData (Frame frame)
  
  pure () 

newtype Frame = Frame JSVal 

-- recurseSetData :: MonadJSM m => Int -> Frame -> m ()
-- recurseSetData 0 
-- recurseSetData idx (Frame frame) = do
--   grey <- frame ^. js "data" !! (

handlePromise :: Promise -> (JSVal -> JSM b) -> (JSVal -> JSM a) -> JSM (Either a b)
handlePromise (UnsafeToPromise val) thenHandler catchHandler = do
  mvar <- liftIO $ newEmptyMVar
  nextVal <- val ^. js1 "then" (fun $ \_ _ [v] -> thenHandler v >>= liftIO . putMVar mvar . Right)
  _ <- nextVal ^. js1 "catch" (fun $ \_ _ [err] -> catchHandler err >>= liftIO . putMVar mvar . Left)
  liftIO $ takeMVar mvar


newtype Promise =
  UnsafeToPromise JSVal
  deriving (MakeObject)
