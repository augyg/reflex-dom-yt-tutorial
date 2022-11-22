{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-} 

module Frontend where

import JS (setVideoSrcObject)

import GHC.Generics hiding (R)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM, JSVal, obj, jss)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Data.Text (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.Map
import Data.Aeson 
import Text.Email.Validate as EmailValidate

-- requires adding to cabal 
import Data.Time.Clock

import Control.Monad.IO.Class
import Control.Monad.Fix 

import Control.Lens ((^.))

-- requires tuple sections
tickLossyFrom'' :: ( PerformEvent t m
                   , MonadIO (Performable m)
                   , TriggerEvent t m
                   , MonadFix m
                   ) => NominalDiffTime -> Event t a -> m (Event t TickInfo)
tickLossyFrom'' nomnom ev = do
  eventTime <- performEvent $ liftIO getCurrentTime <$ ev
  tickLossyFrom' $ (nomnom,) <$> eventTime 
  

countTimeFrom :: ( PerformEvent t m
                 , MonadIO (Performable m)
                 , TriggerEvent t m
                 , MonadFix m
                 , MonadHold t m
                 ) => NominalDiffTime -> Event t a -> m (Event t NominalDiffTime) -- could expand to tickinfo?
countTimeFrom interval ev = do
  eventTime <- performEvent $ liftIO getCurrentTime <$ ev
  eventTimeDyn <- foldDyn (\x _ -> x) undefined {-never evals, this fires once-} eventTime
  tick <- tickLossyFrom' $ (interval,) <$> eventTime
  --pure $ ffor2 eventTime tick $ \s n -> diffUTCTime (_tickInfo_lastUTC n) s 
  pure $ attachWith (\start now -> diffUTCTime (_tickInfo_lastUTC now) start) (current eventTimeDyn) tick 
  
-- NominalDiffTime affects how often we "sample" the system time 



-- data Maybe a = Just a | Nothing




-- do
--   document <- getCurrentDocumentUnchecked
--   document ^. js1 "getElementById" "someId" .^ jss "innerText" "Whassup dog" 
  







-- document.getElementByID('someId').innerText = "whassup dog" 

elSelfClosing :: DomBuilder t m => T.Text -> Map T.Text T.Text -> m ()
elSelfClosing tag attrs = elAttr tag attrs blank


-- MaybeT IO a 

-- MaybeT IO a

 
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Galen Sprout Consulting"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do

      elAttr "video" ("id" =: "myVideo" <> "controls" =: "true" <> "width" =: "480" <> "height" =: "270" <> "muted" =: "true") blank --- $ do
        -- elSelfClosing "source" ("src" =: "" <> "type" =: "video/webm") 

      elAttr "canvas" ("id" =: "myCanvas" <> "width" =: "480" <> "height" =: "270") blank

      

      -- postBuildEvent <- getPostBuild

      prerender (pure ()) setVideoSrcObject  

      
      pure ()

  }

-- print :: IO ()

-- liftIO print 


-- print :: MonadIO m => m () 











      
      -- t <- performEvent $ liftIO getCurrentTime

      -- click <- button "click me!"
      -- click2 <- button "click 2" 
      -- -- tick <- tickLossyFrom'' 1 click
      -- -- dynText =<< holdDyn "" (T.pack . show <$> tick)



      -- eval 
  




      -- -- el "div" $ pure 1 :: DomBuilder t m => m Int ------> Event t Int

      -- let
      --   newDom :: DomBuilder t m => m () 
      --   newDom = el "div" $ text "new and improved!" 

      --   oldDom :: DomBuilder t m => m () 
      --   oldDom = el "div" $ text "old!"

      -- dynamicWidget {-DomBuilder t m => Dynamic t (m a)-} <- holdDyn oldDom $ mergeWith const [ newDom <$ click
      --                                                                                         , oldDom <$ click2
      --                                                                                         ]

      -- dyn dynamicWidget


      -- widgetHold oldDom $ newDom <$ click



  
      -- timeFrom <- countTimeFrom 0.01 click
      -- dynamic <- holdDyn "" (T.pack . show <$> timeFrom)
      -- dynText dynamic

      

      {-
         document.<query selector>.attribute = X
         document.getElementByID("someId").innerText = "hello" 
      -} 
        
      -- eventTime <- performEvent $ liftIO getCurrentTime <$ click

      -- holdDyn "" (T.pack . show <$> eventTime) >>= dynText

      
      -- t <- tickLossyFromPostBuildTime 1 
      -- --dynText =<< holdDyn "HEY" (T.pack . show <$> t)
      -- let dt = 1
      
      -- click <- button "shrimp on da barbie" 
      -- eventTime <- performEvent $ liftIO getCurrentTime <$ click
      -- tickLossyFrom' (dt,) <$> eventTime 
      -- pure ()














      
      -- elAttr "div" ("style" =: "background-color:azul;width:100%;height:600px" <> "class" =: "pt-10 flex justify-center") $ do
      --   eithForm <- elClass "div" "" $ contactMeForm --prerender (pure never) contactMeForm
      --   submit <- button "Submit"
      --   let
      --     (err, formSubmit) = fanEither $ tag (current eithForm) submit
        
      --   errorMessage <- holdDyn Nothing (Just <$> err)
      --   el "div" $ dynText $ (fromMaybe "nothing") <$> errorMessage 

      --   res <- prerender (pure never) $ do
      --     res <- performRequestAsync $ fmap (postJson "http://localhost:8000/email") $ gate (fmap isJust $ current errorMessage) $ formSubmit
      --     pure $ _xhrResponse_statusText <$> res

      --   dynText =<< holdDyn "" (switchDyn res)
      --   pure ()
  


elStyle :: DomBuilder t m =>
           T.Text
        -> T.Text -- change this to map
        -> m a
        -> m a 
elStyle tag styleString inner = elAttr tag ("style" =: styleString) inner

-- contactMeForm :: ( DomBuilder t m
--           --       , MonadJSM (Performable m)
--                  , PerformEvent t m
--                  , TriggerEvent t m
--                  , MonadHold t m
--                  , PostBuild t m 
--                  ) => m (Dynamic t (Either T.Text Form))
-- contactMeForm = elAttr "div" ("class" =: "p-10 border rounded-lg bg-white" <> "style" =: "border-color:#FC74FD;border-width:4px;") $ do
--   let inputClass = "border rounded-md border-black p-2"
--   elStyle "div" "font-size:30px;color:#FC74FD" $ text "Contact Me"
--   elClass "div" "pt-3" $ text "Have a question or want to work together?"
--   elClass "div" "pt-3" $ text "Your Name"
--   name <- elClass "div" "" $ fmap value $ inputElement $ def & initialAttributes .~ "class" =: inputClass
--   elClass "div" "pt-3" $ text "Your Email"
--   email <- elClass "div" "" $ fmap value $ inputElement $ def & initialAttributes .~ "class" =: inputClass
--   elClass "div" "pt-3" $ text "Message"
--   message <- elStyle "div" "" $ fmap value $ textAreaElement $ def & initialAttributes .~ ("style" =: "height:100px;width:100%"
--                                                                                             <> "rows" =: "2"
--                                                                                             <> "class" =: inputClass
--                                                                                           )


--   pure $ validateForm <$> name <*> email <*> message
  
--   -- let
--   --   dynamic :: _ = validateForm <$> (("NAME:" <>) <$> name) <*> (("EMAIL:" <>) <$> email) <*> (("MESSAGE:" <>) <$> message)
--   --   formRaw = (,,) <$> name <*> email <*> message
--   --   eithForm = ffor formRaw $ \(n, e, m) -> validateForm n e m
--   --   (err, form) = fanEither $ tag eithForm submit

--   -- submit <- button "Submit"
--   -- errorMessage <- holdDyn Nothing (Just <$> err)
--   -- el "div" $ dynText $ (fromMaybe "") <$> errorMessage 
  
--   -- --fanEither e 
--   -- res <- performRequestAsync $ fmap (postJson "http://localhost:8000/email") $ gate (fmap isJust $ current errorMessage) $ form

--   -- -- TOOO(galen): what if the internet is out? 502
  
  
--   -- pure (() <$ res)

-- validateForm :: T.Text -> T.Text -> T.Text -> Either T.Text Form
-- validateForm name email msg =
--   case T.null name of
--     True -> Left "Name cannot be empty"
--     False -> case EmailValidate.validate (T.encodeUtf8 email) of
--       Left str -> Left $ T.pack str
--       Right _ -> case T.null msg of
--         True -> Left "message cannot be empty"
--         False -> Right $ Form name email msg 



-- data Form = Form T.Text T.Text T.Text deriving Generic

-- instance ToJSON Form

  
