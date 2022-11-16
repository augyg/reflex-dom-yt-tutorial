{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Frontend where

import GHC.Generics hiding (R)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM)

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

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      elAttr "div" ("style" =: "background-color:azul;width:100%;height:600px" <> "class" =: "pt-10 flex justify-center") $ do
        eithForm <- elClass "div" "" $ contactMeForm --prerender (pure never) contactMeForm
        submit <- button "Submit"
        let
          (err, formSubmit) = fanEither $ tag (current eithForm) submit
        
        errorMessage <- holdDyn Nothing (Just <$> err)
        el "div" $ dynText $ (fromMaybe "nothing") <$> errorMessage 

        res <- prerender (pure never) $ do
          res <- performRequestAsync $ fmap (postJson "http://localhost:8000/email") $ gate (fmap isJust $ current errorMessage) $ formSubmit
          pure $ _xhrResponse_statusText <$> res

        dynText =<< holdDyn "" (switchDyn res)
        pure ()
  }


elStyle :: DomBuilder t m =>
           T.Text
        -> T.Text -- change this to map
        -> m a
        -> m a 
elStyle tag styleString inner = elAttr tag ("style" =: styleString) inner

contactMeForm :: ( DomBuilder t m
          --       , MonadJSM (Performable m)
                 , PerformEvent t m
                 , TriggerEvent t m
                 , MonadHold t m
                 , PostBuild t m 
                 ) => m (Dynamic t (Either T.Text Form))
contactMeForm = elAttr "div" ("class" =: "p-10 border rounded-lg bg-white" <> "style" =: "border-color:#FC74FD;border-width:4px;") $ do
  let inputClass = "border rounded-md border-black p-2"
  elStyle "div" "font-size:30px;color:#FC74FD" $ text "Contact Me"
  elClass "div" "pt-3" $ text "Have a question or want to work together?"
  elClass "div" "pt-3" $ text "Your Name"
  name <- elClass "div" "" $ fmap value $ inputElement $ def & initialAttributes .~ "class" =: inputClass
  elClass "div" "pt-3" $ text "Your Email"
  email <- elClass "div" "" $ fmap value $ inputElement $ def & initialAttributes .~ "class" =: inputClass
  elClass "div" "pt-3" $ text "Message"
  message <- elStyle "div" "" $ fmap value $ textAreaElement $ def & initialAttributes .~ ("style" =: "height:100px;width:100%"
                                                                                            <> "rows" =: "2"
                                                                                            <> "class" =: inputClass
                                                                                          )


  pure $ validateForm <$> name <*> email <*> message
  
  -- let
  --   dynamic :: _ = validateForm <$> (("NAME:" <>) <$> name) <*> (("EMAIL:" <>) <$> email) <*> (("MESSAGE:" <>) <$> message)
  --   formRaw = (,,) <$> name <*> email <*> message
  --   eithForm = ffor formRaw $ \(n, e, m) -> validateForm n e m
  --   (err, form) = fanEither $ tag eithForm submit

  -- submit <- button "Submit"
  -- errorMessage <- holdDyn Nothing (Just <$> err)
  -- el "div" $ dynText $ (fromMaybe "") <$> errorMessage 
  
  -- --fanEither e 
  -- res <- performRequestAsync $ fmap (postJson "http://localhost:8000/email") $ gate (fmap isJust $ current errorMessage) $ form

  -- -- TOOO(galen): what if the internet is out? 502
  
  
  -- pure (() <$ res)

validateForm :: T.Text -> T.Text -> T.Text -> Either T.Text Form
validateForm name email msg =
  case T.null name of
    True -> Left "Name cannot be empty"
    False -> case EmailValidate.validate (T.encodeUtf8 email) of
      Left str -> Left $ T.pack str
      Right _ -> case T.null msg of
        True -> Left "message cannot be empty"
        False -> Right $ Form name email msg 



data Form = Form T.Text T.Text T.Text deriving Generic

instance ToJSON Form

  
  --       ------------------------------------------------------------------------
--       -- reflex-dom specific events 
      
--       inputElValue <- fmap value $ inputElement $ def & inputElementConfig_initialValue .~ "red"

      
--       let baseAttrs = "style" =: "min-height:200px; background-color: red;" 
--       attrs <- holdDyn baseAttrs $ (\x -> "style" =: ("min-height:200px; background-color:" <> x <> ";")) <$> updated inputElValue

--       (e, _) <- elDynAttr' "div" attrs $ do
--         text "clickable region"

-- --      ev <- pure $ domEvent Click e
-- --      ev <- pure $ domEvent Mouseover e 
--       ev <- pure $ keypress Enter e

--       let colorCapd' = tag (current inputElValue) ev
--       colorCapd <- holdDyn "Just minding me own biznuss" colorCapd'
      
--       dynText $ T.toUpper <$> colorCapd
      
  
--       ------------------------------------------------------------------------
--       -- Many sources
      -- el "div" $ do 
      --   clickA <- button "click me"
      --   clickB <- button "or me!!!"
      --   clickC <- button "or meeeeeeee"
  
      --   multSources <- holdDyn "fwhfwefknjwekfjwj" $ mergeWith const [ "Hi" <$ clickA
      --                                                                , "Bye" <$ clickB
      --                                                                , "Wait" <$ clickC
      --                                                                ]

      --   dynText $ T.pack . show <$> multSources

--       ------------------------------------------------------------------------
      -- -- Accumulators
      -- el "div" $ do 
      --   click2 <- button "click me"
      --   dynamicValue <- accumDyn (\x y -> if T.length (y <> x) > 20 then "WOOHOO" else (y <> x)) "" $ "hey" <$ click2

      --   dynText $ T.pack . show <$> dynamicValue

--       ------------------------------------------------------------------------
--       -- FOLD DYN

      -- el "div" $ do 
      --   click2 <- button "click me"
      --   dynamicValue <- foldDyn (\x y -> x + y) 0 $ 1 <$ click2 
  
      --   dynText $ T.pack . show <$> dynamicValue

      -- ---or count

      --   countItUp <- count click2
      --   dynText $ T.pack . show <$> countItUp

        
--       ------------------------------------------------------------------------
      -- Events as streams || MonadHold
      -- el "div" $ do 
      --   clic <- button "clic meh"
      --   clic1 <- headE clic
      --   clic2 <- tailE clic

      --   a <- foldDyn (\x y -> x + y) 0 $ 1 <$ clic1 
      --   b <- foldDyn (\x y -> x + y) 0 $ 1 <$ clic2

      --   dynText $ T.pack . show <$> a
      --   dynText $ T.pack . show <$> b
        
      --takeWhileE :: (a -> Bool) -> Event t a -> m (Event t a)
--       ------------------------------------------------------------------------
--       -- SAMPLING Behaviors

      -- el "div" $ do 
      --   sampler <- button "sample" 
        
      --   let behave = constant False
      --   dynamic' <- holdDyn "hello" $ "itsameee" <$ sampler 
        
      --   let sampled = tag behave sampler
      --   let sampled2 = tag (current dynamic') sampler

      --   -- attachWith *** 

      --   dynamicValue <- foldDyn (\x y -> x : y) [] $ mergeWith const $ [ sampled2]
      --   dynText $ T.pack . mconcat <$> dynamicValue
      

--       ------------------------------------------------------------------------
--       -- Gating on a Behavior
      -- el "div" $ do 
      --   sampler <- button "gated" 
        
      --   let
      --     behave :: Reflex t => Behavior t Bool 
      --     behave = constant False
      

      --   d <- holdDyn "Come at me bro" $ T.pack . show <$> gate behave sampler
      --   dynText d 
      
      
--       ------------------------------------------------------------------------
      -- Subscribe to updates
      -- el "div" $ do 
      --   clickity <- button "click me"

      --   takeMeOutFuhDynna <- holdDyn () clickity 

      --   output <- toggle True $ updated takeMeOutFuhDynna

      --   dynText $ T.pack . show <$> output
      
--       ------------------------------------------------------------------------
--       -- filtering events 
      
      -- el "div" $ do 
    
      --   let fm :: Int -> Maybe Int 
      --       fm x = case x < 10 of
      --         True -> Just x
      --         False -> Nothing
            
      
      --   eventt <- button "click 4 event"

      --   dynnnn <- foldDyn (\x y -> x + y) 0 $ 1 <$ eventt 
        
      --   let x = mapMaybe fm $ updated dynnnn

      --   x' <- holdDyn 1 x
  
      --   dynText $ T.pack . show <$> x'

--       ------------------------------------------------------------------------
--       -- Switches

      -- c1 <- button "1"
      -- c2 <- button "2"

      -- add <- button "add" 

      -- --holdDyn c1 c2


      -- -- one ~ add if c1 is clickedLast 
      
      -- one <- switchHold never . (mergeWith const) $ [
      --   add <$ c1
      --   , never <$ c2
      --   ]

      -- two <- switchHold add . (mergeWith const) $ [
      --   add <$ c2
      --   , never <$ c1
      --   ]

      -- -- Event t (Event t ()) 

      -- -- holdDyn "" [ Event t (Event t a) ] 

      -- -- mergeWith const [ Event, Event ] -----> Event 

      -- one' <- count one
      -- two' <- count two

      -- dynText $ T.pack . show <$> one'
      -- dynText $ "----------------------------------"
      -- dynText $ T.pack . show <$> two'
      
      
      
      
      ------------------------------------------------------------------------
      -- identity cases:

      -- constant :: Reflex t => a -> Behavior t a
      -- constDyn :: Reflex t => a -> Dynamic t a
      -- never :: Reflex t => Event t a

      -- time-dep cases
      -- alignEventWithMaybe :: Reflex t => (These a b -> Maybe c) -> Event t a -> Event t b -> Event t c
      -- coincidence :: Event t (Event t a) -> Event t a
      -- difference :: Reflex t => Event t a -> Event t b -> Event t a
      -- now :: m (Event t ())


  
   
