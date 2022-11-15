{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Data.Text (toLower)

--- Events
   -- Start with reflex and then how its impl. in reflex-dom
   -- Events can be like a monoid (accumulator, fold)
   -- look at what events are available
   -- button
   -- dynText
   -- Behavior, Dynamic, Event
      -- Behavior is like a snapshot of a potentially changing value
      
   -- Sampling
       -- tag
       -- attachWith
   -- Creating a dynamic - then sampling it
      -- Working backwards from dynamics
      -- updated 
   -- Combining Events

   -- Gate 
   -- domEvent and creating custom events

   -- END WITH: what about a subscription to a DOM event further down? 



-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      elAttr "div" ("id" =: "myElement" ) $ do
        el "div" $ do
          el "div" blank 
        el "div" $ text "hey"
        el "span" $ text "there"


  
      ------------------------------------------------------------------------
      -- Simple Ol' button
      el "div" $ do 
        click1 <- button "click me1"
        becomesUnit <- holdDyn "" $ T.pack . show <$> click1 

        dynText becomesUnit

      ------------------------------------------------------------------------
      -- reflex-dom specific events 
      
      inputEl <- fmap value $ inputElement $ def & inputElementConfig_initialValue .~ "red"

      
      let baseAttrs = "style" =: "min-height:200px; background-color: red;"
      attrs <- holdDyn baseAttrs $ (\x -> "style" =: ("min-height:200px; background-color:" <> x <> ";")) <$> updated inputEl 

      (e, _) <- elDynAttr' "div" attrs $ do
        text "clickable region"
        pure () 

      ev <- pure $ domEvent Click e
--      ev <- pure $ domEvent Mouseover e 
--      ev <- pure $ keypress Enter e

      let colorCapd' = tag (current inputEl) ev
      colorCapd <- holdDyn "Just minding me own biznuss" colorCapd'
      
      dynText $ T.toUpper <$> colorCapd
      
  
      ------------------------------------------------------------------------
      -- Many sources
      el "div" $ do 
        clickA <- button "click me"
        clickB <- button "or me!!!"
  
        multSources <- holdDyn "" $ mergeWith const [ "Hi" <$ clickA
                                                    , "Bye" <$ clickB
                                                    ]

        dynText $ T.pack . show <$> multSources

      ------------------------------------------------------------------------
      -- Accumulators
      el "div" $ do 
        click2 <- button "click me"
        dynamicValue <- accumDyn (\x y -> y : x) [] $ "hey" <$ click2

        dynText $ T.pack . show <$> dynamicValue

      ------------------------------------------------------------------------
      -- FOLD DYN

      el "div" $ do 
        click2 <- button "click me"
        dynamicValue <- foldDyn (\x y -> x + y) 0 $ 1 <$ click2
  
        dynText $ T.pack . show <$> dynamicValue

      ---or count

        countItUp <- count click2
        dynText $ T.pack . show <$> countItUp

        
      ------------------------------------------------------------------------
      -- Events as streams || MonadHold
      el "div" $ do 
        clic <- button "clic meh"
        clic1 <- headE clic
        clic2 <- tailE clic

        a <- foldDyn (\x y -> x + y) 0 $ 1 <$ clic1 
        b <- foldDyn (\x y -> x + y) 0 $ 1 <$ clic2

        dynText $ T.pack . show <$> a
        dynText $ T.pack . show <$> b
        
      --takeWhileE :: (a -> Bool) -> Event t a -> m (Event t a)
      ------------------------------------------------------------------------
      -- SAMPLING Behaviors

      el "div" $ do 
        sampler <- button "sample" 
        
        let behave = constant False
        dynamic' <- holdDyn "hello" $ "itsameee" <$ sampler 
        
        let sampled = tag behave sampler
        let sampled2 = tag (current dynamic') sampler

        -- attachWith *** 

        dynamicValue <- foldDyn (\x y -> x : y) [] $ mergeWith const $ [show <$> sampled, sampled2]
        dynText $ T.pack . mconcat <$> dynamicValue
      

      ------------------------------------------------------------------------
      -- Gating on a Behavior
      el "div" $ do 
        sampler <- button "gated" 
        
        let
          behave :: Reflex t => Behavior t Bool 
          behave = constant False
      

        d <- holdDyn "Come at me bro" $ T.pack . show <$> gate behave sampler
        dynText d 
      
      
      ------------------------------------------------------------------------
      -- Subscribe to updates
      el "div" $ do 
        clickity <- button "click me"

        takeMeOutFuhDynna <- holdDyn () clickity 

        output <- toggle True $ updated takeMeOutFuhDynna

        dynText $ T.pack . show <$> output
      
      ------------------------------------------------------------------------
      -- filtering events 
      
      el "div" $ do 
    
        let fm :: Int -> Maybe Int 
            fm x = case x < 10 of
              True -> Just x
              False -> Nothing
            
      
        eventt <- button "click 4 event"

        dynnnn <- foldDyn (\x y -> x + y) 0 $ 1 <$ eventt 
        
        let x =  mapMaybe fm $ updated dynnnn

        x' <- holdDyn 1 x
  
        dynText $ T.pack . show <$> x'

      ------------------------------------------------------------------------
      -- Switches

      c1 <- button "1"
      c2 <- button "2"

      add <- button "add" 

      --holdDyn c1 c2

      one <- switchHold never . (mergeWith const) $ [
        add  <$ c1
        , never <$ c2
        ]

      two <- switchHold add . (mergeWith const) $ [
        add  <$ c2
        , never <$ c1
        ]


      one' <- count one
      two' <- count two

      dynText $ T.pack . show <$> one'
      dynText $ T.pack . show <$> two'
      
      
      
      
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


  
      pure ()

      
      
  }
