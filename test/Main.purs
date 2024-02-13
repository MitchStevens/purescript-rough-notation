module Test.Main where

import Prelude

import Control.Parallel (parSequence_)
import Data.Either (either)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), runAff_, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)
import RoughNotation (annotate, annotationGroup, hideAnnotation, showAnnotation)
import RoughNotation.Config (BracketType(..), RoughPadding(..))
import RoughNotation.Config as RoughAnnotationType
import Web.DOM.Element (Element, toEventTarget, toParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


main :: Effect Unit
main = runAff_ (either throwError logShow) $ unsafePartial do
  document <- toNonElementParentNode <$> liftEffect (window >>= document)
  let lookupSection sectionId = fromJust <$> liftEffect (getElementById sectionId document)

  lookupSection "underlineSection" >>= annotateUnderlineSection
  lookupSection "boxSection" >>= annotateBoxSection
  lookupSection "circleSection" >>= annotateCircleSection
  lookupSection "highlightSection" >>= annotateHighlightSection
  lookupSection "strikeSection" >>= annotateStrikeThroughSection
  lookupSection "crossSection" >>= annotateCrossedOffSection
  lookupSection "bracketSection" >>= annotateBracketSection
  lookupSection "multilineSection" >>= annotateMultilineSection
  lookupSection "groupSection" >>= annotateGroupSection
  lookupSection "configSection" >>= annotateConfigSection
  lookupSection "noanimSection" >>= annotateNoAnimationSection

annotateUnderlineSection :: Partial => Element -> Aff Unit
annotateUnderlineSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  let config = { strokeWidth: 3.0, padding: Padding 3.0, color: "#B71C1C"}
  a1 <- liftAff $ annotate span RoughAnnotationType.Underline config
  a2 <- liftAff $ annotate h3 RoughAnnotationType.Underline config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)

annotateBoxSection :: Partial => Element -> Aff Unit
annotateBoxSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  let config = { strokeWidth: 2.0, padding: Padding 4.0, color: "#4A148C"}
  a1 <- liftAff $ annotate span RoughAnnotationType.Box config
  a2 <- liftAff $ annotate h3 RoughAnnotationType.Box config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)



annotateCircleSection :: Partial => Element -> Aff Unit
annotateCircleSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  let config = { padding: Padding 6.0, color: "#0D47A1"}
  a1 <- liftAff $ annotate span RoughAnnotationType.Circle config
  a2 <- liftAff $ annotate h3 RoughAnnotationType.Circle config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)

annotateHighlightSection :: Partial => Element -> Aff Unit
annotateHighlightSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  let config = { color: "#FFD54F"}
  a1 <- liftAff $ annotate span RoughAnnotationType.Highlight config
  a2 <- liftAff $ annotate h3 RoughAnnotationType.Highlight config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)


annotateStrikeThroughSection :: Partial => Element -> Aff Unit
annotateStrikeThroughSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  let config = { color: "#1B5E20", strokeWidth: 2.0 }
  a1 <- liftAff $ annotate span RoughAnnotationType.StrikeThrough config
  a2 <- liftAff $ annotate h3 RoughAnnotationType.StrikeThrough config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)


annotateCrossedOffSection :: Partial => Element -> Aff Unit
annotateCrossedOffSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  let config = { color: "#F57F17", strokeWidth: 2.0 }
  a1 <- liftAff $ annotate span RoughAnnotationType.CrossedOff config
  a2 <- liftAff $ annotate h3 RoughAnnotationType.CrossedOff config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)

annotateBracketSection :: Partial => Element -> Aff Unit
annotateBracketSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "p.blockp") (toParentNode section))
  h3     <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  a1 <- liftAff $ annotate span RoughAnnotationType.Bracket
          { color: "red"
          , strokeWidth: 2.0
          , padding: FullPadding 2.0 10.0 2.0 10.0
          , brackets: [ Left, Right ]}
  a2 <- liftAff $ annotate h3 RoughAnnotationType.Bracket
          { color: "red"
          , strokeWidth: 2.0, brackets: [Top]}
  ag <- annotationGroup [a1, a2]

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation ag
    showAnnotation ag

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)


annotateMultilineSection :: Partial => Element -> Aff Unit
annotateMultilineSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  mlspan   <- fromJust <$> liftEffect (querySelector (QuerySelector "#mlspan") (toParentNode section))
  let config =  { color: "#ffD54F", animationDuration: Milliseconds 1500.0, multiline: true, iterations: 1 }
  a1 <- liftAff $ annotate mlspan RoughAnnotationType.Highlight config

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    showAnnotation a1

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)

annotateGroupSection :: Partial => Element -> Aff Unit
annotateGroupSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  h3   <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))

  a1 <- annotate h3 RoughAnnotationType.Box { color: "#bf360C" }
  a2 <- annotate span RoughAnnotationType.Highlight { color: "#ffff00" }
  a3 <- annotate span RoughAnnotationType.Underline { color: "#bf360c", animationDuration: Milliseconds 300.0 }
  ag <- annotationGroup [a2, a3, a1]

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation ag
    showAnnotation ag

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)

annotateNoAnimationSection :: Partial => Element -> Aff Unit
annotateNoAnimationSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  h3   <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  i   <- fromJust <$> liftEffect (querySelector (QuerySelector "i") (toParentNode section))

  a1 <- annotate h3 RoughAnnotationType.Box { color: "#263238", animate: false }
  a2 <- annotate i RoughAnnotationType.Underline { color: "#263238", strokeWidth: 4.0, animate: false}

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)

annotateConfigSection :: Partial => Element -> Aff Unit
annotateConfigSection section = do
  button <- fromJust <$> liftEffect (querySelector (QuerySelector "button") (toParentNode section))
  h3   <- fromJust <$> liftEffect (querySelector (QuerySelector "h3") (toParentNode section))
  span   <- fromJust <$> liftEffect (querySelector (QuerySelector "span") (toParentNode section))

  a1 <- annotate h3 RoughAnnotationType.Box { color: "#D50000", strokeWidth: 10.0 }
  a2 <- annotate span RoughAnnotationType.Box { color: "#33691E", animationDuration: Milliseconds 3000.0}

  eventListener <- liftEffect $ eventListener \_ -> runAff_ (\_ -> pure unit) do
    hideAnnotation a1
    hideAnnotation a2
    parSequence_
      [ showAnnotation a1
      , showAnnotation a2
      ]

  liftEffect $ addEventListener EventTypes.click eventListener false (toEventTarget button)