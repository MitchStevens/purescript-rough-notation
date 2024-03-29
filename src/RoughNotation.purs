module RoughNotation
  ( Annotation
  , animationDuration
  , annotate
  , hideAnnotation
  , removeAnnotation
  , showAnnotation
  , withAnnotation
  )
  where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), bracket, cancelWith, delay, finally)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Prim.Row (class Nub, class Union)
import Record as Record
import RoughNotation.Config (Config, NativeConfig, RoughAnnotationType, ConfigRows, defaultConfig, toNativeConfig)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

foreign import data Annotation :: Type


foreign import annotate_ :: Element -> String -> NativeConfig -> Effect Annotation
-- | Creates an annotation from a target element, an annotation type, and a config record.
-- | Note that the config record must be a subrecord of `Config` type, see README.md for more details
annotate :: forall config rest. Union config rest ConfigRows => Nub ConfigRows ConfigRows => 
  Element -> RoughAnnotationType -> Record config -> Aff Annotation
annotate element roughAnnotationType config = 
  liftEffect (annotate_ element (show roughAnnotationType) (toNativeConfig config'))
  where
    config' = (Record.merge :: Record config -> Record rest -> Config) config (unsafeCoerce defaultConfig)

foreign import show_ :: forall a. a -> Effect Unit
-- | Shows annotation, blocks until annotation is completed
showAnnotation :: Annotation -> Aff Unit
showAnnotation annotation = show `cancelWith` Canceler \_ -> removeAnnotation annotation
  where
    show = do
      Milliseconds duration <- animationDuration annotation
      liftEffect (show_ annotation)
      delay (Milliseconds duration)

foreign import hide_ :: forall a. a -> Effect Unit
-- | Hides annotation, annotation can be shown again
hideAnnotation :: Annotation -> Aff Unit
hideAnnotation annotation =
  liftEffect (hide_ annotation) `cancelWith` Canceler \_ -> removeAnnotation annotation

foreign import remove_ :: Annotation -> Effect Unit
-- | Removes annotation, annotation cannot be run again
removeAnnotation :: Annotation -> Aff Unit
removeAnnotation = liftEffect <<< remove_


foreign import animationDuration_ :: Annotation -> Effect Milliseconds
-- | total duration of an annotation, used to block Aff while annotation is running
animationDuration :: Annotation -> Aff Milliseconds
animationDuration = liftEffect <<< animationDuration_

-- | Ensures that all the annotation is removed after display
withAnnotation :: Aff Annotation -> (Annotation -> Aff Unit) -> Aff Unit
withAnnotation annotation f = do
  a <- annotation
  finally (removeAnnotation a) (f a)

  --bracket
  --  (annotation <* log "got annotation") 
  --  (\a -> log "using annotation" *> f a <* log "finished with annotation")
  --  (\a -> removeAnnotation a <* log "annotation removed")

--type BracketConditions a b =
--  { killed :: Error -> a -> Aff Unit
--  , failed :: Error -> a -> Aff Unit
--  , completed :: b -> a -> Aff Unit
--  }
--
---- | A general purpose bracket which lets you observe the status of the
---- | bracketed action. The bracketed action may have been killed with an
---- | exception, thrown an exception, or completed successfully.
--foreign import generalBracket :: forall a b. Aff a -> BracketConditions a b -> (a -> Aff b) -> Aff b
