module RoughNotation where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Prim.Row (class Union)
import RoughNotation.Config (RoughAnnotationType, RoughAnnotationConfig, toNativeConfig)
import Web.DOM (Element)

--data AnnotationState = Unattached | NotShowing | Showing

foreign import data Annotation :: Type
foreign import data AnnotationGroup :: Type

class RoughAnnotation a where
  showAnnotation :: a -> Aff Unit
  hideAnnotation :: a -> Aff Unit

instance RoughAnnotation Annotation where
  showAnnotation annotation = do
    duration <- liftEffect (show_ annotation)
    delay (Milliseconds duration)
  hideAnnotation annotation = do
    liftEffect (hide_ annotation)

instance RoughAnnotation AnnotationGroup where
  showAnnotation annotation = do
    duration <- liftEffect (show_ annotation)
    delay (Milliseconds duration)
  hideAnnotation annotation = do
    liftEffect (hide_ annotation)

foreign import annotate_ :: forall config. Element -> String -> Record config -> Effect Annotation
annotate :: forall config rest. Union config rest RoughAnnotationConfig => Element -> RoughAnnotationType -> Record config -> Aff Annotation
annotate element roughAnnotationType config = 
  liftEffect (annotate_ element (show roughAnnotationType) (toNativeConfig config))

foreign import annotationGroup_ :: Array Annotation -> Effect AnnotationGroup
annotationGroup :: Array Annotation -> Aff AnnotationGroup
annotationGroup annotations = liftEffect (annotationGroup_ annotations)

foreign import show_ :: forall a. a -> Effect Number

foreign import hide_ :: forall a. a -> Effect Unit

foreign import remove_ :: Annotation -> Effect Unit
removeAnnotation :: Annotation -> Aff Unit
removeAnnotation annotation = liftEffect (remove_ annotation)