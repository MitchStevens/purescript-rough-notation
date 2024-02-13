module RoughNotation
  ( Annotation
  , AnnotationGroup
  , animationDuration
  , annotate
  , annotationGroup
  , class RoughAnnotation
  , hideAnnotation
  , isShowing
  , iterations
  , removeAnnotation
  , showAnnotation
  )
  where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds)
import Effect.Class (liftEffect)
import Prim.Row (class Union)
import RoughNotation.Config (RoughAnnotationType, RoughAnnotationConfig, toNativeConfig)
import Web.DOM (Element)

foreign import data Annotation :: Type
foreign import data AnnotationGroup :: Type

class RoughAnnotation a where
  showAnnotation :: a -> Aff Unit
  hideAnnotation :: a -> Aff Unit

instance RoughAnnotation Annotation where
  showAnnotation annotation = do
    --Milliseconds duration <- animationDuration annotation
    --n <- iterations annotation
    liftEffect (show_ annotation)
    --delay $ Milliseconds (duration * toNumber n)
  hideAnnotation annotation = do
    liftEffect (hide_ annotation)

instance RoughAnnotation AnnotationGroup where
  showAnnotation annotation = do
    liftEffect (show_ annotation)
  hideAnnotation annotation = do
    liftEffect (hide_ annotation)

foreign import annotate_ :: forall config. Element -> String -> Record config -> Effect Annotation
annotate :: forall config rest. Union config rest RoughAnnotationConfig => Element -> RoughAnnotationType -> Record config -> Aff Annotation
annotate element roughAnnotationType config = 
  liftEffect (annotate_ element (show roughAnnotationType) (toNativeConfig config))

foreign import annotationGroup_ :: Array Annotation -> Effect AnnotationGroup
annotationGroup :: Array Annotation -> Aff AnnotationGroup
annotationGroup = liftEffect <<< annotationGroup_

foreign import show_ :: forall a. a -> Effect Unit
foreign import hide_ :: forall a. a -> Effect Unit

foreign import isShowing_ :: Annotation -> Effect Boolean
isShowing :: Annotation -> Aff Boolean
isShowing = liftEffect <<< isShowing_

foreign import animationDuration_ :: Annotation -> Effect Milliseconds
animationDuration :: Annotation -> Aff Milliseconds
animationDuration = liftEffect <<< animationDuration_

foreign import iterations_ :: Annotation -> Effect Int
iterations :: Annotation -> Aff Int
iterations = liftEffect <<< iterations_

foreign import remove_ :: Annotation -> Effect Unit
removeAnnotation :: Annotation -> Aff Unit
removeAnnotation = liftEffect <<< remove_