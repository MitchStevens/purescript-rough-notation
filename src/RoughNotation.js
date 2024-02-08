import { annotate, annotationGroup } from "rough-notation"

export const annotate_ = element => roughAnnotationType => config => () => {
  fullConfig = { ...config }
  fullConfig.type = roughAnnotationType
  console.log(fullConfig)
  return annotate(element, fullConfig)
}

export const annotationGroup_ = annotations => () => {
  return annotationGroup(annotations)
}

export const show_ = annotation => () => {
  annotation.show()
  return 1000
}

export const hide_ = annotation => () => {
  annotation.hide()
}

export const remove_ = annotation => () => {
  annotation.remove()
}