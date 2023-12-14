

#' Add Heatmap to the Heatmap List
#' 
#' Add Heatmap to the Heatmap List
#' 
#' Normally we directly use \code{+} for horizontal concatenation and
#' \code{\link[=pct_v_pct]{%v%}} for vertical concatenation.
#' 
#' @param object A \code{\link{Heatmap-class}} object.
#' @param x a \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @param direction Whether the heatmap is added horizontal or vertically?
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu \href{mailto:z.gu@@dkfz.dez.gu@@dkfz.de}
#' @examples
#' 
#' 
#' # There is no example
#' NULL
#' 
NULL





#' Method dispatch page for add_heatmap
#' 
#' Method dispatch page for \code{add_heatmap}.
#' 
#' 
#' @section Dispatch: \code{add_heatmap} can be dispatched on following
#' classes:
#' 
#' \itemize{ \item \code{\link{add_heatmap,HeatmapAnnotation-method}},
#' \code{\link{HeatmapAnnotation-class}} class method \item
#' \code{\link{add_heatmap,Heatmap-method}}, \code{\link{Heatmap-class}} class
#' method \item \code{\link{add_heatmap,HeatmapList-method}},
#' \code{\link{HeatmapList-class}} class method }
#' @examples
#' 
#' 
#' # no example
#' NULL
#' 
#' 
NULL





#' Class for Concatenating Heatmaps and Annotations
#' 
#' Class for Concatenating Heatmaps and Annotations
#' 
#' This class is a super class for \code{\link{Heatmap-class}},
#' \code{\link{HeatmapList-class}} and \code{\link{HeatmapAnnotation-class}}
#' classes. It is only designed for \code{+} generic method and the \code{%v%v}
#' method so that above three classes can be appended to each other.
#' 
#' @name AdditiveUnit-class
#' @docType class
#' @examples
#' 
#' 
#' # There is no example
#' NULL
#' 
#' 
#' 
NULL





#' Horizontally Add Heatmaps or Annotations to a Heatmap List
#' 
#' Horizontally Add Heatmaps or Annotations to a Heatmap List
#' 
#' It is only a helper function. It actually calls
#' \code{\link{add_heatmap,Heatmap-method}},
#' \code{\link{add_heatmap,HeatmapList-method}} or
#' \code{\link{add_heatmap,HeatmapAnnotation-method}} depending on the class of
#' the input objects.
#' 
#' The \code{\link{HeatmapAnnotation-class}} object to be added should only be
#' row annotations. Column annotations should be added to the heatmap list by
#' \code{\link[=pct_v_pct]{%v%}}.
#' 
#' \code{x} and \code{y} can also be \code{NULL}.
#' 
#' @aliases +.AdditiveUnit add.AdditiveUnit
#' @param x A \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @param y A \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu \href{mailto:z.gu@@dkfz.dez.gu@@dkfz.de}
#' @seealso \code{\link[=pct_v_pct]{%v%}} operator is used for vertical heatmap
#' list.
#' @examples
#' 
#' 
#' # There is no example
#' NULL
#' 
#' 
NULL



