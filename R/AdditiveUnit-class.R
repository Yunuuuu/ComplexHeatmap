#' Constructor Method for AdditiveUnit Class
#' 
#' Constructor Method for AdditiveUnit Class
#' 
#' This method is not used in the package.
#' 
#' @param ... Black hole arguments.
#' @return No value is returned.
#' @author Zuguang Gu \href{mailto:z.gu@@dkfz.dez.gu@@dkfz.de}
#' @examples
#' 
#' 
#' # There is no example
#' NULL
#' 
#' 
AdditiveUnit <- function(...) {
  methods::new("AdditiveUnit", ...)
}

#' Class for Concatenating Heatmaps and Annotations
#' 
#' Class for Concatenating Heatmaps and Annotations
#' 
#' This class is a super class for \code{\link{Heatmap-class}},
#' \code{\link{HeatmapList-class}} and \code{\link{HeatmapAnnotation-class}}
#' classes. It is only designed for \code{+} generic method and the \code{%v%v}
#' method so that above three classes can be appended to each other.
#' 
#' @examples
#' 
#' # There is no example
#' NULL
#' 
#' 
#' @export 
#' @docType class
#' @name AdditiveUnit-class
methods::setClass("AdditiveUnit")

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
#' # no example
#' NULL
#'
#' @export 
#' @name add_heatmap
methods::setGeneric("add_heatmap", function(object, ...) {
    standardGeneric("add_heatmap")
})

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
#' @param x A \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @param y A \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @seealso \code{\link[=pct_v_pct]{%v%}} operator is used for vertical heatmap
#' list.
#' @examples
#' 
#' # There is no example
#' NULL
#' 
#' @export 
#' @aliases add.AdditiveUnit +.AdditiveUnit
"+.AdditiveUnit" <- function(x, y) {
  if (inherits(x, "HeatmapAnnotation")) {
    if (x@which != "row") {
      stop_wrap("You should specify `which = row` or use `rowAnnotation()` directly if you want to add row annotations horizontally.")
    }
  }
  if (inherits(y, "HeatmapAnnotation")) {
    if (y@which != "row") {
      stop_wrap("You should specify `which = row` or use `rowAnnotation()` directly if you want to add row annotations horizontally.")
    }
  }
  if (is.null(x)) {
    ht_list <- new("HeatmapList")
    ht_list@direction <- "horizontal"
    add_heatmap(ht_list, y)
  } else if (is.null(y)) {
    ht_list <- new("HeatmapList")
    ht_list@direction <- "horizontal"
    add_heatmap(ht_list, x)
  } else {
    add_heatmap(x, y)
  }
}

"%v%" <- function(x, y) {
  if (inherits(x, "HeatmapAnnotation")) {
    if (x@which != "column") {
      stop_wrap("You should specify `which = column` or use `columnAnnotation()` directly if you want to add column annotations vertically.")
    }
  }
  if (inherits(y, "HeatmapAnnotation")) {
    if (y@which != "column") {
      stop_wrap("You should specify `which = column` or use `columnAnnotation()` directly if you want to add column annotations vertically.")
    }
  }
  if (is.null(x)) {
    ht_list <- new("HeatmapList")
    ht_list@direction <- "vertical"
    add_heatmap(ht_list, y, direction = "vertical")
  } else if (is.null(y)) {
    ht_list <- new("HeatmapList")
    ht_list@direction <- "vertical"
    add_heatmap(ht_list, x, direction = "vertical")
  } else {
    add_heatmap(x, y, direction = "vertical")
  }
}
