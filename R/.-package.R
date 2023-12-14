#' Method dispatch page for add_heatmap
#'
#' Method dispatch page for \code{add_heatmap}.
#'
#'
#' @aliases add_heatmap
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
NULL





#' Add Heatmap to the Heatmap List
#'
#' Add Heatmap to the Heatmap List
#'
#' Normally we directly use \code{+} for horizontal concatenation and
#' \code{\link[=pct_v_pct]{%v%}} for vertical concatenation.
#'
#' @aliases add_heatmap,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @param x a \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @param direction Whether the heatmap is added horizontal or vertically?
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Add Annotations or Heatmaps as a Heatmap List
#'
#' Add Annotations or Heatmaps as a Heatmap List
#'
#' Normally we directly use \code{+} for horizontal concatenation and
#' \code{\link[=pct_v_pct]{%v%}} for vertical concatenation.
#'
#' @aliases add_heatmap,HeatmapAnnotation-method
#' @param object A \code{\link{HeatmapAnnotation-class}} object.
#' @param x A \code{\link{Heatmap-class}} object, a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @param direction Whether it is horizontal list or a vertical list?
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Add heatmaps and row annotations to the heatmap list
#'
#' Add heatmaps and row annotations to the heatmap list
#'
#' There is a shortcut function \code{+.AdditiveUnit}.
#'
#' @aliases add_heatmap,HeatmapList-method
#' @param object a \code{\link{HeatmapList-class}} object.
#' @param x a \code{\link{Heatmap-class}} object or a
#' \code{\link{HeatmapAnnotation-class}} object or a
#' \code{\link{HeatmapList-class}} object.
#' @param direction direction of the concatenation.
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
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
#' @aliases add.AdditiveUnit +.AdditiveUnit
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
#' # There is no example
#' NULL
#'
NULL





#' Adjust Heatmap List
#'
#' Adjust Heatmap List
#'
#' This function adjusts settings in all other heatmaps according to the main
#' heatmap. It also adjust the size of heatmap annotations to make them aligned
#' nicely.
#'
#' This function is only for internal use.
#'
#' @aliases adjust_heatmap_list,HeatmapList-method adjust_heatmap_list
#' @param object A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Size of the Annotation Legends
#'
#' Size of the Annotation Legends
#'
#' Internally, all annotation legends are packed by \code{\link{packLegend}} as
#' a single \code{\link[grid:grid.grob]{grob}} object.
#'
#' This function is only for internal use.
#'
#' @aliases annotation_legend_size,HeatmapList-method annotation_legend_size
#' @param object a \code{\link{HeatmapList-class}} object.
#' @param legend_list A list of self-defined legend, should be wrapped into
#' \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by
#' \code{\link{Legend}}.
#' @param ... Other arguments.
#' @return A \code{\link[grid]{unit}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' The AnnotationFunction Class
#'
#' The AnnotationFunction Class
#'
#' The heatmap annotation is basically graphics aligned to the heatmap columns
#' or rows. There is no restriction for the graphic types, e.g. it can be
#' heatmap-like annotation or points. Here the AnnotationFunction class is
#' designed for creating complex and flexible annotation graphics. As the main
#' part of the class, it uses a user-defined function to define the graphics.
#' It also keeps information of the size of the plotting regions of the
#' annotation. And most importantly, it allows subsetting to the annotation to
#' draw a subset of the graphics, which is the base for the splitting of the
#' annotations.
#'
#' See \code{\link{AnnotationFunction}} constructor for details.
#'
#' @name AnnotationFunction-class
#' @docType class
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Attach heatmap annotations to the heatmap
#'
#' Attach heatmap annotations to the heatmap
#'
#'
#' @aliases attach_annotation,Heatmap-method attach_annotation
#' @param object A \code{\link{Heatmap-class}} object.
#' @param ha A \code{\link{HeatmapAnnotation-class}} object.
#' @param side Which side of the heatmap. Value should be in "top", "bottom",
#' "left", "right".
#' @param gap Space between the two heatmap annotations.
#' @examples
#'
#' m <- matrix(rnorm(100), 10)
#' ht <- Heatmap(m)
#' ha <- HeatmapAnnotation(foo = 1:10)
#' ht <- attach_annotation(ht, ha)
#' ht
#' ha2 <- HeatmapAnnotation(bar = letters[1:10])
#' ht <- attach_annotation(ht, ha2)
#' ht
#'
NULL





#' Draw Legend Based on Color Mapping
#'
#' Draw Legend Based on Color Mapping
#'
#' The legend is constructed by \code{\link{Legend}}.
#'
#' @aliases color_mapping_legend,ColorMapping-method color_mapping_legend
#' @param object A \code{\link{ColorMapping-class}} object.
#' @param plot Whether to plot or just return the legend object?
#' @param ... Pass to \code{\link{draw,Legends-method}}.
#' @param color_bar "continous" or "discrete". It controls whether to show the
#' discrete legend for the continuous color mapping.
#' @param title Title of the legend, by default it is the name of the legend.
#' @param title_gp Graphical parameters for legend title.
#' @param title_position Position of the title. See \code{\link{Legend}} for
#' all possible values.
#' @param grid_height Height of each legend grid. Pass to \code{\link{Legend}}.
#' @param grid_width Width of each legend grid. Pass to \code{\link{Legend}}.
#' @param tick_length Length of the ticks on the continuous legends. Value
#' should be a \code{\link[grid]{unit}} object.
#' @param border Color for legend grid borders. Pass to \code{\link{Legend}}.
#' @param at Break values of the legend. By default it is the levels in the
#' \code{\link{ColorMapping-class}} object.
#' @param labels Labels corresponding to break values.
#' @param labels_gp Graphcial parameters for legend labels.
#' @param labels_rot Rotation of labels.
#' @param nrow Pass to \code{\link{Legend}}. It controls the layout of legend
#' grids if they are arranged in multiple rows or columns.
#' @param ncol Pass to \code{\link{Legend}}. It controls the layout of legend
#' grids if they are arranged in multiple rows or columns.
#' @param by_row Pass to \code{\link{Legend}}. It controls the order of legend
#' grids if they are arranged in multiple rows or columns.
#' @param legend_gp Graphic parameters for legend.
#' @param legend_height Height of the legend body. It only works when
#' \code{color_bar} is \code{continuous} and \code{direction} is
#' \code{vertical}. Pass to \code{\link{Legend}}.
#' @param legend_width Width of the legend body. It only works when
#' \code{color_bar} is \code{continuous} and \code{direction} is
#' \code{horizontal}. Pass to \code{\link{Legend}}.
#' @param legend_direction When \code{color_bar} is \code{continuous}, whether
#' the legend is vertical or horizontal? Pass to \code{\link{Legend}}.
#' @param break_dist A zooming factor to control relative distance of two
#' neighbouring break values.The length of it should be \code{length(at) - 1}
#' or a scalar.
#' @param graphics Internally used.
#' @param param All the legend-related parameters can be specified as a single
#' list.
#' @return A \code{\link{Legends-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Class for Color Mapping
#'
#' Class for Color Mapping
#'
#' The \code{\link{ColorMapping-class}} handles color mapping for discrete
#' values and continuous values. Discrete values are mapped by setting a vector
#' of colors and continuous values are mapped by setting a color mapping
#' function.
#'
#' @name ColorMapping-class
#' @docType class
#' @section Methods: The \code{\link{ColorMapping-class}} provides following
#' methods:
#'
#' \itemize{ \item \code{\link{ColorMapping}}: contructor methods.  \item
#' \code{\link{map_to_colors,ColorMapping-method}}: mapping values to colors.
#' \item \code{\link{color_mapping_legend,ColorMapping-method}}: draw legend or
#' get legend as an object. }
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Method dispatch page for column_dend
#'
#' Method dispatch page for \code{column_dend}.
#'
#'
#' @aliases column_dend
#' @section Dispatch: \code{column_dend} can be dispatched on following
#' classes:
#'
#' \itemize{ \item \code{\link{column_dend,Heatmap-method}},
#' \code{\link{Heatmap-class}} class method \item
#' \code{\link{column_dend,HeatmapList-method}},
#' \code{\link{HeatmapList-class}} class method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Get Column Dendrograms from a Heatmap
#'
#' Get Column Dendrograms from a Heatmap
#'
#'
#' @aliases column_dend,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @param on_slice If the value is TRUE, it returns the dendrogram on the slice
#' level.
#' @return The format of the returned object depends on whether rows/columns of
#' the heatmaps are split.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' mat <- matrix(rnorm(100), 10)
#' ht <- Heatmap(mat)
#' ht <- draw(ht)
#' column_dend(ht)
#' ht <- Heatmap(mat, column_km = 2)
#' ht <- draw(ht)
#' column_dend(ht)
#'
NULL





#' Get Column Dendrograms from a hHeatmap List
#'
#' Get Column Dendrograms from a hHeatmap List
#'
#'
#' @aliases column_dend,HeatmapList-method
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param name Name of a specific heatmap.
#' @param on_slice If the value is TRUE, it returns the dendrogram on the slice
#' level.
#' @return The format of the returned object depends on whether rows/columns of
#' the heatmaps are split.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' mat <- matrix(rnorm(100), 10)
#' ht_list <- Heatmap(mat) + Heatmap(mat)
#' ht_list <- draw(ht_list)
#' column_dend(ht_list)
#' ht_list <- Heatmap(mat, column_km = 2) + Heatmap(mat, column_km = 2)
#' ht_list <- draw(ht_list)
#' column_dend(ht_list)
#' column_dend(ht_list, on_slice = TRUE)
#' ht_list <- Heatmap(mat) %v% Heatmap(mat)
#' ht_list <- draw(ht_list)
#' column_dend(ht_list)
#' ht_list <- Heatmap(mat, column_km = 2) %v% Heatmap(mat)
#' ht_list <- draw(ht_list)
#' column_dend(ht_list)
#'
NULL





#' Method dispatch page for column_order
#'
#' Method dispatch page for \code{column_order}.
#'
#'
#' @aliases column_order
#' @section Dispatch: \code{column_order} can be dispatched on following
#' classes:
#'
#' \itemize{ \item \code{\link{column_order,Heatmap-method}},
#' \code{\link{Heatmap-class}} class method \item
#' \code{\link{column_order,HeatmapList-method}},
#' \code{\link{HeatmapList-class}} class method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Get Column Order from a Aeatmap List
#'
#' Get Column Order from a Aeatmap List
#'
#'
#' @aliases column_order,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @return The format of the returned object depends on whether rows/columns of
#' the heatmaps are split.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' mat <- matrix(rnorm(100), 10)
#' ht <- Heatmap(mat)
#' ht <- draw(ht)
#' column_order(ht)
#' ht <- Heatmap(mat, column_km = 2)
#' ht <- draw(ht)
#' column_order(ht)
#'
NULL





#' Get Column Order from a Heatmap List
#'
#' Get Column Order from a Heatmap List
#'
#'
#' @aliases column_order,HeatmapList-method
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param name Name of a specific heatmap.
#' @return The format of the returned object depends on whether rows/columns of
#' the heatmaps are split.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' mat <- matrix(rnorm(100), 10)
#' ht_list <- Heatmap(mat) + Heatmap(mat)
#' ht_list <- draw(ht_list)
#' column_order(ht_list)
#' ht_list <- Heatmap(mat, column_km = 2) + Heatmap(mat, column_km = 2)
#' ht_list <- draw(ht_list)
#' column_order(ht_list)
#' ht_list <- Heatmap(mat) %v% Heatmap(mat)
#' ht_list <- draw(ht_list)
#' column_order(ht_list)
#' ht_list <- Heatmap(mat, column_km = 2) %v% Heatmap(mat)
#' ht_list <- draw(ht_list)
#' column_order(ht_list)
#'
NULL





#' Make complex heatmaps
#'
#' Make complex heatmaps
#'
#' This package aims to provide a simple and flexible way to arrange multiple
#' heatmaps as well as flexible annotation graphics.
#'
#' The package is implemented in an object-oriented way.  The heatmap lists are
#' abstracted into several classes.
#'
#' \itemize{ \item \code{\link{Heatmap-class}}: a single heatmap containing
#' heatmap body, row/column names, titles, dendrograms and annotations.  \item
#' \code{\link{HeatmapList-class}}: a list of heatmaps and annotations.  \item
#' \code{\link{HeatmapAnnotation-class}}: a list of row/column annotations. }
#'
#' There are also several internal classes:
#'
#' \itemize{ \item \code{\link{SingleAnnotation-class}}: a single row
#' annotation or column annotation.  \item \code{\link{ColorMapping-class}}:
#' mapping from values to colors.  \item
#' \code{\link{AnnotationFunction-class}}: construct an annotation function
#' which allows subsetting. }
#'
#' Following two high-level functions take use of functionality of complex
#' heatmaps:
#'
#' \itemize{ \item \code{\link{oncoPrint}}: oncoPrint plot which visualize
#' genomic alterations in a set of genes.  \item \code{\link{densityHeatmap}}:
#' use heatmaps to visualize density distributions. }
#'
#' The complete reference of ComplexHeatmap package is available at
#' \url{http://jokergoo.github.io/ComplexHeatmap-reference/book.}
#'
#' @name ComplexHeatmap-package
#' @docType package
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Method dispatch page for component_height
#'
#' Method dispatch page for \code{component_height}.
#'
#'
#' @aliases component_height
#' @section Dispatch: \code{component_height} can be dispatched on following
#' classes:
#'
#' \itemize{ \item \code{\link{component_height,HeatmapList-method}},
#' \code{\link{HeatmapList-class}} class method \item
#' \code{\link{component_height,Heatmap-method}}, \code{\link{Heatmap-class}}
#' class method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Heights of Heatmap Components
#'
#' Heights of Heatmap Components
#'
#' All column components are: \code{column_title_top}, \code{column_dend_top},
#' \code{column_names_top}, \code{column_anno_top}, \code{heatmap_body},
#' \code{column_anno_bottom}, \code{column_names_bottom},
#' \code{column_dend_bottom}, \code{column_title_bottom}.
#'
#' This function is only for internal use.
#'
#' @aliases component_height,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @param k Which components in the heatmap. The value should numeric indices
#' or the names of the corresponding column component. See **Detials**.
#' @return A \code{\link[grid]{unit}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Height of Heatmap List Components
#'
#' Height of Heatmap List Components
#'
#'
#' @aliases component_height,HeatmapList-method
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param k Which component in the heatmap list. Values are in
#' \code{ComplexHeatmap:::HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT}.
#' @return A \code{\link[grid]{unit}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Method dispatch page for component_width
#'
#' Method dispatch page for \code{component_width}.
#'
#'
#' @aliases component_width
#' @section Dispatch: \code{component_width} can be dispatched on following
#' classes:
#'
#' \itemize{ \item \code{\link{component_width,HeatmapList-method}},
#' \code{\link{HeatmapList-class}} class method \item
#' \code{\link{component_width,Heatmap-method}}, \code{\link{Heatmap-class}}
#' class method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Widths of Heatmap Components
#'
#' Widths of Heatmap Components
#'
#' All row components are: \code{row_title_left}, \code{row_dend_left},
#' \code{row_names_left}, \code{row_anno_left}, \code{heatmap_body},
#' \code{row_anno_right}, \code{row_names_right}, \code{row_dend_right},
#' \code{row_title_right}.
#'
#' This function is only for internal use.
#'
#' @aliases component_width,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @param k Which components in the heatmap. The value should numeric indices
#' or the names of the corresponding row component. See **Detials**.
#' @return A \code{\link[grid]{unit}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Width of Heatmap List Components
#'
#' Width of Heatmap List Components
#'
#' This function is only for internal use.
#'
#' @aliases component_width,HeatmapList-method
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param k Which component in the heatmap list. Values are in
#' \code{ComplexHeatmap:::HEATMAP_LIST_LAYOUT_ROW_COMPONENT}.
#' @return A \code{\link[grid]{unit}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Copy the AnnotationFunction Object
#'
#' Copy the AnnotationFunction Object
#'
#' In \code{\link{AnnotationFunction-class}}, there is an environment which
#' stores some external variables for the annotation function (specified by the
#' \code{var_import} argument when constructing the
#' \code{\link{AnnotationFunction-class}} object. This
#' \code{\link{copy_all,AnnotationFunction-method}} hard copies all the
#' variables into a new isolated environment.
#'
#' The environment is at \code{object@var_env}.
#'
#' @aliases copy_all,AnnotationFunction-method
#' @param object The \code{\link{AnnotationFunction-class}} object.
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Method dispatch page for copy_all
#'
#' Method dispatch page for \code{copy_all}.
#'
#'
#' @aliases copy_all
#' @section Dispatch: \code{copy_all} can be dispatched on following classes:
#'
#' \itemize{ \item \code{\link{copy_all,AnnotationFunction-method}},
#' \code{\link{AnnotationFunction-class}} class method \item
#' \code{\link{copy_all,SingleAnnotation-method}},
#' \code{\link{SingleAnnotation-class}} class method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Copy the SingleAnnotation object
#'
#' Copy the SingleAnnotation object
#'
#' Since the SingleAnnotation object always contains an
#' \code{\link{AnnotationFunction-class}} object, it calls
#' \code{\link{copy_all,AnnotationFunction-method}} to hard copy the variable
#' environment.
#'
#' @aliases copy_all,SingleAnnotation-method
#' @param object The \code{\link{SingleAnnotation-class}} object.
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw legends for All Annotations
#'
#' Draw legends for All Annotations
#'
#' We call the "annotation legends" as the secondary legends. For horizontal
#' heamtap list, the legends are those from all top/bottom annotations, and for
#' vertical heatmap list, the legends are those from all left/right
#' annotations.
#'
#' A viewport is created which contains annotation legends.
#'
#' This function is only for internal use.
#'
#' @aliases draw_annotation_legend,HeatmapList-method draw_annotation_legend
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param legend_list A list of self-defined legends, should be wrapped into
#' \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by
#' \code{\link{Legend}}.
#' @param ... Other arguments.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw Heatmap Annotations on the Heatmap
#'
#' Draw Heatmap Annotations on the Heatmap
#'
#' A viewport is created which contains column/top annotations.
#'
#' The function calls \code{\link{draw,HeatmapAnnotation-method}} to draw the
#' annotations.
#'
#' This function is only for internal use.
#'
#' @aliases draw_annotation,Heatmap-method draw_annotation
#' @param object A \code{\link{Heatmap-class}} object.
#' @param which The position of the heamtap annotation.
#' @param k Slice index.
#' @param ... Pass to \code{\link[grid]{viewport}} which includes the complete
#' heatmap annotation.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw Heatmap Dendrograms
#'
#' Draw Heatmap Dendrograms
#'
#' A viewport is created which contains dendrograms.
#'
#' This function is only for internal use.
#'
#' @aliases draw_dend,Heatmap-method draw_dend
#' @param object A \code{\link{Heatmap-class}} object.
#' @param which Are the dendrograms put on the row or on the column of the
#' heatmap?
#' @param k Slice index.
#' @param max_height maximal height of dendrogram.
#' @param ... Pass to \code{\link[grid]{viewport}} which includes the complete
#' heatmap dendrograms.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @seealso \code{\link{grid.dendrogram}}
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw row names or column names
#'
#' Draw row names or column names
#'
#' A viewport is created which contains row names or column names.
#'
#' This function is only for internal use.
#'
#' @aliases draw_dimnames,Heatmap-method draw_dimnames
#' @param object A \code{\link{Heatmap-class}} object.
#' @param which Are the names put on the row or on the column of the heatmap?
#' @param k Slice index.
#' @param ... Pass to \code{\link[grid]{viewport}} which includes the complete
#' heatmap row/column names.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw Heatmap Body
#'
#' Draw Heatmap Body
#'
#' A viewport is created which contains subset rows and columns of the heatmap.
#'
#' This function is only for internal use.
#'
#' @aliases draw_heatmap_body,Heatmap-method draw_heatmap_body
#' @param object A \code{\link{Heatmap-class}} object.
#' @param kr Row slice index.
#' @param kc Column slice index.
#' @param ... Pass to \code{\link[grid]{viewport}} which includes the slice of
#' heatmap body.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw legends for All Heatmaps
#'
#' Draw legends for All Heatmaps
#'
#' Actually we call the "heatmap legends" as the main legends. For horizontal
#' heatmap list, the legends are those from heamtap/row annotation/left/right
#' annotation. For vertical heatmap list, the legends are those from
#' heamtap/column annotation/top/bottom annotation. if \code{merge_legends} is
#' true in \code{\link{draw,HeatmapList-method}}, then it contains all legends
#' shown on the plot.
#'
#' A viewport is created which contains heatmap legends.
#'
#' This function is only for internal use.
#'
#' @aliases draw_heatmap_legend,HeatmapList-method draw_heatmap_legend
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param legend_list A list of self-defined legends, should be wrapped into
#' \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by
#' \code{\link{Legend}}.
#' @param ... Other arguments.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw the List of Heatmaps
#'
#' Draw the List of Heatmaps
#'
#' It only draws the list of heatmaps without legends and titles.
#'
#' This function is only for internal use.
#'
#' @aliases draw_heatmap_list,HeatmapList-method draw_heatmap_list
#' @param object A \code{\link{HeatmapList-class}} object.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Method dispatch page for draw_title
#'
#' Method dispatch page for \code{draw_title}.
#'
#'
#' @aliases draw_title
#' @section Dispatch: \code{draw_title} can be dispatched on following classes:
#'
#' \itemize{ \item \code{\link{draw_title,HeatmapList-method}},
#' \code{\link{HeatmapList-class}} class method \item
#' \code{\link{draw_title,Heatmap-method}}, \code{\link{Heatmap-class}} class
#' method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Draw Heatmap Title
#'
#' Draw Heatmap Title
#'
#' A viewport is created which contains heatmap title.
#'
#' This function is only for internal use.
#'
#' @aliases draw_title,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @param which Is title put on the row or on the column of the heatmap?
#' @param k Slice index.
#' @param ... Pass to \code{\link[grid]{viewport}} which includes the complete
#' heatmap title.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw Heatmap List Title
#'
#' Draw Heatmap List Title
#'
#' A viewport is created which contains heatmap list title.
#'
#' This function is only for internal use.
#'
#' @aliases draw_title,HeatmapList-method
#' @param object A \code{\link{HeatmapList-class}} object.
#' @param which Is it a row title or a column title.
#' @return This function returns no value.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw the AnnotationFunction Object
#'
#' Draw the AnnotationFunction Object
#'
#' Normally it is called internally by the
#' \code{\link{SingleAnnotation-class}}.
#'
#' When \code{test} is set to \code{TRUE}, the annotation graphic is directly
#' drawn, which is generally for testing purpose.
#'
#' @aliases draw,AnnotationFunction-method
#' @param object The \code{\link{AnnotationFunction-class}} object.
#' @param index Index of observations.
#' @param k Current slice index.
#' @param n Total number of slices.
#' @param test Is it in test mode? The value can be logical or a text which is
#' plotted as the title of plot.
#' @param ... Pass to \code{\link[grid]{viewport}}.
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Method dispatch page for draw
#'
#' Method dispatch page for \code{draw}.
#'
#'
#' @aliases draw
#' @section Dispatch: \code{draw} can be dispatched on following classes:
#'
#' \itemize{ \item \code{\link{draw,HeatmapAnnotation-method}},
#' \code{\link{HeatmapAnnotation-class}} class method \item
#' \code{\link{draw,Legends-method}}, \code{\link{Legends-class}} class method
#' \item \code{\link{draw,SingleAnnotation-method}},
#' \code{\link{SingleAnnotation-class}} class method \item
#' \code{\link{draw,AnnotationFunction-method}},
#' \code{\link{AnnotationFunction-class}} class method \item
#' \code{\link{draw,Heatmap-method}}, \code{\link{Heatmap-class}} class method
#' \item \code{\link{draw,HeatmapList-method}}, \code{\link{HeatmapList-class}}
#' class method }
#' @examples
#'
#' # no example
#' NULL
#'
NULL





#' Draw a Single Heatmap
#'
#' Draw a Single Heatmap
#'
#' The function creates a \code{\link{HeatmapList-class}} object which only
#' contains a single heatmap and call \code{\link{draw,HeatmapList-method}} to
#' make the final heatmap.
#'
#' There are some arguments which control the some settings of the heatmap such
#' as legends. Please go to \code{\link{draw,HeatmapList-method}} for these
#' arguments.
#'
#' @aliases draw,Heatmap-method
#' @param object A \code{\link{Heatmap-class}} object.
#' @param internal If \code{TRUE}, it is only used inside the calling of
#' \code{\link{draw,HeatmapList-method}}.  It only draws the heatmap without
#' legends where the legend will be drawn by
#' \code{\link{draw,HeatmapList-method}}.
#' @param test Only for testing. If it is \code{TRUE}, the heatmap body is
#' directly drawn.
#' @param ... Pass to \code{\link{draw,HeatmapList-method}}.
#' @return A \code{\link{HeatmapList-class}} object.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw the Heatmap Annotations
#'
#' Draw the Heatmap Annotations
#'
#'
#' @aliases draw,HeatmapAnnotation-method
#' @param object A \code{\link{HeatmapAnnotation-class}} object.
#' @param index A vector of indices.
#' @param k The current slice index for the annotation if it is split.
#' @param n Total number of slices.
#' @param ... Pass to \code{\link[grid]{viewport}} which contains all the
#' annotations.
#' @param test Is it in test mode? The value can be logical or a text which is
#' plotted as the title of plot.
#' @param anno_mark_param It contains specific parameters for drawing
#' \code{\link{anno_mark}} and pass to the
#' \code{\link{draw,SingleAnnotation-method}}.
#' @return No value is returned.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw a list of heatmaps
#'
#' Draw a list of heatmaps
#'
#' The function first calls \code{\link{make_layout,HeatmapList-method}} to
#' calculate the layout of the heatmap list and the layout of every single
#' heatmap, then makes the plot by re-calling the graphic functions which are
#' already recorded in the layout.
#'
#' @aliases draw,HeatmapList-method
#' @param object a \code{\link{HeatmapList-class}} object.
#' @param newpage whether create a new page for the graphics. If you want to
#' arrange multiple plots in one page, I suggest to use
#' \code{\link[grid:grid.grab]{grid.grabExpr}}.
#' @param background Background color of the whole plot.
#' @param row_title title on the row.
#' @param row_title_side will the title be put on the left or right of the
#' heatmap.
#' @param row_title_gp graphic parameters for drawing text.
#' @param column_title title on the column.
#' @param column_title_side will the title be put on the top or bottom of the
#' heatmap.
#' @param column_title_gp graphic parameters for drawing text.
#' @param heatmap_legend_side side to put heatmap legend
#' @param merge_legends merge heatmap legends and annotation legends to put
#' into one column.
#' @param show_heatmap_legend whether show all heatmap legends
#' @param heatmap_legend_list use-defined legends which are put after the
#' heatmap legends
#' @param annotation_legend_side side of the annotation legends
#' @param show_annotation_legend whether show annotation legends
#' @param annotation_legend_list user-defined legends which are put after the
#' annotation legends
#' @param align_heatmap_legend How to align the legends to heatmap. Possible
#' values are "heatmap_center", "heatmap_top" and "global_center". If the value
#' is \code{NULL}, it automatically picks the proper value from the three
#' options.
#' @param align_annotation_legend How to align the legends to heatmap. Possible
#' values are "heatmap_center", "heatmap_top" and "global_center".
#' @param legend_grouping How the legends are grouped. Values should be
#' "adjusted" or "original". If it is set as "original", all annotation legends
#' are grouped together.
#' @param gap gap between heatmaps/annotations
#' @param ht_gap same as \code{gap}.
#' @param main_heatmap index of main heatmap. The value can be a numeric index
#' or the heatmap name
#' @param padding padding of the whole plot. The value is a unit vector of
#' length 4, which corresponds to bottom, left, top and right.
#' @param adjust_annotation_extension whether take annotation name into account
#' when calculating positions of graphic elements.
#' @param auto_adjust whether apply automatic adjustment? The auto-adjustment
#' includes turning off dendrograms, titles and row/columns for non-main
#' heatmaps.
#' @param row_dend_side side of the dendrogram from the main heatmap
#' @param row_sub_title_side side of the row title from the main heatmap
#' @param column_dend_side side of the dendrogram from the main heatmap
#' @param column_sub_title_side side of the column title from the main heatmap
#' @param row_gap this modifies \code{row_gap} of the main heatmap
#' @param cluster_rows this modifies \code{cluster_rows} of the main heatmap
#' @param cluster_row_slices this modifies \code{cluster_row_slices} of the
#' main heatmap
#' @param clustering_distance_rows this modifies
#' \code{clustering_distance_rows} of the main heatmap
#' @param clustering_method_rows this modifies \code{clustering_method_rows} of
#' the main heatmap
#' @param row_dend_width this modifies \code{row_dend_width} of the main
#' heatmap
#' @param show_row_dend this modifies \code{show_row_dend} of the main heatmap
#' @param row_dend_reorder this modifies \code{row_dend_reorder} of the main
#' heatmap
#' @param row_dend_gp this modifies \code{row_dend_gp} of the main heatmap
#' @param row_order this modifies \code{row_order} of the main heatmap
#' @param km = this modifies \code{km} of the main heatmap
#' @param split this modifies \code{split} of the main heatmap
#' @param row_km this modifies \code{row_km} of the main heatmap
#' @param row_km_repeats this modifies \code{row_km_repeats} of the main
#' heatmap
#' @param row_split this modifies \code{row_split} of the main heatmap
#' @param height this modifies \code{height} of the main heatmap
#' @param heatmap_height this modifies \code{heatmap_height} of the main
#' heatmap
#' @param column_gap this modifies \code{column_gap} of the main heatmap
#' @param cluster_columns this modifies \code{cluster_columns} of the main
#' heatmap
#' @param cluster_column_slices this modifies \code{cluster_column_slices} of
#' the main heatmap
#' @param clustering_distance_columns this modifies
#' \code{clustering_distance_columns} of the main heatmap
#' @param clustering_method_columns this modifies
#' \code{clustering_method_columns} of the main heatmap
#' @param column_dend_width this modifies \code{column_dend_width} of the main
#' heatmap
#' @param show_column_dend this modifies \code{show_column_dend} of the main
#' heatmap
#' @param column_dend_reorder this modifies \code{column_dend_reorder} of the
#' main heatmap
#' @param column_dend_gp this modifies \code{column_dend_gp} of the main
#' heatmap
#' @param column_order this modifies \code{column_order} of the main heatmap
#' @param column_km this modifies \code{column_km} of the main heatmap
#' @param column_km_repeats this modifies \code{column_km_repeats} of the main
#' heatmap
#' @param column_split this modifies \code{column_split} of the main heatmap
#' @param width this modifies \code{width} of the main heatmap
#' @param heatmap_width this modifies \code{heatmap_width} of the main heatmap
#' @param use_raster this modifies \code{use_raster} of every heatmap.
#' @param raster_device this modifies \code{raster_device} of every heatmap.
#' @param raster_quality this modifies \code{raster_quality} of every heatmap.
#' @param raster_device_param this modifies \code{raster_device_param} of every
#' heatmap.
#' @param raster_resize this modifies \code{raster_resize} of every heatmap.
#' @param post_fun A self-defined function will be executed after all the
#' heatmaps are drawn.
#' @param save_last Whether to save the last plot?
#' @param heatmap_row_names_gp this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @param heatmap_column_names_gp this set the value in \code{\link{ht_opt}}
#' and reset back after the plot is done
#' @param heatmap_row_title_gp this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @param heatmap_column_title_gp this set the value in \code{\link{ht_opt}}
#' and reset back after the plot is done
#' @param legend_title_gp this set the value in \code{\link{ht_opt}} and reset
#' back after the plot is done
#' @param legend_title_position this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @param legend_labels_gp this set the value in \code{\link{ht_opt}} and reset
#' back after the plot is done
#' @param legend_grid_height this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @param legend_grid_width this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @param legend_border this set the value in \code{\link{ht_opt}} and reset
#' back after the plot is done
#' @param legend_gap Gap between legends. The value should be a vector of two
#' units. One for gaps between vertical legends and one for the horizontal
#' legends. If only one single unit is specified, the same gap set for the
#' vertical and horizontal legends.
#' @param heatmap_border this set the value in \code{\link{ht_opt}} and reset
#' back after the plot is done
#' @param annotation_border this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @param fastcluster this set the value in \code{\link{ht_opt}} and reset back
#' after the plot is done
#' @param simple_anno_size this set the value in \code{\link{ht_opt}} and reset
#' back after the plot is done
#' @param show_parent_dend_line this set the value in \code{\link{ht_opt}} and
#' reset back after the plot is done
#' @return This function returns a \code{\link{HeatmapList-class}} object for
#' which the layout has been created.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @seealso
#' \url{https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html}
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL





#' Draw the Legends
#'
#' Draw the Legends
#'
#' In the legend grob, there should always be a viewport attached which is like
#' a wrapper of all the graphic elements in a legend. If in the \code{object},
#' there is already a viewport attached, it will modify the \code{x}, \code{y}
#' and \code{valid.just} of the viewport. If there is not viewport attached, a
#' viewport with specified \code{x}, \code{y} and \code{valid.just} is created
#' and attached.
#'
#' You can also directly use \code{\link[grid]{grid.draw}} to draw the legend
#' object, but you can only control the position of the legends by first
#' creating a parent viewport and adjusting the position of the parent
#' viewport.
#'
#' @aliases draw,Legends-method
#' @param object The \code{\link[grid:grid.grob]{grob}} object returned by
#' \code{\link{Legend}} or \code{\link{packLegend}}.
#' @param x The x position of the legends, measured in current viewport.
#' @param y The y position of the legends, measured in current viewport.
#' @param just Justification of the legends.
#' @param test Only used for testing.
#' @examples
#'
#' lgd <- Legend(at = 1:4, title = "foo")
#' draw(lgd, x = unit(0, "npc"), y = unit(0, "npc"), just = c("left", "bottom"))
#'
#' # and a similar version of grid.draw
#' pushViewport(viewport(x = unit(0, "npc"), y = unit(0, "npc"), just = c("left", "bottom")))
#' grid.draw(lgd)
#' popViewport()
#'
NULL





#' Draw the Single Annotation
#'
#' Draw the Single Annotation
#'
#'
#' @aliases draw,SingleAnnotation-method
#' @param object A \code{\link{SingleAnnotation-class}} object.
#' @param index A vector of indices.
#' @param k The index of the slice.
#' @param n Total number of slices. \code{k} and \code{n} are used to adjust
#' annotation names. E.g. if \code{k} is 2 and \code{n} is 3, the annotation
#' names are not drawn.
#' @param test Is it in test mode? The value can be logical or a text which is
#' plotted as the title of plot.
#' @param anno_mark_param It contains specific parameters for drawing
#' \code{\link{anno_mark}}.
#' @return No value is returned.
#' @author Zuguang Gu <z.gu@@dkfz.de>
#' @examples
#'
#' # There is no example
#' NULL
#'
NULL
