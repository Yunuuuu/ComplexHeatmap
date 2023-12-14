ht_opt <- function(
    ..., RESET = FALSE, READ.ONLY = NULL,
    LOCAL = FALSE, ADD = FALSE) {

}

ht_opt <- GlobalOptions::setGlobalOptions(
  heatmap_row_names_gp = list(
    .value = NULL,
    .class = "gpar"
  ),
  heatmap_column_names_gp = list(
    .value = NULL,
    .class = "gpar"
  ),
  heatmap_row_title_gp = list(
    .value = NULL,
    .class = "gpar"
  ),
  heatmap_column_title_gp = list(
    .value = NULL,
    .class = "gpar"
  ),
  legend_title_gp = list(
    .value = NULL,
    .class = "gpar"
  ),
  legend_title_position = list(
    .value = NULL,
    .class = "character"
  ),
  legend_labels_gp = list(
    .value = NULL,
    .class = "gpar"
  ),
  legend_grid_height = list(
    .value = NULL,
    .class = "unit"
  ),
  legend_grid_width = list(
    .value = NULL,
    .class = "unit"
  ),
  legend_border = list(
    .value = NULL
  ),
  legend_gap = list(
    .value = unit(c(4, 4), "mm"),
    .length = 1:2,
    .class = "unit",
    .filter = function(x) {
      if (length(x) == 1) {
        rep(x, 2)
      } else {
        x
      }
    }
  ),
  merge_legends = FALSE,
  heatmap_border = list(
    .value = NULL
  ),
  annotation_border = list(
    .value = NULL
  ),
  fast_hclust = list(
    .value = FALSE,
    .class = "logical",
    .length = 1
  ),
  show_parent_dend_line = TRUE,
  verbose = list(
    .value = FALSE,
    .class = "logical",
    .filter = function(x) {
      if (is.null(x)) {
        FALSE
      } else if (is.na(x)) {
        FALSE
      } else {
        x
      }
    },
    .length = 1
  ),
  message = list(
    .value = TRUE,
    .class = "logical",
    .filter = function(x) {
      if (is.null(x)) {
        FALSE
      } else if (is.na(x)) {
        FALSE
      } else {
        x
      }
    },
    .length = 1
  ),
  show_vp = FALSE,
  simple_anno_size = list(
    .value = unit(5, "mm"),
    .class = "unit"
  ),
  DENDROGRAM_PADDING = list(
    .value = unit(0.5, "mm"),
    .class = "unit"
  ),
  DIMNAME_PADDING = list(
    .value = unit(1, "mm"),
    .class = "unit"
  ),
  TITLE_PADDING = list(
    .value = NULL,
    .class = "unit",
    .filter = function(x) {
      if (length(x) == 1) {
        rep(x, 2)
      } else {
        x[1:2]
      }
    }
  ),
  COLUMN_ANNO_PADDING = list(
    .value = unit(1, "mm"),
    .class = "unit"
  ),
  ROW_ANNO_PADDING = list(
    .value = unit(1, "mm"),
    .class = "unit"
  ),
  HEATMAP_LEGEND_PADDING = list(
    .value = unit(2, "mm"),
    .class = "unit"
  ),
  ANNOTATION_LEGEND_PADDING = list(
    .value = unit(2, "mm"),
    .class = "unit"
  ),

  ### invisible
  "__export_image_size__" = list(
    .visible = FALSE,
    .value = FALSE
  ),
  save_last = list(
    .value = FALSE
  ),
  "validate_names" = TRUE,
  raster_temp_image_max_width = 30000,
  raster_temp_image_max_height = 30000,
  COLOR = c("blue", "#EEEEEE", "red")
)

ht_global_opt <- function(
    ..., RESET = FALSE, READ.ONLY = NULL,
    LOCAL = FALSE, ADD = FALSE) {

}

ht_global_opt <- ht_opt

.ENV <- new.env(parent = emptyenv())
.ENV$current_annotation_which <- NULL
.ENV$row_order <- NULL
.ENV$row_pos <- NULL
.ENV$last <- NULL

# DENDROGRAM_PADDING = unit(0.5, "mm")
# DIMNAME_PADDING = unit(1, "mm")
# TITLE_PADDING = unit(2.5, "mm")
# COLUMN_ANNO_PADDING = unit(1, "mm")
# ROW_ANNO_PADDING = unit(1, "mm")

GLOBAL_PADDING <- unit(rep(5.5, 4), "points")


is_under_jupyter <- function() {
  "IRkernel" %in% loadedNamespaces()
}

.ENV$IS_UNDER_JUPYTER <- FALSE
.ENV$IS_UNDER_JUPYTER_IGNORE <- FALSE
