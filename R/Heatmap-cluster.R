make_row_cluster <- function(object) {
    object <- make_cluster(object, "row")
    if (length(object@row_title) > 1) {
        if (length(object@row_title) != length(object@row_order_list)) {
            stop_wrap("If `row_title` is set with length > 1, the length should be as same as the number of row slices.")
        }
    }
    return(object)
}

make_column_cluster <- function(object) {
    object <- make_cluster(object, "column")
    if (length(object@column_title) > 1) {
        if (length(object@column_title) != length(object@column_order_list)) {
            stop_wrap("If `column_title` is set with length > 1, the length should be as same as the number of column slices.")
        }
    }
    return(object)
}

make_cluster <- function(object, which = c("row", "column")) {
    which <- match.arg(which)[1]

    verbose <- object@heatmap_param$verbose

    if (ht_opt("fast_hclust")) {
        hclust <- fastcluster::hclust
        if (verbose) qqcat("apply hclust by fastcluster::hclust\n")
    } else {
        hclust <- stats::hclust
    }

    mat <- object@matrix
    jitter <- object@matrix_param$jitter
    if (is.numeric(mat)) {
        if (is.logical(jitter)) {
            if (jitter) {
                mat <- mat + runif(length(mat), min = 0, max = 1e-10)
            }
        } else {
            mat <- mat + runif(length(mat), min = 0, max = jitter + 0)
        }
    }

    distance <- slot(object, paste0(which, "_dend_param"))$distance
    method <- slot(object, paste0(which, "_dend_param"))$method
    order <- slot(object, paste0(which, "_order")) # pre-defined row order
    km <- getElement(object@matrix_param, paste0(which, "_km"))
    km_repeats <- getElement(object@matrix_param, paste0(which, "_km_repeats"))
    split <- getElement(object@matrix_param, paste0(which, "_split"))
    reorder <- slot(object, paste0(which, "_dend_param"))$reorder
    cluster <- slot(object, paste0(which, "_dend_param"))$cluster
    cluster_slices <- slot(object, paste0(which, "_dend_param"))$cluster_slices
    gap <- getElement(object@matrix_param, paste0(which, "_gap"))

    dend_param <- slot(object, paste0(which, "_dend_param"))
    dend_list <- slot(object, paste0(which, "_dend_list"))
    dend_slice <- slot(object, paste0(which, "_dend_slice"))
    order_list <- slot(object, paste0(which, "_order_list"))
    order <- slot(object, paste0(which, "_order"))

    names_param <- slot(object, paste0(which, "_names_param"))

    dend_param$split_by_cutree <- FALSE

    if (!is.null(dend_param$obj)) {
        if (inherits(dend_param$obj, "hclust")) {
            ncl <- length(dend_param$obj$order)
        } else {
            ncl <- nobs(dend_param$obj)
        }

        if (which == "row") {
            if (ncl != nrow(mat)) {
                stop_wrap("The length of the row clustering object is not the same as the number of matrix rows.")
            }
        } else {
            if (ncl != ncol(mat)) {
                stop_wrap("The length of the column clustering object is not the same as the number of matrix columns")
            }
        }
    }

    if (cluster) {
        if (is.numeric(split) && length(split) == 1) {
            if (is.null(dend_param$obj)) {
                if (verbose) qqcat("split @{which}s by cutree, apply hclust on the entire @{which}s\n")
                if (which == "row") {
                    dend_param$obj <- hclust(get_dist(mat, distance), method = method)
                } else {
                    dend_param$obj <- hclust(get_dist(t(mat), distance), method = method)
                }
            }
        }

        if (!is.null(dend_param$obj)) {
            if (km > 1) {
                stop_wrap("You can not perform k-means clustering since you have already specified a clustering object.")
            }

            if (inherits(dend_param$obj, "hclust")) {
                dend_param$obj <- as.dendrogram(dend_param$obj)
                if (verbose) qqcat("convert hclust object to dendrogram object\n")
            }

            if (is.null(split)) {
                dend_list <- list(dend_param$obj)
                order_list <- list(get_dend_order(dend_param$obj))
                if (verbose) qqcat("since you provided a clustering object and @{which}_split is null, the entrie clustering object is taken as an one-element list.\n")
            } else {
                if (length(split) > 1 || !is.numeric(split)) {
                    stop_wrap(qq("Since you specified a clustering object, you can only split @{which}s by providing a number (number of @{which} slices)."))
                }
                if (split < 2) {
                    stop_wrap(qq("`@{which}_split` should be >= 2."))
                }
                dend_param$split_by_cutree <- TRUE

                ct <- cut_dendrogram(dend_param$obj, split)
                dend_list <- ct$lower
                dend_slice <- ct$upper
                sth <- tapply(order.dendrogram(dend_param$obj),
                    rep(seq_along(dend_list), times = sapply(dend_list, nobs)),
                    function(x) x,
                    simplify = FALSE
                )
                attributes(sth) <- NULL
                order_list <- sth
                if (verbose) qqcat("cut @{which} dendrogram into @{split} slices.\n")
            }

            ### do reordering if specified
            if (identical(reorder, NULL)) {
                if (is.numeric(mat)) {
                    reorder <- TRUE
                } else {
                    reorder <- FALSE
                }
            }

            do_reorder <- TRUE
            if (identical(reorder, NA) || identical(reorder, FALSE)) {
                do_reorder <- FALSE
            }
            if (identical(reorder, TRUE)) {
                do_reorder <- TRUE
                if (which == "row") {
                    reorder <- -rowMeans(mat, na.rm = TRUE)
                } else {
                    reorder <- -colMeans(mat, na.rm = TRUE)
                }
            }

            if (do_reorder) {
                if (which == "row") {
                    if (length(reorder) != nrow(mat)) {
                        stop_wrap("weight of reordering should have same length as number of rows.\n")
                    }
                } else {
                    if (length(reorder) != ncol(mat)) {
                        stop_wrap("weight of reordering should have same length as number of columns\n")
                    }
                }

                for (i in seq_along(dend_list)) {
                    if (length(order_list[[i]]) > 1) {
                        sub_ind <- sort(order_list[[i]])
                        dend_list[[i]] <- reorder(dend_list[[i]], reorder[sub_ind], mean)
                        # the order of object@row_dend_list[[i]] is the order corresponding to the big dendrogram
                        order_list[[i]] <- order.dendrogram(dend_list[[i]])
                    }
                }
            }

            dend_list <- lapply(dend_list, adjust_dend_by_x)

            slot(object, paste0(which, "_order")) <- unlist(order_list)
            slot(object, paste0(which, "_order_list")) <- order_list
            slot(object, paste0(which, "_dend_list")) <- dend_list
            slot(object, paste0(which, "_dend_param")) <- dend_param
            slot(object, paste0(which, "_dend_slice")) <- dend_slice

            if (!is.null(split)) {
                if (is.null(attr(dend_list[[1]], ".class_label"))) {
                    split <- data.frame(rep(seq_along(order_list), times = sapply(order_list, length)))
                } else {
                    split <- data.frame(rep(sapply(dend_list, function(x) attr(x, ".class_label")), times = sapply(order_list, length)))
                }
                object@matrix_param[[paste0(which, "_split")]] <- split

                # adjust row_names_param$gp if the length of some elements is the same as row slices
                for (i in seq_along(names_param$gp)) {
                    if (length(names_param$gp[[i]]) == length(order_list)) {
                        gp_temp <- NULL
                        for (j in seq_along(order_list)) {
                            gp_temp[order_list[[j]]] <- names_param$gp[[i]][j]
                        }
                        names_param$gp[[i]] <- gp_temp
                    }
                }
                if (!is.null(names_param$anno)) {
                    names_param$anno@var_env$gp <- names_param$gp
                }
                slot(object, paste0(which, "_names_param")) <- names_param

                n_slice <- length(order_list)
                if (length(gap) == 1) {
                    gap <- rep(gap, n_slice)
                } else if (length(gap) == n_slice - 1) {
                    gap <- unit.c(gap, unit(0, "mm"))
                } else if (length(gap) != n_slice) {
                    stop_wrap(qq("Length of `gap` should be 1 or number of @{which} slices."))
                }
                object@matrix_param[[paste0(which, "_gap")]] <- gap # adjust title

                title <- slot(object, paste0(which, "_title"))
                if (!is.null(split)) {
                    if (length(title) == 0 && !is.null(title)) { ## default title
                        title <- apply(unique(split), 1, paste, collapse = ",")
                    } else if (length(title) == 1) {
                        if (grepl("%s", title)) {
                            title <- apply(unique(split), 1, function(x) {
                                lt <- lapply(x, function(x) x)
                                lt$fmt <- title
                                do.call(sprintf, lt)
                            })
                        } else if (grepl("@\\{.+\\}", title)) {
                            title <- apply(unique(split), 1, function(x) {
                                x <- x
                                envir <- environment()
                                title <- get("title")
                                op <- parent.env(envir)
                                calling_env <- object@heatmap_param$calling_env
                                parent.env(envir) <- calling_env
                                title <- GetoptLong::qq(title, envir = envir)
                                parent.env(envir) <- op
                                return(title)
                            })
                        } else if (grepl("\\{.+\\}", title)) {
                            if (!requireNamespace("glue")) {
                                stop_wrap("You need to install glue package.")
                            }
                            title <- apply(unique(split), 1, function(x) {
                                x <- x
                                envir <- environment()
                                title <- get("title")
                                op <- parent.env(envir)
                                calling_env <- object@heatmap_param$calling_env
                                parent.env(envir) <- calling_env
                                title <- glue::glue(title, envir = calling_env)
                                parent.env(envir) <- op
                                return(title)
                            })
                        }
                    }
                }
                slot(object, paste0(which, "_title")) <- title
            }
            return(object)
        }
    } else {
        if (verbose) qqcat("no clustering is applied/exists on @{which}s\n")
    }

    if (verbose) qq("clustering object is not pre-defined, clustering is applied to each @{which} slice\n")
    # make k-means clustering to add a split column
    consensus_kmeans <- function(mat, centers, km_repeats) {
        partition_list <- lapply(seq_len(km_repeats), function(i) {
            as.cl_hard_partition(kmeans(mat, centers, iter.max = 50))
        })
        partition_list <- cl_ensemble(list = partition_list)
        partition_consensus <- cl_consensus(partition_list)
        as.vector(cl_class_ids(partition_consensus))
    }
    if (km > 1 && is.numeric(mat)) {
        if (which == "row") {
            # km.fit = kmeans(mat, centers = km)
            # cl = km.fit$cluster
            cl <- consensus_kmeans(mat, km, km_repeats)
            meanmat <- lapply(sort(unique(cl)), function(i) {
                colMeans(mat[cl == i, , drop = FALSE], na.rm = TRUE)
            })
        } else {
            # km.fit = kmeans(t(mat), centers = km)
            # cl = km.fit$cluster
            cl <- consensus_kmeans(t(mat), km, km_repeats)
            meanmat <- lapply(sort(unique(cl)), function(i) {
                rowMeans(mat[, cl == i, drop = FALSE], na.rm = TRUE)
            })
        }

        meanmat <- do.call("cbind", meanmat)
        # if `reorder` is a vector, the slice dendrogram is reordered by the mean of reorder in each slice
        # or else, weighted by the mean of `meanmat`.
        if (length(reorder) > 1) {
            weight <- tapply(reorder, cl, mean)
        } else {
            weight <- colMeans(meanmat)
        }
        if (cluster_slices) {
            hc <- hclust(dist(t(meanmat)))
            hc <- as.hclust(reorder(as.dendrogram(hc), weight, mean))
        } else {
            hc <- list(order = order(weight))
        }

        cl2 <- numeric(length(cl))
        for (i in seq_along(hc$order)) {
            cl2[cl == hc$order[i]] <- i
        }
        cl2 <- factor(cl2, levels = seq_along(hc$order))

        if (is.null(split)) {
            split <- data.frame(cl2)
        } else if (is.matrix(split)) {
            split <- as.data.frame(split)
            split <- cbind(cl2, split)
        } else if (is.null(ncol(split))) {
            split <- data.frame(cl2, split)
        } else {
            split <- cbind(cl2, split)
        }
        if (verbose) qqcat("apply k-means (@{km} groups) on @{which}s, append to the `split` data frame\n")
    }

    # split the original order into a list according to split
    order_list <- list()
    if (is.null(split)) {
        order_list[[1]] <- order
    } else {
        if (verbose) cat("process `split` data frame\n")
        if (is.null(ncol(split))) split <- data.frame(split)
        if (is.matrix(split)) split <- as.data.frame(split)

        for (i in seq_len(ncol(split))) {
            if (is.numeric(split[[i]])) {
                split[[i]] <- factor(as.character(split[[i]]), levels = as.character(sort(unique(split[[i]]))))
            } else if (!is.factor(split[[i]])) {
                split[[i]] <- factor(split[[i]])
            } else {
                # re-factor
                split[[i]] <- factor(split[[i]], levels = intersect(levels(split[[i]]), unique(split[[i]])))
            }
        }

        split_name <- apply(as.matrix(split), 1, paste, collapse = ",")

        order2 <- do.call("order", split)
        level <- unique(split_name[order2])
        for (k in seq_along(level)) {
            l <- split_name == level[k]
            order_list[[k]] <- intersect(order, which(l))
        }
        names(order_list) <- level
    }

    slice_od <- seq_along(order_list)
    # make dend in each slice
    if (cluster) {
        if (verbose) qqcat("apply clustering on each slice (@{length(order_list)} slices)\n")
        dend_list <- rep(list(NULL), length(order_list))
        for (i in seq_along(order_list)) {
            if (which == "row") {
                submat <- mat[order_list[[i]], , drop = FALSE]
            } else {
                submat <- mat[, order_list[[i]], drop = FALSE]
            }
            nd <- 0
            if (which == "row") nd <- nrow(submat) else nd <- ncol(submat)
            if (nd > 1) {
                if (!is.null(dend_param$fun)) {
                    if (which == "row") {
                        obj <- dend_param$fun(submat)
                    } else {
                        obj <- dend_param$fun(t(submat))
                    }
                    if (inherits(obj, "dendrogram") || inherits(obj, "hclust")) {
                        dend_list[[i]] <- obj
                    } else {
                        oe <- try(obj <- as.dendrogram(obj), silent = TRUE)
                        if (inherits(oe, "try-error")) {
                            stop_wrap("the clustering function must return a `dendrogram` object or a object that can be coerced to `dendrogram` class.")
                        }
                        dend_list[[i]] <- obj
                    }
                    order_list[[i]] <- order_list[[i]][get_dend_order(dend_list[[i]])]
                } else {
                    if (which == "row") {
                        dend_list[[i]] <- hclust(get_dist(submat, distance), method = method)
                    } else {
                        dend_list[[i]] <- hclust(get_dist(t(submat), distance), method = method)
                    }
                    order_list[[i]] <- order_list[[i]][get_dend_order(dend_list[[i]])]
                    # }
                }
            } else {
                # a dendrogram with one leaf
                dend_list[[i]] <- structure(1, members = 1, height = 0, leaf = TRUE, class = "dendrogram")
                order_list[[i]] <- order_list[[i]][1]
            }
        }
        names(dend_list) <- names(order_list)

        for (i in seq_along(dend_list)) {
            if (inherits(dend_list[[i]], "hclust")) {
                dend_list[[i]] <- as.dendrogram(dend_list[[i]])
            }
        }

        if (identical(reorder, NULL)) {
            if (is.numeric(mat)) {
                reorder <- TRUE
            } else {
                reorder <- FALSE
            }
        }

        do_reorder <- TRUE
        if (identical(reorder, NA) || identical(reorder, FALSE)) {
            do_reorder <- FALSE
        }
        if (identical(reorder, TRUE)) {
            do_reorder <- TRUE
            if (which == "row") {
                reorder <- -rowMeans(mat, na.rm = TRUE)
            } else {
                reorder <- -colMeans(mat, na.rm = TRUE)
            }
        }

        if (do_reorder) {
            if (which == "row") {
                if (length(reorder) != nrow(mat)) {
                    stop_wrap("weight of reordering should have same length as number of rows\n")
                }
            } else {
                if (length(reorder) != ncol(mat)) {
                    stop_wrap("weight of reordering should have same length as number of columns\n")
                }
            }
            for (i in seq_along(dend_list)) {
                if (length(order_list[[i]]) > 1) {
                    sub_ind <- sort(order_list[[i]])
                    dend_list[[i]] <- reorder(dend_list[[i]], reorder[sub_ind], mean)
                    order_list[[i]] <- sub_ind[order.dendrogram(dend_list[[i]])]
                }
            }
            if (verbose) qqcat("reorder dendrograms in each @{which} slice\n")
        }

        if (length(order_list) > 1 && cluster_slices) {
            if (which == "row") {
                slice_mean <- sapply(order_list, function(ind) colMeans(mat[ind, , drop = FALSE], na.rm = TRUE))
            } else {
                slice_mean <- sapply(order_list, function(ind) rowMeans(mat[, ind, drop = FALSE], na.rm = TRUE))
            }
            if (!is.matrix(slice_mean)) {
                slice_mean <- matrix(slice_mean, nrow = 1)
            }
            dend_slice <- as.dendrogram(hclust(dist(t(slice_mean))))
            dend_slice <- reorder(dend_slice, slice_mean, mean)
            if (verbose) qqcat("perform clustering on mean of @{which} slices\n")

            slice_od <- order.dendrogram(dend_slice)
            order_list <- order_list[slice_od]
            dend_list <- dend_list[slice_od]
        }
    }

    dend_list <- lapply(dend_list, adjust_dend_by_x)

    slot(object, paste0(which, "_order")) <- unlist(order_list)
    slot(object, paste0(which, "_order_list")) <- order_list
    slot(object, paste0(which, "_dend_list")) <- dend_list
    slot(object, paste0(which, "_dend_param")) <- dend_param
    slot(object, paste0(which, "_dend_slice")) <- dend_slice
    object@matrix_param[[paste0(which, "_split")]] <- split

    if (which == "row") {
        if (nrow(mat) != length(order)) {
            stop_wrap(qq("Number of rows in the matrix are not the same as the length of the cluster or the @{which} orders."))
        }
    } else {
        if (ncol(mat) != length(order)) {
            stop_wrap(qq("Number of columns in the matrix are not the same as the length of the cluster or the @{which} orders."))
        }
    }

    # adjust names_param$gp if the length of some elements is the same as slices
    for (i in seq_along(names_param$gp)) {
        if (length(names_param$gp[[i]]) == length(order_list)) {
            gp_temp <- NULL
            for (j in seq_along(order_list)) {
                gp_temp[order_list[[j]]] <- names_param$gp[[i]][j]
            }
            names_param$gp[[i]] <- gp_temp
        }
    }
    if (!is.null(names_param$anno)) {
        names_param$anno@var_env$gp <- names_param$gp
    }
    slot(object, paste0(which, "_names_param")) <- names_param

    n_slice <- length(order_list)
    if (length(gap) == 1) {
        gap <- rep(gap, n_slice)
    } else if (length(gap) == n_slice - 1) {
        gap <- unit.c(gap, unit(0, "mm"))
    } else if (length(gap) != n_slice) {
        stop_wrap(qq("Length of `gap` should be 1 or number of @{which} slices."))
    }
    object@matrix_param[[paste0(which, "_gap")]] <- gap

    # adjust title
    title <- slot(object, paste0(which, "_title"))
    if (!is.null(split)) {
        if (length(title) == 0 && !is.null(title)) { ## default title
            title <- names(order_list)
        } else if (length(title) == 1) {
            if (grepl("%s", title)) {
                title <- apply(unique(split[order2, , drop = FALSE]), 1, function(x) {
                    lt <- lapply(x, function(x) x)
                    lt$fmt <- title
                    do.call(sprintf, lt)
                })[slice_od]
            } else if (grepl("@\\{.+\\}", title)) {
                title <- apply(unique(split[order2, , drop = FALSE]), 1, function(x) {
                    x <- x
                    envir <- environment()
                    title <- get("title")
                    op <- parent.env(envir)
                    calling_env <- object@heatmap_param$calling_env
                    parent.env(envir) <- calling_env
                    title <- GetoptLong::qq(title, envir = envir)
                    parent.env(envir) <- op
                    return(title)
                })[slice_od]
            } else if (grepl("\\{.+\\}", title)) {
                if (!requireNamespace("glue")) {
                    stop_wrap("You need to install glue package.")
                }
                title <- apply(unique(split[order2, , drop = FALSE]), 1, function(x) {
                    x <- x
                    envir <- environment()
                    title <- get("title")
                    op <- parent.env(envir)
                    calling_env <- object@heatmap_param$calling_env
                    parent.env(envir) <- calling_env
                    title <- glue::glue(title, envir = calling_env)
                    parent.env(envir) <- op
                    return(title)
                })[slice_od]
            }
        }
    }
    slot(object, paste0(which, "_title")) <- title
    # check whether height of the dendrogram is zero
    # if(all(sapply(dend_list, dend_heights) == 0)) {
    #     slot(object, paste0(which, "_dend_param"))$show = FALSE
    # }
    return(object)
}
