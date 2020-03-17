
#' @export
`[.icu_tbl` <- function(x, ...) {
  reclass_tbl(NextMethod(), tbl_meta(x))
}

#' @export
dimnames.icu_tbl <- function(x) list(NULL, colnames(x))

#' @export
print.icu_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.icu_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- prt::trunc_dt(x, n = n, width = width, n_extra = n_extra)
  format(mat)
}

#' @export
str.icu_tbl <- function(object, ...) {
  invisible(prt::str_dt(object, ...))
}

#' @importFrom data.table as.data.table
#' @method as.data.table icu_tbl
#'
#' @export
#'
as.data.table.icu_tbl <- function(x, ...) {

  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  unclass_tbl(x)

  x
}

#' @method as.data.frame icu_tbl
#'
#' @export
#'
as.data.frame.icu_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) warning("Ignoring `row.names` argument.")
  if (!isFALSE(optional)) warning("Ignoring `optional` argument.")
  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  data.table::setDF(as.data.table(x))

  x
}

#' @export
.cbind.icu_tbl <- function(..., keep.rownames = FALSE, check.names = FALSE,
                          key = NULL, stringsAsFactors = FALSE) {

  lst <- list(...)
  check <- vapply(lst, is_icu_tbl, logical(1L))

  if (sum(check) == 1L) {
    hit <- which(check)
    lst <- c(lst[hit], lst[-hit])
    meta <- tbl_meta(lst[[hit]])
  } else {
    meta <- NULL
  }

  res <- do.call(data.table::data.table,
    c(lst, list(keep.rownames = keep.rownames, check.names = check.names,
                key = key, stringsAsFactors = stringsAsFactors))
  )

  if (!is.null(meta)) {
    reclass_tbl(res, meta)
  } else {
    res
  }
}

#' @export
.rbind.icu_tbl <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  rbind_lst(list(...), use.names = use.names, fill = fill, idcol = idcol)
}

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(cbind, icu_tbl) }
cbind.icu_tbl <- .cbind.icu_tbl

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(rbind, icu_tbl) }
rbind.icu_tbl <- .rbind.icu_tbl

#' @export
split.icu_tbl <- function(x, ...) {
  lapply(NextMethod(), reclass_tbl, tbl_meta(x))
}

#' @export
merge.icu_tbl <- function(x, y, by = meta_cols(x), ...) {

  if (is_icu_tbl(y)) {

    y <- data.table::copy(y)

    if (is_ts_tbl(y)) {
      y <- make_compatible(y, x, id(x) %in% by, index(x) %in% by)
    } else if (is_id_tbl(y)) {
      y <- make_compatible(y, x, id(x) %in% by)
    }
  }

  reclass_tbl(NextMethod(), tbl_meta(x))
}