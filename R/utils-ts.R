#' Time series utility functions
#'
#' ICU data as handled by `ricu` is mostly comprised of time series data and as
#' such, several utility functions are available for working with time series
#' data in addition to a class dedicated to representing time series data (see
#' [ts_tbl()]). Some terminology to begin with: a time series is considered
#' to have gaps if, per (combination of) ID variable value(s), some time steps
#' are missing. Expanding and collapsing mean to change between
#' representations where time steps are explicit or encoded as interval with
#' start and end times. For sliding window-type operations, `slide()` means to
#' iterate over time-windows, `slide_index()` means to iterate over certain
#' time-windows, selected relative to the index and `hop()` means to iterate
#' over time-windows selected in absolute terms.
#'
#' @details
#' A gap in a `ts_tbl` object is a missing time step, i.e. a missing entry in
#' the sequence `seq(min(index), max(index), by = interval)` in at least one
#' group (as defined by [id_vars()], where the extrema are calculated per
#' group. In this case, `has_gaps()` will return `TRUE`. The function
#' `is_regular()` checks whether the time series has no gaps, in addition to
#' the object being sorted and unique (see [is_sorted()] and [is_unique()]).
#' In order to transform a time series containing gaps into a regular time
#' series, `fill_gaps()` will fill missing time steps with `NA` values in all
#' [data_vars()] columns, while `remove_gaps()` provides the inverse operation
#' of removing time steps that consist of `NA` values in [data_vars()] columns.
#'
#' An `expand()` operation performed on an object inheriting from `data.table`
#' yields a `ts_tbl` where time-steps encoded by columns `start_var` and
#' `end_var` are made explicit with values in `keep_vars` being appropriately
#' repeated. The inverse operation is available as `collapse()`, which groups
#' by `id_vars`, represents `index_var` as group-wise extrema in two new
#' columns `start_var` and `end_var` and allows for further data summary using
#' `...`. An aspect to keep in mind when applying `expand()` to a `win_tbl`
#' object is that values simply are repeated for all time-steps that fall into
#' a given validity interval. This gives correct results when a `win_tbl` for
#' example contains data on infusions as rates, but might not lead to correct
#' results when infusions are represented as drug amounts administered over a
#' given time-span. In such a scenario it might be desirable to evenly
#' distribute the total amount over the corresponding time steps (currently not
#' implemented).
#'
#' Sliding-window type operations are available as `slide()`, `slide_index()`
#' and `hop()` (function naming is inspired by the CRAN package `slider`). The
#' most flexible of the three, `hop` takes as input a `ts_tbl` object `x`
#' containing the data, an `id_tbl` object `windows`, containing for each ID
#' the desired windows represented by two columns `lwr_col` and `upr_col`, as
#' well as an expression `expr` to be evaluated per window. At the other end
#' of the spectrum, `slide()` spans windows for every ID and available
#' time-step using the arguments `before` and `after`, while `slide_index()`
#' can be seen as a compromise between the two, where windows are spanned for
#' certain time-points, specified by `index`.
#'
#' @param x `ts_tbl` object to use
#' @param start_var,end_var Name of the columns that represent lower and upper
#' windows bounds
#' @param step_size Controls the step size used to interpolate between
#' `start_var` and `end_var`
#' @param new_index Name of the new index column
#' @param keep_vars Names of the columns to hold onto
#' @param aggregate Function for aggregating values in overlapping intervals
#'
#' @return Most functions return `ts_tbl` objects with the exception of
#' `has_gaps()`/`has_no_gaps()`/`is_regular()`, which return logical flags.
#'
#' @examples
#' if (FALSE) {
#' tbl <- ts_tbl(x = 1:5, y = hours(1:5), z = hours(2:6), val = rnorm(5),
#'               index_var = "y")
#' exp <- expand(tbl, "y", "z", step_size = 1L, new_index = "y",
#'               keep_vars = c("x", "val"))
#' col <- collapse(exp, start_var = "y", end_var = "z", val = unique(val))
#' all.equal(tbl, col, check.attributes = FALSE)
#'
#' tbl <- ts_tbl(x = rep(1:5, 1:5), y = hours(sequence(1:5)), z = 1:15)
#'
#' win <- id_tbl(x = c(3, 4), a = hours(c(2, 1)), b = hours(c(3, 4)))
#' hop(tbl, list(z = sum(z)), win, lwr_col = "a", upr_col = "b")
#' slide_index(tbl, list(z = sum(z)), hours(c(4, 5)), before = hours(2))
#' slide(tbl, list(z = sum(z)), before = hours(2))
#'
#' tbl <- ts_tbl(x = rep(3:4, 3:4), y = hours(sequence(3:4)), z = 1:7)
#' has_no_gaps(tbl)
#' is_regular(tbl)
#'
#' tbl[1, 2] <- hours(2)
#' has_no_gaps(tbl)
#' is_regular(tbl)
#'
#' tbl[6, 2] <- hours(2)
#' has_no_gaps(tbl)
#' is_regular(tbl)
#' }
#'
#' @rdname ts_utils
#' @export
#'
expand <- function(x, start_var = index_var(x), end_var = NULL,
                   step_size = time_step(x), new_index = start_var,
                   keep_vars = NULL, aggregate = FALSE) {

  do_seq <- function(min, max) seq(min, max, step_size)

  seq_expand <- function(min, max, unit, extra) {
    lst <- Map(do_seq, min, max)
    ind <- rep(seq_along(lst), lengths(lst))
    set(extra[ind, ], j = new_index,
        value = as.difftime(unlist(lst), units = unit))
  }

  assert_that(is_dt(x), is_scalar(step_size), is.numeric(step_size))

  start_var <- coalesce(start_var, if (is_ts_tbl(x)) index_var(x), "start")
  end_var   <- coalesce(end_var, if (is_win_tbl(x)) new_names(x), "end")
  time_unit <- units(x[[start_var]])
  interval  <- as.difftime(step_size, units = time_unit)
  data_var  <- data_vars(x)

  assert_that(is.string(start_var), is.string(end_var), is.string(new_index))

  if (is.null(keep_vars)) {
    keep_vars <- id_vars(x)
    if (is_win_tbl(x)) {
      keep_vars <- c(keep_vars, data_var)
    }
  }

  if (identical(nrow(x), 0L)) {

    if (is_win_tbl(x)) {
      x <- as_ts_tbl(x, index_var = new_index, interval = interval,
                     by_ref = TRUE)
    }

    x <- rm_cols(x, setdiff(colnames(x), c(keep_vars, new_index)),
                 by_ref = TRUE)

    return(x)
  }

  if (is_win_tbl(x) && !end_var %in% colnames(x)) {

    on.exit(rm_cols(x, end_var, by_ref = TRUE))

    dura_var <- dur_var(x)

    x <- x[, c(end_var) := re_time(get(start_var) + get(dura_var), interval)]
    x <- x[get(end_var) < 0, c(end_var) := as.difftime(0, units = time_unit)]
  }

  assert_that(has_time_cols(x, c(start_var, end_var), 2L),
              same_unit(x[[start_var]], x[[end_var]]))

  res <- rm_na(x, c(start_var, end_var), "any")
  res <- res[get(start_var) <= get(end_var), c(keep_vars, start_var, end_var),
             with = FALSE]
  res <- res[, c(start_var, end_var) := lapply(.SD, as.double),
             .SDcols = c(start_var, end_var)]

  res <- res[, seq_expand(get(start_var), get(end_var), time_unit, .SD),
             .SDcols = keep_vars]

  res <- as_ts_tbl(res, index_var = new_index, interval = interval,
                   by_ref = TRUE)

  if (!isFALSE(aggregate)) {
    res <- stats::aggregate(res, aggregate)
  }

  res
}

#' @param id_vars,index_var ID and index variables
#' @param env Environment used as parent to the environment used to evaluate
#' expressions passes as `...`
#' @param as_win_tbl Logical flag indicating whether to return a `win_tbl` or
#' an `id_tbl`
#'
#' @rdname ts_utils
#' @export
#'
collapse <- function(x, id_vars = NULL, index_var = NULL, start_var = "start",
                     end_var = "end", env = NULL, as_win_tbl = TRUE, ...) {

  id_vars   <- coalesce(id_vars,   id_vars(x))
  index_var <- coalesce(index_var, index_var(x))

  if (is.null(env)) {
    env <- caller_env()
  }

  assert_that(is_dt(x), has_cols(x, id_vars), has_time_cols(x, index_var, 1L),
              is.string(start_var), is.string(end_var), is.environment(env))

  expr <- substitute(list(min(get(ind)), max(get(ind)), ...),
                     env = list(ind = index_var))

  names(expr)[c(2L, 3L)] <- c(start_var, end_var)

  res <- do.call(`[`,
    list(x, substitute(), do.call(substitute, list(substitute(expr))),
         by = id_vars),
    envir = env
  )

  if (isTRUE(as_win_tbl)) {
    res <- res[, c(end_var) := get(end_var) - get(start_var)]
    res <- as_win_tbl(res, index_var = start_var, dur_var = end_var,
                      by_ref = TRUE)
  }

  res
}

#' @rdname ts_utils
#' @export
#'
has_no_gaps <- function(x) {

  check_time_col <- function(time, step) {
    ext <- range(time, na.rm = TRUE)
    setequal(time, seq(ext[1L], ext[2L], by = step))
  }

  assert_that(is_ts_tbl(x))

  id_cols <- id_vars(x)

  expr <- substitute(
    check_time_col(as.double(get(idx)), step),
    env = list(idx = index_var(x), step = time_step(x))
  )

  res <- x[, eval(expr), by = c(id_cols)]

  all(res[[setdiff(colnames(res), id_cols)]])
}

#' @rdname ts_utils
#' @export
#'
has_gaps <- Negate(has_no_gaps)

#' @rdname ts_utils
#' @export
#'
is_regular <- function(x) {

  check_time_col <- function(time, step) {
    len <- length(time)
    if (len < 2L) TRUE
    else identical(len - 1, as.numeric((time[len] - time[1L]) / step))
  }

  assert_that(is_ts_tbl(x))

  if (!is_unique(x) || !is_sorted(x)) {
    return(FALSE)
  }

  id_cols <- id_vars(x)

  expr <- substitute(
    check_time_col(get(idx), step),
    env = list(idx = index_var(x), step = time_step(x))
  )

  res <- x[, eval(expr), by = c(id_cols)]

  all(res[[setdiff(colnames(res), id_cols)]])
}

#' @param limits A table with columns for lower and upper window bounds or a
#' length 2 difftime vector
#'
#' @rdname ts_utils
#' @export
#'
fill_gaps <- function(x, limits = collapse(x), start_var = "start",
                      end_var = "end") {

  assert_that(is_unique(x))

  if (is_difftime(limits)) {

    assert_that(has_length(limits, 2L))

    units(limits) <- time_unit(x)

    limits <- unique(x[, id_vars(x), with = FALSE])[,
      c(start_var, end_var) := as.list(limits)
    ]
  }

  if (is_id_tbl(limits)) {
    id_vars <- id_vars(limits)
  } else {
    id_vars <- id_vars(x)
  }

  join <- expand(limits, start_var, end_var, step_size = time_step(x),
                 new_index = index_var(x), keep_vars = id_vars)

  x[unique(join), on = paste(meta_vars(x), "==", meta_vars(join))]
}

#' @rdname ts_utils
#' @export
remove_gaps <- function(x) rm_na(x, data_vars(x), "all")

#' @param expr Expression (quoted for `*_quo` and unquoted otherwise) to be
#' evaluated over each window
#' @param before,after Time span to look back/forward
#' @param ... Passed to `hop_quo()` and ultimately to
#' [`data.table::[()`][data.table]
#'
#' @rdname ts_utils
#' @export
#'
slide <- function(x, expr, before, after = hours(0L), ...) {

  assert_that(is_scalar(before), is_difftime(before),
              is_scalar(after), is_difftime(after))

  id_cols <- id_vars(x)
  ind_col <- index_var(x)
  interva <- interval(x)
  time_ut <- time_unit(x)

  units(before) <- time_ut
  units(after)  <- time_ut

  join <- x[,
    c(mget(id_cols), list(min_time = get(ind_col) - before,
                          max_time = get(ind_col) + after))
  ]

  if (before == 0) {
    msg <- NULL
  } else {
    msg <- "Using 'by reference' syntax in expressions passed to `slide()`
            might yield undesired results if `before` > 0."
  }

  res <- hopper(x, {{ expr }}, join, lwr_col = "min_time",
                upr_col = "max_time", warn_msg = msg, ...)

  if (!is_ts_tbl(res)) {
    res <- set(res, j = ind_col, value = res[["min_time"]] + before)
    res <- as_ts_tbl(res, index_var = ind_col, interval = interva,
                     by_ref = TRUE)
  }

  res <- rm_cols(res, c("min_time", "max_time"), skip_absent = TRUE,
                 by_ref = TRUE)

  res
}

#' @param index A vector of times around which windows are spanned (relative
#' to the index)
#'
#' @rdname ts_utils
#' @export
#'
slide_index <- function(x, expr, index, before, after = hours(0L), ...) {

  assert_that(is_difftime(index), has_length(index), is_unique(index),
              is_scalar(before), is_difftime(before),
              is_scalar(after),  is_difftime(after))

  id_cols <- id_vars(x)
  ind_col <- index_var(x)
  interva <- interval(x)
  time_ut <- time_unit(x)

  units(before) <- time_ut
  units(after)  <- time_ut

  join <- x[, list(min_time = index - before,
                   max_time = index + after), by = c(id_cols)]

  res <- hopper(x, {{ expr }}, join, lwr_col = "min_time",
                upr_col = "max_time", warn_msg = "
                Using 'by reference' syntax in expressions passed to
                `slide_index()` most likely will not yield the desired
                results.", ...)

  if (!is_ts_tbl(res)) {
    res <- set(res, j = ind_col, value = res[["min_time"]] + before)
    res <- as_ts_tbl(res, index_var = ind_col, interval = interva,
                     by_ref = TRUE)
  }

  res <- rm_cols(res, c("min_time", "max_time"), skip_absent = TRUE,
                 by_ref = TRUE)

  res
}

#' @param windows An `icu_tbl` defining the windows to span
#' @param full_window Logical flag controlling how the situation is handled
#' where the sliding window extends beyond available data
#' @param lwr_col,upr_col Names of columns (in `windows`) of lower/upper
#' window bounds
#' @param left_closed,right_closed Logical flag indicating whether intervals
#' are closed (default) or open.
#' @param eval_env Environment in which `expr` is substituted; `NULL` resolves
#' to the environment in which `expr` was created
#'
#' @importFrom rlang enexpr quo_get_expr quo_get_env caller_env is_quosure
#'
#' @rdname ts_utils
#' @export
#'
hop <- function(x, expr, windows, full_window = FALSE,
                lwr_col = "min_time", upr_col = "max_time",
                left_closed = TRUE, right_closed = TRUE,
                eval_env = NULL, ...) {

  msg <- "Using 'by reference' syntax in expressions passed to `hop()` most
          likely will not yield the desired results."

  hopper(x, {{ expr }}, windows, full_window, lwr_col, upr_col,
         left_closed, right_closed, eval_env, msg, ...)
}

hopper <- function(x, expr, windows, full_window = FALSE,
                   lwr_col = "min_time", upr_col = "max_time",
                   left_closed = TRUE, right_closed = TRUE,
                   eval_env = NULL, warn_msg = FALSE, ...) {

  assert_that(is_ts_tbl(x), is_unique(x), is.flag(full_window),
              is_id_tbl(windows), has_name(windows, c(lwr_col, upr_col)))

  if (identical(nrow(x), 0L)) {
    return(x)
  }

  win_id <- id_vars(windows)
  tbl_id <- id_vars(x)

  orig_unit <- time_unit(x)
  win_cols  <- c(lwr_col, upr_col)

  windows <- windows[, c(win_cols) := lapply(.SD, `units<-`, orig_unit),
                     .SDcols = win_cols]

  tbl_ind <- index_var(x)

  if (full_window) {

    extremes <- x[, list(grp_min = min(get(tbl_ind)),
                         grp_max = max(get(tbl_ind))),
                  by = tbl_id]

    join <- c(paste(tbl_id, "==", win_id), paste("grp_min <=", lwr_col),
                                           paste("grp_max >=", upr_col))

    windows <- extremes[windows, on = join, nomatch = NULL]
    windows <- rename_cols(windows, c(win_id, lwr_col, upr_col), by_ref = TRUE)
    windows <- as_id_tbl(windows, win_id, by_ref = TRUE)
  }

  tmp_col <- new_names(x, n = 2L)
  x <- x[, c(tmp_col) := list(get(tbl_ind), get(tbl_ind))]
  on.exit(rm_cols(x, tmp_col, skip_absent = TRUE, by_ref = TRUE))

  join <- c(paste(tbl_id, "==", win_id),
            paste(tmp_col[2L], if (right_closed) "<=" else "<", upr_col),
            paste(tmp_col[1L], if (left_closed)  ">=" else ">", lwr_col))

  quo <- enexpr(expr)

  if (is_quosure(quo)) {
    env <- coalesce(eval_env, quo_get_env(quo))
    exp <- quo_get_expr(quo)
  } else {
    env <- coalesce(eval_env, caller_env(2))
    exp <- quo
  }

  if (not_null(warn_msg) && identical(exp[[1L]], as.symbol(":="))) {
    warn_ricu(warn_msg, class = "by_ref_slide")
  }

  res <- do.call(`[`, c(
    list(x, windows, substitute(exp), on = join, by = quote(.EACHI)),
    list(...)
  ), envir = env)

  rename_cols(res, win_cols, tmp_col, by_ref = TRUE)
}

#' Utilities for `difftime`
#'
#' As [base::difftime()] vectors are used throughout `ricu`, a set of wrapper
#' functions are exported for convenience of instantiation [base::difftime()]
#' vectors with given time units.
#'
#' @param ... Numeric vector to coerce to [base::difftime()]
#'
#' @return Vector valued time differences as `difftime` object.
#'
#' @examples
#' hours(1L)
#' mins(NA_real_)
#' secs(1:10)
#' hours(numeric(0L))
#'
#' @rdname difftime
#' @export
#'
secs <- function(...) as.difftime(c(...), units = "secs")

#' @rdname difftime
#' @export
#'
mins <- function(...) as.difftime(c(...), units = "mins")

#' @rdname difftime
#' @export
#'
hours <- function(...) as.difftime(c(...), units = "hours")

#' @rdname difftime
#' @export
#'
days <- function(...) as.difftime(c(...), units = "days")

#' @rdname difftime
#' @export
#'
weeks <- function(...) as.difftime(c(...), units = "weeks")

is_one_min <- function(x) all_equal(x, mins(1L))

is_difftime <- is_type("difftime")

re_time <- function(x, interval) {
  round_to(`units<-`(x, units(interval)), as.double(interval))
}

locf <- function(x) {

  res <- last_elem(x)

  if (is.na(res)) {

    no_na <- !is.na(x)

    if (any(no_na)) {
      res <- last_elem(x[no_na])
    }
  }

  res
}

merge_ranges <- function(x, lwr_var = index_var(x), upr_var = data_vars(x),
                         max_gap = 0, by_ref = FALSE) {

  assert_that(is_dt(x), has_col(x, lwr_var), has_col(x, upr_var),
              is_scalar(max_gap), max_gap == 0 || is_interval(max_gap))

  tmp_var <- paste0("i.", upr_var)

  assert_that(!has_col(x, tmp_var))

  if (!isTRUE(by_ref)) {
    x <- copy(x)
  }

  if (max_gap != 0) {
    x <- x[, c(upr_var) := get(upr_var) + max_gap]
  }

  ptype <- as_ptype(x)

  x <- sort(x, by = c(id_vars(x), lwr_var, upr_var), by_ref = TRUE)
  x <- as.data.table(x, by_ref = TRUE)
  x <- reclass_tbl(data.table::foverlaps(x, x, mult = "first"), ptype)

  expr <- quote(list(max(get(tmp_var))))
  names(expr) <- c("", upr_var)

  x <- x[, eval(expr), by = c(meta_vars(x))]

  if (max_gap != 0) {
    x <- x[, c(upr_var) := get(upr_var) - max_gap]
  }

  x
}

group_measurements <- function(x, max_gap = hours(6L), group_var = "grp_var") {

  grp_calc  <- function(x) {

    if (length(x) == 0L) {
      return(integer())
    }

    tmp <- rle(x <= max_gap)

    val <- tmp[["values"]]
    len <- tmp[["lengths"]]

    len[ val] <- len[ val] + 1L
    len[!val] <- len[!val] - 1L

    if (!val[1L]) len[1L] <- len[1L] + 1L

    res <- rep.int(ifelse(val, len, 1L), ifelse(val, 1L, len))
    rep(seq_along(res), res)
  }

  assert_that(is_ts_tbl(x), is_interval(max_gap), is_scalar(max_gap),
              is.string(group_var), is_disjoint(group_var, colnames(x)))

  id_vars   <- id_vars(x)
  index_var <- index_var(x)

  if (interval(x) > max_gap) {
    warn_ricu("splitting durations by gaps of length {format(max_gap)}
               using data with a time resolution of {format(interval(x))}")
  }

  x <- x[, c(group_var) := padded_diff(get(index_var), Inf),
         by = c(id_vars)]
  x <- x[, c(group_var) := grp_calc(get(group_var))]

  x
}

padded_diff <- function(x, final) c(diff(x), final)

padded_capped_diff <- function(x, final, max) {
  res <- padded_diff(x, final)
  res[res > max] <- final
  res
}

trunc_time <- function(x, min, max) {

  if (not_null(min)) {
    x <- replace(x, x < min, min)
  }

  if (not_null(max)) {
    x <- replace(x, x > max, max)
  }

  x
}

create_intervals <- function(x, by_vars = id_vars(x), overhang = hours(1L),
                             max_len = hours(6L), end_var = "endtime") {

  assert_that(is_ts_tbl(x), has_cols(x, by_vars),
              is_interval(overhang), is_scalar(overhang),
              is_interval(max_len), is_scalar(max_len),
              is.string(end_var), is_disjoint(end_var, colnames(x)))

  idx_var <- index_var(x)
  inteval <- interval(x)

  x <- x[, c(end_var) := padded_diff(get(idx_var), overhang), by = c(by_vars)]
  x <- x[, c(end_var) := `units<-`(
         trunc_time(get(end_var), 0L, max_len) - inteval, units(inteval))]
  x <- x[, c(end_var) := get(idx_var) + get(end_var)]

  x
}

min_time_unit <- function(x) {

  assert_that(is.character(x))

  if (any(x == "secs")) "secs"
  else if (any(x == "mins")) "mins"
  else if (any(x == "hours")) "hours"
  else if (any(x == "days")) "days"
  else if (any(x == "weeks")) "weeks"
  else stop_ricu("unknown time units", class = "unknown_time_unit")
}

as_interval <- function(x, length = 1L) {

  x <- as.difftime(x)

  assert_that(is_interval(x, length))

  x
}
