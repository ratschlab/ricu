collect_dots <- function(concepts, interval, ..., merge_dat = FALSE) {

  assert_that(is.character(concepts))

  dots <- list(...)

  if (length(concepts) == 1L) {

    assert_that(identical(length(dots), 1L))

    res <- dots[[1L]]

    if (is_ts_tbl(res)) {
      ival <- coalesce(interval, interval(res))
      assert_that(has_interval(res, ival))
    } else {
      assert_that(is_df(res))
    }

    return(res)
  }

  if (length(dots) == 1L) {
    dots <- dots[[1L]]
  }

  if (is.null(names(dots))) {
    names(dots) <- concepts
  }

  if (not_null(names(concepts))) {
    concepts <- chr_ply(concepts, grep, names(dots), value = TRUE,
                        use_names = TRUE)
  }

  assert_that(setequal(names(dots), concepts))

  res <- dots[concepts]

  assert_that(all_map(has_col, res, concepts))

  if (not_null(names(concepts))) {
    names(res) <- names(concepts)
  }

  ival <- check_interval(res, interval)

  if (merge_dat) {
    res <- reduce(merge, res, all = TRUE)
  } else {
    attr(res, "ival_checked") <- ival
  }

  res
}

check_interval <- function(dat, ival = NULL) {

  check_ival <- function(x, iv) {
    is_df(x) && (!is_ts_tbl(x) || has_interval(x, iv))
  }

  if (has_attr(dat, "ival_checked")) {

    ival <- attr(dat, "ival_checked")

  } else if (is_ts_tbl(dat)) {

    if (is.null(ival)) {
      ival <- interval(dat)
    } else {
      assert_that(has_interval(dat, ival))
    }

  } else if (is_df(dat) || all_fun(dat, Negate(is_ts_tbl))) {

    ival <- NULL

  } else {

    if (is.null(ival)) {
      for (x in dat) {
        if (is_ts_tbl(x)) {
          ival <- interval(x)
          break
        }
      }
    }

    assert_that(all_fun(dat, check_ival, ival))
  }

  invisible(ival)
}

rename_data_var <- function(new_name, old_name = NULL) {
  function(...) rename_cols(..1, new_name, coalesce(old_name, data_var(..1)))
}

#' Concept callback functions
#'
#' Owing to increased complexity and more diverse applications, recursive
#' concepts (class [`rec_cncpt`][new_cncpt()]) may specify callback functions
#' to be called on corresponding data objects and perform post-processing
#' steps.
#'
#' @details
#' Several concept callback functions are exported, mainly for documenting
#' their arguments, as default values oftentimes represent somewhat arbitrary
#' choices and passing non-default values might be of interest for
#' investigating stability with respect to such choices. Furthermore, default
#' values might not be ideal for some datasets and/or analysis tasks.
#'
#' ## `pafi`
#' In order to calculate the PaO\subs{2}/FiO\subs{2} (or Horowitz index), for
#' a given time point, both a PaO\subs{2} and an FiO\subs{2} measurement is
#' required. As the two are often not measured at the same time, some form of
#' imputation or matching procedure is required. Several options are available:
#'
#' * `match_vals` allows for a time difference of maximally `match_win`
#'   between two measurements for calculating their ratio
#' * `extreme_vals` uses the worst PaO\subs{2} and FiO\subs{2} values within
#'   the time window spanned by `match_win`
#' * `fill_gaps` represents a variation of `extreme_vals`, where ratios are
#'   evaluated at every time-point as specified by `interval`as opposed to
#'   only the time points where either a PaO\subs{2} or an FiO\subs{2}
#'   measurement is available
#'
#' Finally, `fix_na_fio2` imputes all remaining missing FiO\subs{2} with 21,
#' the percentage (by volume) of oxygen in (tropospheric) air.
#'
#' ## `vent_ind`
#' Building on the atomic concepts `vent_start` and `vent_end`, `vent_ind`
#' determines time windows during which patients are mechanically ventilated
#' by combining start and end events that are separated by at most `match_win`
#' and at least `min_length`. Durations are represented by the `dur_var` column
#' in the returned `win_tbl` and the `data_var` column simply indicates the
#' ventilation status with `TRUE` values. Currently, no clear distinction
#' between invasive an non-invasive ventilation is made.
#'
#' ## `sed_gcs`
#' In order to construct an indicator for patient sedation (used within the
#' context of `gcs`), information from the two concepts `ett_gcs` and `rass` is
#' pooled: A patient is considered sedated if intubated or has less or equal to
#' -2 on the Richmond Agitation-Sedation Scale.
#'
#' ## `gcs`
#' Aggregating components of the Glasgow Coma Scale into a total score
#' (whenever the total score `tgcs` is not already available) requires
#' coinciding availability of an eye (`egcs`), verbal (`vgcs`) and motor
#' (`mgcs`) score. In order to match values, a last observation carry forward
#' imputation scheme over the time span specified by `valid_win` is performed.
#' Furthermore passing `"max"` as `sed_impute` will assume maximal points for
#' time steps where the patient is sedated (as indicated by `sed_gcs`), while
#' passing `"prev"`, will assign the last observed value previous to the
#' current sedation window and finally passing `FALSE` will in turn use raw
#' values. Finally, passing `TRUE` as `set_na_max` will assume maximal points
#' for missing values (after matching and potentially applying `sed_impute`).
#'
#' ## `urine24`
#' Single urine output events are aggregated into a 24 hour moving window sum.
#' At default value of `limits = NULL`, moving window evaluation begins with
#' the first and ends with the last available measurement. This can however be
#' extended by passing an `id_tbl` object, such as for example returned by
#' [stay_windows()] to full stay windows. In order to provide data earlier
#' than 24 hours before the evaluation start point, `min_win` specifies the
#' minimally required data window and the evaluation scheme is adjusted for
#' shorter than 24 hour windows.
#'
#' ## `vaso60`
#' Building on concepts for drug administration rate and drug administration
#' durations, administration events are filtered if they do not fall into
#' administrations windows of at least 1h. The `max_gap` argument can be used
#' to control how far apart windows can be in order to be merged (negative
#' times are possible as well, meaning that even overlapping windows can be
#' considered as individual windows).
#'
#' @param ... Data input used for concept calculation
#' @param match_win Time-span during which matching of values is allowed
#' @param mode Method for matching PaO\subs{2} and FiO\subs{2} values
#' @param fix_na_fio2 Logical flag indicating whether to impute missing
#' FiO\subs{2} values with 21
#' @param interval Expected time series step size (determined from data if
#' `NULL`)
#'
#' @return Either an `id_tbl` or `ts_tbl` depending on the type of concept.
#'
#' @encoding UTF-8
#' @rdname callback_cncpt
#' @export
#'
pafi <- function(..., match_win = hours(2L),
                 mode = c("match_vals", "extreme_vals", "fill_gaps"),
                 fix_na_fio2 = TRUE, interval = NULL) {

  mode <- match.arg(mode)

  assert_that(is.flag(fix_na_fio2))

  cnc <- c("po2", "fio2")
  res <- collect_dots(cnc, interval, ...)
  res <- match_fio2(res, match_win, mode, if (fix_na_fio2) cnc[2L] else NULL)

  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])) & get(cnc[2L]) != 0, ]
  res <- res[, c("pafi") := 100 * get(cnc[1L]) / get(cnc[2L])]
  res <- rm_cols(res, cnc)

  res
}

#' @rdname callback_cncpt
#' @export
safi <- function(..., match_win = hours(2L),
                 mode = c("match_vals", "extreme_vals", "fill_gaps"),
                 fix_na_fio2 = TRUE, interval = NULL) {

  mode <- match.arg(mode)

  assert_that(is.flag(fix_na_fio2))

  cnc <- c("o2sat", "fio2")
  res <- collect_dots(cnc, interval, ...)
  res <- match_fio2(res, match_win, mode, if (fix_na_fio2) cnc[2L] else NULL)

  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])) & get(cnc[2L]) != 0, ]
  res <- res[, c("safi") := 100 * get(cnc[1L]) / get(cnc[2L])]
  res <- rm_cols(res, cnc)

  res
}

match_fio2 <- function(x, match_win, mode, fio2 = NULL) {

  match_win <- as_interval(match_win)

  assert_that(match_win > check_interval(x))

  if (identical(mode, "match_vals")) {

    on12 <- paste(meta_vars(x[[1L]]), "==", meta_vars(x[[2L]]))
    on21 <- paste(meta_vars(x[[2L]]), "==", meta_vars(x[[1L]]))

    x <- rbind(
      x[[1L]][x[[2L]], on = on12, roll = match_win],
      x[[2L]][x[[1L]], on = on21, roll = match_win]
    )
    x <- unique(x)

  } else {

    x <- reduce(merge, x, all = TRUE)

    if (identical(mode, "fill_gaps")) {
      x <- fill_gaps(x)
    } else {
      assert_that(identical(mode, "extreme_vals"))
    }

    win_expr <- substitute(
      list(o2sat = min_fun(get("o2sat")), fio2 = max_fun(get("fio2"))),
      list(min_fun = min_or_na, max_fun = max_or_na)
    )

    x <- slide(x, !!win_expr, before = match_win, full_window = FALSE)
  }

  if (not_null(fio2)) {
    x <- set(x, which(is.na(x[[fio2]])), fio2, 21)
  }

  x
}

#' @param min_length Minimal time span between a ventilation start and end
#' time
#'
#' @rdname callback_cncpt
#' @export
#'
vent_ind <- function(..., match_win = hours(6L), min_length = mins(30L),
                     interval = NULL) {

  subset_true <- function(x, col) x[is_true(get(col))]
  calc_dur <- function(x, y) fifelse(is.na(y), x + match_win, y - x)

  final_int <- interval

  cnc <- c("vent_start", "vent_end", "mech_vent")
  res <- collect_dots(cnc, NULL, ...)

  interval <- check_interval(res)

  if (is.null(final_int)) {
    final_int <- interval
  }

  match_win  <- as_interval(match_win)
  min_length <- as_interval(min_length)

  assert_that(
    is_interval(final_int), min_length < match_win, interval < min_length
  )

  if (has_rows(res[[3L]])) {

    assert_that(nrow(res[[1L]]) == 0L, nrow(res[[2L]]) == 0L)

    res <- res[[3L]][, c("vent_ind", "mech_vent") := list(
      !is.na(get("mech_vent")), NULL
    )]

    res <- change_interval(res, final_int, by_ref = TRUE)

    return(res)
  }

  assert_that(nrow(res[[3L]]) == 0L)

  units(match_win) <- units(interval)
  units(min_length) <- units(interval)

  cnc <- cnc[-3L]
  res <- Map(subset_true, res[-3L], cnc)
  var <- "vent_dur"

  if (has_rows(res[[2L]])) {

    idx_vars  <- chr_ply(res, index_var)
    res[[2L]] <- res[[2L]][, c(var, idx_vars[2L]) := list(
      get(idx_vars[2L]), get(idx_vars[2L]) - mins(1L))]

    jon <- chr_ply(do.call(map, c("c", lapply(rev(res), meta_vars))), paste,
                   collapse = " == ")


    res <- res[[2L]][res[[1L]], roll = -match_win, on = jon]
    res <- res[, c(var, cnc) := list(
      calc_dur(get(idx_vars[2L]), get(var)), NULL, NULL)]
    res <- res[get(var) >= min_length, ]

  } else {

    res <- res[[1L]][, c(var, "vent_start") := list(match_win, NULL)]
  }

  res <- change_interval(res, final_int, by_ref = TRUE)
  res <- aggregate(res, "max")
  res <- res[, c("vent_ind") := TRUE]

  as_win_tbl(res, dur_var = var, by_ref = TRUE)
}

#' @param valid_win Maximal time window for which a GCS value is valid
#' if no newer measurement is available
#' @param sed_impute Imputation scheme for values taken when patient was
#' sedated (i.e. unconscious).
#' @param set_na_max Logical flag controlling imputation of missing GCS values
#' with the respective maximum values
#'
#' @rdname callback_cncpt
#' @export
#'
gcs <- function(..., valid_win = hours(6L),
                sed_impute = c("max", "prev", "none", "verb"),
                set_na_max = TRUE, interval = NULL) {

  zero_to_na <- function(x) replace(x, x == 0, NA_real_)

  sed_impute <- match.arg(sed_impute)

  cnc <- c("egcs", "vgcs", "mgcs", "tgcs", "ett_gcs")
  res <- collect_dots(cnc, interval, ...)

  valid_win <- as_interval(valid_win)

  assert_that(valid_win > check_interval(res), is.flag(set_na_max))

  sed <- res[[cnc[5L]]]
  res <- reduce(merge, res[cnc[-5L]], all = TRUE)

  expr <- substitute(list(egcs = fun(egcs), vgcs = fun(vgcs),
                          mgcs = fun(mgcs), tgcs = fun(tgcs)),
                     list(fun = locf))

  res <- slide(res, !!expr, before = valid_win)

  if (identical(sed_impute, "none")) {

    cnc <- cnc[-5L]

  } else {

    sed <- sed[is_true(get(cnc[5L])), ]

    if (is_win_tbl(sed)) {
      sed <- expand(sed, aggregate = "any")
    }

    res <- merge(res, sed, all.x = TRUE)
  }

  if (identical(sed_impute, "max")) {

    res <- res[is_true(get(cnc[5L])), c(cnc[4]) := 15]

  } else if (identical(sed_impute, "verb")) {

    res <- res[is_true(get(cnc[5L])), c(cnc[c(2L, 4L)]) := list(5, NA_real_)]

  } else if (identical(sed_impute, "prev")) {

    idv <- id_vars(res)
    res <- res[, c(cnc[-5L]) := lapply(.SD, replace_na, 0), .SDcols = cnc[-5L]]
    res <- res[is_true(get(cnc[5L])), c(cnc[-5L]) := NA_real_]
    res <- res[, c(cnc[-5L]) := lapply(.SD, replace_na, type = "locf"),
               .SDcols = cnc[-5L], by = c(idv)]
    res <- res[, c(cnc[-5L]) := lapply(.SD, zero_to_na), .SDcols = cnc[-5L]]
  }

  if (set_na_max) {
    res <- res[, c(cnc[1L:3L]) := Map(replace_na, .SD, c(4, 5, 6)),
               .SDcols = cnc[1L:3L]]
  }

  res <- res[is.na(get(cnc[4L])), c(cnc[4L]) := rowSums(.SD),
             .SDcols = cnc[1L:3L]]

  if (set_na_max) {
    res <- res[, c(cnc[4L]) := list(replace_na(get(cnc[4L]), 15))]
  }

  res <- rename_cols(res, "gcs", cnc[4L], by_ref = TRUE)
  res <- rm_cols(res, cnc[-4L], by_ref = TRUE)

  res
}

#' @param min_win Minimal time span required for calculation of urine/24h
#' @param limits Passed to [fill_gaps()] in order to expand the time series
#' beyond first and last measurements
#' @param start_var,end_var Passed to [fill_gaps()]
#'
#' @rdname callback_cncpt
#' @export
#'
urine24 <- function(..., min_win = hours(12L), limits = NULL,
                    start_var = "start", end_var = "end", interval = NULL) {

  convert_dt <- function(x) as.double(x, units(interval))

  urine_sum <- function(x) {
    if (length(x) < min_steps) return(NA_real_)
    else sum(x, na.rm = TRUE) * step_factor / length(x)
  }

  res      <- collect_dots("urine", interval, ...)
  interval <- check_interval(res)
  min_win  <- as_interval(min_win)

  assert_that(min_win > interval, min_win <= hours(24L))

  if (nrow(res) == 0L) {
    res <- rename_cols(res, "urine24", "urine")
    return(res)
  }

  min_steps   <- ceiling(convert_dt(min_win) / as.double(interval))
  step_factor <- convert_dt(hours(24L)) / as.double(interval)

  if (is.null(limits)) {
    limits <- collapse(res)
  }

  res <- fill_gaps(res, limits = limits)

  expr <- substitute(list(urine24 = win_agg_fun(urine)),
                     list(win_agg_fun = urine_sum))

  slide(res, !!expr, hours(24L))
}

#' @param max_gap Maximum time gap between administration windows that are
#' merged (can be negative).
#'
#' @rdname callback_cncpt
#' @export
#'
vaso60 <- function(..., max_gap = mins(5L), interval = NULL) {

  final_int <- interval

  dat <- collect_dots(c(rate = "_rate$", dur = "_dur$"), NULL, ...)

  interval <- check_interval(dat)

  if (is.null(final_int)) {
    final_int <- interval
  }

  max_gap <- as_interval(max_gap)

  assert_that(is_interval(final_int))

  if (any(int_ply(dat, nrow) == 0L)) {

    res <- dat[["rate"]]
    res <- rename_cols(res[0L], sub, data_vars(res), pattern = "_rate$",
                       replacement = "60")
    res <- change_interval(res, final_int, by_ref = TRUE)

    return(res)
  }

  dur <- dat[["dur"]]
  dva <- data_vars(dur)
  idx <- index_var(dur)
  dur <- dur[get(dva) > 0, ]

  dur <- dur[, c(dva) := get(idx) + get(dva)]
  dur <- merge_ranges(dur, idx, dva, max_gap = max_gap, by_ref = TRUE)
  dur <- dur[get(dva) - get(idx) >= hours(1L), ]

  rate <- dat[["rate"]]
  temp <- new_names(c(colnames(dur), colnames(rate)), 2L)
  rate <- rate[, c(temp) := get(index_var(rate))]
  on.exit(rm_cols(rate, temp, by_ref = TRUE))

  join <- c(
    paste(id_vars(rate), id_vars(dur), sep = " == "),
    paste(temp, c(">=", "<="), c(index_var(dur), data_vars(dur)))
  )

  res <- rate[dur, on = join, nomatch = NULL]
  res <- rm_cols(res, temp, by_ref = TRUE)
  res <- rename_cols(res, sub, data_vars(res), by_ref = TRUE,
                     pattern = "_rate$", replacement = "60")
  res <- change_interval(res, final_int, by_ref = TRUE)

  if (max_gap < 0L) {
    res <- unique(res)
  }

  aggregate(res, "max")
}

#' @rdname callback_cncpt
#' @export
vaso_ind <- function(..., interval = NULL) {

  cnc <- c("dopa_dur", "norepi_dur", "dobu_dur", "epi_dur")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)
  unt <- time_unit(res)

  res <- res[, c(cnc) := lapply(.SD, as.difftime, units = unt), .SDcols = cnc]
  res <- res[, c("vaso_ind", cnc) := list(pmax(
    get("dopa_dur"), get("norepi_dur"), get("dobu_dur"), get("epi_dur"),
    na.rm = TRUE), NULL, NULL, NULL, NULL)
  ]

  res <- expand(res, index_var(res), "vaso_ind")
  res <- unique(res)
  res <- res[, c("vaso_ind") := TRUE]

  res
}

#' @rdname callback_cncpt
#' @export
supp_o2 <- function(..., interval = NULL) {

  vent_var <- "vent_ind"
  fio2_var <- "fio2"

  res <- collect_dots(c(vent_var, fio2_var), interval, ...)
  res <- merge(res[[fio2_var]], expand(res[[vent_var]], aggregate = "any"),
               all = TRUE)

  res <- res[, c("supp_o2", vent_var, fio2_var) := list(
    is_true(get(vent_var) | get(fio2_var) > 21), NULL, NULL
  )]

  res
}

map_vals <- function(pts, vals) {
  function(x) pts[findInterval(x, vals, left.open = TRUE) + 1]
}

#' @rdname callback_cncpt
#' @export
avpu <- function(..., interval = NULL) {

  avpu_map <- map_vals(c(NA, "U", "P", "V", "A", NA), c(2, 3, 9, 13, 15))

  res <- collect_dots("gcs", interval, ...)
  res <- res[, c("avpu", "gcs") := list(avpu_map(get("gcs")), NULL)]

  res
}

#' @rdname callback_cncpt
#' @export
bmi <- function(..., interval = NULL) {

  cnc <- c("weight", "height")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)
  res <- res[, c("bmi", cnc) := list(
    get("weight") / (get("height") / 100) ^ 2, NULL, NULL
  )]

  res <- filter_bounds(res, "bmi", 10, 100)

  setattr(data_col(res), "units", "kg/m^2")

  res
}

#' @rdname callback_cncpt
#' @export
o2sat_lab_first <- function(..., interval = NULL) {

  # Pulse Oxymetry: `spo2`
  # Arterial Blood Gas: `sao2`
  cnc <- c("sao2", "spo2")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

 # default to sao2 (arterial blood gas)
  res <- res[, o2sat := sao2]

  # if sao2 is missing, use spo2 (pulse oxymetry)
  res <- res[is.na(sao2), o2sat := spo2]

  # Filter out values below 50 and above 100
  res <- filter_bounds(res, "o2sat", 50, 100)

  # remove sao2 and spo2 columns
  res <- rm_cols(res, cnc, by_ref = TRUE)

  res
}

#' @rdname callback_cncpt
#' @export
norepi_equiv <- function(..., interval = NULL) {

  multiply_rename <- function(x, fact, col) {
    x <- x[, c(col) := get(col) * fact]
    x <- rename_cols(x, "norepi_equiv", col)
    x
  }

  cnc <- c("epi_rate", "norepi_rate", "dopa_rate", "adh_rate", "phn_rate")
  res <- collect_dots(cnc, interval, ...)
  res <- map(multiply_rename, res, 1 / c(1, 1, 150, 0.4, 10), cnc)

  res <- rbind_lst(res, fill = TRUE)

  aggregate(res)
}
