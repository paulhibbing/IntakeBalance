#' Calculate energy intake over time via the intake-balance method
#'
#' @param fm_start starting fat mass
#' @param fm_end ending fat mass
#' @param ffm_start starting fat-free mass
#' @param ffm_end ending fat-free mass
#' @param fm_units character scalar indicating units of the values passed for \code{fm_start} and \code{fm_end}
#' @param ffm_units character scalar indicating units of the values passed for \code{ffm_start} and \code{ffm_end}
#' @param ee_per_day energy expenditure expressed in units per day
#' @param ee_units character scalar indicating units of the energy expenditure values
#' @param df optional. A data frame on which to operate
#' @param ... Arguments passed to \code{\link{delta_es}}
#' @param start_date the start date of the measurement period
#' @param end_date the end date of the measurement period
#' @param n_days optional The number of days in the measurement period (will be
#'   calculated from \code{start_date} and \code{end_date} if provided.
#'   Otherwise the default is 1, such that calculations will reflect raw change
#'   in energy storage)
#' @param es_name character scalar. Name to assign the new variable in \code{df}
#'   containing information about change in energy storage
#' @param ei_name character scalar. Name to assign to the new variable in
#'   \code{df} containing information about energy intake
#'
#' @details
#' This function operates in different ways, depending on whether anything is
#' passed for the \code{df} argument. If not, the arguments are passed into
#' \code{IntakeBalance_default}, where the expectation is that numeric values
#' are passed for fat mass, fat-free mass, and energy expenditure variables.
#' Otherwise, so, the arguments are passed into \code{IntakeBalance_df}, where
#' the expectation is that character scalars are passed, which represent the
#' column names of variables in \code{df} which contain the corresponding
#' information.
#'
#'
#' @return Either a numeric vector of energy intake values (if \code{df = NULL},
#'   or a data frame appended with energy intake data
#' @export
#'
#' @examples
#'
#' d <- data.frame(
#'   fm_start = c(50.5, 70.2), fm_end = c(50.7, 70.0),
#'   ffm_start = c(75.1, 90.3), ffm_end = c(74.9, 90.3),
#'   ee = c(1950, 2473), n_days = c(14, 14)
#' )
#'
#' IntakeBalance(50.5, 50.7, 75.1, 74.9, ee_per_day = 1950, n_days = 14)
#'
#' IntakeBalance(
#'   "fm_start", "fm_end", "ffm_start", "ffm_end",
#'   ee_per_day = "ee", n_days = "n_days", df = d
#' )
#'
#' @name IntakeBalance-function
IntakeBalance <- function(
  fm_start, fm_end, ffm_start, ffm_end,
  fm_units = "kg", ffm_units = fm_units,
  ee_per_day, ee_units = "kcal", df = NULL, ...
) {

  if (is.null(df)) IntakeBalance_default(
    fm_start, fm_end, ffm_start, ffm_end,
    fm_units = fm_units, ffm_units = fm_units,
    ee_per_day, ee_units = ee_units, ...
  )

  else IntakeBalance_df(
    df, fm_start, fm_end, ffm_start, ffm_end,
    fm_units = fm_units, ffm_units = fm_units,
    ee_per_day, ee_units = ee_units, ...
  )

}


#' @rdname IntakeBalance-function
IntakeBalance_default <- function(
  fm_start, fm_end, ffm_start, ffm_end,
  fm_units = "kg", ffm_units = fm_units,
  ee_per_day, ee_units = "kcal", ...
) {

  stopifnot(is.numeric(ee_per_day))

  if (ee_units != "kcal") ee_per_day %<>% anthropometry::unit_convert(
    ee_units, "kcal"
  )

  ee_per_day +
  delta_es(
    fm_start, fm_end, ffm_start, ffm_end,
    fm_units = fm_units, ffm_units = fm_units,
    ...
  )

}


#' @rdname IntakeBalance-function
IntakeBalance_df <- function(
  df, fm_start, fm_end, ffm_start, ffm_end,
  fm_units = "kg", ffm_units = fm_units,
  ee_per_day, ee_units = "kcal",
  start_date = NULL, end_date = NULL, n_days = NULL,
  ..., es_name = "delta_ES", ei_name = "EI"
) {

  stopifnot(

    inherits(df, "data.frame"),

    is.character(fm_start), is.character(fm_end),
    exists(fm_start, df), exists(ffm_start, df),
    is.numeric(df[[fm_start]]), is.numeric(df[[ffm_start]]),

    is.character(ffm_start), is.character(ffm_end),
    exists(fm_end, df), exists(ffm_end, df),
    is.numeric(df[[fm_end]]), is.numeric(df[[ffm_end]]),

    is.character(ee_per_day),
    exists(ee_per_day, df),
    is.numeric(df[[ee_per_day]]),

    !is.null(n_days) | (!is.null(start_date) & !is.null(end_date))

  )

  if (!is.null(n_days)) {
    stopifnot(
      is.character(n_days), exists(n_days, df), is.numeric(df[[n_days]])
    )
    n_days <- df[[n_days]]
  }

  if (!is.null(start_date) & !is.null(end_date)) {
    stopifnot(
      is.character(start_date), exists(start_date, df), inherits(df[[start_date]], "Date"),
      is.character(end_date), exists(end_date, df), inherits(df[[end_date]], "Date")
    )
    n_days <-
      difftime(df[[end_date]], df[[start_date]]) %>%
      as.numeric("days")
  }

  if (ee_units != "kcal") df %<>% dplyr::mutate(
    !!as.name(ee_per_day) := anthropometry::unit_convert(
      !!as.name(ee_per_day), ee_units, "kcal"
    )
  )

  df %>%
  dplyr::mutate(
    !!as.name(es_name) := delta_es(
      !!as.name(fm_start), !!as.name(fm_end),
      !!as.name(ffm_start), !!as.name(ffm_end),
      fm_units, ffm_units, start_date = NULL, end_date = NULL,
      n_days = n_days, ...
    ),
    !!as.name(ei_name) := !!as.name(es_name) + !!as.name(ee_per_day)
  )

}
