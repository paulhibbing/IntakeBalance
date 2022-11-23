#' Calculate change in whole-body energy storage
#'
#' @inheritParams IntakeBalance-function
#' @param ... arguments passed to \code{\link[anthropometry]{get_es}}
#'
#' @return numeric vector of change values
#' @export
#'
#' @examples
#' delta_es(50.5, 50.7, 75.1, 74.9, n_days = 14)
delta_es <- function(
  fm_start, fm_end, ffm_start, ffm_end,
  fm_units = "kg", ffm_units = fm_units,
  start_date, end_date, n_days = 1,
  ...
) {

  if (!missing(start_date) & !missing(end_date)) {

    if (!is.null(start_date)  & !is.null(end_date)) {

      stopifnot(
        inherits(start_date, "Date"), inherits(end_date, "Date")
      )

      n_days <-
          difftime(end_date, start_date) %>%
          as.numeric("days")

    }

  }

  anthropometry::get_es(fm_end, ffm_end, as.name(fm_units), as.name(ffm_units), ...) %>%
  {. - anthropometry::get_es(fm_start, ffm_start, as.name(fm_units), as.name(ffm_units), ...)} %>%
  {. / n_days}

}
