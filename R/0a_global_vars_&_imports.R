# Global Variables --------------------------------------------------------

  if(getRversion() >= "2.15.1") utils::globalVariables(c(
    ".", "bedrest", "Choi_is_NonWear", "days", "is_NonWear",
    "is_Sleep", "is_WakeWear", "is_weekend", "Timestamp", "time",
    "Tracy_is_Sleep", "TS", "valid_status", "weekday", "weekend"
  ))

# Imports -----------------------------------------------------------------

  #' @importFrom rlang :=
  #' @import magrittr
  NULL
