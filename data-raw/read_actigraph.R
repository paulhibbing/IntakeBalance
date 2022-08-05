f <- "data-raw/4001 (2018-02-10).gt3x"
age <- "adult"
site <- "wrist"
verbose <- TRUE

read_actigraph <- function(
  f, age = c("adult", "youth", "preschool"),
  site = c("waist", "wrist"),
  verbose = FALSE
) {


  #* Setup

    timer <- PAutilities::manage_procedure(
      "Start", "\nProcessing ", basename(f)
    )

    age <- match.arg(age)

    site <- match.arg(site)


  #* Initial reading

    if (verbose) cat("\n... Reading gt3x file")

      stopifnot(
        is.character(f),
        read.gt3x::is_gt3x(f)
      )

      AG <- AGread::read_gt3x(
        f,
        parser = "external",
        imputeZeroes = TRUE
      )


  #* Further raw processing

      accel <-

        AG$RAW %T>%

        {if (verbose) cat("\n... Calculating ENMO etc")} %>%
        AGread::collapse_gt3x(basename(f), 1) %T>%

        {if (verbose) cat("\n... Formatting the raw acceleration data")} %>%
        dplyr::mutate(ENMO_CV10s = TwoRegression::get_cvPER(ENMO))

      stop("Calculate additional features for Montoye, Staudenmayer etc.")

  #* Count processing

    counts <-

      AG$RAW %T>%

      {if (verbose) cat("\n... Calculating counts")} %>%
        structure(
          start_time = dplyr::first(AG$info$start_time),
          stop_time = dplyr::first(AG$info$stop_time)
        ) %>%
        stats::setNames(c("time", "X", "Y", "Z")) %>%
        agcounts::calculate_counts(60, lfe_select = TRUE) %>%
        dplyr::rename(Timestamp = time) %T>%

      {if (verbose) cat("\n... Applying Choi algorithm (non-wear)")} %>%
        PhysicalActivity::wearingMarking(
          perMinuteCts = 1, TS = "Timestamp",
          cts = "Axis1", newcolname = "Choi_is_NonWear"
        ) %T>%

      {if (verbose) cat("\n... Applying Tracy algorithm (sleep)")} %>%
        dplyr::mutate(TS = Timestamp) %>%
        PhysActBedRest::markbedrest(
          "TS", "Axis1", age, site, tempdir()
        ) %T>%

      {if (verbose) cat("\n... Formatting the count data")} %>%
        within({

          TS = NULL
          days = NULL

          Tracy_is_Sleep = bedrest == "br"
          bedrest = NULL

          Choi_is_NonWear = Choi_is_NonWear == "nw"

          valid_status = ifelse(
            Choi_is_NonWear,
            "Non-Wear",
            ifelse(Tracy_is_Sleep, "Sleep", "Awake-Wear")
          )

          is_WakeWear = valid_status == "Awake-Wear"
          is_Sleep = valid_status == "Sleep"
          is_NonWear = valid_status == "Non-Wear"

          weekday = factor(
            weekday,
            c(
              "Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday", "Sunday"
            )
          )

          is_weekend = weekday %in% c("Saturday", "Sunday")

        }) %>%
        dplyr::relocate(
          weekday, is_weekend,
          .after = Timestamp
        ) %>%
        dplyr::relocate(
          Tracy_is_Sleep, valid_status,
          .after = Choi_is_NonWear
        ) %>%
        dplyr::select(!dplyr::matches("^Inclinometer")) %T>%
        {file.remove(file.path(tempdir(), "subj_slp_sum.csv"))}


}
