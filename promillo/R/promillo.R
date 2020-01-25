#' Compute blood alcohol content in permille.
#'
#' @param age positive integer between 10 and 110
#' @param sex male or female
#' @param height in cm
#' @param weight in kg
#' @param drinking_time vector of start and end time of drinking
#' @param drinks character vector of predefined drinks
#'
#' @return blood alcohol content in permille
#' @export
#'
#' @examples
#' drinking_time <- as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00"))
#' drinks <- c("wein" = 2)
#' tell_me_how_drunk(21, "female",
#' height = 172,
#' weight = 57,
#' drinking_time = drinking_time,
#' drinks = drinks)
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

#' Plot blood alcohol content every 5 minutes.
#'
#' @inheritParams tell_me_how_drunk
#' @examples
#' drinking_time <- as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00"))
#' drinks <- c("wein" = 2)
#' show_me_how_drunk(21, "female",
#' height = 172,
#' weight = 57,
#' drinking_time = drinking_time,
#' drinks = drinks)
#' @export
show_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  # start time and end times every 5 minutes
  start_time <- drinking_time[1]
  end_time_offsets <- seq.POSIXt(start_time, drinking_time[2], by = "5 min")

  # alcohol amount over the complete drinking time
  alcohol_drunk <- get_alcohol(drinks)
  # assuming uniform alcohol consumption over the complete drinking time
  alcohol_drunk_offsets <- seq(from = 0, to = alcohol_drunk, length.out = length(end_time_offsets))
  bodywater <- get_bodywater(sex, age, height, weight)

  permilles <- mapply(function(ado, eto) {
    get_permille(
      ado,
      bodywater,
      as.POSIXct(c(start_time, eto))
    )
  },
  ad = alcohol_drunk_offsets,
  et = end_time_offsets
  )
  ggplot2::ggplot(
    data.frame(
      "minutes_while_drinking" = 0:(length(permilles) - 1) * 5,
      "permille" = permilles
    ),
    ggplot2::aes(x = "minutes_while_drinking", y = "permille")
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
}

# utilities --------------------------------------------------------------------

#' Compute total amount of alcohol intake.
#'
#' @inheritParams tell_me_how_drunk
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  checkmate::assert_subset(names(drinks),
    choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE
  )
  checkmate::assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn" = 1000,
    "hoibe" = 500,
    "wein" = 200,
    "schnaps" = 40
  )
  alcohol_concentration <- c(
    "massn" = 0.06,
    "hoibe" = 0.06,
    "wein" = 0.11,
    "schnaps" = 0.4
  )
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
    alcohol_concentration[names(drinks)] * alcohol_density)
}


#' Compute amount of bodywater for a person.
#'
#' @inheritParams tell_me_how_drunk
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  checkmate::assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("...ts ts ts, this at your age!")
  }
  checkmate::assert_number(height, lower = 100, upper = 230)
  checkmate::assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}


#' Compute blood alcohol content in permille.
#' @param alcohol_drunk total amount of alcohol intake
#' @param bodywater total amount of bodywater
#' @inheritParams tell_me_how_drunk
get_permille <- function(alcohol_drunk, bodywater, drinking_time) {
  checkmate::assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille - (max(0, partylength - 1) * sober_per_hour))
}
