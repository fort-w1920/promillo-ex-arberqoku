% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/promillo.R
\name{show_me_how_drunk}
\alias{show_me_how_drunk}
\title{Plot blood alcohol content every 5 minutes.}
\usage{
show_me_how_drunk(age, sex = c("male", "female"), height, weight,
  drinking_time, drinks)
}
\arguments{
\item{age}{positive integer between 10 and 110}

\item{sex}{male or female}

\item{height}{in cm}

\item{weight}{in kg}

\item{drinking_time}{vector of start and end time of drinking}

\item{drinks}{character vector of predefined drinks}
}
\description{
Plot blood alcohol content every 5 minutes.
}
\examples{
drinking_time <- as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00"))
drinks <- c("wein" = 2)
show_me_how_drunk(21, "female",
height = 172,
weight = 57,
drinking_time = drinking_time,
drinks = drinks)
}
