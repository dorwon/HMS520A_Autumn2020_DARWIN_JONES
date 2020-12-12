#' Is it spooky time?
#'
#' @param today date or a string which can be coerced into a date. by default uses current date.
#'  Date format can be mm/dd/yyyy or yyyy/mm/dd. and use '-' or '/" as separators
#'
#' @return boolean. TRUE when it is a spooky time of year
#' @export
#'
#' @examples
#' is_spooky_time()
#' is_spooky_time(as.Date("2020-10-31"))
is_spooky_time <- function(today = Sys.Date()) {

  #Check if "today" is correct type and in correct format
  if(!lubridate::is.Date(today) & !is.character(today)) {
    stop("Error: must supply date or character in date format")
  }


  if(is.character(today)) {
    today = as.Date(today, tryFormats = c("%Y-%m-%d", "%Y/%m/%d",
                                          "%m-%d-%Y", "%m/%d/%Y"))
  }

  # Grab day of week and Month-day combo
  day_of_week <- weekdays(today)
  month_day <- format(today, format = "%m-%d")

  #load music library
  file_loc <- system.file("assets", package = "hms520functions")

  #check if "today" is a spooky day (Halloween or Friday 13th)
  if (month_day == "10-31" | (day_of_week == "Friday" & lubridate::mday(today) == 13)) {

    print("IT'S SPOOKY TIME!!!!")

    play_mp3("spookysong")

    return(TRUE)

  } else {

    print("it's not spooky time :/")

    play_mp3("sadtrombone")

    return(FALSE)

  }
}
