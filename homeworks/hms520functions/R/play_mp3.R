#' Play MP3 from assets folder
#'
#' @description this is used internally in is_spooky_time() and not for end users.
#'
#' @param song string. name of .mp3 file in assets folder.
#'
#' @export
#'
#' @examples
#' play_mp3("spookysong")
play_mp3 <- function(song) {

  #song name must be character length 1
  if(!is.character(song) | length(song) != 1) {
    stop("Error, Enter Song name as string")
  }

  #Check if file exists
  assets <- system.file("assets", package = "hms520functions")

  if(!file.exists(file.path(assets, paste0(song, ".mp3")))) {
    stop("Error: Song not found in assets folder")
  }

  song_wav <- tuneR::readMP3(filename = file.path(assets, paste0(song, ".mp3")))

  # don't play if being tested
  if(!is_testing()) {
    play(song_wav)
  }
}
