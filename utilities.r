#' @importFrom magrittr "%>%"

#' @title Search for Spotify pitches
#' @return A vector of possible pitches
pitch_class_lookup <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')

#' @title Search for Spotify possible scopes
#' @return A vector of Spotify possible authorization scopes
#' @examples 
#' \dontrun{
#' scopes()
#' }
#' @export
scopes <- function() {
    xml2::read_html("https://developer.spotify.com/documentation/general/guides/authorization/scopes/") %>%
    rvest::html_elements('code') %>%
    rvest::html_text() %>%
    unique()
}