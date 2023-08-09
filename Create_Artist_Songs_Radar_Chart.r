#' @title Create a radar chart of track features
#' @param songs - A vector of Spotify track ids
#' @param vars - A vector of variables returned from get_track_audio_features()
#' @param colors - A vector of colors
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A radar chart displaying valence, energy, and speechiness, along with any other inputed variables
#' @examples 
#' \dontrun{
#'  create_artist_songs_radar_chart(songs = c("6YbhspuOar1D9WSSnfe7ds", "5Tbpp3OLLClPJF8t1DmrFD", "2NBQmPrOEEjA8VbeWOQGxO"), vars = "liveness")
#' }
#' @export
create_artist_songs_radar_chart <- function(songs, vars = c(), colors = c(), authorization = get_spotify_access_token()){
  if (length(songs) > 3){
    stop("Please input only 3 or less tracks!")
  }
  colors = c("#6B8E23", "#89A8E0", "#A291B5")
  create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
    fmsb::radarchart(
      data, axistype = 1,
      # Customize the polygon
      pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
      # Customize the grid
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      # Customize the axis
      axislabcol = "grey", 
      # Variable labels
      vlcex = vlcex, vlabels = vlabels,
      caxislabels = caxislabels, title = title, ...
    )
  }

  min_max <- data.frame(
    row.names = c("min", "max"),
    valence = c(0, 1),
    energy = c(0, 1),
    speechiness = c(0, 1)
  )

  if (length(vars) > 0){
    create_data_frame <- function(var) {
      data.frame(
        var = c(0, 1)  
      )
    }

    combinations <- purrr::map_dfc(vars, create_data_frame) %>% 
                    dplyr::rename_with(~ vars, dplyr::everything())

    min_max <- cbind(min_max, combinations)
  }

  song_summaries <- purrr::map(songs, ~ get_track_audio_features(.x, authorization = authorization))

  final_summary_df <- dplyr::bind_rows(song_summaries)

  rownames(final_summary_df) <- songs

  final_summary_df <- final_summary_df %>%
                      dplyr::select(
                        valence,
                        energy,
                        speechiness,
                        vars
                      )
                      
  final_summary_df <- rbind(min_max, final_summary_df)  
  
  op <- par(mar = c(0, 0, 0, 0))
  
  create_beautiful_radarchart(
    data = final_summary_df, caxislabels = c(0, 0.25, 0.50, 0.75, 1),
    color = colors[1:length(songs)],
    vlcex = 1.5
  )
  
  tracks <- purrr::map(songs, get_tracks) %>% 
    as.data.frame() %>% 
    dplyr::select(
        dplyr::starts_with("track_name")
    )
  tracks <- tidyr::pivot_longer(tracks, cols = dplyr::starts_with("track_name"), 
                          names_to = "track_names", values_to = "names")
  legend(
    x = "bottom", legend = tracks$names, horiz = TRUE,
    bty = "n", pch = 20 , col = colors[1:length(songs)],
    text.col = "black", cex = 1
    )

  par(op)
}