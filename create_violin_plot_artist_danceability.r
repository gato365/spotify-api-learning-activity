#' @title Create a violin plot based on an artist's tracks
#' @param artist_ids - A vector of Spotify artist ids
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A violin plot displaying a number of artist's tracks' danceability
#' @examples 
#' \dontrun{
#'  create_violin_plot_artist_danceability("0du5cEVh5yTK9QJze8zA0C")
#' }
#' @export
create_violin_plot_artist_danceability <- function(artist_ids, authorization = get_spotify_access_token()) {
    # Get top 10 tracks of artist:
    list_df_artist_top_tracks <- artist_ids %>% 
                                    purrr::map(~ get_artist_top_tracks(.x))

    # From list above, pull out the song ids and make into vector:
    vector_song_ids <- list_df_artist_top_tracks %>%
                            map(~ .x %>% pull(id)) %>% 
                            unlist()

    # Vector of artist names:
    # Get artist names and duplicate them
    vector_artist_names <- artist_ids %>% 
                                map(~ rep(get_artists(.x)$name, times = nrow(get_artist_top_tracks(.x)))) %>% 
                                unlist()

    # Get track audio features of artist 10 songs:
    df_tracks <- get_track_audio_features(vector_song_ids, authorization = get_spotify_access_token()) %>% 
                    mutate(artist = vector_artist_names)
    
    if ((length(artist_ids) <= 3 & any(nchar(vector_artist_names) > 15) & any(nchar(vector_artist_names) < 30)) | (length(artist_ids) <= 5 & all(nchar(vector_artist_names) <= 15))) {
        gg <- ggplot2::ggplot(df_tracks, aes(x=artist, y=danceability))  + 
            ggplot2::geom_violin(aes(fill = artist)) + 
            labs(title = "Danceability of Artist Top Tracks", x = "Artist", y = "Danceability", fill = "Artist") + 
            theme(plot.title = element_text(hjust = 0.5))
    } 
    else {
        gg <- ggplot2::ggplot(df_tracks, aes(x=artist, y=danceability))  + 
            ggplot2::geom_violin(aes(fill = artist)) + 
            labs(title = "Danceability of Artist Top Tracks", x = "") + 
            theme(plot.title = element_text(hjust = 0.5), 
                    axis.text.x=element_blank(), 
                    axis.ticks.x=element_blank() 
                    )
    }
    plotly::ggplotly(gg)
}