#' @title Create a violin plot based on genres
#' @param genres - A vector of Spotify genres
#' @param limit - Number of songs wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A violin plot displaying a specific number of an genres's tracks' danceability
#' @examples 
#' \dontrun{
#'  create_violin_plot_album_danceability(c("rap", "jazz"))
#'  create_violin_plot_album_danceability("hip-hop", limit = 30)
#' }
#' @export
create_violin_plot_genre_danceability <- function(genres, limit=20, authorization = get_spotify_access_token()) {
    list_df_genres <- genres %>% 
                        purrr::map(~ get_genre_track_features(.x, limit=limit, authorization=authorization) %>% mutate(genre = .x))
    
    df_genres <- bind_rows(list_df_genres)

    if (length(genres) <= 5) {
        gg <- ggplot2::ggplot(df_genres, aes(x=genre, y=danceability))  + 
                ggplot2::geom_violin(aes(fill = genre)) + 
                labs(title = "Danceability of Genre Top Tracks", x = "Genre", y = "Danceability", fill = "Genre") + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                stat_summary(fun.data = "mean_cl_normal", geom = "point", shape = 20, size = 4, color = "red")
    } 
    else {
        gg <- ggplot2::ggplot(df_genres, aes(x=genre, y=danceability))  + 
                ggplot2::geom_violin(aes(fill = genre)) + 
                labs(title = "Danceability of Genre Top Tracks", x = "") + 
                theme(plot.title = element_text(hjust = 0.5), 
                        axis.text.x=element_blank(), 
                        axis.ticks.x=element_blank()
                        ) + 
                stat_summary(fun.data = "mean_cl_normal", geom = "point", shape = 20, size = 4, color = "red")
    }
    plotly::ggplotly(gg)
}