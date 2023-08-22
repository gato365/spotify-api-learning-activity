#' @title Create a valenece and energy plot based on an artist's tracks
#' @param artist_ids - A vector of Spotify artist ids
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A valence and energy plot which plots the top tracks from multiple artists
#' @examples 
#' \dontrun{
#'  artist_track_valence_energy_visual("0du5cEVh5yTK9QJze8zA0C")
#'  artist_track_valence_energy_visual(c("0du5cEVh5yTK9QJze8zA0C", "73sIBHcqh3Z3NyqHKZ7FOL"))
#' }
#' @export
artist_track_valence_energy_visual <- function(artist_ids, authorization = get_spotify_access_token()) {
  artist_data <- map(artist_ids, ~ {
    track_info <- get_artist_top_tracks(.x)  
    track_ids <- track_info$track_id
    track_names <- track_info$track_name
    audio_features <- get_track_audio_features(track_ids, authorization = authorization)  
    
    min_length <- min(length(track_ids), length(track_names), length(audio_features))
    
    artist_name <- rep(get_artists(.x, authorization = authorization)$artist_name, each = min_length)  
    
    data.frame(
      track_id = track_ids[1:min_length],
      track_name = track_names[1:min_length],
      audio_features = audio_features[1:min_length],
      artist_name = artist_name,
      stringsAsFactors = FALSE
    )
  })

  artist_colors <- setNames(c("red", "blue", "green","grey","black","orange","yellow","purple","pink","brown"), (artist_data$track_name))

  df <- data.frame(x = c(0, 1), y = c(0, 1))
  alpha_score <- 0.2

  p1 <- df %>%
    ggplot(mapping = aes(x = x, y = y)) +
    geom_blank() +
    geom_vline(xintercept = 0.5, linewidth = 1) +
    geom_hline(yintercept = 0.5, linewidth = 1) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                       labels = scales::label_number(accuracy = 0.1)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0))  +
    theme_bw() +
    theme(
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      plot.margin = margin(0.3, 0.5, 0.1, 0.5, "cm")
    ) +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1),
              alpha = alpha_score, fill = "red1") +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1),
              alpha = alpha_score, fill = "violetred1") +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5),
              alpha = alpha_score, fill = "slategray3") +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5),
              alpha = alpha_score, fill = "royalblue2") +
    labs(x = "Valence", y = "Energy") +
    # Adjust the font size for annotations
    annotate("text", x = 0.25, y = 0.25, label = "Sad", size = 5,
             color = "white") +
    annotate("text", x = 0.75, y = 0.25, label = "Chill", size = 5,
             color = "white") +
    annotate("text", x = 0.25, y = 0.75, label = "Angry", size = 5,
             color = "white") +
    annotate("text", x = 0.75, y = 0.75, label = "Happy", size = 5,
             color = "white")

  combined_data <- bind_rows(artist_data)

  p2 <- p1 + 
    geom_point(data = combined_data,  
             aes(x = audio_features.valence, y = audio_features.energy, color = artist_name), size = 3) +
    labs(color = "Artist Name") +
    scale_color_manual(values = artist_colors, labels = (combined_data$artist_name)) +
    
theme(
  plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
  plot.subtitle =element_text(hjust = 0.5, size = 13, face = "bold"),
  legend.text = element_text(size = 10),
  legend.position = "bottom",
  legend.direction = "vertical"
)

p2_interactive <- ggplotly(p2)


p2_interactive
}