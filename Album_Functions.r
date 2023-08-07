#' @title Search for Spotify album information
#' @param ids - A vector of Spotify album ids
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album data, including album name and id, label, album popularity, release date, number of tracks, and artist info
#' @examples
#' \dontrun{
#' get_albums("1uyf3l2d4XYwiEqAb7t7fX")
#' get_albums(c("1uyf3l2d4XYwiEqAb7t7fX", "58ufpQsJ1DS5kq4hhzQDiI"))
#' }
#' @export
get_albums <- function(ids, authorization = get_spotify_access_token()){
  url <- "https://api.spotify.com/v1/albums"
  parameters <- list(
    market = "US",
    access_token = authorization
  )
  if (length(ids) > 1) {
    # For multiple IDs
    param <- list(
        ids = paste(ids, collapse = ",")
  )
    parameters <- c(parameters, param)
  } else {
    # For single ID
    url <- stringr::str_glue("{url}/{ids}")
  }
  result <- httr::RETRY("GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))
  httr::stop_for_status(result)
  result <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"), flatten = TRUE)
  if (length(ids) > 1) {
    # For multiple IDs
    result <- result$albums
    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result <- result %>%
      dplyr::mutate(
        artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
        artist_name = purrr::map_chr(artists, ~ toString(.x$name))
      ) %>%
      dplyr::select(
        -album_type, 
        -copyrights, 
        -genres, 
        -href, 
        -images, 
        -uri, 
        -external_ids.upc, 
        -external_urls.spotify, 
        -tracks.href, 
        -tracks.limit,
        -tracks.next, 
        -tracks.offset, 
        -tracks.previous, 
        -tracks.total,
        -artists,
        -tracks.items,
        -is_playable
      ) %>%
      dplyr::rename(
        album_id = id,
        album_name = name
      )
  } else {
    # For single ID
    df <- data.frame(
      album_id = result$id,
      label = result$label,
      album_name = result$name,
      popularity = result$popularity,
      release_date = result$release_date,
      release_date_precision = result$release_date_precision,
      total_tracks = result$total_tracks,
      type = result$album_type,
      artist_id = result$artists$id,
      artist_name = result$artists$name
    )
    result <- df
  }
  result
}

#' @title Search for Spotify artist's albums
#' @param id - A single Spotify artist id
#' @param limit - Optional.  Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param offset - Optional.  Index of first album wanted.  Defaults to 0
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album data, including ids and names for all albums by the artist, release date, number of tracks, and artists involved in the albums
#' @examples
#' \dontrun{
#' get_artist_albums("0du5cEVh5yTK9QJze8zA0C")
#' get_artist_albums("0du5cEVh5yTK9QJze8zA0C", limit = 50, offset = 2)
#' }
#' @export
get_artist_albums <- function(id, limit = 20, offset = 0, authorization = get_spotify_access_token()){
    url <- 'https://api.spotify.com/v1/artists'
    parameters <- list(
        include_groups = "album",
        market = "US",
        limit = limit,
        offset = offset,
        access_token = authorization
    )
    url <- stringr::str_glue('{url}/{id}/albums')
    result <- httr::RETRY(verb = 'GET', url,
                 query = parameters,
                 encode = 'json', terminate_on = c(401, 403, 404))

    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(
        httr::content(result, as = 'text', encoding = 'UTF-8'),
        flatten = TRUE
        )
    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result <- result$items %>% 
            dplyr::mutate(
                artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
                artist_name = purrr::map_chr(artists, ~ toString(.x$name))
                ) %>%
            dplyr::distinct(name, .keep_all = TRUE) %>%
            dplyr::select(-album_group, -href, -images, -is_playable, -type, -uri, -external_urls.spotify, -artists) %>% 
            dplyr::rename(
                album_id = id,
                album_name = name,

            )
    result
}

#' @title Search for Spotify artist's projects
#' @param id - A single Spotify artist id
#' @param limit - Optional.  Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param offset - Optional.  Index of first album wanted.  Defaults to 0
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of project data, including the project type, project id and name, release date, number of tracks, and the artists names and ids
#' @examples
#' \dontrun{
#' get_artist_projects("0du5cEVh5yTK9QJze8zA0C")
#' get_artist_projects("0du5cEVh5yTK9QJze8zA0C", limit = 50, offset = 2)
#' }
#' @export
get_artist_projects <- function(id, limit = 20, offset = 0, authorization = get_spotify_access_token()){
    url <- 'https://api.spotify.com/v1/artists'
    parameters <- list(
        include_groups = paste(c('album', 'single'), collapse = ','),
        market = "US",
        limit = limit,
        offset = offset,
        access_token = authorization
    )
    url <- stringr::str_glue('{url}/{id}/albums')
    result <- httr::RETRY(verb = 'GET', url,
                 query = parameters,
                 encode = 'json', terminate_on = c(401, 403, 404))

    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(
        httr::content(result, as = 'text', encoding = 'UTF-8'),
        flatten = TRUE
        )
    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result <- result$items %>% 
            dplyr::mutate(
                artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
                artist_name = purrr::map_chr(artists, ~ toString(.x$name))
                ) %>%
            dplyr::distinct(name, .keep_all = TRUE) %>%
            dplyr::select(-type, -album_group, -href, -images, -is_playable, -uri, -external_urls.spotify, -artists) %>%
            dplyr::rename(
                album_id = id,
                album_name = name
            )
    result
}

#' @title Search for album's feature summary
#' @param id - A single Spotify album id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album data, including album name and id, album popularity, number of tracks, and the mean and standard deviation for:
#'        danceability
#'        energy
#'        loudness
#'        speechiness
#'        acousticness
#'        instrumentalness
#'        liveness
#'        valence
#'        tempo
#'        duration_ms
#'        mode
#' @examples
#' \dontrun{
#' get_album_summary("1uyf3l2d4XYwiEqAb7t7fX")
#' }
#' @export
get_album_summary <- function(id, authorization = get_spotify_access_token()){
    album <- get_albums(id, authorization = authorization)

    tracks <- get_albums_tracks(id, authorization = authorization)
    
    features <- get_track_audio_features(tracks$track_id, authorization = authorization)
    
    summary <- features %>%
               dplyr::select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms, mode) %>%
               dplyr::summarize(dplyr::across(dplyr::everything(), list(mean = ~ mean(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE)), .names = "{.col}_{.fn}"))
    
    result <- merge(album, summary) %>% 
              dplyr::select(-label,
                            -release_date,
                            -release_date_precision,
                            -type,
                            -artist_id,
                            -artist_name)
    result
}

#' @title Search for Spotify album track features
#' @param id - A single Spotify artist id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album data, including album name and id, album popularity, number of tracks, and the mean and standard deviation for:
#'        danceability
#'        energy
#'        loudness
#'        mode
#'        speechiness
#'        acousticness
#'        instrumentalness
#'        liveness
#'        valence
#'        tempo
#'        duration_ms
#'        time_signature
#' @examples
#' \dontrun{
#' get_album_track_features("1uyf3l2d4XYwiEqAb7t7fX")
#' }
#' @export
get_album_track_features <- function(id, authorization = get_spotify_access_token()){
    tracks <- get_albums_tracks(id, authorization = authorization)

    features <- get_track_audio_features(tracks$track_id, authorization = authorization)

    result <- dplyr::left_join(tracks, features, by = "track_id") %>%
              dplyr::select(
                -duration_ms.x,
                -is_playable,
                -disc_number
              ) %>%
              dplyr::rename(
                duration_ms = duration_ms.y,
                track_name = name
              )

    result
}