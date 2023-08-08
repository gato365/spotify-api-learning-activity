#' @title Search for Spotify album's tracks
#' @param ids - A vector of Spotify album ids
#' @param limit - Optional.  Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param offset - Optional.  Index of first album wanted.  Defaults to 0
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album track information, including duration, explicitness, track id and name, track number in album, and artist name and id
#' @examples
#' \dontrun{
#' get_albums_tracks(c("4VZ7jhV0wHpoNPCB7Vmiml", "58ufpQsJ1DS5kq4hhzQDiI"))
#' get_albums_tracks("4VZ7jhV0wHpoNPCB7Vmiml", limit = 50, offset = 2)
#' }
#' @export
get_albums_tracks <- function(ids, limit = 20, offset = 0, authorization = get_spotify_access_token()){
    url <- "https://api.spotify.com/v1/albums"
    parameters <- list(
        ids = paste(ids, collapse = ","),
        market = "US",
        limit = limit,
        offset = offset,
        access_token = authorization
    )
    result = httr::RETRY(verb = "GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)
    result <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"), flatten = TRUE)
    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result <- result$albums$tracks.items %>%
            purrr::map_df(as.data.frame) %>%
            dplyr::mutate(
                artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
                artist_name = purrr::map_chr(artists, ~ toString(.x$name))
                ) %>%
            dplyr::select(-href,
            -preview_url,
            -uri,
            -external_urls.spotify,
            -is_local,
            -artists,
            -is_playable) %>%
            dplyr::rename(
                track_id = id,
                track_name = name
            )
    result
}

#' @title Search for Spotify track information
#' @param ids - A vector of Spotify track ids
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track information, including duration, explicitness, track name and id, popularity, whether the song was in an album or a single, album name and id, release date, number of tracks on album, and artist name and id
#' @examples
#' \dontrun{
#' get_tracks("3G5iN5QBqMeXx3uZPy8tgB")
#' get_tracks(c("3G5iN5QBqMeXx3uZPy8tgB", "3w3y8KPTfNeOKPiqUTakBh"))
#' }
#' @export
get_tracks <- function(ids, authorization = get_spotify_access_token()){
    url <- "https://api.spotify.com/v1/tracks"
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
    result = httr::RETRY(verb = "GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)
    result <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"), flatten = TRUE)
    if (length(ids) > 1) {
    # For multiple IDs
        result <- result$tracks
        result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
        result <- result %>%
      dplyr::mutate(
        artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
        artist_name = purrr::map_chr(artists, ~ toString(.x$name))
      ) %>%
      dplyr::select(
        -is_playable,
        -album.is_playable,
        -href,
        -is_local,
        -preview_url,
        -uri,
        -album.artists,
        -album.href,
        -album.type,
        -album.uri,
        -album.external_urls.spotify,
        -external_ids.isrc,
        -external_urls.spotify,
        -album.images,
        -artists
      ) %>%
      dplyr::rename(
        album_type = album.album_type,
        track_name = name,
        track_id = id,
        album_name = album.name,
        album_id = album.id
      )
  } else {
    # For single ID
        df <- data.frame(
        disc_number = result$disc_number,
        duration_ms = result$duration_ms,
        explicit = result$explicit,
        track_id = result$id,
        track_name = result$name,
        popularity = result$popularity,
        album_type = result$album$album_type,
        album_name = result$album$name,
        album.release_date = result$album$release_date,
        album.release_date_precision = result$album$release_date_precision,
        album.total_tracks = result$album$total_tracks,
        artist_id = paste(result$artist$id, collapse = ", "),
        artist_name = paste(result$artist$name, collapse = ", ")
        )
    result <- df
  }
  result
}
View(get_tracks("4K09vJ27xCOreumtSuU6Ao"))

#' @title Search for Spotify track features
#' @param ids - A vector of Spotify track ids
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track feature information, including track id and the following variables:
#'        danceability
#'        energy
#'        key
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
#' get_track_audio_features("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_audio_features <- function(ids, authorization = get_spotify_access_token()){
    assertthat::assert_that(
        length(ids) <= 100,
        msg = "The maximum length of the length of the ids vector is 100.  Please shorten the length of the inputed vector."
    )
    url <- 'https://api.spotify.com/v1/audio-features'
    parameters <- list(
        ids = paste0(ids, collapse = ','),
        access_token = authorization
    )
    result <- httr::RETRY('GET', url, query = parameters, encode = 'json', terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(httr::content(result, as = 'text', encoding = 'UTF-8'), flatten = TRUE)
    result <- result$audio_features %>% 
    dplyr::select(
        -type,
        -uri,
        -track_href,
        -analysis_url
    ) %>%
    dplyr::rename(
        track_id = id
    )

    result
}

#' @title Search for Sportify track audio analysis
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A list of audio analysis data, including meta, track, bars, beats, sections, tatums, and segments data
#' @examples
#' \dontrun{
#' get_track_audio_analysis("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @noRd
get_track_audio_analysis <- function(id, authorization = get_spotify_access_token()){
    url <- stringr::str_glue("https://api.spotify.com/v1/audio-analysis/{id}")

    parameters <- list(
        access_token = authorization
    )
    result <- httr::RETRY('GET', url, query = parameters, encode = 'json', terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(httr::content(result, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    result
}

#' @title Search for Spotify track analysis
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track data, including track id, duration, loudness, start and end of fade, tempo and confidence, time signature and confidence, key and confidence, and mode and confidence
#' @examples
#' \dontrun{
#' get_track_analysis("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_analysis <- function(id, authorization = get_spotify_access_token()){
    result <- as.data.frame(get_track_audio_analysis(id, authorization = authorization)$track) %>%
              dplyr::select(-sample_md5,
                     -codestring,
                     -code_version,
                     -echoprintstring,
                     -echoprint_version,
                     -synchstring,
                     -synch_version,
                     -rhythmstring)
    track_id <- data.frame(track_id = id)
    result <- merge(track_id, result)
    result
}

#' @title Search for Spotify track bar information
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track bar information, including track id, and start, end, and confidence of bar intervals
#' @examples
#' \dontrun{
#' get_track_bars("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_bars <- function(id, authorization = get_spotify_access_token()){
    result <- as.data.frame(get_track_audio_analysis(id, authorization = authorization)$bars)
    track_id <- data.frame(track_id = id)
    result <- merge(track_id, result)
    result
}

#' @title Search for Spotify track beats information
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track beat information, including track id, and start, end, and confidence of beat intervals
#' @examples
#' \dontrun{
#' get_track_beats("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_beats <- function(id, authorization = get_spotify_access_token()){
    result <- as.data.frame(get_track_audio_analysis(id, authorization = authorization)$beats)
    track_id <- data.frame(track_id = id)
    result <- merge(track_id, result)
    result
}

#' @title Search for Spotify track section information
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track section information, including track id, start, end, and confidence of section intervals, loudness, tempo and confidence, key and confidence, mode and confidence, and time signature and confidence
#' @examples
#' \dontrun{
#' get_track_sections("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_sections <- function(id, authorization = get_spotify_access_token()){
    result <- as.data.frame(get_track_audio_analysis(id, authorization = authorization)$sections)
    track_id <- data.frame(track_id = id)
    result <- merge(track_id, result)
    result
}

#' @title Search for Spotify track tatum information
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track tatum information, including track id, and start, end, and confidence of tatum intervals
#' @examples
#' \dontrun{
#' get_track_tatums("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_tatums <- function(id, authorization = get_spotify_access_token()){
    result <- as.data.frame(get_track_audio_analysis(id, authorization = authorization)$tatums)
    track_id <- data.frame(track_id = id)
    result <- merge(track_id, result)
    result
}

#' @title Search for Spotify track segment information
#' @param id - A single Spotify track id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track segment information, including track id, start, end, and confidence of segment intervals, loudness at the start of the interval, time of maximum loudness, the maximum loudness, the end loudness, the pitches, and the timbre
#' @examples
#' \dontrun{
#' get_track_segments("3w3y8KPTfNeOKPiqUTakBh")
#' }
#' @export
get_track_segments <- function(id, authorization = get_spotify_access_token()){
    result <- as.data.frame(get_track_audio_analysis(id, authorization = authorization)$segments)
    track_id <- data.frame(track_id = id)
    result <- merge(track_id, result)
    result
}

#' @title Search for Spotify album's track features
#' @param id - A single Spotify album id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album track data, including ids and names for the tracks, whether the track is explicit, track number in the album, artist names and ids, and the following variables:
#'        danceability
#'        energy
#'        key
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
#' get_album_track_features("58ufpQsJ1DS5kq4hhzQDiI")
#' }
#' @export
get_album_track_features <- function(id, authorization = get_spotify_access_token()){
    tracks <- get_albums_tracks(id, authorization = authorization)

    features <- get_track_audio_features(tracks$track_id, authorization = authorization)

    result <- dplyr::left_join(tracks, features, by = "track_id") %>%
              dplyr::select(
                -duration_ms.x,
                -disc_number
              ) %>%
              dplyr::rename(
                duration_ms = duration_ms.y
              )

    result
}

#' @title Search for Spotify artist's top tracks
#' @param id - A single Spotify artist id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track data, including the duration, track id and name, explicitness, popularity, track number in its album, album name and id, and artist name and id
#' @examples
#' \dontrun{
#' get_artist_top_tracks("0du5cEVh5yTK9QJze8zA0C")
#' }
#' @export
get_artist_top_tracks <- function(id, authorization = get_spotify_access_token()){
    url <- stringr::str_glue("https://api.spotify.com/v1/artists/{id}/top-tracks")
    parameters = list(
        market = "US",
        access_token = authorization
    )
    result <- httr::RETRY(verb = 'GET', url, query = parameters, encode = 'json', terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)
    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result <- jsonlite::fromJSON(httr::content(result, as = 'text', encoding = 'UTF-8'), flatten = TRUE)$tracks %>%
              dplyr::mutate(
                artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
                artist_name = purrr::map_chr(artists, ~ toString(.x$name))
                ) %>%
              dplyr::select(
                -href,
                -is_local,
                -is_playable,
                -preview_url,
                -uri,
                -album.artists,
                -album.href,
                -album.images,
                -album.is_playable,
                -album.release_date,
                -album.release_date_precision,
                -album.total_tracks,
                -album.type,
                -album.uri,
                -album.external_urls.spotify,
                -external_ids.isrc,
                -external_urls.spotify,
                -artists
                ) %>%
              dplyr::rename(
                track_id = id,
                album_type = album.album_type,
                album_name = album.name,
                track_name = name,
                album_id = album.id
              )

    result
}