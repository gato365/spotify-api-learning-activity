search_helper <- function(query, type, authorization = get_spotify_access_token()){
    search_url <- "https://api.spotify.com/v1/search"

    if(length(type) != 1){
        stop("Please input ONE and only ONE type!")
    }

    parameters <- list(
        q = query,
        type = type,
        market = "US",
        limit = 1,
        offset = 0,
        access_token = authorization
    )

    res <- httr::RETRY("GET", search_url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))

    httr::stop_for_status(res)

    res <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                    flatten = TRUE)

    res <- res[[str_glue('{type}s')]]$items %>%
            as_tibble

    res 
}

search_spotify <- function(queries, type, authorization = get_spotify_access_token()){
    purrr::map_df(queries, ~search_helper(query = .x, type = type, authorization = authorization)) %>%
    bind_rows
}