% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Album_Functions.r
\name{get_albums}
\alias{get_albums}
\title{Search for Spotify album information}
\usage{
get_albums(ids, authorization = get_spotify_access_token())
}
\arguments{
\item{ids}{\itemize{
\item A vector of Spotify album ids
}}

\item{authorization}{\itemize{
\item An access_token generated from the get_spotify_access_token() function
}}
}
\value{
A dataframe of album data, including album name and id, label, album popularity, release date, number of tracks, and artist info
}
\description{
Search for Spotify album information
}
\examples{
\dontrun{
get_albums("1uyf3l2d4XYwiEqAb7t7fX")
get_albums(c("1uyf3l2d4XYwiEqAb7t7fX", "58ufpQsJ1DS5kq4hhzQDiI"))
}
}
