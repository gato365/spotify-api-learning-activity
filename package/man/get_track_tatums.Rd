% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Tracks_Functions.r
\name{get_track_tatums}
\alias{get_track_tatums}
\title{Search for Spotify track tatum information}
\usage{
get_track_tatums(id, authorization = get_spotify_access_token())
}
\arguments{
\item{id}{\itemize{
\item A single Spotify track id
}}

\item{authorization}{\itemize{
\item An access_token generated from the get_spotify_access_token() function
}}
}
\value{
A dataframe of track tatum information, including track id, and start, end, and confidence of tatum intervals
}
\description{
Search for Spotify track tatum information
}
\examples{
\dontrun{
get_track_tatums("3w3y8KPTfNeOKPiqUTakBh")
}
}
