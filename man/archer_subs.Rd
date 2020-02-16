\name{archer_subs}
\alias{archer_subs}
\docType{data}
\title{
Archer subtitles (seasons 1-10)
}
\description{
TV show subtitles in tidy format
}
\usage{archer_subs}
\format{
  A data frame with 49194 observations on the following 7 variables.
  \describe{
    \item{\code{linenumber}}{a numeric vector}
    \item{\code{timecode_in}}{a hms}
    \item{\code{timecode_out}}{a hms}
    \item{\code{text}}{a character vector}
    \item{\code{season}}{a character vector}
    \item{\code{episode}}{a character vector}
    \item{\code{episode_name}}{a character vector}
  }
}
\source{
Open Subtitles (opensubtitles.org)
}
\examples{
archer_subs
}
\keyword{datasets}
