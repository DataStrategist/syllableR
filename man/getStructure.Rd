% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{getStructure}
\alias{getStructure}
\title{Takes a sentence and figures out the syllable inflection for each word}
\usage{
getStructure(
  sentence = "Damn that Little Mouse cause i'm the Albatross",
  explain = FALSE
)
}
\arguments{
\item{sentence}{text to parse, Default: 'Damn that Little Mouse cause i'm
the Albatross'}

\item{explain}{if FALSE, just returns the syllable intonation, if TRUE, will
return the input sentence with the corresponding syllable numbers,
Default: FALSE}
}
\value{
Will give you a list of 0, 1, 2 depending on where the emphasis falls
on each syllable.
}
\description{
depending on the parameters, it'll either give back a string of
numbers, or a
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[purrr]{map}}
}
