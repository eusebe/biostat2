% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/officer.r
\name{body_add_normal}
\alias{body_add_normal}
\title{Add a new paragraph with a Normal style}
\usage{
body_add_normal(x, value, style = "Normal", pos = "after")
}
\arguments{
\item{x}{the rdocx object (created with the read_docx function of officer package)}

\item{value}{a character string}

\item{style}{a character string}

\item{pos}{where to add the new element relative to the cursor, one of "after", "before", "on"}
}
\value{
a new rdocx object
}
\description{
Add a new paragraph with a Normal style
}
\examples{
\dontrun{
library(biostat2)
library(officer)
doc <- body_add_normal(doc, "Coucou")
}
}
\author{
David Hajage
}
\keyword{univar}
