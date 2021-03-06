% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/officer.r
\name{body_add_flextable_autofit}
\alias{body_add_flextable_autofit}
\title{add a table into an rdocx object, with autofiting of width}
\usage{
body_add_flextable_autofit(x, value, align = "center", pos = "after",
  split = FALSE, landscape = FALSE)
}
\arguments{
\item{x}{an rdocx object}

\item{value}{\code{flextable} object}

\item{align}{left, center (default) or right}

\item{pos}{where to add the flextable relative to the cursor, one of "after", "before", "on" (end of line).}

\item{split}{set to TRUE if you want to activate Word option 'Allow row to break across pages'.}

\item{landscape}{is the table inserted in a landscape section? (default: FALSE)}
}
\value{
A new \code{rdocx} object
}
\description{
add a table into an rdocx object, with autofiting of width
}
\examples{
\dontrun{
library(biostat2)
library(officer)
library(flextable)
mytable <- flextable(head(iris))
doc <- read_docx()
doc <- body_add_flextable(doc, mytable)
doc <- body_add_break(doc)
doc <- body_add_flextable_autofit(doc, mytable)
doc <- body_end_section_portrait(doc)
doc <- body_add_flextable(doc, mytable)
doc <- body_add_break(doc)
doc <- body_add_flextable_autofit(doc, mytable, landscape = TRUE)
doc <- body_end_section_landscape(doc)

print(doc, target = "toto.docx")
}
}
\author{
David Hajage
}
\keyword{univar}
