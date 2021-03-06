% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sections.R
\name{gu_section}
\alias{gu_section}
\title{Sections}
\usage{
gu_section(query = NULL, ..., verbose = TRUE, tidy = TRUE,
  tidy_style = "snake_case")
}
\arguments{
\item{query}{A string, containing the search query. Defaults to \code{NULL},
which returns all available sections subject to other parameters.
Supports AND, OR and NOT operators, and exact phrase queries
using double quotes. E.g. \code{'"football" OR "politics"'}. Also accepts a
character vector of section names and returns those sections.}

\item{...}{Use to pass any other parameters to the API. See the
\href{https://open-platform.theguardian.com/documentation/search}{docs} for a
full list of options.}

\item{verbose}{Prints messages to console. Defaults to \code{TRUE}.}

\item{tidy}{Convert variable names to snake_case, remove some \code{"<NA>"}
strings. Defaults to \code{TRUE}.}

\item{tidy_style}{Style to variable names with.}
}
\description{
Returns details on the sections and subsections used to
organise content.

See the
\href{https://open-platform.theguardian.com/documentation/section}{API docs} for
full details on the query options available for the sections endpoint.
}
\examples{
\dontrun{
business <- gu_section(query = "business")

foot_pol <- gu_section(query = c("politics", "business", "football"))
}

}
