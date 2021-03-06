% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/items.R
\name{gu_items}
\alias{gu_items}
\title{Items}
\usage{
gu_items(query = NULL, show_fields = "all", show_tags = "all",
  tag = NULL, from_date = NULL, to_date = NULL,
  use_date = "published", ..., verbose = TRUE, tidy = TRUE,
  tidy_style = "snake_case")
}
\arguments{
\item{query}{A string, containing the search query, either the URL of a
single item or all items listed under a given profile, tag, etc.
For example, to return all articles by a given contributor, use
\code{"profile/{contributorname}"}, e.g. \code{"profile/brianlogan"}.}

\item{show_fields}{A string or character vector of fields to include in
the returned data. Defaults to \code{"all"}. See details for a list of options.}

\item{show_tags}{A string or character vector of tags to include in
the returned data. Defaults to \code{"all"}. See details for a list of options.}

\item{tag}{A string or character vector of tags to filter
the returned data. Defaults to \code{NULL}.}

\item{from_date}{Accepts character values in \code{'YYYY-MM-DD'}
format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
\code{POSIXlt} or anything else that can be coerced to a date with
\code{as.Date()}. Defaults to \code{NULL}.}

\item{to_date}{Accepts character values in \code{'YYYY-MM-DD'}
format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
\code{POSIXlt} or anything else that can be coerced to a date with
\code{as.Date()}. Defaults to \code{NULL}.}

\item{use_date}{The date type to use for the \code{from_date} and \code{to_date}
parameters. One of \code{"published"}, \code{"first-publication"},
\code{"newspaper-edition"} or \code{"last-modified"}. Defaults to \code{"published"}.}

\item{...}{Use to pass any other parameters to the API. See the
\href{https://open-platform.theguardian.com/documentation/item}{item docs} for a
full list of options, including those not included here.}

\item{verbose}{Prints messages to console. Defaults to \code{TRUE}.}

\item{tidy}{Convert variable names to snake_case, remove some \code{"<NA>"}
strings. Defaults to \code{TRUE}.}

\item{tidy_style}{Style to variable names with.}
}
\value{
A tibble.
}
\description{
Query and return one or more API items.

See the
\href{https://open-platform.theguardian.com/documentation/item}{API docs} for
full details on the query options available for this endpoint.
}
\section{Fields options}{


The following are the options for the \code{show_fields} parameter:
\itemize{
\item "\code{all}" Includes all the fields (default)
\item "\code{trailText}"
\item "\code{headline}"
\item "\code{showInRelatedContent}" Whether this content can appear in automatically
generated Related Content
\item "\code{body}"
\item "\code{lastModified}"
\item "\code{hasStoryPackage}" Has related content selected by editors
\item "\code{score}" A relevance score based on the search query used
\item "\code{standfirst}"
\item "\code{shortUrl}"
\item "\code{thumbnail}"
\item "\code{wordcount}"
\item "\code{commentable}"
\item "\code{isPremoderated}" Comments will be checked by a moderator prior to
publication if true.
\item "\code{allowUgc}" May have associated User Generated Content. This typically
means the content has an associated Guardian Witness assignment
which can be accessed by querying "\code{show-references=witness-assignment}",
using the \code{query} parameter.
\item "\code{byline}"
\item "\code{publication}"
\item "\code{internalPageCode}"
\item "\code{productionOffice}"
\item "\code{shouldHideAdverts}" Adverts will not be displayed if true
\item "\code{liveBloggingNow}" Content is currently live blogged if true
\item "\code{commentCloseDate}" The date the comments have been closed
\item "\code{starRating}"
}
}

\section{The following are the options for the \code{show_tags} parameter}{

\itemize{
\item "\code{blog}"
\item "\code{contributor}"
\item "\code{keyword}"
\item "\code{newspaper-book}"
\item "\code{newspaper-book-section}"
\item "\code{publication}"
\item "\code{series}"
\item "\code{tone}"
\item "\code{type}"
\item "\code{all}": The default option.
}
}

\examples{
\dontrun{
x <- gu_content(query = "films")
}

}
