% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_all_classes.R
\docType{class}
\name{PugView-classes}
\alias{PugView-classes}
\title{Classes for Pug View Request}
\description{
The Pug View API of PubChem database returns more detailed information about a PubChem request, such as assays, compounds,
substances, etc. A super-class \code{PugViewInstance} is defined, which is returned from \link{get_pug_view} function. This class has
slots detailed below.
}
\note{
Pug View API returns many section about the requested instance, which includes detailed information from PubChem database.
There may be many nested sections, where each contains details about different features of the instance requested. These sections
can be listed via \link{sectionList} function.

Other classes, called \code{PugViewSectionList} and \code{PugViewSection}, are defined to control the outputs of available sections
and sub-sections returned from \link{get_pug_view}. See related functions for details.
}
\section{Slots}{

\describe{
  \item{\code{results}:}{A list containing elements of each of the requested compounds, assays, substances, etc.}
  \item{\code{request_args}:}{A list containing the input arguments of a PubChem request.}
  \item{\code{success}:}{A logical value indicating whether the request was successfully completed (TRUE) or not (FALSE).}
  \item{\code{error}:}{A list detailing any errors encountered during the request, if applicable.}
}
}

\seealso{
\link{section}, \link{sectionList}
}
