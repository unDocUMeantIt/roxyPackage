# Copyright 2018 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package roxyPackage.
#
# roxyPackage is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# roxyPackage is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with roxyPackage.  If not, see <http://www.gnu.org/licenses/>.

#' Generate a valid package description
#' 
#' Use this function to describe your package. It will do some plausibility checks
#' and make sure you end up with the correct format and all info needed for proper packaging.
#' 
#' It might seem odd that it does not use the dots argument for additional parameters.
#' That is because it would be next to impossible to check for wrong spelling of
#' the default parameters.
#' 
#' All values must be a single character string, except \code{extra} (named character vector).
#' Logical values are also possible for \code{LazyLoad} and all entries of \code{extra}.
#' They will be translated into \code{"yes"} or \code{"no"}.
#' 
#' @param Package Mandatory: Name of the package.
#' @param Title Mandatory: Short description in one catchy sentence and with proper capitalization.
#' @param Description Mandatory: Long description.
#' @param AuthorsR Mandatory: A character string that, if parsed and evaluated, will result in a vector of \code{\link[utils::person]{person}} objects (see example).
#'    All authors, maintainers and significant comtributors must be given.
#' @param Author Optional author field in old format. Should be omitted for CRAN releases, as it is automatically generated from \code{AuthorsR}.
#' @param Maintainer Like \code{Author}, but for the maintainer field.
#' @param Depends Optional: Comma separated names of packages this package depends on.
#' @param Imports Optional: Comma separated names of packages this package imports from.
#' @param Enhances Optional: Comma separated names of packages this package enhances.
#' @param Suggests Optional: Comma separated names of packages this package suggests.
#' @param VignetteBuilder Optional: Specify a vignette builder, e.g., \code{"knitr"} for vignettes in RMarkdown format.
#' @param URL Optional: Homepage.
#' @param BugReports Optional URL to a bug tracker, mailing list etc.
#' @param Additional_repositories Optional URL to additional repositories for suggested packages that are not available from the standard repos.
#' @param Type "Package" Package type, mandatory.
#' @param License "GPL (>= 3)" Optional: License information.
#' @param Encoding "UTF-8" Optional: Default character encoding.
#' @param LazyLoad "yes" Optional: Should lazy loading be supported?
#' @param extra A named character vector with additional extra fields not explicitly defined above, will be added as-is.
#' @return A data.frame.
#' @export
#' @examples
#' pckg.dscrptn <- package_description(
#'   Package="SquareTheCircle",
#'   Type="Package",
#'   Title="Squaring the circle using Heisenberg compensation",
#'   Author="Ernst Dölle [aut, cre, cph], Ludwig Dölle [trl, ctb] (initial translation to whitespace)",
#'   AuthorsR="c(person(given=\"Ernst\", family=\"Dölle\",
#'        email=\"e.a.doelle@example.com\",
#'        role=c(\"aut\", \"cre\", \"cph\")),
#'      person(given=\"Ludwig\", family=\"Dölle\",
#'        role=c(\"trl\", \"ctb\"),
#'        comment=\"initial translation to whitespace\")
#'      )",
#'   Maintainer="E.A. Dölle <doelle@@eternalwondermaths.example.org>",
#'   Depends="R (>= 2.10.0),heisenberg (>= 0.23),tools",
#'   Enhances="rkward",
#'   Description="This package squares the circle using Heisenberg compensation.
#'       The code came from a meeting with Yrla Nor that i had in a dream. Please
#'       don't forget to chain your computer to the ground, because these
#'       algorithms might make it fly.",
#'   License="GPL (>= 3)",
#'   Encoding="UTF-8",
#'   LazyLoad="yes",
#'   URL="http://eternalwondermaths.example.org"
#' )
package_description <- function(
  Package,
  Title,
  Description,
  AuthorsR,
  Author=NULL,
  Maintainer=NULL,
  Depends=NULL,
  Imports=NULL,
  Enhances=NULL,
  Suggests=NULL,
  VignetteBuilder=NULL,
  URL=NULL,
  BugReports=NULL,
  Additional_repositories=NULL,
  Type="Package",
  License="GPL (>= 3)",
  Encoding="UTF-8",
  LazyLoad="yes",
  extra=c()
){
  all_args <- as.list(match.call()[-1])
  all_args_extra <- as.list(all_args[["extra"]])[-1]
  all_args[["extra"]] <- NULL

  result <- data.frame(
    Package=Package,
    Type=Type,
    Title=Title,
    Description=Description,
    AuthorsR=AuthorsR,
    stringsAsFactors=FALSE
  )

  # we'll do this separately for all_args and all_args_extra for easier checking
  for (this_arg in names(all_args)){
    if(is.null(all_args[[this_arg]])){
      next
    } else if(is.character(all_args[[this_arg]])){
      result[[this_arg]] <- all_args[[this_arg]]
    } else if(all(
      this_arg %in% c("LazyLoad"),
      is.logical(all_args[[this_arg]])
    )){
      result[[this_arg]] <- ifelse(isTRUE(all_args[[this_arg]]), "yes", "no")
    } else {
      stop(simpleError(
        paste0(
          "all values must be character, please check the following:\n  ", this_arg
        )
      ))
    }
  }
  for (this_arg in names(all_args_extra)){
    if(is.null(all_args_extra[[this_arg]])){
      next
    } else if(is.character(all_args_extra[[this_arg]])){
      result[[this_arg]] <- all_args_extra[[this_arg]]
    } else if(is.logical(all_args_extra[[this_arg]])){
      result[[this_arg]] <- ifelse(isTRUE(all_args_extra[[this_arg]]), "yes", "no")
    } else {
      stop(simpleError(
        paste0(
          "all values must be character, please check the following:\n  ", this_arg
        )
      ))
    }
  }
  # final check: is.character(result)
  characterCheck <- sapply(result, is.character)
  if(!all(characterCheck)){
    stop(simpleError(
      paste0(
        "all values must be character, please check the following:\n  ",
        paste0(names(result)[!characterCheck], collapse=", ")
      )
    ))
  }
  return(result)
}
