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
#' All values must be a single character string.
#' 
#' @param Package
#' @param Title
#' @param Description
#' @param AuthorsR
#' @param Author
#' @param Maintainer
#' @param Depends
#' @param Imports
#' @param Enhances
#' @param Suggests
#' @param VignetteBuilder
#' @param URL
#' @param BugReports
#' @param Additional_repositories
#' @param Type "Package"
#' @param License "GPL (>= 3)"
#' @param Encoding "UTF-8"
#' @param LazyLoad "yes"
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
  LazyLoad="yes"
){
  result <- data.frame(
    Package=Package,
    Type=Type,
    Title=Title,
    Description=Description,
    AuthorsR=AuthorsR,
    Author=Author,
    Maintainer=Maintainer,
    Depends=Depends,
    Imports=Imports,
    Enhances=Enhances,
    Suggests=Suggests,
    VignetteBuilder=VignetteBuilder,
    URL=URL,
    BugReports=BugReports,
    Additional_repositories=Additional_repositories,
    License=License,
    Encoding=Encoding,
    LazyLoad=LazyLoad,
    stringsAsFactors=FALSE
  )
  # TODO: check: is.character(result)
  return(result)
}
