# Copyright 2022 Meik Michalke <meik.michalke@hhu.de>
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

#' Generate package file skeletons
#' 
#' Writes a new file with some basic comments and documentation hints, according to the type of R object to cover.
#' 
#' @param name Character string, name of the object the file should cover (i.e., a function or class name).
#' @param type One of \code{"function"}, \code{"S4Class"}, \code{"S4Method"}, or \code{"data"}, categorizing the object to be documented.
#' @export
# @examples

package_file_skeleton <- function(
    name,
    type=c(
        "function",
        "S4Class",
        "S4Method",
        "data"
    )
){
    type <- match.arg(type)
    txt <- switch(
        type,
        "function"=paste0(),
        "S4Class"=paste0(),
        "S4Method"=paste0(),
        "data"=paste0()
    )
    return(txt)
}
