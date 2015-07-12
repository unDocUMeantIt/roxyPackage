# Copyright 2011-2014 Meik Michalke <meik.michalke@hhu.de>
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


# this file covers all internal functions to manage ChangeLogs

# classes for ChangeLog objects
setClass("ChangeLog.items",
  representation=representation(
    plain="character",
    sections="list"),
  prototype=prototype(
    plain=character(),
    sections=list())
)

setClass("ChangeLog.entry",
  representation=representation(
    version="character",
    date="character",
    entry="ChangeLog.items"),
  prototype=prototype(
    version=character(),
    date=character(),
    entry=new("ChangeLog.items"))
)

setClass("ChangeLog",
  representation=representation(
    fullLog="character",
    package="character",
    entries="list"),
  prototype=prototype(
    fullLog=character(),
    package=character(),
    entries=list())
)

setValidity("ChangeLog", function(object){
  entries <- object@entries
  if(length(entries) > 0){
    if(all(sapply(entries, function(thisEntry){
        inherits(thisEntry, "ChangeLog.entry")
      }))){
      return(TRUE)
    } else {
      stop(simpleError("Invalid ChangeLog object: Wrong class in entries."))
    }
  } else {}
  return(TRUE)
})
