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


#' Run actions in a sandbox
#'
#' If you want to test the effects of \code{\link[roxyPackage:roxy.package]{roxy.package}},
#' \code{\link[roxyPackage:archive.packages]{archive.packages}} or \code{\link[roxyPackage:debianize]{debianize}},
#' you can activate a sandbox with this function.
#'
#' Sandboxing means that you are able to specify which groups of actions should
#' only be run in a separate environment. This can be useful if you don't want
#' to make changes to your actual package code, but inspect the result first.
#'
#' With this function, you can turn sandboxing on and off. This setting has effects
#' only in the currently running R session. By default, sandboxing is off.
#' 
#' @param active Logical, whether sandboxing should be active or not
#' @param sandbox.dir Character string, full path to the sandbox root directory to use.
#'    Will be created if neccessary (at first use, not when setting this here!).
#' @param pck.source.dir Logical, whether to sandbox the package sources. If \code{TRUE}
#'    the full package sources will be copied to \code{file.path(sandbox.dir, "src")}
#'    (at first use, not when setting this here!).
#' @param R.libs Logical, whether to sandbox the R library directory, that is, the directory
#'    to install the package to. Since this needs also to provide all package dependencies,
#'    those packages will be copied to \code{file.path(sandbox.dir, "R")}
#'    (at first use, not when setting this here!).
#' @param repo.root Logical, whether to sandbox the repository. This repository will be
#'    set up in \code{file.path(sandbox.dir, "repo")} (at first use, not when setting this here!).
#' @param archive Logical, whether to sandbox the repository archive. The archive will be
#'    set up in \code{file.path(sandbox.dir, "repo_archive")} (at first use, not when setting this here!).
#' @return Settings are stored in an internal environment, so there is no actual return value.
#' @seealso \code{\link[roxyPackage:sandbox.status]{sandbox.status}} to see the current settings.
#' @export
#' @examples
#' \dontrun{
#' # turn sandboxing on
#' sandbox(active=TRUE)
#' }
sandbox <- function(active=FALSE,
  sandbox.dir=file.path(tempdir(),"roxyPackge","sandbox"),
  pck.source.dir=TRUE, R.libs=TRUE, repo.root=TRUE, archive=repo.root){

  if(isTRUE(active)){
    if(!is.character(sandbox.dir)){
      stop(simpleError("'sandbox.dir' must be character!"))
    } else {
      # normalize root path
      sandbox.dir <- normalizePathByOS(path=sandbox.dir, is.unix=isUNIX(), mustWork=FALSE)
    }
  } else {
    sandbox.dir <- ""
  }

  if(isTRUE(pck.source.dir) && isTRUE(active)){
    set.pck.source.dir <- file.path(sandbox.dir, "src")
  } else {
    set.pck.source.dir <- ""
  }
  if(isTRUE(R.libs) && isTRUE(active)){
    set.R.libs <- file.path(sandbox.dir, "R")
  } else {
    set.R.libs <- ""
  }
  if(isTRUE(repo.root) && isTRUE(active)){
    set.repo.root <- file.path(sandbox.dir, "repo")
  } else {
    set.repo.root <- ""
  }
  if(isTRUE(archive) && isTRUE(active)){
    set.archive.root <- sandbox.dir
    set.to.dir <- "repo_archive"
  } else {
    set.archive.root <- ""
    set.to.dir <- ""
  }

  # get and check sandbox settings
  snd.config <- get.roxyEnv("sandbox")
  if(!inherits(snd.config, "roxySandbox")){
    warning("got strange readings for sandbox settings, resetting to default")
    snd.config <- new("roxySandbox", active=FALSE)
    set.roxyEnv(name="sandbox", value=snd.config)
  } else {}

  missionSettings <- c(
    sandbox.dir=sandbox.dir,
    pck.source.dir=set.pck.source.dir,
    R.libs=set.R.libs,
    repo.root=set.repo.root,
    archive.root=set.archive.root,
    to.dir=set.to.dir)
  # for prettier message printout, determine the longest option
  alignment.max <- max(nchar(names(missionSettings))) + 1

  for (thisSetting in names(missionSettings)){
    old.setting <- slot(snd.config, thisSetting)
    new.setting <- missionSettings[[thisSetting]]
    if(!identical(old.setting, new.setting)){
      slot(snd.config, thisSetting) <- new.setting
    } else {}
    rm("old.setting", "new.setting")
  }
  if(is.logical(active)){
    if(!identical(slot(snd.config, "active"), active)){
      slot(snd.config, "active") <- active
    } else {}
  }
  
  # write new settings back to enviroment
  set.roxyEnv(name="sandbox", value=snd.config)
  message(show(snd.config))

  return(invisible(NULL))
}

#' Show sandboxing status
#'
#' This function prints the current sandbox settings. It has no parameters.
#'
#' @seealso \code{\link[roxyPackage:sandbox]{sandbox}} to change these settings.
#' @return The function invisibly returns the sandbox root directory path (\code{sandbox.dir}).
#'    If sandboxing is inactive, this is an empty character string  (\code{""}).
#' @export
#' @examples
#' \dontrun{
#' sandbox.status()
#' }
sandbox.status <- function(){
  # get sandbox settings
  snd.settings <- get.roxyEnv("sandbox")
  snd.path <- slot(snd.settings, "sandbox.dir")
  message(show(snd.settings))
  return(invisible(snd.path))
}
