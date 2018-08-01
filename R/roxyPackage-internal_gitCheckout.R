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

## function git_branch()
# returns the current branch of a git repository, or an error
git_branch <- function(src_dir){
  git_cmd <- checkTools("git")
  sys_cmd <- paste0(git_cmd, " -C ", shQuote(src_dir), " branch")
  if(isUNIX()){
    all_branches <- system(sys_cmd, intern=TRUE)
  } else {
    all_branches <- shell(sys_cmd, intern=TRUE)
  }
  current_branch <- gsub("\\*[[:space:]]+", "", all_branches[which(grepl("^[[:space:]]*\\*[[:space:]]+", all_branches))])
  if("" %in% current_branch){
    stop(simpleError("git: sorry, unable to fetch the current branch name in your git repository!"))
  } else {
    return(current_branch)
  }
} ## function git_branch()


## function git_checkout()
# checks out a given branch/tag of a git repository; returns either TRUE or FALSE, depending on the result
git_checkout <- function(src_dir, ref){
  git_cmd <- checkTools("git")
  sys_cmd <- paste0(git_cmd, " -C ", shQuote(src_dir), " checkout ", shQuote(ref))
  if(isUNIX()){
    successful_checkout <- system(sys_cmd, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
  } else {
    successful_checkout <- shell(sys_cmd, translate=TRUE, ignore.stderr=TRUE, intern=FALSE)
  }
  
  if(successful_checkout != 0){
    warning(paste0("git: unable to checkout branch/tag \"", pck.version, "\", are you sure it exists?"))
    return(FALSE)
  } else {
    return(TRUE)
  }
} ## function git_checkout()
