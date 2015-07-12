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


# show method, used internally to print status reports
setMethod("show", signature(object="roxySandbox"), function(object){
  missionMessage <- paste("sandbox settings\n")

  missionSettings <- c(
    sandbox.dir=object@sandbox.dir,
    pck.source.dir=object@pck.source.dir,
    R.libs=object@R.libs,
    repo.root=object@repo.root,
    archive.root=object@archive.root)
  # for prettier message printout, longest option + 1
  alignment.max <- 15

  for (thisSetting in names(missionSettings)){
    if(identical(thisSetting, "archive.root")){
      # we need to combine this with to.dir first,
      # but only if there's actual values set
      archive.root <- missionSettings[["archive.root"]]
      to.dir <- slot(object, "to.dir")
      if(!identical(archive.root, "") && !identical(to.dir, "")){
        new.setting <- file.path(archive.root, to.dir)
      } else {
        new.setting <- ""
      }
      # for the printout, overwrite thisSetting with "archive"
      thisSetting <- "archive"
    } else {
      new.setting <- missionSettings[[thisSetting]]
    }
    alignment <- paste(rep(" ", alignment.max - nchar(thisSetting)), collapse="")
    if(identical(new.setting, "")){
      create.status <- "(not sandboxed)\n"
    } else {
      if(!file_test("-d", new.setting)){
        create.status <- " (created on demand)\n"
      } else {
        create.status <- " (exists)\n"
      }
    }
    missionMessage <- paste0(missionMessage, "  ", thisSetting, ":", alignment, new.setting, create.status)
    rm("new.setting")
  }
  missionMessage <- paste0(missionMessage, "\n  sandboxing active: ", object@active)
  message(missionMessage)
})
