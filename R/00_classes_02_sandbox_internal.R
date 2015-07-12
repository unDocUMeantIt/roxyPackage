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


# this class will only be used internally, in one single instance.
# it holds all information needed for sandboxing and will be stored
# in the package internal environment .roxyPackage.env
setClass("roxySandbox",
  representation=representation(
    active="logical",
    sandbox.dir="character",
    pck.source.dir="character",
    R.libs="character",
    repo.root="character",
    archive.root="character",
    to.dir="character"),
  prototype=prototype(
    active=FALSE,
    sandbox.dir="",
    pck.source.dir="",
    R.libs="",
    repo.root="",
    archive.root="",
    to.dir="")
)
