# Copyright 2014-2020 Meik Michalke <meik.michalke@hhu.de>
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

#' Package your OpenPGP keyring in Debian package format
#'
#' Similar to \code{\link[roxyPackage:debianize]{debianize}}, this function generates a Debian package,
#' but it specialises on packaging OpenPGP/GnuPG keyrings. The resulting package can be used to
#' provide keys in a Debian package repository, hence enabling secure apt. They are probably easier to handle
#' for users.
#' 
#' @param gpg.key Character string or vector, the OpenPGP key ID(s) for the key(s) that should be included in the package.
#'    All keys must be available in your keyring (or in the one specified by \code{keyring}).
#' @param repo.name Character string, name of the repository this keyring will be used for. Must not include spaces or special characters!
#' @param repo.root Character string, valid path to a directory where to build/update a local package repository.
#' @param maintainer Character string, name an mail address of the maintainer of the keyring package, in the format of
#'    \code{firstName lastName <your@@mail.address>}.
#' @param build.dir Character string, valid path to a directory where to build the package. If this directory is not empty, a temporary
#'    directory will be created inside automatically.
#' @param keyname Character string, a name for keyring. Will be used for both the exported keyring file and debian package name.
#'    Using something like \code{"myrepo-keyring"} is a good choice.
#' @param pck.source.dir Character string, path pointing to the root directory of the keyring package sources. If this
#'    directory does not exist yet, it will be created and filled with the necessary files.
#' @param version Numeric or a character string, the main Debian package version indicator for the keyring package.
#' @param revision Numeric or a character string, the Debian package revision information.
#' @param distribution Character string, the Debain (based) distribution your package is intended for.
#' @param component Character string, the Debain component of the distribution.
#' @param urgency Character string, urgency information for this release (refer to [1] if you want to change this).
#' @param URL Character string, should point to the repository this keyring package is built for.
#' @param changelog Character vector, log entries for the \code{./debian/changelog} file if it is going to be changed.
#' @param description Character string, some description of the keyring package.
#' @param actions Character vector, naming the actions to perform:
#'    \describe{
#'      \item{"bin"}{Build the Debian package.}
#'      \item{"src"}{Build a Debian source package.}
#'    }
#' @param overwrite Character vector, naming the files which should be updated:
#'    \describe{
#'      \item{"changelog"}{Update \code{./debian/changelog}, but only if no entry for this package version and revision is there yet}
#'      \item{"compat"}{Re-write \code{./debian/compat}}
#'      \item{"control"}{Re-write \code{./debian/control}}
#'      \item{"copyright"}{Re-write \code{./debian/copyright}}
#'      \item{"postinst"}{Re-write \code{./debian/postinst}}
#'      \item{"prerm"}{Re-write \code{./debian/prerm}}
#'      \item{"rules"}{Re-write \code{./debian/rules}}
#'      \item{"gpg.key"}{Re-write the exported key in \code{./keyrings/}}
#'    }
#' @param bin.opts Character string, options to pass through to \code{dpkg-buildpackage} for the \code{"bin"} action.
#' @param compat Integer value, specifying the \code{debhelper} compatibility level.
#' @param epoch Integer value, the Debian package epoch information.
#' @param keyring Character string, path to an additional keyring file to use.
#' @param gpg.version Integer number, specifiying the GnuPG major version number. By default \code{gpg2} is assumed.
#' @param sign.key Character string, the OpenPGP key ID for the key that should be used for signing the Release file (secure apt).
#'    This key must be available in your keyring (or in the one specified by \code{keyring}). Skipped if \code{NULL}.
#' @param compression Character string, compression format for Debian source packages. Currently \code{"xz"} and \code{"gzip"}
#'    are supported.
#' @param keep.build Logical. If \code{build.dir} is not \code{pck.source.dir}, work is done in generated folder with a random name. Usually it
#'    is removed afterwards, unless you set this option to \code{TRUE}.
#' @param deb.dir Character string, name to use for the root directory of the debian repository. See \code{\link[roxyPackage:debianize]{debianize}}
#'    for details.
#' @seealso \code{\link[roxyPackage:debianize]{debianize}}.
#' @export
#' @examples
#' \dontrun{
#' debianizeKeyring(
#'   gpg.key="DDCDA632",
#'   repo.name="doelle",
#'   repo.root="/var/www/repo",
#'   maintainer="A. Sistent <sistent@@eternalwondermaths.example.org>"
#' )
#' }

debianizeKeyring <- function(
  gpg.key,
  repo.name,
  repo.root,
  maintainer,
  build.dir=tempdir(),
  keyname=paste0(repo.name, "-keyring"),
  pck.source.dir=file.path(tempdir(), keyname),
  version="0.01",
  revision=1,
  distribution="unstable",
  component="main",
  urgency="low",
  URL=NULL,
  changelog=c("new upstream release"),
  description=paste("OpenPGP keyring for Debian packages hosted at the", repo.name, "repository.",
    "This keyring is necessary to use secure apt."),
  actions=c("bin", "src"),
  overwrite=c("changelog", "control", "copyright", "install", "rules", "compat"),
  bin.opts="-rfakeroot -b -uc",
  compat=9,
  epoch=NULL,
  keyring=NULL,
  gpg.version=2,
  sign.key=gpg.key,
  compression="xz",
  keep.build=FALSE,
  deb.dir="deb"
){

  # anything to do at all?
  if(!any(c("bin", "src") %in% actions)){
    return(invisible(NULL))
  } else {}

  createMissingDir(dirPath=pck.source.dir, action="deb-key", quiet=FALSE)

  ## check for sandboxing
  if(isTRUE(check.sandbox())){
    message("preparing sandbox...")
    # prepare folder structure; this will also insure sane values and abort
    # if locations are invalid. the function returns a list of paths to use
    adjust.paths <- prepare.sandbox.deb(
      pck.source.dir=pck.source.dir,
      repo.root=repo.root,
      package=keyname)
    # replace paths with sandbox
    pck.source.dir <- adjust.paths[["pck.source.dir"]]
    repo.root <- adjust.paths[["repo.root"]]
    sandbox.status()
  } else {}

  # gpg will either be NULL, "gpg" or "gpg<version>"
  gpg <- GPGversion(key=gpg.key, version=gpg.version)
  # we also need a native tar for the extra arguments to work
  neededTools <- c("dpkg-buildpackage", "fakeroot", "dpkg-source", "dpkg-genchanges", "apt-ftparchive", "tar", gpg)

  # basic checks:
  #  - is this a UNIX system?
  #  - can the needed tools be found?
  #  - can we use the build dir or need to create a temp dir?
  #  - what is the debian repo path?
  debChecked <- deb.basic.checks(
    pck.source.dir=pck.source.dir,
    repo.root=repo.root,
    build.dir=build.dir,
    actions=actions,
    neededTools=neededTools,
    renameTools=c(gpg=gpg),
    repo.name=repo.name,
    msg.action="deb-key",
    deb.dir=deb.dir
  )

  build.dir <- debChecked[["build.dir"]]
  if(all(!isTRUE(keep.build), !identical(build.dir, pck.source.dir))){
    # for security reasons, check if build dir has any content -- and don't delete it if it does!
    BD.content <- list.files(build.dir, all.files=TRUE, include.dirs=TRUE, no..=TRUE)
    if(length(BD.content) > 0){
      warning(paste0("deb-key: build.dir (", build.dir, ") not empty, it will not be removed automatically!"), call.=FALSE)
    } else {
      on.exit(unlink(build.dir, recursive=TRUE))
    }
  } else {}
  repo.deb.path <- debChecked[["repo.deb.path"]]
  buildTools <- debChecked[["buildTools"]]
  
  # define some paths
#  pck.src.path.parts <- unlist(strsplit(pck.source.dir, .Platform$file.sep))
#  pck.src.folder.name <- pck.src.path.parts[length(pck.src.path.parts)]
  deb.dir.debian <- file.path(pck.source.dir, "debian")

  deb.pckg.name <- deb.srcs.name <- debianPkgName(package=keyname, origin=NULL, version=NULL, replace.dots=FALSE)
  deb.pckg.vers <- debianPkgVersion(version=version, revision=revision, epoch=epoch)

  # the package will have the OpenPGP key in ASCII format in key/<keyID>.asc
  # so the term "keyrings" is actually a bit misleading and only kept because
  # the file eventually needs to end up in /usr/share/keyrings
  keyring.dir <- file.path(pck.source.dir, "keyrings")
  createMissingDir(dirPath=keyring.dir, action="deb-key", quiet=FALSE)
  keyring.file <- paste0(keyname, ".asc")
  keyring.file.path.local <- file.path("keyrings", keyring.file)
  keyring.file.path.full <- file.path(keyring.dir, keyring.file)

  thisYear <- format(Sys.Date(), "%Y")

  ## directory structure
  # keyname/
  #   debian/
  #     source/
  #       format
  #     changelog
  #     compat
  #     control
  #     copyright
  #     postinst
  #     prerm
  #     rules
  #   keyrings/
  #     keyname.gpg

  # check for/create directories
  deb.gen.format(deb.dir=deb.dir.debian, action="deb-key")

  deb.description <- deb.prepare.description(
    deb.description=NULL,
    R.description=NULL,
    origin=NULL,
    origin.alt=NULL,
    arch="all",
    defaults=list(
      Section="misc",
      Priority="extra",
      Depends="gpgv, ${misc:Depends}",
      "Build.Depends.Indep"="debhelper (>> 9.0.0), cdbs",
      Homepage=URL
    ),
    maintainer=maintainer,
    replace.dots=FALSE,
    isRpackage=FALSE
  )

  R.dscrptn <- data.frame(
    Package=keyname,
    License="GPL (>= 3)",
    Author=deb.description[["Maintainer"]],
    Maintainer=deb.description[["Maintainer"]],
    stringsAsFactors=FALSE
  )

  ## debian/compat
  deb.gen.compat(
    compat=compat,
    deb.dir=deb.dir.debian,
    overwrite="compat" %in% overwrite,
    action="deb-key")

  ## debian/control
  deb.gen.control(
    srcs.name=deb.pckg.name,
    deb.name=deb.pckg.name,
    description=deb.description,
    R.dscrptn=NULL,
    deb.dir=deb.dir.debian,
    overwrite=TRUE,
    arch="all",
    fullDescription=description,
    isRpackage=FALSE
  )

  ## debian/copyright
  deb.gen.copyright(
    R.dscrptn=R.dscrptn,
    deb.name=deb.pckg.name,
    description=deb.description,
    year=thisYear,
    deb.dir=deb.dir.debian,
    overwrite="copyright" %in% overwrite,
    repo.name=repo.name,
    isRpackage=FALSE,
    URL=URL
  )

  ## debian/changelog
  deb.gen.changelog(
    srcs.name=deb.pckg.name,
    version=deb.pckg.vers,
    maintainer=deb.description[["Maintainer"]], 
    logs=changelog,
    distribution=distribution,
    urgency=urgency,
    deb.dir=deb.dir.debian,
    overwrite="changelog" %in% overwrite,
    action="deb-key"
  )

  ## keyrings/<keyname>.asc
  GPGwriteKey(
    key=gpg.key,
    file=keyring.file.path.full,
    gpg=buildTools[["gpg"]],
    overwrite="gpg.key" %in% overwrite,
    keyring=keyring,
    action="deb-key"
  )
  
  ## debian/rules
  deb.gen.rules(
    deb.name=deb.pckg.name,
    maintainer=deb.description[["Maintainer"]],
    year=thisYear,
    origin=NULL,
    deb.dir=deb.dir.debian,
    overwrite="rules" %in% overwrite,
    isRpackage=FALSE,
    keyringFiles=keyring.file.path.local
  )

  ## debian/install
  deb.gen.install(
    deb.dir=deb.dir.debian,
    keyringFiles=keyring.file,
    overwrite="install" %in% overwrite
  )

  if(any(c("bin","src") %in% actions)){
    # copy sources to build dir
    deb.prepare.buildDir(
      source=pck.source.dir,
      build=build.dir,
      tar=buildTools[["tar"]]
    )
  } else {}
 
  if("src" %in% actions){
    deb.build.sources(
      srcs.name=deb.srcs.name,
      build=build.dir,
      src.dir.name=basename(pck.source.dir),
      version=version,
      repo=repo.deb.path,
      distribution=distribution,
      component=component,
      compression=compression,
      tar=buildTools[["tar"]],
      dpkg.source=buildTools[["dpkg-source"]],
      apt.ftparchive=buildTools[["apt-ftparchive"]],
      action="deb-key"
    )
  } else {}

  if("bin" %in% actions){
    deb.build.binary(
      deb.name=deb.pckg.name,
      build=build.dir,
      src.dir.name=basename(pck.source.dir),
      options=bin.opts,
      version=version,
      repo=repo.deb.path,
      revision=revision,
      distribution=distribution,
      component=component,
      arch="all",
      repo.all.arch=c("binary-i386","binary-amd64"),
      dpkg.buildpackage=buildTools[["dpkg-buildpackage"]],
      dpkg.genchanges=buildTools[["dpkg-genchanges"]],
      apt.ftparchive=buildTools[["apt-ftparchive"]],
      action="deb-key"
    )
  } else {}
 
  # update the a Release file
  if(any(c("bin","src") %in% actions)){
    deb.update.release(
      repo.root=repo.root,
      repo=repo.deb.path,
      gpg.key=sign.key,
      keyring=keyring,
      distribution=distribution,
      component=component,
      arch=c("i386", "amd64", "source"),
      apt.ftparchive=buildTools[["apt-ftparchive"]],
      gpg=buildTools[["gpg"]],
      action="deb-key"
    )
  } else {}

  if(isTRUE(keep.build)){
    message(paste0("deb-key: build files can be found under ", build.dir, "."))
    return(build.dir)
  } else {
    return(invisible(NULL))
  }
}
