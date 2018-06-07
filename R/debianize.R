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


#' Basic Debian package creation from R source packages
#' 
#' This function attempts to 'debianize' your R source package. This means, it will add a \code{debian} directory
#' to sources' root directory, and populate it with needed files for Debian package building, as outlined in the Debian R Policy
#' by Eddelbuettel & Bates (2003) and the Debian Policy Manual[1], version 3.9.3.1.
#' 
#' The file \code{./debian/source/format} will also be created only once. The files \code{./debian/control}, \code{./debian/copyright} and
#' \code{./debian/rules} will be generated from the information found in the \code{DESCRIPTION} file of the R package.
#' Once created, these files won't be touched again if they are not defined in the \code{overwrite} parameter. This enables you to save
#' files from being re-written, e.g. if you altered them manually.
#' 
#' The \code{./debian/changelog} is special here, as \code{overwrite} doesn't mean the whole file will be overwritten, but rather that the
#' function checks if the changelog already contains an entry for this particular package version and revision, and only if this is not the
#' case will add one at the beginning of the file, including the log entries defined by the \code{changelog} parameter (each string will
#' become one log entry).
#' 
#' The function will try to detect the license you specified in the \code{DESCRIPTION} file, and if it is one of the following licenses,
#' generate some useful info on how to get the full license on a Debian system:
#' \itemize{
#'    \item{Apache License}
#'    \item{Artisitic License}
#'    \item{BSD License}
#'    \item{GNU General Public License (GPL)}
#'    \item{GNU Lesser General Public License (LGPL)}
#' }
#' 
#' @section Building the actual package: If you're running the R session on a Debian based system, the function can build the debian package,
#' but it would likely fail when it comes to signing the .changes/.dsc files, because \code{gpg} gets invoked without \code{"--no-tty"}.
#' You'd have to sign those files later, e.g. with \code{debsign}, if you really need this. However, secure-apt can still be ensured, if you provide
#' a valid GnuPG key ID from your keyring, which will then be used to sign the generated \code{Release} file. If not present yet, a copy of
#' the public key will automatically be saved to the repository, in a file named \code{<key ID>.asc}.
#' 
#' Package building is done in a temporal directory, and the source files a copied there first. Set \code{build.dir=pck.source.dir} if
#' you want to build in-place instead.
#' 
#' @section Package dependencies: This function will make no attempts to guess what package dependencies must be fulfilled.
#' That is, if the defaults don't fit (see below), then you must define these dependencies yourself via the \code{deb.description}
#' parameter (setting appropriate values for fields like \code{Build.Depends}, \code{Build.Depends.Indep} and \code{Depends}). In case your R package
#' depends on other R packages, you will have to ensure that these are also available as Debian packages (and define them
#' as dependencies), so the package management can take care of resolving these dependencies transparently. Otherwise users might
#' have a hard time figuring out how to get your package to work, if the building process doesn't fail in the first place.
#' 
#' That said, you should always try to debianize the package without manual dependencies set first. After that, look at the generated \code{control}
#' file and see if there are problems at all. Usually the default method is supposed to be quite clever when it comes to detect dependencies from the
#' actual package \code{DESCRIPTION} file (it will automatically translate those into proper Debain package names, where tuning is possible via the
#' \code{depends.origin} and \code{depends.origin.alt} parameters).
#' 
#' @section Repository access: After you debianized your package and built some Debian packages, \code{debianize} will prepare a Debain package repository
#' in the specified directory (can be the same as used with \code{roxy.package}). You can now access it locally on your machine, or upload the whole thing
#' to a web server etc. Basically, it should work if you add these lines to your repository configuration:
#' 
#' \code{deb http://<URL you uploaded to>/deb <distribution> <component>}
#'
#' \code{deb-src http://<URL you uploaded to>/deb <distribution> <component>}
#' 
#' @section Debianizing arbitrary packages: With a little luck, this function can almot automatically debianize any R package sources. You can even provide
#' the \code{pck.source.dir} parameter with a URL to package sources (e.g., a source package from CRAN), and \code{debianize} will do its best to end up
#' with an installable debian package in the specified repository root.
#'
#' @note Please note that the package will always be built against the R version installed by your package management!
#' Also, this function responds to \code{\link[roxyPackage:sandbox]{sandbox}}.
#'
#' @param pck.source.dir Character string, path pointing to the root directory of your package sources, to a local R package source tarball, or
#'    a full URL to such a package tarball. Tarballs will be downloaded to a temporary directory, if needed, extracted, and then debianized.
#' @param repo.root Character string, valid path to a directory where to build/update a local package repository.
#' @param build.dir Character string, valid path to a directory where to build the package. If this directory is not empty, a temporary
#'    directory will be created inside automatically.
#' @param revision Numeric or a character string, the Debian package revision information.
#' @param repo.name Character string, the name for your debian package repository. This can be used to generate an OpenPGP debian package from the
#'    given \code{gpg.key}, unless you change the default behaviour with the parameter \code{deb.keyring.options}
#' @param origin Character string, should be either "noncran" or "other-<yourname>", used for the package name. This indicates that your package is
#'    not an official CRAN or BioC package.
#' @param distribution Character string, the Debain (based) distribution your package is intended for.
#' @param component Character string, the Debain component of the distribution.
#' @param urgency Character string, urgency information for this release (refer to [1] if you want to change this).
#' @param changelog Character vector, log entries for the \code{./debian/changelog} file if it is going to be changed.
#' @param deb.description A named list or data.frame with further information, especially for the \code{./debian/control} file. This is similar to
#'    the \code{pck.description} parameter of \code{\link[roxyPackage:roxy.package]{roxy.package}}, only with different variables.
#'    Note that if certain key values are missing, \code{debianize} will automatically use some defaults:
#'    \describe{
#'      \item{Build.Depends.Indep}{\code{"debhelper (>> 9.0.0), r-base-dev (>= <R.vers>), cdbs"}, plus Depends/Imports in \code{DESCRIPTION} in debianized format;
#'        if \code{arch} is not set to \code{"all"}, the field \code{Build.Depends} is used instead}
#'      \item{Depends}{\code{"r-base-core (>= <R vers>)"}, plus Depends/Imports in \code{DESCRIPTION} in debianized format}
#'      \item{Suggests}{Suggests in \code{DESCRIPTION} in debianized format}
#'      \item{Maintainer}{generated from \code{\link[base:Sys.info]{Sys.info}} (\code{user <login@@nodename>}), with a warning.}
#'      \item{Section}{\code{"gnu-r"}}
#'      \item{Priority}{\code{"optional"}}
#'      \item{Homepage}{URL in \code{DESCRIPTION}}
#'    }
#'    Refer to [1] for further available fields in the \code{./debian/control} file. In case you would like to add to the fields definig relations to other packages
#'    like \code{Build.Depends.Indep} or \code{Depends} rather than replacing them, provide a named list with a character vector called "append". For example:
#'    \code{Depends=list(append=c("libmysql++3"))}.
#' @param actions Character vector, naming the actions to perform:
#'    \describe{
#'      \item{"deb"}{Debianize the package sources.}
#'      \item{"bin"}{Build the Debian package.}
#'      \item{"src"}{Build a Debian source package.}
#'    }
#' @param overwrite Character vector, naming the files which should be updated:
#'    \describe{
#'      \item{"changelog"}{Update \code{./debian/changelog}, but only if no entry for this package version and revision is there yet}
#'      \item{"compat"}{Re-write \code{./debian/compat}}
#'      \item{"control"}{Re-write \code{./debian/control}}
#'      \item{"copyright"}{Re-write \code{./debian/copyright}}
#'      \item{"rules"}{Re-write \code{./debian/rules}}
#'      \item{"gpg.key"}{Re-write the keyring package in the repository (by default present packages are left unchanged)}
#'    }
#' @param depends.origin A character string to set the default origin for R packages which are a dependency of this one. In case all dependencies can be
#'    met by Debian packages from CRAN releases, you can leave this to the default setting. If you need more control, see \code{depends.origin.alt}.
#' @param depends.origin.alt A named list of alternative origins for R packages which are a dependency of this one. By default, \code{depends.origin} is used,
#'    but if you know that certain dependencies are of different origin (e.g., your own repository), you can set this here. Each list element must be named after
#'    the R package you want to set an origin for, and must be a character vector or single string, like \code{list(foo="other-janedoe")}. If more than one origin
#'    is given, they will be set as alternatives (using the pipe \code{"|"} as "or"). For full control over the package name use \code{list(foo=NULL)},
#'    which will fallback to \code{foo} as the name of the Debian package.
#' @param bin.opts Character string, options to pass through to \code{dpkg-buildpackage} for the \code{"bin"} action.
#' @param arch Character string, architecture the package is build for.
#' @param compat Integer value, specifying the \code{debhelper} compatibility level.
#' @param epoch Integer value, the Debian package epoch information.
#' @param gpg.key Character string, the GnuPG key ID for the key that should be used for signing the Release file (secure apt).
#'    This key must be available in your keyring (or in the one specified by \code{keyring}). Note that this function defaults to using the SHA256 algorithm for signing (not SHA1).
#'    Mandatory for \code{"bin"} and \code{"src"} actions.
#' @param keyring Character string, path to an additional keyring file to use.
#' @param gpg.version Integer number, specifiying the GnuPG major version number. By default \code{gpg2} is assumed.
#' @param deb.keyring.options Named list, extra options to pass through to \code{\link[roxyPackage:debianizeKeyring]{debianizeKeyring}}.
#'    By default, the value for \code{maintainer} will be taken from \code{deb.description}, and the values for \code{gpg.key},
#'    \code{repo.name}, \code{repo.root}, \code{build.dir}, \code{distribution}, \code{component}, \code{urgency}, \code{keyring}, and \code{gpg.version}
#'    are taken from the settings given with the \code{debianize} function call.
#' @param compression Character string, compression format for Debian source packages. Currently \code{"xz"} and \code{"gzip"}
#'    are supported.
#' @param keep.build Logical. If \code{build.dir} is not \code{pck.source.dir}, work is done in generated folder with a random name.
#'    Usually it is removed afterwards, unless you set this option to \code{TRUE}.
#' @param keep.existing.orig Logical, if \code{TRUE} and there is already a \code{*.orig.tar.[gz|xz]} archive in the repository matching this version,
#'    it will not be replaced with a re-packaged one but remains as is. This is useful for binary-only rebuilds.
#' @param replace.dots Logical. The proposed Debian R Policy actually asks to replace all dots in package names by hyphens. However,
#'    this is implemented differently in \code{r-cran.mk} and will lead to unbuildable packages. So the default here is to ignore the policy draft and keep dots
#'    in package names, as is true for a lot of CRAN packages as well (code is law). In case you run into problems here
#'    (symptoms include a failing .deb build because the directory \code{build/<package name>} doesn't exist), try turning this switch. If \code{TRUE}
#'    dots will be replaced by hyphens in both source and binary package names. Note that building a package by calling this function should always
#'    work, because it will automatically create a symlink in the build directory if needed.
#' @param deb.dir Character string, name to use for the root directory of the debian repository. Defaults to \code{"deb"}, which is obviously a good choice,
#'    but you might want to use different directories for different builds, e.g., a separate one for R 3.5 packages.
#' @seealso \code{\link[roxyPackage:sandbox]{sandbox}} to run debianize() in a sandbox.
#' @references
#' Eddelbuettel, D. & Bates, D. (2003). \emph{Debian R Policy -- Draft Proposal v 0.1.3}.
#'   Available from \url{http://lists.debian.org/debian-devel/2003/12/msg02332.html}
#'
#' [1] Debian Policy Manual: \url{http://www.debian.org/doc/debian-policy}
#' @export
#' @examples
#' \dontrun{
#' debianize(
#'   pck.source.dir="~/my_R_stuff/SquareTheCircle",
#'   repo.root="/var/www/repo",
#'   origin="other-doelle",
#'   revision=4,
#'   changelog=c("re-compiled docs"),
#'   deb.description=list(
#'     Depends=c("r-base-dev (>> 2.12.0), r-cran-foreign"),
#'     Maintainer="A. Sistent <sistent@@eternalwondermaths.example.org>"),
#'   actions=c("deb"))
#'
#' # let's try to debianize some R package from CRAN
#' debianize(
#'   pck.source.dir="http://cran.r-project.org/src/contrib/roxygen2_4.0.1.tar.gz",
#'   repo.root=tempdir(),
#'   deb.description=list(
#'     Maintainer="A. Sistent <sistent@@eternalwondermaths.example.org>"
#'   )
#' )
#' }

debianize <- function(
  pck.source.dir,
  repo.root,
  build.dir=tempdir(),
  revision=1,
  repo.name="roxypackage",
  origin=paste0("other-", repo.name),
  distribution="unstable",
  component="main",
  urgency="low",
  changelog=c("new upstream release"),
  deb.description=NULL,
  depends.origin="cran",
  depends.origin.alt=list(),
  actions=c("deb", "bin", "src"),
  overwrite=c("changelog", "control", "copyright", "rules", "compat"),
  bin.opts="-rfakeroot -b -uc",
  arch="all",
  compat=9,
  epoch=NULL,
  gpg.key=NULL,
  keyring=NULL,
  gpg.version=2,
  deb.keyring.options=NULL,
  compression="xz",
  keep.build=FALSE,
  keep.existing.orig=FALSE,
  replace.dots=FALSE,
  deb.dir="deb"
){

  # anything to do at all?
  if(!any(c("deb", "bin", "src") %in% actions)){
    return(invisible(NULL))
  } else {}

  # check if we're about to debianize local sources, a tarbal or
  # something we need to download first
  pck.source.dir <- deb.check.sources(src=pck.source.dir)

  ## check for sandboxing
  if(isTRUE(check.sandbox())){
    message("preparing sandbox...")
    # prepare folder structure; this will also insure sane values and abort
    # if locations are invalid. the function returns a list of paths to use
    adjust.paths <- prepare.sandbox.deb(
      pck.source.dir=pck.source.dir,
      repo.root=repo.root)
    # replace paths with sandbox
    pck.source.dir <- adjust.paths[["pck.source.dir"]]
    repo.root <- adjust.paths[["repo.root"]]
    sandbox.status()
  } else {}

  # gpg will either be NULL, "gpg" or "gpg<version>"
  gpg <- GPGversion(key=gpg.key, version=gpg.version)
  # we also need a native tar for the extra arguments to work
  neededTools <- c("apt-ftparchive", "dch", "dpkg-buildpackage", "dpkg-genchanges", "dpkg-parsechangelog", "dpkg-source", "fakeroot", "tar", gpg)

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
    deb.dir=deb.dir
  )

  build.dir <- debChecked[["build.dir"]]
  if(all(!isTRUE(keep.build), !identical(build.dir, pck.source.dir))){
    # for security reasons, check if build dir has any content -- and don't delete it if it does!
    BD.content <- list.files(build.dir, all.files=TRUE, include.dirs=TRUE, no..=TRUE)
    if(length(BD.content) > 0){
      warning(paste0("deb: build.dir (", build.dir, ") not empty, it will not be removed automatically!"), call.=FALSE)
    } else {
      on.exit(unlink(build.dir, recursive=TRUE))
    }
  } else {}
  repo.deb.path <- debChecked[["repo.deb.path"]]
  buildTools <- debChecked[["buildTools"]]

  # read the description file
  pck.dscrptn <- as.data.frame(read.dcf(file=file.path(pck.source.dir, "DESCRIPTION")), stringsAsFactors=FALSE)
  # clean from newlines
  pck.dscrptn <- as.data.frame(t(sapply(pck.dscrptn, function(x) gsub("\n", " ", x))), stringsAsFactors=FALSE)
  pck.version <- as.character(pck.dscrptn[["Version"]])
  pck.date <- as.character(pck.dscrptn[["Date"]])
  pck.package <- as.character(pck.dscrptn[["Package"]])
  pck.title <- as.character(pck.dscrptn[["Title"]])

  # define some paths
  deb.dir.debian <- file.path(pck.source.dir, "debian")

  # deb.pckg.name.lower is needed to work around possible implementation bugs, see below!
  deb.pckg.name.lower <- debianPkgName(package=pck.package, origin=origin, version=NULL, replace.dots=FALSE)
  deb.pckg.name <- deb.srcs.name <- debianPkgName(package=pck.package, origin=origin, version=NULL, replace.dots=replace.dots)
  deb.pckg.vers <- debianPkgVersion(version=pck.version, revision=revision, epoch=epoch)

  thisYear <- format(Sys.Date(), "%Y")

  ## debianize: create & populate debian directory
  if("deb" %in% actions){
    # we'll try the quilt way:
    # debian/
    #   source/
    #     format       (includes only the line "3.0 (quilt)")
    #   changelog
    #   control
    #   compat
    #   copyright
    #   rules

    # check for/create directories
    deb.gen.format(deb.dir=deb.dir.debian)

    # check missing contents of deb.description and set defaults
    deb.description <- deb.prepare.description(
      deb.description=deb.description,
      R.description=pck.dscrptn,
      origin=depends.origin,
      origin.alt=depends.origin.alt,
      arch=arch,
      replace.dots=replace.dots)

    ## debian/compat
    deb.gen.compat(
      compat=compat,
      deb.dir=deb.dir.debian,
      overwrite="compat" %in% overwrite)

    ## debian/control
    deb.gen.control(
      srcs.name=deb.srcs.name,
      deb.name=deb.pckg.name,
      description=deb.description,
      R.dscrptn=pck.dscrptn,
      deb.dir=deb.dir.debian,
      overwrite="control" %in% overwrite,
      arch=arch
    )

    ## debian/copyright
    deb.gen.copyright(
      R.dscrptn=pck.dscrptn,
      deb.name=deb.pckg.name,
      description=deb.description,
      year=thisYear,
      deb.dir=deb.dir.debian,
      overwrite="copyright" %in% overwrite
    )

    ## debian/changelog
    deb.gen.changelog(
      srcs.name=deb.srcs.name,
      version=deb.pckg.vers,
      maintainer=deb.description[["Maintainer"]], 
      logs=changelog,
      distribution=distribution,
      urgency=urgency,
      deb.dir=deb.dir.debian,
      overwrite="changelog" %in% overwrite
    )

    ## debian/rules
    deb.gen.rules(
      deb.name=deb.pckg.name,
      maintainer=deb.description[["Maintainer"]],
      year=thisYear,
      origin=origin,
      deb.dir=deb.dir.debian,
      overwrite="rules" %in% overwrite
    )
  } else {}
  ## end "deb"

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
      version=pck.version,
      repo=repo.deb.path,
      distribution=distribution,
      component=component,
      compression=compression,
      keep.existing.orig=keep.existing.orig,
      tar=buildTools[["tar"]],
      dpkg.source=buildTools[["dpkg-source"]],
      apt.ftparchive=buildTools[["apt-ftparchive"]]
    )
  } else {}

  if("bin" %in% actions){
    deb.build.binary(
      deb.name=deb.pckg.name,
      build=build.dir,
      src.dir.name=basename(pck.source.dir),
      options=bin.opts,
      version=pck.version,
      repo=repo.deb.path,
      revision=revision,
      distribution=distribution,
      component=component,
      arch=arch,
      repo.all.arch=c("binary-i386","binary-amd64"),
      dpkg.buildpackage=buildTools[["dpkg-buildpackage"]],
      dpkg.genchanges=buildTools[["dpkg-genchanges"]],
      apt.ftparchive=buildTools[["apt-ftparchive"]],
      deb.name.lower=deb.pckg.name.lower
    )
  } else {}

  # update the a Release file
  if(any(c("bin","src") %in% actions)){
    # set options for keyring packaging
    keyring.options <- mergeOptions(
      someFunction=debianizeKeyring,
      customOptions=deb.keyring.options,
      newDefaults=list(
        gpg.key=gpg.key,
        repo.name=repo.name,
        repo.root=repo.root,
        maintainer=deb.description[["Maintainer"]],
        build.dir=build.dir,
        distribution=distribution,
        component=component,
        compression=compression,
        urgency=urgency,
        keyring=keyring,
        gpg.version=gpg.version,
        deb.dir=deb.dir
      )
    )
    # check for debian keyring in repo
    deb.keyring.in.repo(
      repo.root=repo.root,
      gpg.key=gpg.key,
      keyring.options=keyring.options,
      gpg=buildTools[["gpg"]],
      overwrite="gpg.key" %in% overwrite,
      keyring=keyring,
      deb.dir=deb.dir
    )

    deb.update.release(
      repo.root=repo.root,
      repo=repo.deb.path,
      gpg.key=gpg.key,
      keyring=keyring,
      distribution=distribution,
      component=component,
      arch=c("i386", "amd64", "source"),
      apt.ftparchive=buildTools[["apt-ftparchive"]],
      gpg=buildTools[["gpg"]]
    )
  } else {}

  if(isTRUE(keep.build)){
    message(paste0("deb: build files can be found under ", build.dir, "."))
    return(build.dir)
  } else {
    return(invisible(NULL))
  }
}
