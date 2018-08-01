# Copyright 2011-2018 Meik Michalke <meik.michalke@hhu.de>
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


#' Automatic doc creation, package building and repository update
#'
#' This function should help to create R packages with full documentation and updates to a local repository.
#' It supports source and binary packaging (Windows and Mac OS X; see Note section on the limitations).
#'
#' For the documentation \code{roxygen2}[1] is used. Next to the actual in-line documentation of the package's contents, you only need to
#' prepare a data.frame to be used to write a package \code{DESCRIPTION} file. See the example section for details on that. This means
#' that you \emph{neither} edit the \code{DESCRIPTION} \emph{nor} the \code{*-package.R} file manually, they will both be created \emph{automatically}
#' by this function with contents according to these settings!
#' 
#' @section Sandboxing:
#' If you want to check out the effects of roxy.package() without touching you actual package sources, try \code{\link[roxyPackage:sandbox]{sandbox}}
#' to set up a safe testing environment.
#'
#' @section Repository layout:
#' The repository will have this directory structure, that is, below the defined \code{repo.root}:
#'
#' \describe{
#'    \item{\code{./src/contrib}}{Here go the source packages}
#'    \item{\code{./bin/windows/contrib/$RVERSION}}{Here go the Windows binaries}
#'    \item{\code{./bin/macosx/contrib/$RVERSION}}{Here go the Mac OS X binaries (see \code{OSX.repo} for further options)}
#'    \item{\code{./pckg/index.html}}{A global package index with links to packages' index files, if actions included \code{"html"}}
#'    \item{\code{./pckg/web.css}}{A CRAN-style CSS file, if actions included \code{"html"}}
#'    \item{\code{./pckg/$PACKAGENAME}}{Here go documentation PDF and vignette, as well as a \code{ChangeLog} file, if found.
#'      and an \code{index.html} with package information, if actions included \code{"html"}.
#'      This is probably a bit off-standard, but practical if you several packages.}
#' }
#'
#' @section Converting ChangeLogs into NEWS:
#' See \code{\link[roxyPackage:cl2news]{cl2news}} for details.
#' 
#' @section Build for multiple R versions:
#' The options \code{R.libs} and \code{R.homes} can take a vector of strings. This can be used to build packages for multiple R versions,
#' provided you installed them on your system. By default, \code{roxy.package} will only use the first entry of both and ignore the rest,
#' except if you use the \code{"buildEmAll"} action. This makes it easy to use \code{roxy.package} in a script, as you can turn multiple builds
#' on and off with one action, and leave the rest untouched.
#' 
#' If you're running GNU/Linux, an easy way of preparing for multiple builds is to fetch the R sources from CRAN, calling
#' \code{"./configure"} with something like \code{"--prefix=$HOME/R/<R version>"}, so that \code{"make install"} installs to that path.
#' Let's assume you did that with R 3.4.4 and 3.3.3, you could then call \code{roxy.package} with options like
#' \code{R.homes=c("home/user/R/R-3.4.4", "home/user/R/R-3.3.3")} and \code{R.libs=c("home/user/R/R-3.4.4/lib64/R/library",}
#' \code{"home/user/R/R-3.3.3/lib64/R/library")}. If you add \code{"buildEmAll"} to the actions to perform, \code{roxy.package} will then
#' call itself recursively for each given R installation; if you omit \code{"buildEmAll"}, it will only build packages for R 3.4.4, as that
#' is the first configured version.
#'
#' One thing you should be aware of is that \code{roxy.package} will not perform all actions each time. That is because some of them, namely
#' \code{"roxy"}, \code{"cite"}, \code{"license"}, \code{"doc"}, \code{"cl2news"}, \code{"news2rss"}, \code{"cleanRd"}, \code{"readme"},
#' \code{"buildVignettes"}, and \code{"vignette"},
#' would overwrite previous results anyway, so they are only considered during the first run. Therefore, you should always place the R version which
#' should be used for these actions first in line. The \code{"html"} action will list all Windows and OS X binary packages. The \code{"deb"}
#' action will only actually debianize and build a binary package during the first run, too.
#'
#' @section Windows: On Windows, the actions \code{"doc"} and \code{"check"} will only work correctly if you have installed and configured LaTeX
#' accordingly, and you will also need Rtools set up for packaging.
#'
#' @section CRAN compliance: The CRAN policies can sometimes be very strict. This package should allow you to produce packages which are suitable
#' for release on CRAN. But some steps have to be taken care of by yourself. For instance, CRAN does currently not allow copies of common licenses
#' in a source package, nor a \code{debian} folder. Therefore, if your package is supposed to be released on CRAN, you should include
#' \code{Rbuildignore=c("debian", "LICENSE")} to the function call.
#' 
#' @section Temporary git checkouts: If you want to rebuild binaries of something that was already released, i.e. by using the \code{"binonly"} action,
#' and if your source directory is a git repository, then the action \code{"gitCheckout"} can temporarily checkout the source version to build
#' and switch back to the status quo afterwards again. This might or might not work as you expect, depending on whether you organize your code like
#' it is expected here. That is, each release must be tagged properly, with the exact version number as the tag name. You should also commit all
#' current changes to the code before you use this. Internally, \code{roxy.package} will try to find out the current branch of the git repository,
#' then checkout the version number you provided as the new branch or tag, do all the packaging, and checkout bach to the previous branch.
#'
#' @note The binary packaging is done simply by zipping (Windows) or targzipping (Mac OS X) the built and installed package. This should
#' do the trick as long as your package is written in pure R code. It will most likely not produce usable packages if it contains
#' code in other languages like C++.
#'
#' @param pck.source.dir Character string, path pointing to the root directory of your package sources.
#' @param pck.version Character string, defining the designated version number. Can be omitted if actions don't
#'    include \code{"roxy"}, then this information is read from the present DESCRIPTION file.
#' @param pck.description Data.frame holding the package description (see Examples section). Any data.frame with valid fields
#'    will do, but you should use \code{\link[roxyPackage:package_description]{package_description}} if possible because it does
#'    some basic validity checks.
#' @param R.libs Character string, valid path to the R library where the package should be installed to.
#' @param repo.root Character string, valid path to a directory where to build/update a local package repository.
#' @param pck.date Date class object or character string of the release date in YYYY-MM-DD format. Defaults to \code{Sys.Date()}.
#'    If actions don't include \code{"roxy"} and neither \code{Date}, \code{Packaged}, nor \code{Date/Publication} are found
#'    in the present DESCRIPTION file, then \code{pck.date} will be used. Otherwise, the information from the DESCRIPTION file is used.
#' @param actions Character vector, must contain at least one of the following values:
#'    \describe{
#'      \item{"roxy"}{Roxygenize the docs}
#'      \item{"cite"}{Update CITATION file}
#'      \item{"license"}{Update LICENSE file}
#'      \item{"readme"}{Generate initial README.md file}
#'      \item{"check"}{Do a full package check, calling \code{R CMD check}. Combine with \code{"package"} to do the check on the tarball, not the source directory.}
#'      \item{"package"}{Build & install the package, update source repository, calling \code{R CMD build} and \code{R CMD INSTALL}}
#'      \item{"binonly"}{Like \code{"package"}, but doesn't copy the source package to the repository, to enable binary-only rebuilds}
#'      \item{"gitCheckout"}{Treats \code{pck.source.dir} as a git repository and \code{pck.version} as a branch or tag to checkout temporarily;
#'        only valid in combination with \code{"binonly"}}
#'      \item{"cl2news"}{Try to convert a ChangeLog file into an NEWS.Rd file}
#'      \item{"news2rss"}{Try to convert \code{inst/NEWS.Rd} into an RSS feed. You must also set
#'        \code{URL} accordingly}
#'      \item{"doc"}{Update PDF documentation (\code{R CMD Rd2pdf}) and vignettes if present; }
#'      \item{"html"}{Update HTML index files}
#'      \item{"win"}{Update the Windows binary package}
#'      \item{"macosx"}{Update the Mac OS X binary package}
#'      \item{"log"}{Generate initial ChangeLog or update a present ChangeLog file}
#'      \item{"deb"}{Update the Debian binary package with \code{\link[roxyPackage:debianize]{debianize}} (works only on Debian systems;
#'        see \code{deb.options}, too). \code{URL} must also be set to generate Debian repository information}
#'      \item{"cleanRd"}{Insert line breaks in Rd files with lines longer than 90 chars}
#'      \item{"vignette"}{Generate initial vignette stub in directory \code{vignettes}; if \code{html.options} has a \code{flattr.id}, it will be included}
#'      \item{"buildVignettes"}{Re-build all vignettes during the \code{"package"} action, to force generation of a vignette index in
#'        the source package (recommended if \code{VignetteBuilder} is set in the package description)}
#'      \item{"buildEmAll"}{Build binary packages for all configured R versions, not just the first. Only effective if multiple versions of R are actually provided (see above)}
#'    }
#'    Note that \code{"cl2news"} will write the \code{NEWS.Rd} file to the \code{inst} directory of your sources, which will overwrite
#'    an existing file with the same name! Also note that if both a \code{NEWS/NEWS.Rd} and \code{ChangeLog} file are found, only
#'    news files will be linked by the \code{"html"} action.
#' @param cleanup Logical, if \code{TRUE} will remove backup files (matching \code{.*~$} or \code{.*backup$}) from the source directory.
#' @param rm.vignette Logical, if \code{TRUE} and a vignette was build during the \code{"doc"} action and vignettes live in the directory \code{inst/doc},
#'    they will not be kept in the source package but just be moved to the \code{./pckg/$PACKAGENAME} directory of the repository.
#' @param R.homes Path to the R installation to use. Can be set manually to build packages for other R versions than the default one,
#'    if you have installed them in parallel. Should probably be used together with \code{R.libs}.
#' @param Rcmd.options A named character vector with options to be passed on to the internal calls of \code{R CMD build},
#'    \code{R CMD INSTALL}, \code{R CMD check} and \code{R CMD Rd2pdf}. Change these only if you know what you're doing!
#'    Will be passed on as given here. To deactivate, options must explicitly be se to \code{""}, missing options will be used with the default values.
#'    Please note that if you've set \code{VignetteBuilder} in the package description, the vignettes will always be re-build if you enabled the
#'    \code{"buildVignettes"} action, even if you keep \code{--no-build-vignettes} in the build options.
#' @param URL Either a single character string defining the URL to the root of the repository (i.e., which holds the directories \code{src}
#'    etc., see below), or a named character vector if you need different URLs for different services. If you provide more than one URL, these are valid
#'    names for values:
#'    \describe{
#'      \item{\code{default}}{A mandatory fallback URL, will be used if not overridden by one of the other values. This is fully equivalent to the global value
#'        if only one character string is provided.}
#'      \item{\code{debian}}{Used for the Debian package repository if different from the default.}
#'      \item{\code{mirror.list}}{URL pointing to a list of mirrors users should choose from, rather than using one particular host name for the Debian repository.
#'        Will only be used in the HTML instructions for a Debian repository.}
#'      \item{\code{debian.path}}{Can be used to define a custom path users would need to specify in addition to the main URL.
#'        Defaults to \code{"/deb"}, and if given, it must start with a slash.
#'        Will be used in combination with \code{default}, \code{debian} or \code{mirror.list}. It is not advisable to combine it with \code{default}, because you will
#'        have to manually rename the directory generated after each run!}
#'    }
#'    These URLs are not the path to the local file system, but should be the URLs to the respecive repository as it is available
#'    via internet. This option is necessary for (and only interpreted by) the actions \code{"news2rss"}, \code{"deb"}, and possibly \code{"html"} --
#'    if \code{flattr.id} is also set in \code{html.options}, a Flattr meta tag be added to the HTML page.
#' @param deb.options A named list with parameters to pass through to \code{\link[roxyPackage:debianize]{debianize}}. By default, \code{pck.source.dir}
#'    and \code{repo.root} are set to the values given to the parameters above, and if packages are being build for R 3.5, the default \code{deb.dir} changes
#'    from \code{"deb"} to \code{"debR35"}. As for the other options, if not set, the defaults of \code{debianize} will be used.
#' @param readme.options A named list with parameters that add optional extra information to an initial README.md file, namely instructions to install the package
#'    directly from a GitHub repository. Ignore this if you don't use GitHub. Theoretically, you can overwrite all values of the internal
#'    function \code{readme_text} (e.g., try \code{formals(roxyPackage:::readme_text)}). But in practice, these two should be all you need to set:
#'    \describe{
#'      \item{\code{githubUser}}{Your GitHub user name, can be used to contruct the GitHub repo URL}
#'    }
#'    All other missing values are then guessed from the other package information. It is then assumed that the GitHub repo has the same name as the package.
#' @param html.options A named list with parameters to be used for generating the HTML files of the repository. These values are recognized:
#'    \describe{
#'      \item{\code{index}}{A character string for the headline of the global index HTML file; if missing, "Available R Packages" will be used as default}
#'      \item{\code{title}}{A character string for the title tag prefix of the package index HTML file; if missing, "R package" will be used as default}
#'      \item{\code{flattr.id}}{A Flattr meta ID, will be added to the headers of package specific HTML files, and to a vignette stub if the \code{"vignette"} action is active}
#'      \item{\code{repo.flattr.id}}{A Flattr meta ID, will be added to the headers of all global HTML files of the repository}
#'      \item{\code{imprint}}{A named character string used as a URL to link to an imprint page; he name is used as the link text}
#'      \item{\code{privacy.policy}}{A named character string used as a URL to link to a privacy policy statement in accordance with GDPR; the name is used as the link text}
#'    }
#' @param ChangeLog A named list of character vectors with log entry items. The element names will be used as section names in the ChangeLog entry,
#'    and each character string in a vector will be pasted as a log item. The news you provide here will be appended to probably present news, while
#'    trying to prevent duplicate entries to appear. If you need more control, don't use the \code{"log"} action, but have a look at
#'    \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}}. Also note that the date of altered entries will be updated automatically, unless
#'    you don't call the \code{"roxy"} action, too.
#' @param Rbuildignore A character vector to be used as lines of an \code{.Rbuildignore} file. If set, this will replace an existing \code{.Rbuildignore}
#'    file. Setting it to an empty string (\code{""}) will remove the file, the default value \code{NULL} will simply keep the file, if one is present.
#' @param Rinstignore A character vector to be used as lines of an \code{.Rinstignore} file. If set, this will replace an existing \code{.Rinstignore}
#'    file. Setting it to an empty string (\code{""}) will remove the file, the default value \code{NULL} will simply keep the file, if one is present.
#' @param OSX.repo A named list of character vectors, one named \code{"main"} defines the main directory below \code{./bin/macosx/} where packages for
#'    Mac OS X should be copied, and the second optional one named \code{"symlink"} can be used to set symbolic links, e.g., \code{symlinks="el-capitan"}
#'    would also make the repository available via \code{./bin/macosx/mavericks}. Symbolic links will be ignored when run on on Windows. If you use them,
#'    make sure they're correctly transferred to your server, where applicable.
#' @param ... Additional options passed through to \code{roxygenize}.
#' @references
#' [1] \url{https://CRAN.R-project.org/package=roxygen2}
#' @seealso
#'    \code{\link[roxyPackage:package_description]{package_description}} for proper package description, and
#'    \code{\link[roxyPackage:sandbox]{sandbox}} to run roxy.package() in a sandbox.
#' @importFrom roxygen2 roxygenize
#' @export
#' @examples
#' \dontrun{
#' ## package description as data.frame:
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
#' # hint no. 1: you *don't* specify version number and release date here,
#' #   but all other valid fields for DESCRIPTION files must/can be defined
#' # hint no. 2: most of this rarely changes, so you can add this to the
#' #   internals of your package and refer to it as
#' #   roxy.package(pck.description=SquareTheCircle:::pckg.dscrptn, ...)
#' # hint no. 3: use "AuthorR" for the "Author@@R" field, or "AuthorsR" for
#' # R >= 2.14, to work around naming problems
#'
#' roxy.package(pck.source.dir="~/my_R_stuff/SquareTheCircle",
#'   pck.version="0.01-2",
#'   pck.description=pckg.dscrptn,
#'   R.libs="~/R",
#'   repo.root="/var/www/repo",
#'   actions=c("roxy", "package", "doc"))
#' }

roxy.package <- function(
  pck.source.dir,
  pck.version,
  pck.description,
  R.libs,
  repo.root,
  pck.date=Sys.Date(),
  actions=c("roxy", "package"),
  cleanup=FALSE,
  rm.vignette=FALSE,
  R.homes=R.home(),
  Rcmd.options=c(
    install="--install-tests",
    build="--no-manual --no-build-vignettes --md5",
    check="--as-cran",
    Rd2pdf="--pdf --no-preview"),
  URL=NULL,
  deb.options=NULL,
  readme.options=NULL,
  html.options=NULL,
  ChangeLog=list(changed=c("initial release"), fixed=c("missing ChangeLog")),
  Rbuildignore=NULL,
  Rinstignore=NULL,
  OSX.repo=list(main="contrib", symlinks="el-capitan"),
  ...){
  
  # ensure backwards compatibility
  extra_options <- list(...)
  if("html.index" %in% names(extra_options)){
    warning("The 'html.index' argument was moved to 'html.options', please see the docs and adjust your code!", call.=FALSE)
    html.options[["index"]] <- extra_options[["html.index"]]
  } else {}
  if("html.title" %in% names(extra_options)){
    warning("The 'html.title' argument was moved to 'html.options', please see the docs and adjust your code!", call.=FALSE)
    html.options[["title"]] <- extra_options[["html.title"]]
  } else {}
  if("flattrUser" %in% names(readme.options)){
    warning("The 'flattrUser' argument was renamed into 'flattr.id' and moved from 'readme.options' to 'html.options', please see the docs and adjust your code!", call.=FALSE)
    html.options[["flattr.id"]] <- readme.options[["flattrUser"]]
    readme.options[["flattrUser"]] <- NULL
  } else {}

  # avoid some NOTEs from R CMD check
  AuthorR <- AuthorsR <- Author.R <- Authors.R <- NULL

  # check the OS
  unix.OS <- isUNIX()
  
  # do we need to worry about multiple R versions?
  R.versions <- length(R.homes)
  R.libraries <- length(R.libs)
  if(R.versions > 1){
    if(R.libraries != R.versions){
      stop(simpleError("If you specify more than one R.home, you must also define as many R.libs!"))
    } else {}
    if("buildEmAll" %in% actions){
      # if so, iterate recursively through it and then end
      for (this.R in seq_along(R.homes)){
        this.home <- R.homes[this.R]
        this.libs <- R.libs[this.R]
        this.actions <- actions
        this.deb.options <- deb.options
        if("deb" %in% actions){
          if(this.R == 1){
            # for the time being, debianizing the package once is enough
            # we'll only do it the *first* run
            this.deb.options[["actions"]] <- deb.options[["actions"]][deb.options[["actions"]] %in% c("deb", "bin", "src")]
          } else {
            # all other cases: no debianizing
            this.deb.options[["actions"]] <- ""
          }
        } else {}
        if(this.R > 1){
          # well, the same is true for some other actions
          # we'll only perform them during the *first* run
          this.actions <- actions[!actions %in% c(
            "roxy",
            "cite",
            "license",
            "doc",
            "cl2news",
            "news2rss",
            "cleanRd",
            "readme",
            "vignette",
            "buildVignettes",
            "gitCheckout"
          )]
          # we also don't need to repeat handling of .Rbuildignore and .Rinstignore
          Rbuildignore <- Rinstignore <- NULL
        } else {}
        roxy.package(
          pck.source.dir=pck.source.dir,
          pck.version=pck.version,
          pck.description=pck.description,
          R.libs=this.libs,
          repo.root=repo.root,
          pck.date=pck.date,
          actions=this.actions,
          cleanup=cleanup,
          rm.vignette=rm.vignette,
          R.homes=this.home,
          Rcmd.options=Rcmd.options,
          URL=URL,
          deb.options=this.deb.options,
          readme.options=readme.options,
          html.options=html.options,
          ChangeLog=ChangeLog,
          Rbuildignore=Rbuildignore,
          Rinstignore=Rinstignore,
          OSX.repo=OSX.repo,
          ...)
      }
      return(invisible(NULL))
    } else {
      R.homes <- R.homes[1]
      R.libraries <- R.libraries[1]
      R.versions <- R.libraries <- 1
    }
  } else {}

  old.dir <- getwd()
  on.exit(
    setwd(old.dir),
    add=TRUE
  )

  # fist see if we need to rename an "AuthorR" field. data.frame definitions tend to
  # replace the "@" with a dot
  if("AuthorR" %in% names(pck.description)){
    pck.description$`Author@R` <- pck.description[["AuthorR"]]
    pck.description <- subset(pck.description, select=-AuthorR)
  } else if("AuthorsR" %in% names(pck.description)){
    pck.description$`Authors@R` <- pck.description[["AuthorsR"]]
    pck.description <- subset(pck.description, select=-AuthorsR)
  } else if("Author.R" %in% names(pck.description)){
    pck.description$`Author@R` <- pck.description[["Author.R"]]
    pck.description <- subset(pck.description, select=-Author.R)
  } else if("Authors.R" %in% names(pck.description)){
    pck.description$`Authors@R` <- pck.description[["Authors.R"]]
    pck.description <- subset(pck.description, select=-Authors.R)
  } else {}

  # normalize all root paths
  R.homes <- normalizePathByOS(path=R.homes, is.unix=unix.OS, mustWork=TRUE)
  repo.root <- normalizePathByOS(path=repo.root, is.unix=unix.OS, mustWork=FALSE)
  pck.source.dir <- normalizePathByOS(path=pck.source.dir, is.unix=unix.OS, mustWork=TRUE)

  # get info on the R version used
  R.Version.full <- getRvers(R.homes=R.homes)
  R.Version.win <- getRvers(R.homes=R.homes, win=TRUE)
  if(nchar(R.Version.full) < 3 | nchar(R.Version.win) < 3){
    stop(simpleError("R version number cannot be detected, it seems."))
  } else {}

  # keep checking for older R versions, as they might still be requested
  # in multi-version builds
  if(isTRUE(R_system_version(R.Version.full) < "3.0")){
    stop(simpleError("Sorry, but support for R < 3.0.0 was dropped with roxyPackage 0.06-1!"))
  } else {}

  # adjust the default repo directory if we're building against R 3.5, as those packages
  # can't be installed with previous R versions
  if(all(
    "deb" %in% actions,
    isTRUE(R_system_version(R.Version.full) >= "3.5"),
    is.null(deb.options[["deb.dir"]])
  )){
    message("deb: R >= 3.5 detected, using \"debR35\" as the default directory for debian package files")
    deb.options[["deb.dir"]] <- "debR35"
  } else {}

  # should we try to checkout a certain git tag?
  if(all(c("gitCheckout", "binonly") %in% actions)){
    if(isTRUE(unix.OS)){
      # make sure git is available
      git_cmd <- Sys.which("git")
      if("" %in% git_cmd){
        stop(simpleError("git: command 'git' is not available in system search path! you can't use the \"gitCheckout\" action :-("))
      } else {
        git_dir <- file.path(pck.source.dir, ".git")
        all_branches <- system(
          paste0(git_cmd, " --git-dir \"", git_dir, "\" branch"),
          intern=TRUE
        )
        current_branch <- gsub("\\*[[:space:]]+", "", all_branches[which(grepl("^[[:space:]]*\\*[[:space:]]+", all_branches))])
        if("" %in% current_branch){
          stop(simpleError("git: sorry, unable to fetch the current branch name in your git repository!"))
        } else {}
        successful_checkout <- system(
          paste0(git_cmd, " --git-dir \"", git_dir, "\" checkout ", pck.version),
          intern=FALSE
        )
        if(x == 0){
          on.exit(
            system(
              paste0(git_cmd, " --git-dir \"", git_dir, "\" checkout ", current_branch)
            ),
            add=TRUE
          )
        } else {
          stop(simpleError(paste0("git: unable to checkout branch/tag \"", pck.version, "\", are you sure it exists?")))
        }
      }
    } else {
      stop(simpleError("git: sorry, \"gitCheckout\" is currently only supported on unix systems!"))
    }
  } else {}

  # special cases: if actions do not include "roxy",
  # take infos from the DESCRIPTION file
  if(!"roxy" %in% actions | is.null(pck.description)){
    pck.dscrptn <- as.data.frame(read.dcf(file=file.path(pck.source.dir, "DESCRIPTION")), stringsAsFactors=FALSE)
    # clean from newlines
    pck.dscrptn <- as.data.frame(t(sapply(pck.dscrptn, function(x) gsub("\n", " ", x))), stringsAsFactors=FALSE)
    pck.version <- getDescField(pck.dscrptn, field="Version")
    # if "Date" is missing in DESCRIPTION file, try some fallbacks
    if(any(c("Date","Packaged","Date/Publication") %in% colnames(pck.dscrptn))){
      pck.date <- as.character(as.Date(getDescField(pck.dscrptn, field=c("Date","Packaged","Date/Publication"))))
    } else {
      message("There was no 'Date', 'Packaged', or 'Date/Publication' in DESCRIPTION, using pck.date as fallback.")
      pck.dscrptn["Date"] <- pck.date
    }
    pck.package <- getDescField(pck.dscrptn, field="Package")
    pck.title <- getDescField(pck.dscrptn, field="Title")
    rss.description <- getDescField(pck.dscrptn, field="Description")
    pckg.license <- getDescField(pck.dscrptn, field="License")
    pckg.dscrptn <- pck.description <- pck.dscrptn
  } else {
    rss.description <- pck.description[["Description"]]
    pck.package <- roxy.description("package", description=pck.description)
    pck.title <- roxy.description("title", description=pck.description)
    pckg.dscrptn <- roxy.description("description", description=pck.description, version=pck.version, date=as.character(pck.date))
    pckg.license <- pck.description[["License"]]
  }
  pckg.package <- roxy.description("pckg.description", description=pck.description, version=pck.version, date=as.character(pck.date))

  ## check for sandboxing
  if(isTRUE(check.sandbox())){
    message("preparing sandbox...")
    # prepare folder structure; this will also insure sane values and abort
    # if locations are invalid. the function returns a list of paths to use.
    if("check" %in% actions){
      # for check runs we need also all suggested packages
      initSuggests <- TRUE
    } else {
      initSuggests <- FALSE
    }
    adjust.paths <- prepare.sandbox(
      package=pck.package,
      description=pckg.dscrptn,
      pck.source.dir=pck.source.dir,
      R.libs=R.libs,
      R.version=R.Version.full,
      repo.root=repo.root,
      depLevel=c("Depends", "Imports"),
      initSuggests=initSuggests)
    # replace paths with sandbox
    pck.source.dir <- adjust.paths[["pck.source.dir"]]
    R.libs <- adjust.paths[["R.libs"]]
    repo.root <- adjust.paths[["repo.root"]]
    sandbox.status()
  } else {}

  # check environment
  R.bin <- file.path(R.homes, "bin", "R")
  message(paste("R environment\n  R.home:", R.homes, "\n  R.libs:", R.libs))

  repo.src.contrib <- file.path(repo.root, "src", "contrib")
  repo.win <- file.path(repo.root, "bin", "windows", "contrib", R.Version.win)
  # we'll do proper checks in the mac section, no need to freak out here just yet
  # if OSX.repo is set incorrectly
  repo.macosx <- file.path(repo.root, "bin", "macosx")
  repo.macosx.main <- file.path(repo.macosx, OSX.repo[["main"]])
  repo.macosx.R <- file.path(repo.macosx.main, R.Version.win)
  repo.pckg.info.main <- file.path(repo.root, "pckg")
  repo.pckg.info <- file.path(repo.pckg.info.main, pck.package)
  pckg.basename <- paste0(pck.package, "_", pck.version)
  pckg.name.src <- paste0(pckg.basename, ".tar.gz")
  pckg.name.win <- paste0(pckg.basename, ".zip")
  pckg.name.mac <- paste0(pckg.basename, ".tgz")
  pckg.inst.dir <- file.path(pck.source.dir, "inst")
  pckg.cite.file <- file.path(pckg.inst.dir, "CITATION")
  pckg.cite.file.html <- file.path(repo.pckg.info, "citation.html")
  src.changelog <- file.path(pck.source.dir, "ChangeLog")
  pckg.changelog <- file.path(repo.pckg.info, "ChangeLog")
  repo.src.gz <- file.path(repo.src.contrib, pckg.name.src)
  win.package <- file.path(repo.win, pckg.name.win)
  macosx.package <- file.path(repo.macosx.R, pckg.name.mac)
  pckg.NEWS.Rd <- file.path(pckg.inst.dir, "NEWS.Rd")
  pckg.NEWS.inst <- file.path(pckg.inst.dir, "NEWS")
  pckg.NEWS <- file.path(pck.source.dir, "NEWS")
  pckg.NEWS.html <- file.path(repo.pckg.info, "NEWS.html")
  RSS.file.name <- "RSS.xml"
  pckg.NEWS.rss <- file.path(repo.pckg.info, RSS.file.name)
  pckg.license.file <- file.path(pck.source.dir, "LICENSE")
  pckg.license.file.old <- file.path(pck.source.dir, "LICENSE.txt")
  pckg.readme.file <- file.path(pck.source.dir, "README.md")
  pckg.pdf.doc <- paste0(pck.package, ".pdf")
  pckg.vign.dir <- file.path(pck.source.dir, "vignettes")
  pckg.vign.file <- file.path(pckg.vign.dir, paste0(pck.package, "_vignette.Rmd"))
  clean.env.unix <- "unset R_LIBS_USER R_BINARY R_CMD ; "
  clean.env.win  <- "SET R_LIBS_USER=\"\" & SET R_BINARY=\"\" & SET R_CMD=\"\" & "
  set.env.unix <- paste0("export R_LIBS_USER=\"", R.libs, "\" ; ")
  set.env.win <- paste0("SET R_LIBS_USER=\"", R.libs, "\" & ")
  
  # take care of .Rbuildignore and .Rinstignore
  pckg.Rbuildignore <- configFile(root=pck.source.dir, type="build", content=Rbuildignore)
  pckg.Rinstignore <- configFile(root=pck.source.dir, type="inst", content=Rinstignore)

  # debian package specific stuff, probably needed for "html" action
  deb.defaults <- mergeOptions(
    someFunction=debianize,
    customOptions=deb.options,
    newDefaults=list(
      pck.source.dir=pck.source.dir,
      repo.root=repo.root,
      deb.keyring.options=list(URL=getURL(URL, purpose="debian")),
      keep.existing.orig="binonly" %in% actions
    )
  )
  # try to set pckg.name.deb and deb.repo.path.part
  # this will only work if repo.root is unchanged, the rest is too messy now...
  # don't ry to replace this without checking the outcome in the HTML file!
  deb.repo.path.part <- debRepoPath(
    dist=deb.defaults[["distribution"]],
    comp=deb.defaults[["component"]],
    arch=deb.defaults[["arch"]],
    part=TRUE,
    deb.dir=deb.defaults[["deb.dir"]]
  )
  # need to get repo.name to be able to call eval() on deb.defaults[["origin"]], because that pastes repo.name
  repo.name <- deb.defaults[["repo.name"]]
  deb.defaults[["origin"]] <- eval(deb.defaults[["origin"]])
  pckg.name.deb.part <- debianPkgName(package=pck.package, origin=deb.defaults[["origin"]], version=NULL, replace.dots=FALSE)
  pckg.name.deb <-  paste0(
    pckg.name.deb.part, "_",
    debianPkgVersion(version=pck.version, revision=deb.defaults[["revision"]], epoch=deb.defaults[["epoch"]]), "_",
    deb.defaults[["arch"]], ".deb"
  )
  deb.package <- file.path(repo.root, deb.repo.path.part, pckg.name.deb)

  # ensure we have at least 'index' and 'title' set to defaults
  html.options <- mergeOptions(
    someFunction=function(index, title, flattr.id=NULL, repo.flattr.id=NULL, imprint=NULL, privacy.policy=NULL){},
    customOptions=html.options,
    newDefaults=list(
      index="Available R Packages",
      title="R package"
    )
  )

  # check for additional CMD options
  if("build" %in% names(Rcmd.options)){
    Rcmd.opt.build <- paste0(Rcmd.options[["build"]], " ")
  } else {
    Rcmd.opt.build <- "--no-manual --no-build-vignettes "
  }
  
  Rcmd.opt.install <- ifelse("install" %in% names(Rcmd.options), paste0(Rcmd.options[["install"]], " "), "")
  Rcmd.opt.check <- ifelse("check" %in% names(Rcmd.options), paste0(Rcmd.options[["check"]], " "), "")
  Rcmd.cmd.Rd2pdf <- "Rd2pdf"
  if("Rd2pdf" %in% names(Rcmd.options)){
    Rcmd.opt.Rd2pdf <- paste0(Rcmd.options[["Rd2pdf"]], " ")
  } else {
    Rcmd.opt.Rd2pdf <- "--pdf --no-preview "
  }

  # check for/create info directory
  createMissingDir(dirPath=repo.pckg.info, action="repo")
  
  # clean up
  if(isTRUE(cleanup)){
    unlink(list.files(pck.source.dir, pattern=".*~$", full.names=TRUE, recursive=TRUE))
    unlink(list.files(pck.source.dir, pattern=".*backup$", full.names=TRUE, recursive=TRUE))
  } else {}

  if("license" %in% actions){
    if(checkLicence(pckg.license)){
      copyLicence(pckg.license, pckg.license.file, overwrite=TRUE)
      if(file.exists(pckg.license.file.old)){
        warning("license: you have both LICENSE and LICENSE.txt in your project! if LICENSE.txt is one of the standard licenses, please rename it to prevent its intallation.", call.=FALSE)
      } else {}
    } else {
      stop(simpleError(paste0("license: unrecognized license (", pckg.license, "), please provide your own LICENSE file! ")))
    }
  } else {}

  if("readme" %in% actions){
    if(file.exists(pckg.readme.file)){
      warning("readme: README.md exists, please remove it if you want it re-written!", call.=FALSE)
    } else {
      readme.defaults <- mergeOptions(
        someFunction=readme_text,
        customOptions=readme.options,
        newDefaults=list(
          package=pck.package,
          description=pck.description[["Description"]],
          url=pck.description[["URL"]],
          license=pck.description[["License"]],
          author=paste0(
            gsub(
              "[[:space:]]*<[^>]*>",
              "",
              get.authors(description=pck.description, maintainer=FALSE, contributor=FALSE, copyright=FALSE)[["aut"]]
            ),
            collapse=", "
          ),
          githubRepo=pck.package
        )
      )
      formals(readme_text) <- readme.defaults
      cat(
        readme_text(),
        file=pckg.readme.file
      )
      message(paste0("readme: generated initial ", pckg.readme.file, "."))
    }
  } else {}
  
  if("roxy" %in% actions){
    add.options <- list(...)
    if(any(c("local.roxy.dir", "roxy.unlink.target") %in% names(add.options))){
      stop(simpleError("you are still using \"local.roxy.dir\" and/or \"roxy.unlink.target\", these options have been removed from roxyPackage as of 0.04-2, since they are no longer supported by roxygen2. please change your roxy.package() call!"))
    } else {}

    # re-write DESCRIPTION files
    write.dcf(pckg.dscrptn, file=file.path(pck.source.dir, "DESCRIPTION"))
    # copy DESCRIPTION to pckg directory for easy HTML indexing
    stopifnot(file.copy(file.path(pck.source.dir, "DESCRIPTION"), file.path(repo.pckg.info, "DESCRIPTION"), overwrite=TRUE))
    pckg.package.file.R <- file.path(pck.source.dir, "R", paste0(pck.package, "-package.R"))
    cat(paste(pckg.package), file=pckg.package.file.R)
    message(paste0("roxy: updated ", pckg.package.file.R, "."))

    # another experimental feature:
    # sometimes roxygen2::roxygenize is somewhat buggy, but functions like devtools::document() still work.
    # manually setting "roxyFunction" makes it possible to use that function instead.
    # the default value is set in roxyPackage-internal.R!
    useRoxyFunction <- get.roxyEnv(name="roxyFunction")
    stopifnot(is.function(useRoxyFunction))
    # should normally default to roxygen2::roxygenize(pck.source.dir, ...)
    useRoxyFunction(pck.source.dir, ...)
  } else {}

  if("cleanRd" %in% actions){
    pckg.man.dir <- file.path(pck.source.dir, "man")
    Rd.files <- list.files(pckg.man.dir, pattern="*.Rd", ignore.case=TRUE)
    if(length(Rd.files) > 0){
      for (this.file in Rd.files){
        sanitizeRdFiles(this.file, root.dir=pckg.man.dir, maxlength=90)
      }
    } else {}
  } else {}

  if("cite" %in% actions){
    createMissingDir(dirPath=pckg.inst.dir, action="cite")
    # calling internal function to generate citation object
    cite.obj <- citationText(pck.dscr=pck.description, pck.version=pck.version, pck.date=pck.date)
    cat(cite.obj, file=pckg.cite.file)
    message(paste0("cite: updated ", pckg.cite.file, "."))
    if("html" %in% actions){
      cite.obj.html <- roxy.html.cite(
        cite.obj=eval(parse(text=cite.obj)),
        page.css="../web.css",
        package=pck.package,
        imprint=html.options[["imprint"]],
        privacy.policy=html.options[["privacy.policy"]]
      )
      cat(cite.obj.html, file=pckg.cite.file.html)
      message(paste0("cite: updated ", pckg.cite.file.html, "."))
    } else {}
  } else {}

  if("log" %in% actions){
    if(!file.exists(src.changelog)){
      newLog <- initChangeLog(entry=ChangeLog, package=pck.package, version=pck.version, date=pck.date)
      writeChangeLog(log=newLog, file=src.changelog)
      message("log: generated initial ChangeLog")
    } else {
      oldLog <- readChangeLog(src.changelog)
      newLog <- updateChangeLog(oldLog, entry=ChangeLog, version=pck.version, date=pck.date, append=TRUE)
      newLogEntry <- getChangeLogEntry(newLog, pck.version)
      if(!identical(oldLog, newLog)){
        writeChangeLog(log=newLog, file=src.changelog)
        message("log: updated ChangeLog")
        print(newLogEntry)
      } else {
        message("log: ChangeLog is up-to-date, skipped")
      }
    }
  } else {}

  ## generate vignette stub
  if("vignette" %in% actions){
    if(file.exists(pckg.vign.file)){
      warning("vignette: Rmd file exists, please remove it if you want it re-written!", call.=FALSE)
    } else {
      createMissingDir(dirPath=pckg.vign.dir, action="vignette")
      stub_text <- vignette_stub(
        preamble=NULL,
        txt_body=paste0(
          "\n# A Section\n\n",
          "## A Subsection\n\n",
          "* Level 1\n",
          "  + Level 2\n\n...\n\n",
          "Don't forget to add 'knitr,rmarkdown' to the 'Suggests' field and 'VignetteBuilder=\"knitr\"' to your package description!\n"
        ),
        flattr_id=html.options[["flattr.id"]],
        R.dscrptn=pck.description
      )
      cat(stub_text, file=pckg.vign.file)
      message(paste0("vignette: stub file written to ", pckg.vign.file))
      warning("vignette: don't forget to add 'knitr,rmarkdown' to the 'Suggests' field and 'VignetteBuilder=\"knitr\"' to your package description!", call.=FALSE)
    }
  }

  ## update docs (reference manual and vignettes)
  if("doc" %in% actions){
    # had to move this down here, because tools::pkgVignettes() and tools::buildVignettes()
    # don't work without an existing DESCRIPTION file since R 3.0.1... hooooray...
    # makes sense because buildVignettes() needs to know the VignetteBuilder from DESCRIPTION

    # any vignettes?
    pckg.vigns <- tools::pkgVignettes(dir=pck.source.dir)
    if(length(pckg.vigns$docs) > 0){
      # build vignettes and move them (partly inspired by devtools::build_vignettes)
      
      # create vignettes and gather fresh information
      tools::buildVignettes(dir=pck.source.dir, tangle=TRUE)
      pckg.vigns <- tools::pkgVignettes(dir=pck.source.dir, output=TRUE, source=TRUE)
      
      # only do the moving if any vignettes were built
      if(length(pckg.vigns$outputs) > 0){
        message("build: vignettes ", paste(basename(pckg.vigns$outputs), collapse=', '))
        
        # what to move and where to move it to
        what.mv <- unique(c(pckg.vigns$outputs, unlist(pckg.vigns$sources, use.names=FALSE)))
        what.cp <- pckg.vigns$docs
        # filter files according to .install_extras
        inst_extras_path <- file.path(pckg.vigns$dir, ".install_extras")
        what.mv <- keep_files(files=what.mv, install_extras=inst_extras_path)
        what.cp <- keep_files(files=what.cp, install_extras=inst_extras_path)
        to.bin.doc <- file.path(R.libs, pck.package, "doc")
        to.src.inst.doc <- file.path(pck.source.dir, "inst", "doc")
        
        # copy to bin-package
        createMissingDir(to.bin.doc, action="doc")
        if(length(what.mv) > 0){
          stopifnot(file.copy(what.mv, to.bin.doc, overwrite=TRUE))
        } else {}
        if(length(what.cp) > 0){
          stopifnot(file.copy(what.cp, to.bin.doc, overwrite=TRUE))
        } else {}
        
        ## copy to repo/website
        stopifnot(file.copy(pckg.vigns$outputs, repo.pckg.info, overwrite=TRUE))
        message("repo: updated vignettes ", paste(basename(pckg.vigns$outputs), collapse=', '))
        
        # copy to src-package's inst/doc if not already there (this is the case if they live in directory vignettes)
        if(!grepl("inst/doc$", pckg.vigns$dir)){
          createMissingDir(to.src.inst.doc, action="doc")
          if(length(what.mv) > 0){
            stopifnot(file.copy(what.mv, to.src.inst.doc, overwrite=TRUE))
            stopifnot(file.remove(what.mv))
          } else {}
          if(length(what.cp) > 0){
            stopifnot(file.copy(what.cp, to.src.inst.doc, overwrite=TRUE))
          } else {}
        } else {
          # mimicking previous implementation
          if(isTRUE(rm.vignette)){
            if(length(what.mv) > 0){
              stopifnot(file.remove(what.mv))
            } else {}
          } else {}
        }
      } else {
        warning("build: Couldn't build all vignettes", call.=FALSE)
      }
    } else {}

    # do the reference manual
    pdf.docs <- file.path(repo.pckg.info, pckg.pdf.doc)
    removeIfExists(filePath=pdf.docs)
    if(isTRUE(unix.OS)){
      r.cmd.doc.call <- paste0(clean.env.unix, R.bin, " CMD ", Rcmd.cmd.Rd2pdf, " ", Rcmd.opt.Rd2pdf, "--output=", pdf.docs, " ", pck.source.dir)
      system(r.cmd.doc.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
    } else {
      r.cmd.doc.call <- paste0(clean.env.win, R.bin, " CMD ", Rcmd.cmd.Rd2pdf, " ", Rcmd.opt.Rd2pdf, "--output=", shQuote(pdf.docs, type="cmd"), " ", shQuote(pck.source.dir, type="cmd"))
      shell(r.cmd.doc.call, translate=TRUE, ignore.stderr=TRUE, intern=FALSE)
    }
    message("build: created PDF docs")
  } else {}

  if(any(c("package", "binonly") %in% actions)){
    ## fill source repo
    createMissingDir(dirPath=repo.src.contrib, action="repo")
    # if the package uses 'VignetteBuilder' we do need to rebuild vignettes
    # because otherwise the resulting tarball will be missing the vignette index
    if(
      all(
        "package" %in% actions,
        !"binonly" %in% actions,
        "VignetteBuilder" %in% names(pckg.dscrptn),
        grepl("--no-build-vignettes", Rcmd.opt.build)
      )
    ){
      if("buildVignettes" %in% actions){
        message("build: dropping '--no-build-vignettes' from build options to force generation of vignette index")
        Rcmd.opt.build <- gsub("--no-build-vignettes", "", Rcmd.opt.build)
      } else {
        warning("build: 'VignetteBuilder' is specified but '--no-build-vignettes' set; run with \"buildVignettes\" action to force generation of vignette index!", call.=FALSE)
      }
    } else {}
    ## TODO: find a solution without setwd()
    jmp.back <- getwd()
    pck.source.dir.parent <- dirname(file.path(pck.source.dir))
    setwd(pck.source.dir.parent)
    if(isTRUE(unix.OS)){
      r.cmd.build.call <- paste0(clean.env.unix, set.env.unix, R.bin, " CMD build ", Rcmd.opt.build, pck.source.dir)
      system(r.cmd.build.call, intern=TRUE)
    } else {
      r.cmd.build.call <- paste0(clean.env.win, set.env.win, R.bin, " CMD build ", Rcmd.opt.build, shQuote(pck.source.dir, type="cmd"))
      shell(r.cmd.build.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
    }
    if(!"binonly" %in% actions){
      file.mv(from=file.path(pck.source.dir.parent,pckg.name.src), to=repo.src.gz, overwrite=TRUE)
      message(paste0("repo: copied ", pckg.name.src, " to src/contrib."))
    } else {}
    setwd(jmp.back)
    # install.packages() doesn't work if we want to build for/with other R installations than
    # the actual running one, so we'll use  R CMD INSTALL instead
    if(isTRUE(unix.OS)){
      r.cmd.install.call <- paste0(clean.env.unix, R.bin, " CMD INSTALL -l ", R.libs, " ",
        Rcmd.opt.install, pck.source.dir)
      system(r.cmd.install.call, intern=TRUE)
    } else {
      r.cmd.install.call <- paste0(clean.env.win, R.bin, " CMD INSTALL -l ", shQuote(R.libs, type="cmd"), " ",
        Rcmd.opt.install, shQuote(pck.source.dir, type="cmd"))
      shell(r.cmd.install.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
    }
    message("build: built and installed package")

    if(!"binonly" %in% actions){
      tools::write_PACKAGES(dir=repo.src.contrib, type="source", verbose=TRUE, latestOnly=FALSE)
      message("repo: updated src/contrib/PACKAGES (source)")

      ## update ChangeLog
      if(file.exists(src.changelog)){
        stopifnot(file.copy(src.changelog, pckg.changelog, overwrite=TRUE))
        message("pckg: updated ChangeLog")
      } else {}
    } else {}
  } else {}

  ## create NEWS.Rd from ChangeLog
  if("cl2news" %in% actions){
    createMissingDir(dirPath=pckg.inst.dir, action="news")
    cl2news(log=src.changelog, news=pckg.NEWS.Rd, codify=TRUE, overwrite=TRUE)
  } else {}

  ## create RSS feed from NEWS.Rd
  add.RSS <- FALSE
  if("news2rss" %in% actions){
    if(is.null(URL)){
      warning("news: no URL specified, RSS feed creation was skipped!", call.=FALSE)
    } else {
      package.URL <- paste(gsub("/*$", "", getURL(URL, purpose="default")), "pckg", pck.package, sep="/")
      RSS.atom.URL <- paste(package.URL, RSS.file.name, sep="/")
      news2rss(
        news=pckg.NEWS.Rd, rss=pckg.NEWS.rss, html=FALSE, encoding="UTF-8",
        channel=c(
          title=pck.package,
          link=package.URL,
          description=rss.description,
          atom=RSS.atom.URL))
      add.RSS <- TRUE
    }
  } else {}

  ## fill windows repo
  if("win" %in% actions){
    createMissingDir(dirPath=repo.win, action="repo")
    removeIfExists(filePath=win.package)
    ## TODO: find a solution without sedwd()
    jmp.back <- getwd()
    setwd(R.libs)
    # make a list of backup files to exclude
    win.exclude.files <- unique(list.files(pck.package, pattern=".*~$", recursive=TRUE))
    if(length(win.exclude.files) > 0){
      win.exclude.files <- paste0("-x \"", paste(file.path(pck.package, win.exclude.files), collapse="\" \""), "\"")
    } else {}
    # windows packages also include an MD5 file, so we'll create one and remove it again
    win.MD5.path <- file.path(R.libs, pck.package)
    installMD5sums(pkgDir=win.MD5.path)
    suppressWarnings(zip(win.package, pck.package, extras=win.exclude.files))
    unlink(file.path(win.MD5.path, "MD5"), recursive=FALSE) # clean up the MD5 file again
    message(paste0("repo: created ", pckg.name.win, " (windows)"))
    setwd(jmp.back)
    tools::write_PACKAGES(dir=repo.win, type="win.binary", verbose=TRUE, latestOnly=FALSE)
    message("repo: updated bin/PACKAGES (windows)")
  } else {}

  ## fill macosx repo
  if("macosx" %in% actions){
    # sanity check
    if(nchar(OSX.repo[["main"]]) > 0){
      createMissingDir(dirPath=repo.macosx.R, action="repo")
      if(!is.null(OSX.repo[["symlinks"]])){
        for (thisSymlink in OSX.repo[["symlinks"]]){
          thisSymPathRoot <- file.path(repo.macosx, thisSymlink)
          thisSymPath <- file.path(thisSymPathRoot, "contrib")
          if(isTRUE(unix.OS)){
            if(!file.exists(thisSymPath)){
              jmp.back <- getwd()
              message(paste0("repo: creating symbolic link ", thisSymPath))
              # if we create the symlink directly, some use cases (like github project pages)
              # get into trouble because of infinite link loops. so we first create the symlink directory
              if(!file.exists(thisSymPathRoot)){
                setwd(repo.macosx)
                r.cmd.symfldr.call <- paste0(Sys.which("mkdir"), " ", thisSymlink)
                system(r.cmd.symfldr.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
              } else {}
              # now create the actual symlink in the newly created directory
              setwd(file.path(repo.macosx, thisSymlink))
              r.cmd.symlink.call <- paste0(Sys.which("ln"), " -s ../contrib .")
              system(r.cmd.symlink.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
              setwd(jmp.back)
            } else {}
          } else {
            message(paste0("repo: skip creating symbolic link ", thisSymPath, " (windows)"))
          }
        }
      } else {}
    } else {
      stop(simpleError("repo: you *must* provide a character string for \"main\" via OSX.repo (mac)!"))
    }
    removeIfExists(filePath=macosx.package)
    # since not all tar implementations (especially the BSD default on Mac OS X) support --exclude-vcs,
    # we'll exclude these manually
    tar.extraFlags <- excludeVCSDirs(
      src=file.path(R.libs,pck.package),
      exclude.dirs=c(".svn", "CVS", ".git", "_darcs", ".hg"),
      action="repo", target="mac binary"
    )
    if(isTRUE(unix.OS)){
      # make more portable archives, should work with GNU and BSD tar
      tar.extraFlags <- paste0(tar.extraFlags, " --format=ustar")
    } else {}
    # failsafe exclusion of backup files
    # --exclude=*\\~ caused trouble for path names with tilde
    tilde.allFiles <- list.files(file.path(R.libs,pck.package))
    tilde.excludeFiles <- tilde.allFiles[grepl(paste(paste0(".*~$"), collapse="|"), tilde.allFiles)]
    if(length(tilde.excludeFiles) > 0){
      tar.extraFlags <- paste(paste0(" --exclude='", tilde.excludeFiles, "'"), tar.extraFlags, collapse="")
      message(paste("repo: excluded these files from the mac binary:\n  ", tilde.excludeFiles, collapse="\n  "))
    } else {}

    ## TODO: find a solution without sedwd()
    jmp.back <- getwd()
    setwd(R.libs)
    # it seems that tar() in R 3.3 produces archives that contain files twice and
    # cannot be unpacked with OS X's bsdtar, so we'll try this manually
    #     tar(macosx.package, files=pck.package,
    #       tar=Sys.which("tar"),
    #       compression="gzip", extra_flags=paste("-h ", tar.extraFlags))
    mac.tar.call <- paste0(Sys.which("tar"), " -czhf ", macosx.package, " ", tar.extraFlags, " ", pck.package)
    system(mac.tar.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
    message(paste0("repo: created ", pckg.name.mac, " (mac OS X)"))
    setwd(jmp.back)
    tools::write_PACKAGES(dir=repo.macosx.R, type="mac.binary", verbose=TRUE, latestOnly=FALSE)
    message("repo: updated bin/PACKAGES (mac OS X)")
  } else {}

  ## fill debian repo
  if("deb" %in% actions){
    formals(debianize) <- deb.defaults
    debianize()
  } else {}

  ## update HTML index
  if("html" %in% actions){
    if(!"roxy" %in% actions){
      # copy DESCRIPTION to pckg directory
      stopifnot(file.copy(file.path(pck.source.dir, "DESCRIPTION"), file.path(repo.pckg.info, "DESCRIPTION"), overwrite=TRUE))
    } else {}
    # write CSS file, if none is present
    css.file <- file.path(repo.pckg.info.main, "web.css")
    if(!file_test("-f", css.file)){
      cat(rx.css(), file=css.file)
      message(paste0("html: created CSS file ", css.file))
    } else {}
    # copy RSS image, if not present
    RSS.image <- file.path(repo.pckg.info.main, "feed-icon-14x14.png")
    if(!file_test("-f", RSS.image)){
      RSS.local.image <- file.path(find.package("roxyPackage"), "images", "feed-icon-14x14.png")
      stopifnot(file.copy(RSS.local.image, RSS.image))
      message(paste0("html: copied RSS image to ", RSS.image))
    } else {}
    # check for binaries to link
    url.src <- url.win <- url.mac <- url.deb <- url.doc <- url.vgn <- title.vgn <- deb.repo <- NULL
    if(file_test("-f", repo.src.gz)){
      url.src <- pckg.name.src
    } else {}
    if(file_test("-f", win.package)){
      url.win <- binPackageLinks(package=pck.package, version=pck.version, repo.root=repo.root, type="win")[["win"]]
    } else {}
    if(file_test("-f", macosx.package)){
      url.mac <- binPackageLinks(package=pck.package, version=pck.version, repo.root=repo.root, type="mac")[["mac"]]
    } else {}
    url.debRepo.info <- NULL
    if(file_test("-f", deb.package)){
      if(!is.null(URL)){
        # generate repository info
        url.debRepo.info <- file.path(repo.pckg.info, "deb_repo.html")
        cat(debRepoInfo(
          URL=URL,
          dist=deb.defaults[["distribution"]],
          comp=deb.defaults[["component"]],
          arch=deb.defaults[["arch"]],
          version=pck.version,
          revision=deb.defaults[["revision"]],
          compression=deb.defaults[["compression"]],
          repo=deb.defaults[["origin"]],
          repo.name=deb.defaults[["repo.name"]],
          repo.root=repo.root,
          package=pckg.name.deb.part,
          keyring.options=deb.defaults[["keyring.options"]],
          page.css="../web.css",
          package.full=pckg.name.deb,
          imprint=html.options[["imprint"]],
          privacy.policy=html.options[["privacy.policy"]],
          deb.dir=deb.defaults[["deb.dir"]]
        ),
        file=url.debRepo.info)
        message(paste0("html: updated ", url.debRepo.info))
      } else {
        message("html: you need to specify 'URL' to generate debian repository information!")
      }
    } else {}
    # check if there is actually any built debian package in the repo
    # if not, the link to the debian installation notes will be omitted
    deb.built.packages <- dir(file.path(repo.root, deb.repo.path.part), pattern=paste0(pckg.name.deb.part, ".*", ".deb"))
    url.deb.repo <- NULL
    if(length(deb.built.packages) > 0){
      url.deb.repo <- "deb_repo.html"
    } else {}
    # check for docs to link
    pdf.docs <- file.path(repo.pckg.info, pckg.pdf.doc)
    # gather information (title, file name) about built vignettes
    vig.info <- tools::getVignetteInfo(package=pck.package, lib.loc=R.libs)
    if(file_test("-f", pdf.docs)){
      url.doc <- pckg.pdf.doc
    } else {}
    if(nrow(vig.info) > 0){
      if(all( c('Title', 'PDF') %in% colnames(vig.info))){
        url.vgn <- vig.info[, "PDF"]
        title.vgn <- vig.info[, "Title"]
      } else{
        stop(
          simpleError(
            paste(
              "doc: the output format of tools::getVignetteInfo()", 
              "is not compatible with this version of roxyPackage.",
              "Please consider filing a bug report!"
            )
          )
        )
      }
    } else {}
    # check for NEWS.Rd or NEWS file
    if(file_test("-f", pckg.NEWS.Rd)){
      roxy.NEWS2HTML(newsRd=pckg.NEWS.Rd, newsHTML=file.path(repo.pckg.info, "NEWS.html"), pckg=pck.package, css="../web.css")
      url.NEWS <- pckg.NEWS.html
    } else if(file_test("-f", pckg.NEWS.inst)){
      stopifnot(file.copy(pckg.NEWS.inst, file.path(repo.pckg.info, "NEWS"), overwrite=TRUE))
      url.NEWS <- file.path(repo.pckg.info, "NEWS")
    } else if(file_test("-f", pckg.NEWS)){
      stopifnot(file.copy(pckg.NEWS, file.path(repo.pckg.info, "NEWS"), overwrite=TRUE))
      url.NEWS <- file.path(repo.pckg.info, "NEWS")
    } else {
      url.NEWS <- ""
    }
    # generate package index file
    if(!file_test("-f", pckg.NEWS.rss)){
      RSS.file.name <- NULL
    } else {}
    package.html <- roxy.html(
      pckg.dscrptn,
      index=FALSE,
      css="web.css",
      R.version=R.Version.win,
      url.src=url.src,
      url.win=url.win,
      url.mac=url.mac,
      url.doc=url.doc,
      url.vgn=url.vgn,
      title.vgn=title.vgn,
      url.deb.repo=url.deb.repo,
      main.path.mac=OSX.repo[["main"]],
      title=html.options[["title"]],
      cite=pckg.cite.file.html,
      news=url.NEWS,
      changelog=pckg.changelog,
      rss.file=RSS.file.name,
      flattr.id=html.options[["flattr.id"]],
      URL=getURL(URL, purpose="default"),
      imprint=html.options[["imprint"]],
      privacy.policy=html.options[["privacy.policy"]]
    )
    target.file.pckg <- file.path(repo.pckg.info, "index.html")
    cat(package.html, file=target.file.pckg)
    message(paste0("html: updated ", target.file.pckg))
    # now generate the global index file by scanning for DESCRIPTIONs in all pckg folders
    all.descs <- list()
    for (this.elmt in file.path(repo.pckg.info.main, dir(repo.pckg.info.main))){
        desc.path <- file.path(this.elmt, "DESCRIPTION")
        if(file_test("-f", desc.path)){
          # ok, there a DESCRIPTION file
          all.descs[[length(all.descs) + 1]] <- read.dcf(desc.path)
        } else {}
      }
    target.file.pckg <- file.path(repo.pckg.info.main, "index.html")
    pckg.index.html <- roxy.html(
      all.descs,
      index=TRUE,
      css="web.css",
      title=html.options[["index"]],
      flattr.id=html.options[["repo.flattr.id"]],
      imprint=html.options[["imprint"]],
      privacy.policy=html.options[["privacy.policy"]]
    )
    cat(pckg.index.html, file=target.file.pckg)
    message(paste0("html: updated pckg index ", target.file.pckg))
    target.file.glob <- file.path(repo.root, "index.html")
    global.html <- roxy.html(
      all.descs,
      index=TRUE,
      css="web.css",
      title=html.options[["index"]],
      redirect="pckg/",
      flattr.id=html.options[["repo.flattr.id"]],
      imprint=html.options[["imprint"]],
      privacy.policy=html.options[["privacy.policy"]]
    )
    cat(global.html, file=target.file.glob)
    message(paste0("html: updated global index ", target.file.glob))
  } else {}

  if("check" %in% actions){
    # check for examples check file before
    chk.ex.file <- file.path(pck.source.dir, paste0(pck.package, "-Ex.R"))
    chk.ex.file.present <- ifelse(file_test("-f", chk.ex.file), TRUE, FALSE)
    tryCatch(chk.out.dir <- tempdir(), error=function(e) stop(e))
    # checks should better be performed on built packages not source directories
    if(all("package" %in% actions, file_test("-f", repo.src.gz))){
      pck.check.target <- repo.src.gz
    } else {
      warning(
        paste0(
          "check: checks should be done on built packages, not the unpackaged source. ",
          "to do so, add the \"package\" action."
        ),
        call.=FALSE
      )
      pck.check.target <- pck.source.dir
    }
    message(paste0("check: calling R CMD check, this might take a while..."))
    if(isTRUE(unix.OS)){
      r.cmd.check.call <- paste0("R_LIBS_USER=", R.libs, " ; ",
        R.bin, " CMD check --output=", chk.out.dir, " ", Rcmd.opt.check, pck.check.target)
      print(system(r.cmd.check.call, intern=TRUE))
    } else {
      r.cmd.check.call <- paste0("set R_LIBS_USER=", shQuote(R.libs, type="cmd"), " && ",
        R.bin, " CMD check --output=", chk.out.dir, " ", Rcmd.opt.check, shQuote(pck.check.target, type="cmd"))
      print(shell(r.cmd.check.call, translate=TRUE, intern=TRUE))
    }
    on.exit(
      message(paste0("check: saved results to ", chk.out.dir, "/", pck.package, ".Rcheck")),
      add=TRUE
    )
    # need to clean up?
    if(!isTRUE(chk.ex.file.present) & file_test("-f", chk.ex.file)){
      # there's an example file which wasn't here before
      unlink(chk.ex.file)
    } else {}
  } else {}

  return(invisible(NULL))
} ## end function roxy.package()
