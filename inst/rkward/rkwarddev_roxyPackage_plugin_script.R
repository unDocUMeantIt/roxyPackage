local({
## Vorbereiten
require(rkwarddev)
rkwarddev.required("0.06-6")

# define where the plugin should write its files
output.dir <- tempdir()
# overwrite an existing plugin in output.dir?
overwrite <- TRUE
# if you set guess.getters to TRUE, the resulting code will need RKWard >= 0.6.0
guess.getter <- TRUE

## Berechne
about.plugin <- rk.XML.about(
  name="rk.roxyPackage",
  author=person(given="m.eik", family="michalke", email="meik.michalke@hhu.de", role=c("aut", "cre")),
  about=list(desc="RKWard GUI dialogs to create and maintain your own R packages",
  version="0.01-0", license="GPL (>= 3)")
)

plugin.dependencies <- rk.XML.dependencies(
  dependencies=list(rkward.min="0.6.0"),
  package=list(
    c(name="rkwarddev")
  )
)

rk.set.comp("Create R package")

rxp.tab.about <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      rk.XML.row(
        rxp.input.package.name <- rk.XML.input("Package name", required=TRUE,
          help="Set the name of the R package."),
        rxp.input.package.version <- rk.XML.input("Package version", required=TRUE,
          help="Set the version number of the R package.")
      ),
      rk.XML.row(
        rxp.browser.package.root <- rk.XML.browser("Root directory of the package sources", type="dir", required=TRUE,
        help="Set the directory where your package code can be found. It is the directory containing at least a subdirectory
          R with the actual code, and it must be named after the package.")
      ),
      rk.XML.row(
        rxp.oset.authors <- rk.XML.optionset(
          content=rk.XML.col(rk.XML.stretch(before=list(
            rk.XML.row(
            about.contact <- rk.XML.frame(
              rk.XML.row(
                rk.XML.col(
                  rxp.aut.given <- rk.XML.input("Given name", required=TRUE,
                    help="First name of the package author."),
                  rxp.aut.family <- rk.XML.input("Family name", required=TRUE,
                    help="Family name of the package author."),
                  rxp.aut.email <- rk.XML.input("E-mail", required=FALSE,
                    help="The authors e-mail address, important for bug reports and receiving a myriad of thank yous..."),
                  rk.XML.stretch()),
                rk.XML.col(rk.XML.frame(
                  rxp.aut.auth <- rk.XML.cbox("Author", chk=TRUE,
                    help="Check this if this person is author of the package code."),
                  rxp.aut.maint <- rk.XML.cbox("Maintainer", chk=TRUE,
                    help="Check this if this person maintains the package."),
                  rxp.aut.cntr <- rk.XML.cbox("Contributor", chk=FALSE,
                    help="Check this if this person is a contributor to the package code (e.g., translations)."),
                  rk.XML.stretch(), label="Roles"))),
                label="Package author"
              )
            )
          ))),
          optioncolumn=list(
            rxp.optioncol.aut.given <- rk.XML.optioncolumn(connect=rxp.aut.given, modifier="text"),
            rxp.optioncol.aut.family <- rk.XML.optioncolumn(connect=rxp.aut.family, modifier="text"),
            rxp.optioncol.aut.email <- rk.XML.optioncolumn(connect=rxp.aut.email, modifier="text"),
            rxp.optioncol.aut.auth <- rk.XML.optioncolumn(connect=rxp.aut.auth, modifier="state"),
            rxp.optioncol.aut.maint <- rk.XML.optioncolumn(connect=rxp.aut.maint, modifier="state"),
            rxp.optioncol.aut.cntr <- rk.XML.optioncolumn(connect=rxp.aut.cntr, modifier="state")
          ),
          logic=rk.XML.logic(
            rk.XML.connect(governor=rxp.aut.maint, client=rxp.aut.email, set="required")
          )
        )
      ),
      label="Basic information"
    )
  ),
  rk.XML.stretch()
)

rxp.tab.description <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      rk.XML.row(
      ),
      rk.XML.row(
      ),
      label="Package description"
    ),
    rk.XML.stretch()
  )
)

# pckg.dscrptn <- data.frame(
#     Package="sdfasdfas",
#     Type="Package",
#     Title="An R Package for foo",
#     Author="foo foo, with contributions from bar bar",
#     AuthorsR="c(person(given=\"foo\", family=\"foo\", email=\"foo@example.com\",
#         role=c(\"aut\", \"cre\")),
#       person(given=\"Bar\", family=\"Bar\", email=\"bar@example.com\",
#         role=c(\"ctb\")))",
#     Maintainer="foo foo <foo@example.com>",
#     Depends="R (>= 2.10.0),methods",
#     Enhances="rkward",
#     Suggests="testthat,tm,SnowballC,shiny",
#     Description="bla bla bla",
#     License="GPL (>= 3)",
#     Encoding="UTF-8",
#     LazyLoad="yes",
#     URL="http://example.com/foo/stuff",
#     stringsAsFactors=FALSE)

rxp.tab.create <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      rk.XML.row(
        rk.XML.col(
          rxp.cbox.action.roxy <- rk.XML.cbox("Roxygenize the docs", chk=TRUE,
            help="If this is checked, the roxygenize() function of the roxygen package is called to rebuild the documentation."),
          rxp.cbox.action.package <- rk.XML.cbox("Build & install the package", chk=TRUE,
            help="If this is checked, the main package will be built and installed to the specified R library location."),
          rxp.cbox.action.doc <- rk.XML.cbox("Update PDF documentation", chk=TRUE,
            help="If this is checked, all PDF documentation (manual and vignettes) will be re-created."),
          rxp.cbox.action.log <- rk.XML.cbox("Update the ChangeLog file", chk=TRUE,
            help="If this is checked, the ChangeLog file will be updated."),
          rxp.cbox.action.cl2news <- rk.XML.cbox("Transform ChangeLog to NEWS.Rd", chk=TRUE,
            help="If this is checked, the ChangeLog file will be translated into a NEWS.Rd file."),
          rxp.cbox.action.news2rss <- rk.XML.cbox("Transform NEWS.Rd to RSS feed", chk=TRUE,
            help="If this is checked, the NEWS.Rd file will be translated into a RSS feed (available in the repository HTML pages)."),
          rxp.cbox.action.html <- rk.XML.cbox("Update repository HTML files", chk=TRUE,
            help="If this is checked, all relevant repository HTML files will be re-written."),
          rk.XML.stretch()
        ),
        rk.XML.col(
          rxp.cbox.action.win <- rk.XML.cbox("Windows binary package", chk=FALSE,
            help="If this is checked, a Windows binary package is being built as well. Only produces proper results if the package was written in pure R code."),
          rxp.cbox.action.macosx <- rk.XML.cbox("Mac OS X binary package", chk=FALSE,
            help="If this is checked, a Mac OS X binary package is being built as well. Only produces proper results if the package was written in pure R code."),
          rxp.cbox.action.check <- rk.XML.cbox("Run R package check", chk=FALSE,
            help="If this is checked, \"R CMD check\" is performed on the package sources."),
          rxp.cbox.action.cleanRd <- rk.XML.cbox("Clean line breaks in *.Rd files", chk=TRUE,
            help="If this is checked, all manual pages will get a line break after 90 characters. CRAN won't accept the package otherwise."),
          rxp.cbox.action.cite <- rk.XML.cbox("Update CITATION file", chk=TRUE,
            help="If this is checked, the CITATION file will be re-written."),
          rxp.cbox.action.license <- rk.XML.cbox("Update the LICENSE file", chk=FALSE,
            help="If this is checked, the LICENSE file will be re-written."),
          rk.XML.stretch()
        )
      ),
      label="Packaging actions"
    )
  ),
  rk.XML.row(
    rxp.frame.sndbx <- rk.XML.frame(
      rk.XML.row(
        rxp.cbox.sndbx.source <- rk.XML.cbox("Source directory", chk=TRUE,
          help="If this is checked, a copy of the package sources will be made below the sandbox directory and all
            changes will be applied to this copy. This means, running roxy.package() will not change your actual
            sources, unless you turn sandboxing off."
        ),
        rxp.cbox.sndbx.Rlibs <- rk.XML.cbox("R library", chk=TRUE,
          help="If this is checked, the resulting package will not be installed to your pre-defined R library,
            but a new R library below the sandbox directory, unless you turn sandboxing off."
        )
      ),
      rk.XML.row(
        rxp.cbox.sndbx.repo <- rk.XML.cbox("Repository", chk=TRUE,
          help="If this is checked, built packages (be it R source packages or binary packages) will not be
            copied to the defiend repository, but a new repository below the sandboxing directory, unless you turn sandboxing off. 
            In addition, all other repository files, like HTML files, vignettes, RSS feeds etc., are also only copied to the sandboxed
            repositry."
        ),
        rxp.cbox.sndbx.archive <- rk.XML.cbox("Repository archive", chk=TRUE,
          help="If this is checked, archiving of ord packages will not be done in the defined archive directory, but a new archive
            below the sandbox directory, unless you turn sandboxing off. It is recommended to keep this in sync with the repository setting."
        )
      ),
      rk.XML.row(
        rxp.browser.sndbx.dir <- rk.XML.browser("Sandbox directory (default: $TEMPDIR/roxyPackage/sandbox)", type="dir", required=FALSE,
          help=list("Set the directory to use as the sandbox root.
          It is recommended to leave this as is and use the default value (", XMLNode("code", "$TEMPDIR/roxyPackage/sandbox"), ").")
        )
      ),
      label="Use sandbox",
      checkable=TRUE,
      chk=TRUE)
  )
)

rxp.tab.multiR <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      rk.XML.row(
        rxp.browser.Rhomes <- rk.XML.browser("R homes (defaults to R.home())", type="dir", required=FALSE, help=FALSE)
      ),
      rk.XML.row(
        rxp.valsl.Rhomes <- rk.XML.valueslot("R homes", source=rxp.browser.Rhomes, property="selection", multi=TRUE, min=0,
          help="In case you want to build for multiple R versions and have all of them installed an this machine,
            you can define all R root directories to build against here."
        )
      ),
      label="Build agains multiple R versions"
    ),
    rk.XML.stretch()
  )
)

#       deb.description=list(
# #         Build.Depends.Indep="debhelper (>> 7.0.0), r-base-dev (>= 3.0.0), cdbs",
# #         Depends="r-base (>= 3.0.0)",
#         Maintainer="foo foo <foo@example.com>",
#         Section="math",
#         Priority="optional"
#       ),
#       actions=c("deb", "bin", "src"),
# #     actions=c("deb"),
#       overwrite=c("changelog", "control", "copyright", "rules"),
#       bin.opts="-rfakeroot -b -uc",
#       arch="all",
#       gpg.key="DDCDA632",
#       keep.build=FALSE),
#       ChangeLog=ChangeLog.entry,
#       Rbuildignore=pckg.Rbuildignore,
#       Rinstignore=c("inst/doc/koRpus_lit.bib", "inst/doc/ttr.pdf")

rxp.tab.debianize <- rk.XML.col(
  rk.XML.row(
    rxp.frame.action.deb <- rk.XML.frame(
      rk.XML.frame(
        rk.XML.col(
          rk.XML.row(
            rxp.deb.mnt.given <- rk.XML.input("Given name", id.name="debMaintainerGivenName", required=TRUE,
              help="First name of the Debian package maintainer."),
            rxp.deb.mnt.family <- rk.XML.input("Family name", id.name="debMaintainerFamilyName", required=TRUE,
              help="Family name of the Debian package maintainer.")
          ),
          rk.XML.row(
            rxp.deb.mnt.email <- rk.XML.input("E-mail", id.name="debMaintainerEMail", required=TRUE,
              help="The Debian package maintainers' e-mail address.")
          ),
          rk.XML.stretch()
  #         rk.XML.col(
  #           rxp.cbox.action.deb <- rk.XML.cbox("Debianize the package", chk=FALSE,
  #             help="If this is checked, the package sources are properly debianized and a .deb package is being built as well. Only works if run on a Debian based
  #               installation with a proper packaging setup."),
  #           rk.XML.stretch()
  #         )
        ),
         label="Debain package maintainer"
      ),
      label="Debianize the package",
      checkable=TRUE,
      chk=FALSE
    )
  )
)


rxp.dialog <- rk.XML.dialog(
  rk.XML.tabbook("Create R package",
    tabs=list(
      "About the package"=rxp.tab.about,
      "Create options"=rxp.tab.create,
      "R homes"=rxp.tab.multiR,
      "Debianize"=rxp.tab.debianize
#       "Sandboxing"=rk.XML.col()
    )
  ),
  label="Create R package"
)


#############
## JavaScript
JS.preprocess <- rk.paste.JS(
  rk.JS.vars(rxp.input.package.version, rxp.input.package.name, rxp.browser.package.root,
    rxp.frame.sndbx, rxp.cbox.sndbx.source, rxp.cbox.sndbx.Rlibs, rxp.cbox.sndbx.repo,
    rxp.cbox.sndbx.archive, rxp.browser.sndbx.dir
  ),
  rk.JS.vars(rxp.valsl.Rhomes, join="\\\",\\n\\t\\\""),
  echo("\npackageVersion <- \"", rxp.input.package.version, "\"\n"),
  echo("packageName <- \"", rxp.input.package.name, "\"\n"),
  echo("packageRoot <- \"", rxp.browser.package.root, "\"\n\n"),
  echo("packageDescription <- data.frame(\n",
    "\tPackage=packageName,\n", 
    "\tType=\"Package\",\n",
    "\tTitle=shortDescription,\n"
  ),
  rk.JS.optionset(rxp.oset.authors, vars=TRUE, guess.getter=guess.getter),
  ite(id(rxp.optioncol.aut.given, " != \"\""),
    rk.paste.JS(
      echo("\tAuthorsR=\"c(\n\t\t\t"),
      rk.JS.optionset(rxp.oset.authors,
        js.rxp.oset.authors.role <- rk.JS.options("optAuthorRole",
          ite(id(rxp.optioncol.aut.auth, " == 1"), qp("\\\"aut\\\"")),
          ite(id(rxp.optioncol.aut.maint, " == 1"), qp("\\\"cre\\\"")),
          ite(id(rxp.optioncol.aut.cntr, " == 1"), qp("\\\"ctb\\\"")),
          funct="c", option="role", collapse=""),
        echo("person("),
        echo("given=\\\"", rxp.optioncol.aut.given, "\\\""),
        ite(rxp.optioncol.aut.family, echo(", family=\\\"", rxp.optioncol.aut.family, "\\\"")),
        ite(rxp.optioncol.aut.email, echo(", email=\\\"", rxp.optioncol.aut.email, "\\\"")),
        ite(js.rxp.oset.authors.role, echo(js.rxp.oset.authors.role)),
        echo(")"),
        collapse=",\\n\\t\\t\\t"
      ),
      echo("\n\t\t)\",\n")
    )
  ),
  echo("\tstringsAsFactors=FALSE\n)\n\n"),
  ## multiple R homes
  ite(id(rxp.valsl.Rhomes, " != \"\""),
    echo(
      "R.homes <- c(\n\t\"", rxp.valsl.Rhomes, "\"\n)\n",
      "all.homes <- c(R.homes, R.home())\n",
      "all.libs <- c(file.path(R.homes,\"lib64\",\"R\",\"library\"))\n\n"
    ),
    echo(
      "all.homes <- R.home()\n",
      "all.libs <- c(file.path(R.home(),\"lib64\",\"R\",\"library\"))\n\n"
    )
  ),
  ## sandbox
  ite(rxp.frame.sndbx,
    rk.paste.JS(
      echo("sandbox(TRUE"),
      ite(id(rxp.browser.sndbx.dir, " != \"\""),
        echo(",\n\tsandbox.dir=\"", rxp.browser.sndbx.dir, "\"")
      ),
      tf(rxp.cbox.sndbx.source, opt="pck.source.dir", ifelse=TRUE, level=2),
      tf(rxp.cbox.sndbx.Rlibs, opt="R.libs", ifelse=TRUE, level=2),
      tf(rxp.cbox.sndbx.repo, opt="repo.root", ifelse=TRUE, level=2),
      ite(id(rxp.cbox.sndbx.archive, " != ", rxp.cbox.sndbx.repo),
        tf(rxp.cbox.sndbx.archive, opt="archive", ifelse=TRUE, level=2)
      ),
      echo("\n)\n\n")
    ),
    echo("sandbox(FALSE)\n\n")
  )
)

rxp.opt.actions <- rk.JS.options("actions",
  ite(rxp.cbox.action.roxy, qp("\n\t\t\"roxy\"")),
  ite(rxp.cbox.action.package, qp("\n\t\t\"package\"")),
  ite(rxp.cbox.action.doc, qp("\n\t\t\"doc\"")),
  ite(rxp.cbox.action.log, qp("\n\t\t\"log\"")),
  ite(rxp.cbox.action.cl2news, qp("\n\t\t\"cl2news\"")),
  ite(rxp.cbox.action.news2rss, qp("\n\t\t\"news2rss\"")),
  ite(rxp.cbox.action.html, qp("\n\t\t\"html\"")),
  ite(rxp.cbox.action.win, qp("\n\t\t\"win\"")),
  ite(rxp.cbox.action.macosx, qp("\n\t\t\"macosx\"")),
  ite(rxp.frame.action.deb, qp("\n\t\t\"deb\"")),
  ite(rxp.cbox.action.check, qp("\n\t\t\"check\"")),
  ite(rxp.cbox.action.cleanRd, qp("\n\t\t\"cleanRd\"")),
  ite(rxp.cbox.action.cite, qp("\n\t\t\"cite\"")),
  ite(rxp.cbox.action.license, qp("\n\t\t\"license\"")),
  collapse="",
  option="actions",
  funct="c",
  opt.sep=",\\n\\t"
)

JS.calculate <- rk.paste.JS(
  rk.JS.vars(rxp.frame.action.deb),
  rxp.opt.actions,
  echo("roxy.package(", rxp.opt.actions, ",\n"),
    echo("\tpck.description=packageDescription,\n"),
    echo("\tpck.source.dir=packageRoot,\n"),
    echo("\tpck.version=packageVersion,\n"),
    echo("\tR.homes=all.homes,\n"),
    echo("\tR.libs=all.libs,\n"),
    echo("\trepo.root=repositoryRoot,\n"),
# #   pck.date="2014-01-22","),
#     roxy.unlink.target=FALSE,"),
    echo("\tcleanup=TRUE,\n"),
    echo("\tURL=\"http://R.reaktanz.de\",\n"),
    ite(rxp.frame.action.deb,
      rk.paste.JS(
        echo("\tdeb.options=list(\n"),
          echo("\t\tbuild.dir=file.path(main.root,pck.name),\n"),
          echo("\t\trevision=deb.revision,\n"),
          echo("\t\torigin=\"other-reaktanz\",\n"),
          echo("\t\tdistribution=\"unstable\",\n"),
          echo("\t\tcomponent=\"main\",\n"),
          echo("\t\turgency=\"low\",\n"),
          echo("\t\tchangelog=deb.changelog,\n"),
          echo("\t\tdeb.description=list(\n"),
            echo("\t\t\tBuild.Depends.Indep=\"debhelper (>> 7.0.0), r-base-dev (>= 3.0.0), cdbs\",\n"),
            echo("\t\t\tDepends=\"r-base (>= 3.0.0)\",\n"),
            ite(rxp.deb.mnt.given,
              echo("\t\t\tMaintainer=\"", rxp.deb.mnt.given, " ", rxp.deb.mnt.family, " <", rxp.deb.mnt.email, ">\"\n")
            ),
          echo("\t\t),\n"),
          echo("\t\tSection=\"math\",\n"),
          echo("\t\tPriority=\"optional\"\n"),
          echo("\t\tactions=c(\"deb\", \"bin\", \"src\"),\n"),
          echo("\t\toverwrite=c(\"changelog\", \"control\", \"copyright\", \"rules\"),\n"),
          echo("\t\tbin.opts=\"-rfakeroot -b -uc\",\n"),
          echo("\t\tarch=\"all\",\n"),
          echo("\t\tgpg.key=\"DDCDA632\",\n"),
          echo("\t\tkeep.build=FALSE\n"),
        echo("\t)\n")
      ),
      echo("\tdeb.options=list()\n")
    ),
    echo("\tChangeLog=ChangeLog.entry,\n"),
    echo("\tRbuildignore=pckg.Rbuildignore,\n"),
    echo("\tRinstignore=c(\"inst/doc/koRpus_lit.bib\", \"inst/doc/ttr.pdf\")\n"),
  echo(")\n\n")
)
#   ChangeLog.entry <- list(
# #       added=c(),
# #       fixed=c()
#   )
# 
#   pck.version.v <- "0.05-5"
#   deb.revision <-  1 #2
#   deb.changelog <- "new upstream release" #c("corrected build dependecies")
#   build.em.all <- checkDevel <- CRAN <- FALSE
#   sandbox(TRUE, pck.source.dir=FALSE, R.libs=FALSE)
# #   sandbox(FALSE)
# #   build.em.all <- TRUE
# # checkDevel <- TRUE
# # CRAN <- TRUE ## nur für den CRAN-*build*!
# 
#   pck.name <- "sdfasdfas"
#   main.root <- file.path("/my","files","R")
#   r.dir <- file.path(main.root,pck.name,"svn",pck.name)
#   roxy.dir <- r.dir
#   repo.root.v <- file.path(main.root,"repo")
#   repo.archive.v <- file.path(main.root,"repo_archive")
#   R.libs.v <- file.path("/my","files","R")
#   if(isTRUE(checkDevel)){
#     sandbox(TRUE)
#     all.homes <- file.path("/my","files","software","R","inst","R-devel")
#     all.libs <- R.libs.v
#     roxyPackage:::set.roxyEnv(name="Rdevel", value=TRUE)
#     roxyPackage:::set.roxyEnv(name="Rversion", value="3.0.3")
#   } else if(isTRUE(build.em.all)){
#     source(file.path(main.root, "R.homes.mult.R"), local=TRUE, echo=TRUE)
#     all.homes <- c(R.homes.mult, R.home())
#     all.libs <- c(file.path(R.homes.mult,"lib64","R","library"), R.libs.v)
#   } else {
#     all.homes <- R.home()
#     all.libs <- R.libs.v
#   }
#   if(isTRUE(CRAN)){
#     pckg.dscrptn <- pckg.dscrptn[!names(pckg.dscrptn) %in% c("Author", "Maintainer")]
#     pckg.Rbuildignore <- c("debian", "LICENSE.txt","ChangeLog_hyph_patterns.txt")
#   } else {
#     pckg.Rbuildignore <- ""
#   }

#   archive.packages(paste("file://",repo.root.v, sep=""),
#     archive.root=repo.archive.v, to.dir="",
#     type=c("source", "win.binary", "mac.binary.leopard"),
#     overwrite=TRUE,
#     keep=2,
#     reallyDoIt=TRUE)


############
## help page
plugin.summary <- rk.rkh.summary(
  "The dialog walks you through most of the basic questions that you face when you want to
   transform your R code into a proper package. You can use the code generated by the dialog
   as the starting point for your own roxyPackage scripts."
)
plugin.usage <- rk.rkh.usage(
  "Most fields should have useful defaults. Look at the following explanaitions for details."
)

#############
## the main call
## if you run the following function call, files will be written to output.dir!
#############
# this is where things get serious, that is, here all of the above is put together into one plugin
plugin.dir <- rk.plugin.skeleton(
  about=about.plugin,
  path=output.dir,
  guess.getter=guess.getter,
  scan=c("var", "saveobj", "settings"),
  xml=list(
    dialog=rxp.dialog#,
    #wizard=,
    #logic=,
    #snippets=
  ),
  js=list(
    results.header=FALSE,
    #load.silencer=,
    require="roxyPackage",
    #variables=,
    #globals=,
    preprocess=JS.preprocess,
    calculate=JS.calculate,
    printout=""
  ),
  rkh=list(
    summary=plugin.summary,
    usage=plugin.usage#,
    #sections=,
    #settings=,
    #related=,
    #technical=
  ),
  create=c("pmap", "xml", "js", "desc", "rkh"),
  overwrite=overwrite,
  #components=list(),
  #provides=c("logic", "dialog"), 
  pluginmap=list(name="Create R package", hierarchy=c("file","export")),
  dependencies=plugin.dependencies,
  tests=FALSE,
  edit=FALSE,
  load=TRUE,
  show=TRUE
)

})
