require(rkwarddev)
rkwarddev.required("0.08-1")

rk.local({
# define where the plugin should write its files
output.dir <- tempdir()
# overwrite an existing plugin in output.dir?
overwrite <- TRUE
# if you set guess.getters to TRUE, the resulting code will need RKWard >= 0.6.0
guess.getter <- TRUE
rk.set.indent(by="  ")
rk.set.empty.e(TRUE)
update.translations <- FALSE

about.plugin <- rk.XML.about(
  name="rk.roxyPackage",
  author=person(given="m.eik", family="michalke", email="meik.michalke@hhu.de", role=c("aut", "cre")),
  about=list(
    desc="RKWard GUI dialogs to create and maintain your own R packages",
    version="0.01-0",
    license="GPL (>= 3)",
    url="http://reaktanz.de/?c=hacking&s=roxyPackage"
  )
)

plugin.dependencies <- rk.XML.dependencies(
  dependencies=list(rkward.min="0.6.0"),
  package=list(
    c(name="rkwarddev")
  )
)

rk.set.comp("Create R package")

tabAbout <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      rk.XML.row(
        pckgName <- rk.XML.input("Package name", required=TRUE,
          help="Set the name of the R package.", id.name="pckgName"),
        pckgVersion <- rk.XML.input("Package version", required=TRUE,
          help="Set the version number of the R package.", id.name="pckgVersion")
      ),
      rk.XML.row(pckgShortDescription <- rk.XML.input("Title (short description)", required=TRUE,
        help="Describe your package in a sentence: What does it do? Will be used for the \"Title:\" field of
          the DESCRIPTION file.", id.name="pckgShortDescription")),
      rk.XML.row(pckgLongDescription <- rk.XML.input("Long description", size="large", required=TRUE,
        help="Give a summary of the package, to be used as the \"Description:\" field of the DESCRIPTION file.", id.name="pckgLongDescription")
      ),
      rk.XML.row(
        pckgHomepage <- rk.XML.input("Package homepage", required=TRUE,
          help="Provide an URL for the package homepage, to be used as the \"URL:\" field of the DESCRIPTION file.", id.name="pckgHomepage"),
        pckgLicense <- rk.XML.input("License", initial="GPL (>= 3)", required=TRUE, id.name="pckgLicense",
          help="Define the license for the package. A short form should be sufficient.")
      ),
      label="About the package"
    )
  ),
  rk.XML.stretch(),
  id.name="tabAbout"
)

tabAuthors <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      optionsetAuthors <- rk.XML.optionset(
        content=rk.XML.col(rk.XML.stretch(before=list(
          rk.XML.row(
          authContact <- rk.XML.row(
            rk.XML.col(
              authGivenName <- rk.XML.input("Given name", required=TRUE,
                help="First name of the package author.", id.name="authGivenName"),
              authFamilyName <- rk.XML.input("Family name", required=TRUE,
                help="Family name of the package author.", id.name="authFamilyName"),
              authEMail <- rk.XML.input("E-mail", required=FALSE,
                help="The authors e-mail address, important for bug reports and receiving a myriad of thank yous...", id.name="authEMail"),
              rk.XML.stretch(), id.name="colAuth"),
            rk.XML.col(rk.XML.frame(
              roleAuthor <- rk.XML.cbox("Author", chk=TRUE,
                help="Check this if this person is author of the package code.", id.name="roleAuthor"),
              roleMaintain <- rk.XML.cbox("Maintainer", chk=TRUE,
                help="Check this if this person maintains the package.", id.name="roleMaintain"),
              roleContrib <- rk.XML.cbox("Contributor", chk=FALSE,
                help="Check this if this person is a contributor to the package code (e.g., translations).", id.name="roleContrib"),
              roleCopyright <- rk.XML.cbox("Copyright holder", chk=FALSE,
                help="Check this if this person is a copyright holder of the package code.", id.name="roleCopyright"),
              rk.XML.stretch(), label="Roles")),
              id.name="authContact"
            )
          )
        )),
        id.name="col_rPckgAuthCont"),
        optioncolumn=list(
          ocolAuthGivenName <- rk.XML.optioncolumn(connect=authGivenName, modifier="text", id.name="ocolAuthGivenName"),
          ocolAuthFamilyName <- rk.XML.optioncolumn(connect=authFamilyName, modifier="text", id.name="ocolAuthFamilyName"),
          ocolAuthEMail <- rk.XML.optioncolumn(connect=authEMail, modifier="text", id.name="ocolAuthEMail"),
          ocolRoleAuthor <- rk.XML.optioncolumn(connect=roleAuthor, modifier="state", id.name="ocolRoleAuthor"),
          ocolRoleMaintain <- rk.XML.optioncolumn(connect=roleMaintain, modifier="state", id.name="ocolRoleMaintain"),
          ocolRoleContrib <- rk.XML.optioncolumn(connect=roleContrib, modifier="state", id.name="ocolRoleContrib"),
          ocolRoleCopyright <- rk.XML.optioncolumn(connect=roleCopyright, modifier="state", id.name="ocolRoleCopyright")
        ),
        logic=rk.XML.logic(
          rk.XML.connect(governor=roleMaintain, client=authEMail, set="required")
        ),
        id.name="optionsetAuthors"
      ),
      label="Authors and contributors"
    ),
    rk.XML.stretch()
  ),
  id.name="tabAuthors"
)

tabDepends <- rk.XML.row(
  rk.XML.col(
    rk.XML.row(
      rk.XML.frame(
        optionsetDepends <- rk.XML.optionset(
          content=rk.XML.col(rk.XML.stretch(before=list(
            rk.XML.row(
              rowDepends <- rk.XML.row(
                rk.XML.col(
                  dependPackage <- rk.XML.input("Package name", required=TRUE,
                    help="Name of a package that this package depends on.", id.name="dependPackage"),
                  dependVersion <- rk.XML.input("Version", required=FALSE,
                    help="Optional version information on the package.", id.name="dependVersion"),
                  rk.XML.stretch(),
                  id.name="colDep"
                ),
                id.name="rowDepends"
              )
            )
          )),
          id.name="col_rDepDepCont"),
          optioncolumn=list(
            ocolDependPackage <- rk.XML.optioncolumn(connect=dependPackage, modifier="text", id.name="ocolDependPackage"),
            dependVersion <- rk.XML.optioncolumn(connect=dependVersion, modifier="text", id.name="ocolDependVersion")
          ),
          optiondisplay=FALSE
        ),
        label="Depends on",
        id.name="optionsetDepends"
      ),
      rk.XML.stretch()
    ),
    rk.XML.row(
      rk.XML.frame(
        optionsetImports <- rk.XML.optionset(
          content=rk.XML.col(rk.XML.stretch(before=list(
            rk.XML.row(
              rowImports <- rk.XML.row(
                rk.XML.col(
                  importPackage <- rk.XML.input("Package name", required=TRUE,
                    help="Name of a package that this package imports from.", id.name="importPackage"),
                  importVersion <- rk.XML.input("Version", required=FALSE,
                    help="Optional version information on the imported package.", id.name="importVersion"),
                  rk.XML.stretch(),
                  id.name="colImports"
                ),
                id.name="rowImports"
              )
            )
          )),
          id.name="col_rDepImpCont"),
          optioncolumn=list(
            ocolImportPackage <- rk.XML.optioncolumn(connect=importPackage, modifier="text", id.name="ocolImportPackage"),
            ocolImportVersion <- rk.XML.optioncolumn(connect=importVersion, modifier="text", id.name="ocolImportVersion")
          ),
          optiondisplay=FALSE
        ),
        label="Imports from",
        id.name="optionsetImports"
      ),
      rk.XML.stretch()
    )

  ),
  rk.XML.col(
    rk.XML.row(
      rk.XML.frame(
        optionsetSuggests <- rk.XML.optionset(
          content=rk.XML.col(rk.XML.stretch(before=list(
            rk.XML.row(
              rxp.frm.dep.suggests <- rk.XML.row(
                rk.XML.col(
                  suggestPackage <- rk.XML.input("Package name", required=TRUE,
                    help="Name of a package that this package suggests.", id.name="suggestPackage"),
                  suggestVersion <- rk.XML.input("Version", required=FALSE,
                    help="Optional version information on the suggested package.", id.name="suggestVersion"),
                  rk.XML.stretch(),
                  id.name="colSuggests"
                ),
                id.name="rowSuggests"
              )
            )
          )),
          id.name="col_rDepSugCont"),
          optioncolumn=list(
            ocolSuggestPackage <- rk.XML.optioncolumn(connect=suggestPackage, modifier="text", id.name="ocolSuggestPackage"),
            ocolSuggestVersion <- rk.XML.optioncolumn(connect=suggestVersion, modifier="text", id.name="ocolSuggestVersion")
          ),
          optiondisplay=FALSE
        ),
        label="Suggests",
        id.name="optionsetSuggests"
      ),
      rk.XML.stretch()
    ),
    rk.XML.row(
      rk.XML.frame(
        optionsetEnhances <- rk.XML.optionset(
          content=rk.XML.col(rk.XML.stretch(before=list(
            rk.XML.row(
              rowEnhances <- rk.XML.row(
                rk.XML.col(
                  enhancePackage <- rk.XML.input("Package name", required=TRUE,
                    help="Name of a package that this package enhances.", id.name="enhancePackage"),
                  enhanceVersion <- rk.XML.input("Version", required=FALSE,
                    help="Optional version information on the enhanced package.", id.name="enhanceVersion"),
                  rk.XML.stretch(),
                  id.name="colEnhances"
                ),
                id.name="rowEnhances"
              )
            )
          )),
          id.name="col_rDepEnhCont"),
          optioncolumn=list(
            ocolEnhancePackage <- rk.XML.optioncolumn(connect=enhancePackage, modifier="text", id.name="ocolEnhancePackage"),
            ocolEnhanceVersion <- rk.XML.optioncolumn(connect=enhanceVersion, modifier="text", id.name="ocolEnhanceVersion")
          ),
          optiondisplay=FALSE
        ),
        label="Enhances",
        id.name="optionsetEnhances"
      ),
      rk.XML.stretch()
    )

  ),
  id.name="tabDepends"
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

tabCreate <- rk.XML.col(
  rk.XML.row(
    rk.XML.frame(
      rk.XML.row(
        rk.XML.col(
          actionRoxy <- rk.XML.cbox("Roxygenize the docs", chk=TRUE, id.name="actionRoxy",
            help="If this is checked, the roxygenize() function of the roxygen package is called to rebuild the documentation."),
          actionPackage <- rk.XML.cbox("Build & install the package", chk=TRUE, id.name="actionPackage",
            help="If this is checked, the main package will be built and installed to the specified R library location."),
          actionDoc <- rk.XML.cbox("Update PDF documentation", chk=TRUE, id.name="actionDoc",
            help="If this is checked, all PDF documentation (manual and vignettes) will be re-created."),
          actionLog <- rk.XML.cbox("Update the ChangeLog file", chk=TRUE, id.name="actionLog",
            help="If this is checked, the ChangeLog file will be updated."),
          actionCl2news <- rk.XML.cbox("Transform ChangeLog to NEWS.Rd", chk=TRUE, id.name="actionCl2news",
            help="If this is checked, the ChangeLog file will be translated into a NEWS.Rd file."),
          actionNews2rss <- rk.XML.cbox("Transform NEWS.Rd to RSS feed", chk=TRUE, id.name="actionNews2rss",
            help="If this is checked, the NEWS.Rd file will be translated into a RSS feed (available in the repository HTML pages)."),
          actionHtml <- rk.XML.cbox("Update repository HTML files", chk=TRUE, id.name="actionHtml",
            help="If this is checked, all relevant repository HTML files will be re-written."),
          rk.XML.stretch()
        ),
        rk.XML.col(
          actionWin <- rk.XML.cbox("Windows binary package", chk=FALSE, id.name="actionWin",
            help="If this is checked, a Windows binary package is being built as well. Only produces proper results if the package was written in pure R code."),
          actionMacosx <- rk.XML.cbox("Mac OS X binary package", chk=FALSE, id.name="actionMacosx",
            help="If this is checked, a Mac OS X binary package is being built as well. Only produces proper results if the package was written in pure R code."),
          actionCheck <- rk.XML.cbox("Run R package check", chk=FALSE, id.name="actionCheck",
            help="If this is checked, \"R CMD check\" is performed on the package sources."),
          actionCleanRd <- rk.XML.cbox("Clean line breaks in *.Rd files", chk=TRUE, id.name="actionCleanRd",
            help="If this is checked, all manual pages will get a line break after 90 characters. CRAN won't accept the package otherwise."),
          actionCite <- rk.XML.cbox("Update CITATION file", chk=TRUE, id.name="actionCite",
            help="If this is checked, the CITATION file will be re-written."),
          actionLicense <- rk.XML.cbox("Update the LICENSE file", chk=FALSE, id.name="actionLicense",
            help="If this is checked, the LICENSE file will be re-written."),
          rk.XML.stretch()
        )
      ),
      label="Packaging actions"
    )
  ),
  rk.XML.row(
    frameSandbox <- rk.XML.frame(
      rk.XML.row(
        sandboxSource <- rk.XML.cbox("Source directory", chk=TRUE, id.name="sandboxSource",
          help="If this is checked, a copy of the package sources will be made below the sandbox directory and all
            changes will be applied to this copy. This means, running roxy.package() will not change your actual
            sources, unless you turn sandboxing off."
        ),
        sandboxRLibs <- rk.XML.cbox("R library", chk=TRUE, id.name="sandboxRLibs",
          help="If this is checked, the resulting package will not be installed to your pre-defined R library,
            but a new R library below the sandbox directory, unless you turn sandboxing off."
        )
      ),
      rk.XML.row(
        sandboxRepo <- rk.XML.cbox("Repository", chk=TRUE, id.name="sandboxRepo",
          help="If this is checked, built packages (be it R source packages or binary packages) will not be
            copied to the defiend repository, but a new repository below the sandboxing directory, unless you turn sandboxing off. 
            In addition, all other repository files, like HTML files, vignettes, RSS feeds etc., are also only copied to the sandboxed
            repositry."
        ),
        sandboxArchive <- rk.XML.cbox("Repository archive", chk=TRUE, id.name="sandboxArchive",
          help="If this is checked, archiving of ord packages will not be done in the defined archive directory, but a new archive
            below the sandbox directory, unless you turn sandboxing off. It is recommended to keep this in sync with the repository setting."
        )
      ),
      rk.XML.row(
        sandboxDir <- rk.XML.browser("Sandbox directory (default: $TEMPDIR/roxyPackage/sandbox)", type="dir", required=FALSE, id.name="sandboxDir",
          help=list("Set the directory to use as the sandbox root.
          It is recommended to leave this as is and use the default value (", XMLNode("code", "$TEMPDIR/roxyPackage/sandbox"), ").")
        )
      ),
      label="Use sandbox",
      checkable=TRUE,
      chk=TRUE,
      id.name="frameSandbox")
  ),
  id.name="tabCreate"
)

tabEnvironment <- rk.XML.row(
  rk.XML.col(
    rk.XML.row(
      envPckgRoot <- rk.XML.browser("Root directory of the package sources", type="dir", required=TRUE, id.name="envPckgRoot",
      help="Set the directory where your package code can be found. It is the directory containing at least a subdirectory
        R with the actual code, and it must be named after the package.")
    ),
    rk.XML.row(
      envRepoRoot <- rk.XML.browser("Root directory of the local repository", type="dir", required=TRUE, id.name="envRepoRoot",
      help="Set the directory where your local package repository should be maintained. It will be created if necessary."),
      id.name="rowRootLocRepo"
    ),
    rk.XML.stretch()
  ),# rk.XML.row(repository info)
  rk.XML.col(
    frameRhomes <- rk.XML.frame(
      rk.XML.row(
        envRhomes <- rk.XML.browser("R homes (in addition to R.home())", type="dir", required=FALSE, id.name="envRhomes", help=FALSE)
      ),
      rk.XML.row(
        envRhomesVarslot <- rk.XML.valueslot("R homes", source=envRhomes, property="selection", multi=TRUE, min=0, id.name="envRhomesVarslot",
          help="In case you want to build for multiple R versions and have all of them installed an this machine,
            you can define all R root directories to build against here."
        )
      ),
      label="Build agains multiple R versions",
      checkable=TRUE,
      chk=FALSE,
      id.name="frameRhomes"
    ),
    rk.XML.stretch()
  ),
  id.name="tabEnvironment"
)

#       deb.description=list(
# #         Build.Depends.Indep="debhelper (>> 7.0.0), r-base-dev (>= 3.0.0), cdbs",
# #         Depends="r-base (>= 3.0.0)",
#       ),
#       bin.opts="-rfakeroot -b -uc",
#       keep.build=FALSE),

#       ChangeLog=ChangeLog.entry,
#       Rbuildignore=pckg.Rbuildignore,
#       Rinstignore=c("inst/doc/koRpus_lit.bib", "inst/doc/ttr.pdf")

#         echo("\t\tchangelog=deb.changelog,\n")
#         echo("\t\tdeb.description=list(\n")
#         echo("\t\t\tBuild.Depends.Indep=\"debhelper (>> 7.0.0), r-base-dev (>= 3.0.0), cdbs\",\n")
#         echo("\t\t\tDepends=\"r-base (>= 3.0.0)\",\n")
#         echo("\t\tbin.opts=\"-rfakeroot -b -uc\",\n")
#         echo("\t\tkeep.build=FALSE\n")


tabDebianize <- rk.XML.col(
  rk.XML.row(
    frameDeb <- rk.XML.frame(
      rk.XML.frame(
        rk.XML.col(
          rk.XML.row(
            debMaintainerGivenName <- rk.XML.input("Given name", id.name="debMaintainerGivenName", required=TRUE,
              help="First name of the Debian package maintainer."),
            debMaintainerFamilyName <- rk.XML.input("Family name", id.name="debMaintainerFamilyName", required=TRUE,
              help="Family name of the Debian package maintainer.")
          ),
          rk.XML.row(
            debMaintainerEMail <- rk.XML.input("E-mail", id.name="debMaintainerEMail", required=TRUE,
              help="The Debian package maintainers' e-mail address."),
            debMaintainerPGP <- rk.XML.input("OpenPGP key ID", id.name="debMaintainerPGP", required=TRUE,
              help="To sign the package and use secure apt features, you must provide the ID (8 alphanumeric characters) of your OpenPGP key.")
          ),
          rk.XML.stretch()
        ),
        label="Debain package maintainer"
      ),
      rk.XML.frame(
        rk.XML.col(
          rk.XML.row(
            debRepoURL <- rk.XML.input("Repository URL", initial="http://R.reaktanz.de", required=TRUE,
              help="Provide an URL for the Debian package repository.", id.name="debRepoURL"),
            debOrigin <- rk.XML.input("Package origin", initial="other-reaktanz", required=TRUE, id.name="debOrigin",
              help="The package origin becomes part of the file name. It's custom to start with \"other-\" followed by
                your unique identifier if the package is not from one of the mainstream R repositories.")
          ),
          rk.XML.stretch()
        ),
        label="Repository"
      ),
      rk.XML.row(
        rk.XML.frame(
          rk.XML.col(
            rk.XML.row(
              debDistribution <- rk.XML.input("Distribution", initial="unstable", required=TRUE,
                help="Identify the distribution this package was built for.", id.name="debDistribution"),
              debComponent <- rk.XML.input("Component", initial="main", required=TRUE,
                help="The component of the distribution this package belongs to.", id.name="debComponent")
            ),
            rk.XML.stretch()
          ),
          label="Distribution"
        ),
        rk.XML.frame(
          rk.XML.col(
            rk.XML.row(
              debUrgency <- rk.XML.input("Urgency", initial="low", required=TRUE,
                help="Urgency flag of the package.", id.name="debUrgency"),
              debPriority <- rk.XML.input("Priority", initial="optional", required=TRUE,
                help="Priority flag of the package.", id.name="debPriority")
            ),
            rk.XML.stretch()
          ),
          label="Importance"
        )
      ),
      rk.XML.frame(
        rk.XML.row(
          rk.XML.col(
            debActionDeb <- rk.XML.cbox("Debianize package sources", chk=TRUE, id.name="debActionDeb",
              help="If this is checked, the package sources are being debianized (i.e, the \"debian\" directory will be created and populated)."),
            debActionBin <- rk.XML.cbox("Build Debian package", chk=TRUE, id.name="debActionBin",
              help="If this is checked, a Debian binary package will be built."),
            debActionSrc <- rk.XML.cbox("Build Debian source package", chk=TRUE, id.name="debActionSrc",
              help="If this is checked, a Debian source package will be built."),
            debOverwritePGP <- rk.XML.cbox("Re-build OpenPGP keyring package", chk=FALSE, id.name="debOverwritePGP",
              help="Re-write the keyring package in the repository (by default present packages are left unchanged)."),
            debKeepBuild <- rk.XML.cbox("Keep build", chk=FALSE, id.name="debKeepBuild",
              help="Work is done in a generated folder with a random name. Usually it is removed afterwards, unless you check this option.")
          ),
          rk.XML.col(
            debOverwriteChangelog <- rk.XML.cbox("Overwrite 'changelog'", chk=TRUE, id.name="debOverwriteChangelog",
              help="Update ./debian/changelog, but only if no entry for this package version and revision is there yet."),
            debOverwriteControl <- rk.XML.cbox("Overwrite 'control'", chk=TRUE, id.name="debOverwriteControl",
              help="Re-write ./debian/control."),
            debOverwriteCopyright <- rk.XML.cbox("Overwrite 'copyright'", chk=TRUE, id.name="debOverwriteCopyright",
              help="Re-write ./debian/copyright."),
            debOverwriteRules <- rk.XML.cbox("Overwrite 'rules'", chk=TRUE, id.name="debOverwriteRules",
              help="Re-write ./debian/rules."),
            debOverwriteCompat <- rk.XML.cbox("Overwrite 'compat'", chk=FALSE, id.name="debOverwriteCompat",
              help="Re-write ./debian/compat."),
            rk.XML.stretch()
          )
        ),
        label="Debianize actions"
      ),
      rk.XML.stretch(),
      label="Debianize the package",
      checkable=TRUE,
      chk=FALSE,
      help="If this is checked, the package sources are properly debianized and a .deb package is being built as well. Only works if run on a Debian based
        installation with a proper packaging setup.",
      id.name="frameDeb"
    )
  ),
  id.name="tabDebianize"
)


rxp.dialog <- rk.XML.dialog(
  rk.XML.tabbook("Create R package",
    tabs=list(
      "Description"=tabAbout,
      "Authors"=tabAuthors,
      "Create options"=tabCreate,
      "Dependencies"=tabDepends,
      "Environment"=tabEnvironment,
      "Debianize"=tabDebianize
#       "Sandboxing"=rk.XML.col()
    )
  ),
  label="Create R package"
)

#############
## JavaScript
JS.preprocess <- rk.paste.JS(
  rk.JS.vars(
    pckgVersion,
    pckgName,
    pckgShortDescription,
    pckgLongDescription,
    pckgHomepage,
    pckgLicense,
    envPckgRoot,
    envRepoRoot,
    sandboxSource,
    sandboxRLibs,
    sandboxRepo,
    sandboxArchive,
    sandboxDir
  ),
  # make sure the variable is available with correct names
  rk.JS.vars(
    frameRhomes,
    frameSandbox,
    modifiers="checked"
  ),
  rk.JS.vars(envRhomesVarslot, join="\\\",\\n\\t\\\""),
  echo("\npackageVersion <- \"", pckgVersion, "\"\n"),
  echo("packageName <- \"", pckgName, "\"\n"),
  echo("packageRoot <- \"", envPckgRoot, "\"\n\n"),
  echo("packageDescription <- data.frame(\n",
    "\tPackage=packageName,\n", 
    "\tType=\"Package\",\n"
  ),
  js(
    if(pckgShortDescription){
      echo("\tTitle=\"", pckgShortDescription, "\",\n")
    } else {},
    if(pckgLongDescription){
      echo("\tDescription=\"", pckgLongDescription, "\",\n")
    } else {},
    rk.JS.optionset(optionsetAuthors, vars=TRUE, guess.getter=guess.getter),
    if(ocolAuthGivenName != ""){
      echo("\tAuthorsR=\"c(\n\t\t\t")
      rk.JS.optionset(optionsetAuthors,
        js.optionsetAuthors.role <- rk.JS.options("optAuthorRole",
          .ite=js(
            if(ocolRoleAuthor == 1){
              qp("\\\"aut\\\"")
            } else {},
            if(ocolRoleMaintain == 1){
              qp("\\\"cre\\\"")
            } else {},
            if(ocolRoleContrib == 1){
              qp("\\\"ctb\\\"")
            } else {},
            if(ocolRoleCopyright == 1){
              qp("\\\"cph\\\"")
            } else {},
            keep.ite=TRUE
          ),
          funct="c", option="role", collapse=""),
        echo("person("),
        echo("given=\\\"", ocolAuthGivenName, "\\\""),
        js(
          if(ocolAuthFamilyName){
            echo(", family=\\\"", ocolAuthFamilyName, "\\\"")
          } else {},
          if(ocolAuthEMail){
            echo(", email=\\\"", ocolAuthEMail, "\\\"")
          } else {},
          if(js.optionsetAuthors.role){
            echo(js.optionsetAuthors.role)
          } else {},
          level=4
        ),
        echo(")"),
        collapse=",\\n\\t\\t\\t"
      )
      echo("\n\t\t)\",\n")
    } else {},
    rk.JS.optionset(optionsetDepends, vars=TRUE, guess.getter=guess.getter),
    if(ocolDependPackage != ""){
      echo("\tDepends=\"")
      rk.JS.optionset(optionsetDepends,
        echo(ocolDependPackage),
        js(
          if(dependVersion){
            echo(" (", dependVersion, ")")
          } else {},
          level=4
        ),
        collapse=","
      )
      echo("\",\n")
    } else {},
    rk.JS.optionset(optionsetImports, vars=TRUE, guess.getter=guess.getter),
    if(ocolImportPackage != ""){
      echo("\tImports=\"")
      rk.JS.optionset(optionsetImports,
        echo(ocolImportPackage),
        js(
          if(ocolImportVersion){
            echo(" (", ocolImportVersion, ")")
          } else {},
          level=4
        ),
        collapse=","
      )
      echo("\",\n")
    } else {},
    rk.JS.optionset(optionsetSuggests, vars=TRUE, guess.getter=guess.getter),
    if(ocolSuggestPackage != ""){
      echo("\tSuggests=\"")
      rk.JS.optionset(optionsetSuggests,
        echo(ocolSuggestPackage),
        js(
          if(ocolSuggestVersion){
            echo(" (", ocolSuggestVersion, ")")
          } else {},
          level=4
        ),
        collapse=","
      )
      echo("\",\n")
    } else {},
    rk.JS.optionset(optionsetEnhances, vars=TRUE, guess.getter=guess.getter),
    if(ocolEnhancePackage != ""){
      echo("\tEnhances=\"")
      rk.JS.optionset(optionsetEnhances,
        echo(ocolEnhancePackage),
        js(
          if(ocolEnhanceVersion){
            echo(" (", ocolEnhanceVersion, ")")
          } else {},
          level=4
        ),
        collapse=","
      )
      echo("\",\n")
    } else {},
    if(pckgHomepage){
      echo("\tURL=\"", pckgHomepage, "\",\n")
    } else {},
    if(pckgLicense){
      echo("\tLicense=\"", pckgLicense, "\",\n")
    } else {},
    echo("\tstringsAsFactors=FALSE\n)\n\n"),
    ## multiple R homes
    if(frameRhomesChecked && envRhomesVarslot != ""){
      echo(
        "R.homes <- c(\n\t\"", envRhomesVarslot, "\"\n)\n",
        "all.homes <- c(R.homes, R.home())\n",
        "all.libs <- c(file.path(R.homes,\"lib64\",\"R\",\"library\"))\n\n"
      )
    } else {
      echo(
        "all.homes <- R.home()\n",
        "all.libs <- c(file.path(R.home(),\"lib64\",\"R\",\"library\"))\n\n"
      )
    },
    ## sandbox
    if(frameSandbox){
      echo("sandbox(TRUE")
      if(sandboxDir != ""){
        echo(",\n\tsandbox.dir=\"", sandboxDir, "\"")
      } else {}
      tf(sandboxSource, opt="pck.source.dir", ifelse=TRUE, level=2)
      tf(sandboxRLibs, opt="R.libs", ifelse=TRUE, level=2)
      tf(sandboxRepo, opt="repo.root", ifelse=TRUE, level=2)
      if(sandboxArchive != sandboxRepo){
        tf(sandboxArchive, opt="archive", ifelse=TRUE, level=2)
      } else {}
      echo("\n)\n\n")
    } else {
      echo("sandbox(FALSE)\n\n")
    }
  )
)

rxp.opt.actions <- rk.JS.options("actions",
  .ite=js(
    if(actionRoxy){
      qp("\n\t\t\"roxy\"")
    } else {},
    if(actionPackage){
      qp("\n\t\t\"package\"")
    } else {},
    if(actionDoc){
      qp("\n\t\t\"doc\"")
    } else {},
    if(actionLog){
      qp("\n\t\t\"log\"")
    } else {},
    if(actionCl2news){
      qp("\n\t\t\"cl2news\"")
    } else {},
    if(actionNews2rss){
      qp("\n\t\t\"news2rss\"")
    } else {},
    if(actionHtml){
      qp("\n\t\t\"html\"")
    } else {},
    if(actionWin){
      qp("\n\t\t\"win\"")
    } else {},
    if(actionMacosx){
      qp("\n\t\t\"macosx\"")
    } else {},
    if(frameDeb){
      qp("\n\t\t\"deb\"")
    } else {},
    if(actionCheck){
      qp("\n\t\t\"check\"")
    } else {},
    if(actionCleanRd){
      qp("\n\t\t\"cleanRd\"")
    } else {},
    if(actionCite){
      qp("\n\t\t\"cite\"")
    } else {},
    if(actionLicense){
      qp("\n\t\t\"license\"")
    } else {},
    keep.ite=TRUE
  ),
  collapse="",
  option="actions",
  funct="c",
  opt.sep=",\\n\\t"
)

rxp.opt.debActions <- rk.JS.options("debActions",
  .ite=js(
    if(debActionDeb){
      qp("\n\t\t\t\"deb\"")
    } else {},
    if(debActionBin){
      qp("\n\t\t\t\"bin\"")
    } else {},
    if(debActionSrc){
      qp("\n\t\t\t\"src\"")
    } else {},
    keep.ite=TRUE
  ),
  collapse="",
  option="actions",
  funct="c",
  opt.sep="\\t\\t"
)

rxp.opt.debOverwrite <- rk.JS.options("debOverwrite",
  .ite=js(
    if(debOverwriteChangelog){
      qp("\n\t\t\t\"changelog\"")
    } else {},
    if(debOverwriteControl){
      qp("\n\t\t\t\"control\"")
    } else {},
    if(debOverwriteCopyright){
      qp("\n\t\t\t\"copyright\"")
    } else {},
    if(debOverwriteRules){
      qp("\n\t\t\t\"rules\"")
    } else {},
    if(debOverwriteCompat){
      qp("\n\t\t\t\"compat\"")
    } else {},
    if(debOverwritePGP){
      qp("\n\t\t\t\"gpg.key\"")
    } else {},
    keep.ite=TRUE
  ),
  collapse="",
  option="overwrite",
  funct="c",
  opt.sep="\\t\\t"
)

JS.calculate <- rk.paste.JS(
  rk.JS.vars(frameDeb),
  rxp.opt.actions,
  rxp.opt.debActions,
  rxp.opt.debOverwrite,
  echo("roxy.package(", rxp.opt.actions, ",\n"),
    echo("\tpck.description=packageDescription,\n"),
    echo("\tpck.source.dir=packageRoot,\n"),
    echo("\tpck.version=packageVersion,\n"),
    echo("\tR.homes=all.homes,\n"),
    echo("\tR.libs=all.libs,\n"),
    js(
      if(envRepoRoot){
        echo("\trepo.root=\"", envRepoRoot, "\",\n")
      } else {},
# #   pck.date="2014-01-22","),
      echo("\tcleanup=TRUE,\n"),
      if(debRepoURL){
        echo("\tURL=\"", debRepoURL, "\",\n")
      } else {},
      if(frameDeb){
        echo("\tdeb.options=list(\n")
        echo("\t\tbuild.dir=file.path(main.root,pck.name),\n")
        echo("\t\trevision=deb.revision,\n")
        if(debOrigin){
          echo("\t\torigin=\"", debOrigin, "\",\n")
        } else {}
        if(debDistribution){
          echo("\t\tdistribution=\"", debDistribution, "\",\n")
        } else {}
        if(debComponent){
          echo("\t\tcomponent=\"", debComponent, "\",\n")
        } else {}
        if(debUrgency){
          echo("\t\turgency=\"", debUrgency, "\",\n")
        } else {}
        echo("\t\tchangelog=deb.changelog,\n")
        echo("\t\tdeb.description=list(\n")
        echo("\t\t\tBuild.Depends.Indep=\"debhelper (>> 7.0.0), r-base-dev (>= 3.0.0), cdbs\",\n")
        echo("\t\t\tDepends=\"r-base (>= 3.0.0)\",\n")
        if(debMaintainerGivenName){
          echo("\t\t\tMaintainer=\"", debMaintainerGivenName, " ", debMaintainerFamilyName, " <", debMaintainerEMail, ">\"\n")
        } else {}
        echo("\t\t),\n")
        echo("\t\tSection=\"math\",\n")
        if(debPriority){
          echo("\t\tPriority=\"", debPriority, "\",\n")
        } else {}
        echo(rxp.opt.debActions, ",\n")
        echo(rxp.opt.debOverwrite, ",\n")
        echo("\t\tbin.opts=\"-rfakeroot -b -uc\",\n")
        echo("\t\tarch=\"all\",\n")
        if(debMaintainerPGP){
          echo("\t\tgpg.key=\"", debMaintainerPGP, "\",\n")
        } else {}
        if(debKeepBuild){
          echo("\t\tkeep.build=TRUE\n")
        } else {
          echo("\t\tkeep.build=FALSE\n")
        }
        echo("\t)\n")
      } else {
        echo("\tdeb.options=list()\n")
      }
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
  show=TRUE,
  gen.info="$SRC/inst/rkward/rkwarddev_roxyPackage_plugin_script.R",
  hints=FALSE
)

if(isTRUE(update.translations)){
  rk.updatePluginMessages(file.path(output.dir,"roxyPackage","inst","rkward","roxyPackage.pluginmap"))
} else {}

})
