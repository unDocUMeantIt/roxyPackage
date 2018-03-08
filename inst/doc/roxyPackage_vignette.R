## ----setup, include=FALSE------------------------------------------------
header_con <- file("vignette_header.html")
writeLines('<meta name="flattr:id" content="4zdzgd" />', header_con)
close(header_con)

## ---- eval=FALSE---------------------------------------------------------
#  simpleExample <- function(){NULL}

## ---- eval=FALSE---------------------------------------------------------
#  #' @export
#  simpleExample <- function(){NULL}

## ---- eval=FALSE---------------------------------------------------------
#  roxy.package(
#    pck.source.dir="~/myRcode/examplePackage",
#    pck.version="0.01-1",
#    R.libs="~/R",
#    repo.root="~/myRcode/repo",
#    pck.description=package_description(
#      Package="examplePackage",
#      Type="Package",
#      Title="An R Example Package",
#      AuthorsR="c(person(given=\"Ernst\", family=\"Dölle\",
#        email=\"e.a.doelle@example.com\",
#        role=c(\"aut\", \"cre\")))",
#      Depends="R (>= 3.0.0)",
#      Description="Provides a great function to produce NULL results.",
#      License="GPL (>= 3)",
#      Encoding="UTF-8",
#      LazyLoad="yes",
#      URL="https://example.com/doelleInnovations"
#    )
#  )

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("examplePackage", repos="https://myRepoURL.example.com")

## ---- eval=FALSE---------------------------------------------------------
#  roxy.package(
#    pck.source.dir="~/myRcode/examplePackage",
#    pck.version="0.01-1",
#    R.libs="~/R",
#    repo.root="~/myRcode/repo",
#    pck.description=package_description(
#      Package="examplePackage",
#      Type="Package",
#      Title="An R Example Package",
#      AuthorsR="c(person(given=\"Ernst\", family=\"Dölle\",
#        email=\"e.a.doelle@example.com\",
#        role=c(\"aut\", \"cre\")))",
#      Depends="R (>= 3.0.0)",
#      Description="Provides a great function to produce NULL results.",
#      License="GPL (>= 3)",
#      Encoding="UTF-8",
#      LazyLoad="yes",
#      URL="https://example.com/doelleInnovations"
#    ),
#    actions=c(
#      "roxy",
#      "cite",
#      "html",
#      "license",
#      "package"
#    ))

## ---- eval=FALSE---------------------------------------------------------
#  roxy.package(
#    pck.source.dir="~/myRcode/examplePackage",
#    pck.version="0.01-1",
#    R.libs="~/R",
#    repo.root="~/myRcode/repo",
#    pck.description=package_description(
#      Package="examplePackage",
#      Type="Package",
#      Title="An R Example Package",
#      AuthorsR="c(person(given=\"Ernst\", family=\"Dölle\",
#        email=\"e.a.doelle@example.com\",
#        role=c(\"aut\", \"cre\")))",
#      Depends="R (>= 3.0.0)",
#      Description="Provides a great function to produce NULL results.",
#      License="GPL (>= 3)",
#      Encoding="UTF-8",
#      LazyLoad="yes",
#      URL="https://example.com/doelleInnovations"
#    ),
#    actions=c(
#      "roxy",
#      "cite",
#      "html",
#      "license",
#      "log",
#      "package"
#    ),
#    ChangeLog=list(
#      added=c("new extra NULL feature", "new oblivion matrix"),
#      fixed=c("finally resolved the reandom results bug")
#    )
#  )

## ---- eval=FALSE---------------------------------------------------------
#  # you can re-use the package description here, but only a
#  # subset of it's data is needed for templates
#  pckg.dscrptn <- data.frame(
#    Package="examplePackage",
#    Author="Ernst A. Dölle <e.a.doelle@example.com>",
#    License="GPL (>= 3)",
#    stringsAsFactors=FALSE
#  )
#  cat(
#    templateFile(
#      name="exampleFunction",
#      pck.description=pckg.dscrptn
#    )
#  )

