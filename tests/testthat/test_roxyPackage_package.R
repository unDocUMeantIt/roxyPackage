context("basic packaging")
  test_that("roxygenizing and building the package", {
    # initialize test environment
    testPck <- roxyPackage:::generateTestPackage()

    sandbox(FALSE)
    roxy.package(
      pck.source.dir=testPck[["packageRoot"]],
      pck.version=testPck[["version"]],
      R.libs=testPck[["RLibs"]],
      repo.root=testPck[["repo"]],
      pck.description=testPck[["packageDescription"]],
      actions=c(
        "roxy",
  #       "cite",
  #       "html",
  #       "cl2news",
  #       "news2rss",
  #      "license"#,
  #       "log",
  #       "win",
  #       "macosx",
        "package"
      )#,
  #     ChangeLog=testPck[["changeLog"]],
  #     URL=testPck[["URL"]]
    )

    # now we should see some more files
    expect_that(
      all(
        file_test("-f", file.path(testPck[["R"]], "examplePackage-package.R")),
        file_test("-f", file.path(testPck[["packageRoot"]], "DESCRIPTION")),
        file_test("-f", file.path(testPck[["packageRoot"]], "NAMESPACE")),
        file_test("-f", file.path(testPck[["packageRoot"]], "man", "examplePackage-package.Rd"))
      ),
      is_true(),
      info="checking generated files in package sources"
    )
    # we should also have a repository
    expect_that(
      all(
        file_test("-f", file.path(testPck[["repo"]], "src", "contrib", "examplePackage_0.01-1.tar.gz")),
        file_test("-f", file.path(testPck[["repo"]], "src", "contrib", "PACKAGES")),
        file_test("-f", file.path(testPck[["repo"]], "src", "contrib", "PACKAGES.gz"))
      ),
      is_true(),
      info="checking the repository"
    )
    # and the package should have been installed
    expect_that(
      all(
        file_test("-f", file.path(testPck[["RLibs"]], "examplePackage", "R", "examplePackage.rdb")),
        file_test("-f", file.path(testPck[["RLibs"]], "examplePackage", "R", "examplePackage.rdx")),
        file_test("-f", file.path(testPck[["RLibs"]], "examplePackage", "DESCRIPTION")),
        file_test("-f", file.path(testPck[["RLibs"]], "examplePackage", "NAMESPACE")),
        file_test("-d", file.path(testPck[["RLibs"]], "examplePackage", "Meta")),
        file_test("-d", file.path(testPck[["RLibs"]], "examplePackage", "html")),
        file_test("-d", file.path(testPck[["RLibs"]], "examplePackage", "help"))
      ),
      is_true(),
      info="checking successful package installation"
    )

    roxyPackage:::removeTestPackage(testPck)
  })

  test_that("packaging for windows", {
  ## debug:
  #cat(loadedNamespaces(), file="/tmp/testNamespace.txt")
    # initialize test environment
    testPck <- roxyPackage:::generateTestPackage()

  ## debug:
  #cat(system(paste0("tree ", testRoot), intern=TRUE), file="/tmp/testRoot.txt", sep="\n")

  ## debug:
  #sink(file="/tmp/testRun.txt")
    sandbox(FALSE)
    roxy.package(
      pck.source.dir=testPck[["packageRoot"]],
      pck.version=testPck[["version"]],
      R.libs=testPck[["RLibs"]],
      repo.root=testPck[["repo"]],
      pck.description=testPck[["packageDescription"]],
      actions=c(
        "roxy",
        "win",
        "package"
      )
    )

  ## debug:
  #cat(system(paste0("tree ", testRoot), intern=TRUE), file="/tmp/testRoot.txt", sep="\n", append=TRUE)

    expect_that(
      all(
        file_test("-f", file.path(testPck[["repoWin"]], "examplePackage_0.01-1.zip")),
        file_test("-f", file.path(testPck[["repoWin"]], "PACKAGES")),
        file_test("-f", file.path(testPck[["repoWin"]], "PACKAGES.gz"))
      ),
      is_true()
    )

    roxyPackage:::removeTestPackage(testPck)
  ## debug:
  #sink()
  })

  test_that("packaging for OS X", {
    # initialize test environment
    testPck <- roxyPackage:::generateTestPackage()

    sandbox(FALSE)
    roxy.package(
      pck.source.dir=testPck[["packageRoot"]],
      pck.version=testPck[["version"]],
      R.libs=testPck[["RLibs"]],
      repo.root=testPck[["repo"]],
      pck.description=testPck[["packageDescription"]],
      actions=c(
        "roxy",
        "macosx",
        "package"
      )
    )

    expect_that(
      all(
        file_test("-f", file.path(testPck[["repoOSX"]], "examplePackage_0.01-1.tgz")),
        file_test("-f", file.path(testPck[["repoOSX"]], "PACKAGES")),
        file_test("-f", file.path(testPck[["repoOSX"]], "PACKAGES.gz"))
      ),
      is_true()
    )

    roxyPackage:::removeTestPackage(testPck)
  })

  # test_that("debianizing the package", {
  #   # initialize test environment
  #   testPck <- roxyPackage:::generateTestPackage()
  # 
  #   # full package building will only run on debian systems,
  #   # so we'll only generate the debian folder
  #   roxy.package(
  #     pck.source.dir=testPck[["packageRoot"]],
  #     pck.version=testPck[["version"]],
  #     R.libs=testPck[["RLibs"]],
  #     repo.root=testPck[["repo"]],
  #     pck.description=testPck[["packageDescription"]],
  #     actions=c(
  #       "roxy",
  #       "deb"
  #     ),
  #     deb.options=list(
  #       build.dir=testPck[["deb.options"]][["build.dir"]],
  #       repo.name=testPck[["deb.options"]][["repo.name"]],
  #       deb.description=testPck[["deb.options"]][["deb.description"]],
  #       keep.build=testPck[["deb.options"]][["keep.build"]],
  #       gpg.key=<SET ME TO A VALID KEY> # include an example key with the package?
  #     ),
  #     URL=testPck[["URL"]]
  #   )
  # 
  # expected structure:
  # /tmp/RtmpXXXXXX/
  # ├── doelle-keyring
  # │   ├── debian
  # │   │   ├── changelog
  # │   │   ├── compat
  # │   │   ├── control
  # │   │   ├── copyright
  # │   │   ├── postinst
  # │   │   ├── prerm
  # │   │   ├── rules
  # │   │   └── source
  # │   │       └── format
  # │   └── keyrings
  # │       └── doelle-keyring.gpg
  # └── roxyPackageTest
  #     ├── examplePackage
  #     │   ├── debian
  #     │   │   ├── changelog
  #     │   │   ├── compat
  #     │   │   ├── control
  #     │   │   ├── copyright
  #     │   │   ├── rules
  #     │   │   └── source
  #     │   │       └── format
  #     │   ├── DESCRIPTION
  #     │   ├── man
  #     │   │   └── examplePackage-package.Rd
  #     │   ├── NAMESPACE
  #     │   └── R
  #     │       ├── examplePackage-package.R
  #     │       └── simpleExample.R
  #     ├── R
  #     └── repo
  #         ├── deb
  #         │   ├── dists
  #         │   │   └── unstable
  #         │   │       ├── InRelease
  #         │   │       ├── main
  #         │   │       │   ├── all
  #         │   │       │   │   ├── doelle-keyring_0.01-1_all.changes
  #         │   │       │   │   ├── doelle-keyring_0.01-1_all.deb
  #         │   │       │   │   ├── doelle-keyring_0.01-1_amd64.changes
  #         │   │       │   │   ├── Packages
  #         │   │       │   │   ├── Packages.bz2
  #         │   │       │   │   ├── Packages.gz
  #         │   │       │   │   ├── r-other-doelle-examplepackage_0.01-1-1_all.changes
  #         │   │       │   │   ├── r-other-doelle-examplepackage_0.01-1-1_all.deb
  #         │   │       │   │   └── r-other-doelle-examplepackage_0.01-1-1_amd64.changes
  #         │   │       │   ├── binary-amd64
  #         │   │       │   │   ├── Packages
  #         │   │       │   │   ├── Packages.bz2
  #         │   │       │   │   └── Packages.gz
  #         │   │       │   ├── binary-i386
  #         │   │       │   │   ├── Packages
  #         │   │       │   │   ├── Packages.bz2
  #         │   │       │   │   └── Packages.gz
  #         │   │       │   └── source
  #         │   │       │       ├── Sources
  #         │   │       │       ├── Sources.bz2
  #         │   │       │       └── Sources.gz
  #         │   │       ├── Release
  #         │   │       └── Release.gpg
  #         │   └── source
  #         │       └── unstable
  #         │           ├── doelle-keyring_0.01-1.debian.tar.xz
  #         │           ├── doelle-keyring_0.01-1.dsc
  #         │           ├── doelle-keyring_0.01.orig.tar.xz
  #         │           ├── r-other-doelle-examplepackage_0.01-1-1.debian.tar.xz
  #         │           ├── r-other-doelle-examplepackage_0.01-1-1.dsc
  #         │           └── r-other-doelle-examplepackage_0.01-1.orig.tar.xz
  #         └── pckg
  #             └── examplePackage
  #                 └── DESCRIPTION
  # 
  #   expect_that(
  #     all(
  #       file_test("-f", file.path(testPck[["repoDeb"]], "examplePackage_0.01-1.tgz")),
  #       file_test("-f", file.path(testPck[["repoDeb"]], "PACKAGES")),
  #       file_test("-f", file.path(testPck[["repoDeb"]], "PACKAGES.gz"))
  #     ),
  #     is_true()
  #   )
  # })

context("license file")
  test_that("adding a license file", {
    # initialize test environment
    testPck <- roxyPackage:::generateTestPackage()

    sandbox(FALSE)
    roxy.package(
      pck.source.dir=testPck[["packageRoot"]],
      pck.version=testPck[["version"]],
      R.libs=testPck[["RLibs"]],
      repo.root=testPck[["repo"]],
      pck.description=testPck[["packageDescription"]],
      actions=c(
        "roxy",
        "license"
      )
    )

    expect_that(
      file_test("-f", file.path(testPck[["packageRoot"]], "LICENSE")),
      is_true()
    )

    roxyPackage:::removeTestPackage(testPck)
  })

context("ChangeLog")
  test_that("adding a ChangeLog", {
    # initialize test environment
    testPck <- roxyPackage:::generateTestPackage()

    sandbox(FALSE)
    roxy.package(
      pck.source.dir=testPck[["packageRoot"]],
      pck.version=testPck[["version"]],
      R.libs=testPck[["RLibs"]],
      repo.root=testPck[["repo"]],
      pck.description=testPck[["packageDescription"]],
      actions=c(
        "roxy",
        "log"
      ),
      ChangeLog=testPck[["changeLog"]]
    )

    expect_that(
      file_test("-f", file.path(testPck[["packageRoot"]], "ChangeLog")),
      is_true()
    )

    roxyPackage:::removeTestPackage(testPck)
  })

context("README.md")
  test_that("adding a README.md", {
    # initialize test environment
    testPck <- roxyPackage:::generateTestPackage()

    sandbox(FALSE)
    roxy.package(
      pck.source.dir=testPck[["packageRoot"]],
      pck.version=testPck[["version"]],
      R.libs=testPck[["RLibs"]],
      repo.root=testPck[["repo"]],
      pck.description=testPck[["packageDescription"]],
      actions=c(
        "roxy",
        "readme"
      )
    )

    expect_that(
      file_test("-f", file.path(testPck[["packageRoot"]], "README.md")),
      is_true()
    )

    roxyPackage:::removeTestPackage(testPck)
  })
