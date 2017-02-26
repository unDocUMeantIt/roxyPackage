# roxyPackage

[![Flattr this git repo](https://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=m.eik&url=https://github.com/unDocUMeantIt/roxyPackage&title=roxyPackage&language=en_GB&tags=github&category=software)

roxyPackage is a collection of tools to automate packaging of [R](https://r-project.org) code.

The intention of this package is to make packaging R code as easy as possible. 'roxyPackage' uses
tools from the 'roxygen2' package to generate documentation. It also automatically generates and
updates files like *-package.R, DESCRIPTION, CITATION, ChangeLog and NEWS.Rd. Building packages
supports source format, as well as several binary formats (MS Windows, Mac OS X, Debian GNU/Linux)
if the package contains pure R code only.

The packages built are stored in a fully functional local R package repository which can be synced to a
web server to share them with others. This includes the generation of browsable HTML pages similar to
CRAN, with support for RSS feeds from the ChangeLog. Please read the vignette for a more detailed
explanation by example.

roxyPackage also includes a plugin for [RKWard](https://rkward.kde.org), a powerful GUI and
IDE for R, providing graphical dialogs for its basic features. To make full use of this feature,
please install [RKWard](https://rkward.kde.org) (plugins are detected automatically).

More information on roxyPackage is available on the [project homepage](https://reaktanz.de/?c=hacking&s=roxyPackage).

## Installation

### Development releases via the project repository

Installation of tha latest stable release is fairly easy, it's available from the project's own repository:

```
install.packages("roxyPackage", repo="https://reaktanz.de/R")
```

To automatically get updates, consider adding the repository to your R configuration. You might also
want to subscribe to the package's [RSS feed](https://reaktanz.de/R/pckg/roxyPackage/RSS.xml) to get notified of new releases.

If you're running a Debian based operating system, you might be interested in the
[precompiled *.deb packages](https://reaktanz.de/R/pckg/roxyPackage/deb_repo.html).

### Installation via GitHub

To install it directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```
library(devtools)
install_github("unDocUMeantIt/roxyPackage") # stable release
install_github("unDocUMeantIt/roxyPackage", ref="develop") # development release
```

### Not on CRAN

Unfortunately, one feature of roxyPackage, namely the generation of a NEWS.Rd file, violates the CRAN policy
because it uses an internal function of the R utils package. Since this is the officially recommended way
of making NEWS.Rd files, there's no obvious workaround for this, hence roxyPackage is not available on CRAN
any longer.

## Contributing

To ask for help, report bugs, suggest feature improvements, or discuss the global
development of the package, please use the issue tracker on GitHub.

### Branches

Please note that all development happens in the `develop` branch. Pull requests against the `master`
branch will be rejected, as it is reserved for the current stable release.

## Licence

Copyright 2012-2017 Meik Michalke <meik.michalke@hhu.de>

roxyPackage is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

roxyPackage is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with roxyPackage.  If not, see <http://www.gnu.org/licenses/>.
