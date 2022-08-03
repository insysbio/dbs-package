[![GitHub issues](https://img.shields.io/github/issues/insysbio/dbs-package.svg)](https://GitHub.com/insysbio/dbs-package/issues/)
[![GitHub license](https://img.shields.io/github/license/insysbio/dbs-package.svg)](https://github.com/insysbio/dbs-package/blob/master/LICENSE)

# dbs-package

_Accessory functions in R to support DBSolveOptimum_

The package provides the extended facilities for DBSolveOptimum users. It has two main purposes: (1) the creation and analysis of DBsolveOptimum inputs and outputs for multiple simulations and statistical analysis, (2) the import/export DBSolveOptimum model and data files from/to different formats.

DBSolveOptimum is a tool for construction and analysis of mathematical models of biological systems <http://insysbio.com/en/software/db-solve-optimum>

## Installation

### From binaries (Windows, R 4.0.3):

Download binaries from https://github.com/insysbio/dbs-package/raw/master/dist/dbs_0.9.1.zip
```R
# install from file
install.packages("dbs_0.9.1.zip", repos = NULL)
```

### From sources (all versions):

```R
# install devtools to simplify installation from Github
install.packages("devtools")
# install from github
devtools::install_github("insysbio/dbs-package")
```

### Troubles in Windows

if something goes wrong try to run 
```R
options(download.file.method = "wininet")
```
and then `devtools::install_github("insysbio/dbs-package")` line.

## Documentation

[dbs-manual.pdf](https://github.com/insysbio/dbs-package/blob/master/inst/doc/dbs-manual.pdf)

## License

MIT

## Authors

* Evgeny Metelkin
* Aleksey Alekseev
* Aleksey Kalinkin

**Copyright:** [InSysBio](http://insysbio.com), LLC
