PKG_VERSION := $(shell perl -ne 'print $$1 if /^Version:\s+([0-9]+\.[0-9]+(-[0-9]+)?)$$/' zipfR/DESCRIPTION)
PKG := zipfR_$(PKG_VERSION).tar.gz

default:
	@echo "Available targets for zipfR v$(PKG_VERSION)"
	@echo "  make build    ...  build source package"
	@echo "  make check    ...  test source package"
	@echo "  make install  ...  install source package locally"
	@echo "  make release  ...  check out and test source package for CRAN release"
	@echo "  make binary   ...  compile binary package for local architecture"
	@echo "  make clean    ...  clean up generated files"

.PHONY:	build check install release clean

build:
	R CMD build zipfR

check:	clean build $(PKG)
	R CMD check --multiarch $(PKG)

install:	clean build $(PKG)
	R CMD INSTALL $(PKG)

release:	clean
	R CMD build --resave-data zipfR
	R CMD check --as-cran $(PKG)
	@echo "$(PKG) is ready for release to CRAN"

binary:	build $(PKG)
	R CMD INSTALL --build $(PKG)

clean:
	-rm -f zipfR_*.tar.gz  # source package(s)
	-rm -f zipfR_*.tgz     # Mac binaries
	-rm -f zipfR_*.zip     # Windows binaries
	-rm -rf zipfR.Rcheck   # working directory of R CMD check
