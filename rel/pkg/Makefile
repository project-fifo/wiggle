VERSION=0.4.8p1
COMPONENT=wiggle

.PHONY: build_info package package_list

package_list:
	echo "@pkgdep erlang>=15.1.1" > packlist
	(cd ../$(COMPONENT); find * -type f | sort) >> packlist
build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=fifo" >>build-info
	echo "HOMEPAGE=http://project-fifo.net/" >>build-info

clean:
	-rm *.tgz build-info packlist

package: package_list build_info
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I /opt/local/$(COMPONENT) -p ../$(COMPONENT) -U $(COMPONENT)-$(VERSION)$(SUFFIX).tgz
