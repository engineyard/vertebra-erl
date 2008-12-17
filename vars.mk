vertebra_confdir = $(sysconfdir)/vertebra
vertebra_libdir = $(libdir)/vertebra/lib

do_subst = $(SED) -e 's,[@]vertebra_confdir[@],$(vertebra_confdir),g' \
			-e 's,[@]vertebra_libdir[@],$(vertebra_libdir),g' \
			-e 's,[@]ERL[@],$(ERL),g' \
			-e 's,[@]VERSION[@],$(VERSION),g'
