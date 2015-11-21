SUBDIRS = programs 
EXTRA_DIST = HOWTO-Add-new-test \
             HOWTO-Run          \
             cleanup.sh         \
             reference/*        \
             test.sh            \
             tests/*
	
all-local:
	test "$(srcdir)" = "$(builddir)" || ln -sf $(srcdir)/reference $(builddir)
	test "$(srcdir)" = "$(builddir)" || ln -sf $(srcdir)/tests $(builddir)
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/test.sh $(builddir)
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/cleanup.sh $(builddir)
