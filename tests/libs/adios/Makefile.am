ACLOCAL_AMFLAGS=-I config

SUBDIRS=tests

bin_SCRIPTS = adios_config

install-data-hook:
        echo "SRCDIR=\"$(PWD)\"" >> adios_config.flags; \
        if git rev-parse --git-dir &>/dev/null; then \
            echo "GITLOG=\"`git log -n 1 --format=\"Last commit on %ad hash %h\"`\"" > adios_git_status; \
            echo "read -d  GITSTAT <<_EOF_" >> adios_git_status; \
            git status -uno >> adios_git_status; \
            echo "_EOF_" >> adios_git_status; \
        else \
            echo "GITLOG=\"git log not available\"" >> adios_git_status; \
            echo "GITSTAT=\"git status not available\"" >> adios_git_status; \
            echo "Note: git command not found or $(PWD) is not a git repository."; \
        fi 
        $(INSTALL_DATA) adios_config.flags $(DESTDIR)$(sysconfdir)
        $(INSTALL_DATA) adios_git_status $(DESTDIR)$(sysconfdir)

CLEANFILES = adios_config adios_config.flags
