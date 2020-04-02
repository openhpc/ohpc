TESTS_ENVIRONMENT       = BATS_NO_SUMMARY=1

check_PROGRAMS          = tutorial_0
tutorial_0_SOURCES      = tutorial_0.c
tutorial_0_CPPFLAGS     = -I$(GEOPM_INC)
tutorial_0_LDFLAGS      = -L$(GEOPM_LIB) -lgeopm

TESTS                   = test_module \
                          rm_execution \
                          # end
