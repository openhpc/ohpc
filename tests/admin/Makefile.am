TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS = init.sh

if CLUSTERSHELL_ENABLED
TESTS  += clustershell
endif

if CONMAN_ENABLED
TESTS  += conman
endif

if GENDERS_ENABLED
TESTS  += genders
endif

TESTS  += nhc

if WAREWULF_ENABLED
TESTS  += warewulf
endif

if SLURM_ENABLED
TESTS  += slurm-plugins
endif