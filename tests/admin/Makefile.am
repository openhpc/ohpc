TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS = init.sh

TESTS  += clustershell
TESTS  += conman
TESTS  += ganglia
TESTS  += genders
TESTS  += nagios
TESTS  += nhc

if WAREWULF_ENABLED
TESTS  += warewulf
endif

if SLURM_ENABLED
TESTS  += mrsh
TESTS  += slurm-plugins
endif
