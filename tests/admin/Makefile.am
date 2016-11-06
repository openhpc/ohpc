TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS = init.sh

TESTS  += clustershell
TESTS  += conman
TESTS  += ganglia
TESTS  += genders
TESTS  += nagios
TESTS  += warewulf

if SLURM_ENABLED
TESTS  += mrsh
endif
