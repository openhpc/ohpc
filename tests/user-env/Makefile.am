TESTS = init.sh

TESTS  += mem_limits
TESTS  += pdsh
TESTS  += ompi_info

if SLURM_ENABLED
TESTS  += munge
TESTS  += sacct
TESTS  += sinfo
endif

if PBS_ENABLED
TESTS  += pbsaccounting
endif
