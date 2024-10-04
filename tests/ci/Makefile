lint: codespell-lint flake8-lint whitespace-lint shellcheck-lint shfmt-lint

codespell-lint:
	@echo "Running 'codespell' on all spec files"
	cd ../..; codespell $$(find components -name "*.spec") \
		README.md \
		CONTRIBUTING.md \
		../../tests/ci/check_spec.py \
		../../tests/ci/run_build.py \
		../../misc/build_order.py \
		../../misc/obs/obs_config.py \
		../../tests/ci/spec_to_test_mapping.py \
		../../components/admin/prun/SOURCES/prun \
		../../components/rms/slurm/SOURCES/slurm.epilog.clean \
		../../tests/user-env/mem_limits \
		../../tests/user-env/ompi_info \
		../../tests/perf-tools/dimemas/tests/test_module \
		../../tests/perf-tools/extrae/tests/test_module \
		../../tests/perf-tools/omb/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/test_module \
		../../tests/perf-tools/scorep/tests/instrumenter_test \
		../../tests/perf-tools/scorep/tests/rm_execution \
		../../tests/perf-tools/scorep/tests/test_module \
		../../tests/libs/superlu_dist/tests/rm_execution \
		../../tests/libs/superlu_dist/tests/test_module \
		../../tests/libs/plasma/tests/rm_execution \
		../../tests/libs/plasma/tests/test_module

flake8-lint:
	@echo "Running 'flake8' on selected Python files"
	flake8 \
		../../tests/ci/check_spec.py \
		../../tests/ci/run_build.py \
		../../misc/build_order.py \
		../../misc/obs/obs_config.py \
		../../tests/ci/spec_to_test_mapping.py

whitespace-lint:
	@echo "Checking spec files for trailing whitespaces"
	cd ../../; ! git --no-pager grep -E '\s+$$' \*.spec \
		components/admin/prun/SOURCES/prun \
		components/rms/slurm/SOURCES/slurm.epilog.clean \
		tests/user-env/mem_limits \
		tests/user-env/ompi_info \
		tests/perf-tools/dimemas/tests/test_module \
		tests/perf-tools/extrae/tests/test_module \
		tests/perf-tools/omb/tests/rm_execution \
		tests/perf-tools/scalasca/tests/rm_execution \
		tests/perf-tools/scalasca/tests/test_module \
		tests/perf-tools/scorep/tests/instrumenter_test \
		tests/perf-tools/scorep/tests/rm_execution \
		tests/perf-tools/scorep/tests/test_module \
		tests/libs/superlu_dist/tests/rm_execution \
		tests/libs/superlu_dist/tests/test_module \
		tests/libs/plasma/tests/rm_execution \
		tests/libs/plasma/tests/test_module \
		\*.tex

shellcheck-lint:
	@echo "Running 'shellcheck' on selected shell scripts"
	shellcheck \
		-o require-variable-braces,quote-safe-variables,deprecate-which,avoid-nullary-conditions \
		../../misc/build_srpm.sh \
		../../misc/build_order.sh \
		../../misc/get_source.sh \
		../../misc/shell-functions \
		../../tests/ci/prepare-ci-environment.sh \
		../../tests/ci/setup_slurm_and_run_tests.sh \
		../../components/OHPC_setup_compiler \
		../../components/OHPC_setup_mpi \
		../../components/admin/prun/SOURCES/prun \
		../../components/admin/docs/SOURCES/get_source.sh \
		../../components/admin/test-suite/SOURCES/get_source.sh \
		../../components/rms/slurm/SOURCES/slurm.epilog.clean \
		../../tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		../../tests/libs/netcdf/ohpc-tests/test_mpi_families
	shellcheck --exclude=SC2239,SC1091 \
		../../tests/apps/hpcg/run \
		../../tests/user-env/mem_limits \
		../../tests/user-env/ompi_info \
		../../tests/perf-tools/dimemas/tests/test_module \
		../../tests/perf-tools/extrae/tests/test_module \
		../../tests/perf-tools/omb/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/test_module \
		../../tests/libs/superlu_dist/tests/rm_execution \
		../../tests/libs/superlu_dist/tests/test_module \
		../../tests/libs/plasma/tests/rm_execution \
		../../tests/libs/plasma/tests/test_module
	shellcheck --exclude=SC1091,SC2030,SC2031 \
		../../tests/perf-tools/scorep/tests/instrumenter_test \
		../../tests/perf-tools/scorep/tests/rm_execution \
		../../tests/perf-tools/scorep/tests/test_module

shfmt-lint:
	@echo "Running 'shfmt' on selected shell scripts"
	shfmt \
		-w -d -ln bats \
		../../tests/apps/hpcg/run \
		../../tests/user-env/mem_limits \
		../../tests/user-env/ompi_info \
		../../tests/perf-tools/dimemas/tests/test_module \
		../../tests/perf-tools/extrae/tests/test_module \
		../../tests/perf-tools/omb/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/test_module \
		../../tests/perf-tools/scorep/tests/instrumenter_test \
		../../tests/perf-tools/scorep/tests/rm_execution \
		../../tests/perf-tools/scorep/tests/test_module \
		../../tests/libs/superlu_dist/tests/rm_execution \
		../../tests/libs/superlu_dist/tests/test_module \
		../../tests/libs/plasma/tests/rm_execution \
		../../tests/libs/plasma/tests/test_module
	shfmt -w -d \
		../../tests/ci/prepare-ci-environment.sh \
		../../components/rms/slurm/SOURCES/slurm.epilog.clean
