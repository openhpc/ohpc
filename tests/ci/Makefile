lint: codespell-lint flake8-lint whitespace-lint shellcheck-lint

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
		../../components/admin/prun/SOURCES/prun

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
	cd ../../; ! git --no-pager grep -E '\s+$$' \*.spec

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
		../../tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		../../tests/libs/netcdf/ohpc-tests/test_mpi_families
