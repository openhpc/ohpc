lint: codespell-lint ruff-check-lint ruff-format-lint whitespace-lint shellcheck-lint

codespell-lint:
	@echo "Running 'codespell' on all spec files"
	cd ../..; codespell $$(find components -name "*.spec") \
		README.md \
		CONTRIBUTING.md \
		../../tests/ci/check_spec.py \
		../../tests/ci/run_build.py \
		../../misc/build_order.py \
		../../misc/obs/obs_config.py \
		../../tests/ci/spec_to_test_mapping.py


ruff-check-lint:
	@echo "Running 'ruff check' on selected Python files"
	ruff check \
		../../tests/ci/check_spec.py \
		../../tests/ci/run_build.py \
		../../misc/build_order.py \
		../../misc/check_for_package_updates.py \
		../../tests/ci/spec_to_test_mapping.py

ruff-format-lint:
	@echo "Running 'ruff format' on selected Python files"
	ruff format \
		../../tests/ci/check_spec.py \
		../../tests/ci/run_build.py \
		../../misc/build_order.py \
		../../misc/check_for_package_updates.py \
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
		../../tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		../../tests/libs/netcdf/ohpc-tests/test_mpi_families
