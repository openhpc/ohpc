lint: codespell-lint flake8-lint whitespace-lint

codespell-lint:
	@echo "Running 'codespell' on all spec files"
	cd ../..; codespell $$(find components -name "*.spec")

flake8-lint:
	@echo "Running 'flake8' on selected Python files"
	flake8 ../../tests/ci/check_spec.py ../../tests/ci/run_build.py ../../misc/build_order.py

whitespace-lint:
	@echo "Checking spec files for trailing whitespaces"
	cd ../../; git --no-pager grep -E '\s+$$' \*.spec

shellcheck-lint:
	@echo "Running 'shellcheck' on selected shell scripts"
	shellcheck ../../misc/build_srpm.sh
