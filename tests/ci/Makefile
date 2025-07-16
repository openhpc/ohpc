lint: codespell-lint flake8-lint whitespace-lint shellcheck-lint shfmt-lint clang-format-lint

codespell-lint:
	@echo "Running 'codespell' on all spec files"
	cd ../..; codespell $$(find components -name "*.spec") \
		$$(find docs/recipes) \
		README.md \
		CONTRIBUTING.md \
		containers \
		tests/ci/check_spec.py \
		tests/ci/run_build.py \
		misc/build_order.py \
		misc/obs/obs_config.py \
		tests/ci/spec_to_test_mapping.py \
		tests/ci/test_parse_doc.bats \
		components/admin/prun/SOURCES/prun \
		components/rms/slurm/SOURCES/slurm.epilog.clean \
		tests/common/functions \
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
		tests/perf-tools/omb/tests/rm_execution \
		tests/perf-tools/omb/tests/test_module \
		tests/perf-tools/omb/ohpc-tests/test_mpi_families \
		tests/libs/hdf5/ohpc-tests/test_compiler_families \
		tests/libs/superlu_dist/tests/rm_execution \
		tests/libs/superlu_dist/tests/test_module \
		tests/libs/netcdf/ohpc-tests/test_mpi_families \
		tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		tests/libs/netcdf/tests-parallel/test_pnetcdf \
		tests/libs/netcdf/tests/test_C_module \
		tests/libs/netcdf/tests/test_CXX_module \
		tests/libs/netcdf/tests/test_Fortran_module \
		tests/libs/netcdf/tests/test_netcdf \
		tests/libs/scalapack/ohpc-tests/test_mpi_families \
		tests/libs/scalapack/tests/rm_execution \
		tests/libs/scalapack/tests/test_module \
		tests/libs/ptscotch/tests/rm_execution \
		tests/libs/ptscotch/tests/test_module \
		tests/libs/boost-mpi/tests/mpi/test/rm_execution \
		tests/libs/plasma/tests/rm_execution \
		tests/libs/plasma/tests/test_module

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
		tests/ci/test_parse_doc.bats \
		components/admin/prun/SOURCES/prun \
		components/compiler-families/intel-compilers-devel/SOURCES/ohpc-update-modules-intel \
		components/mpi-families/impi-devel/SOURCES/ohpc-update-modules-impi \
		components/rms/slurm/SOURCES/slurm.epilog.clean \
		tests/user-env/mem_limits \
		tests/common/functions \
		tests/user-env/ompi_info \
		tests/perf-tools/dimemas/tests/test_module \
		tests/perf-tools/extrae/tests/test_module \
		tests/perf-tools/omb/tests/rm_execution \
		tests/perf-tools/scalasca/tests/rm_execution \
		tests/perf-tools/scalasca/tests/test_module \
		tests/perf-tools/scorep/tests/instrumenter_test \
		tests/perf-tools/scorep/tests/rm_execution \
		tests/perf-tools/scorep/tests/test_module \
		tests/perf-tools/omb/tests/rm_execution \
		tests/perf-tools/omb/tests/test_module \
		tests/perf-tools/omb/ohpc-tests/test_mpi_families \
		tests/libs/superlu_dist/tests/rm_execution \
		tests/libs/superlu_dist/tests/test_module \
		tests/libs/hdf5/ohpc-tests/test_compiler_families \
		tests/libs/phdf5/ohpc-tests/test_mpi_families \
		tests/libs/netcdf/ohpc-tests/test_mpi_families \
		tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		tests/libs/netcdf/tests/test_C_module \
		tests/libs/netcdf/tests/test_CXX_module \
		tests/libs/netcdf/tests/test_Fortran_module \
		tests/libs/netcdf/tests/test_netcdf \
		tests/libs/netcdf/tests-parallel/test_pnetcdf \
		tests/libs/scalapack/ohpc-tests/test_mpi_families \
		tests/libs/scalapack/tests/rm_execution \
		tests/libs/scalapack/tests/test_module \
		tests/libs/phdf5/tests/rm_execution \
		tests/libs/phdf5/tests/test_module \
		tests/libs/ptscotch/tests/rm_execution \
		tests/libs/ptscotch/tests/test_module \
		tests/libs/boost-mpi/tests/mpi/test/rm_execution \
		tests/libs/plasma/tests/* \
		containers/* \
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
		../../tests/ci/test_parse_doc.bats \
		../../tests/common/functions \
		../../components/OHPC_setup_compiler \
		../../components/OHPC_setup_mpi \
		../../components/admin/prun/SOURCES/prun \
		../../components/admin/docs/SOURCES/get_source.sh \
		../../components/admin/test-suite/SOURCES/get_source.sh \
		../../components/compiler-families/intel-compilers-devel/SOURCES/ohpc-update-modules-intel \
		../../components/mpi-families/impi-devel/SOURCES/ohpc-update-modules-impi \
		../../components/rms/slurm/SOURCES/slurm.epilog.clean \
		../../tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		../../tests/libs/hdf5/ohpc-tests/test_compiler_families \
		../../tests/libs/phdf5/ohpc-tests/test_mpi_families \
		../../tests/libs/netcdf/ohpc-tests/test_mpi_families \
		../../containers/*.sh \
		../../containers/examples/mpi/*.sh
	shellcheck --exclude=SC2239,SC1091 \
		../../tests/apps/hpcg/run \
		../../tests/dev-tools/cuda/cuda \
		../../tests/dev-tools/cmake/test_cmake \
		../../tests/user-env/mem_limits \
		../../tests/user-env/ompi_info \
		../../tests/perf-tools/dimemas/tests/test_module \
		../../tests/perf-tools/extrae/tests/test_module \
		../../tests/perf-tools/omb/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/rm_execution \
		../../tests/perf-tools/scalasca/tests/test_module \
		../../tests/perf-tools/omb/tests/rm_execution \
		../../tests/perf-tools/omb/tests/test_module \
		../../tests/perf-tools/omb/ohpc-tests/test_mpi_families \
		../../tests/libs/phdf5/tests/rm_execution \
		../../tests/libs/phdf5/tests/test_module \
		../../tests/libs/netcdf/tests-parallel/test_pnetcdf \
		../../tests/libs/netcdf/tests/test_C_module \
		../../tests/libs/netcdf/tests/test_CXX_module \
		../../tests/libs/netcdf/tests/test_Fortran_module \
		../../tests/libs/netcdf/tests/test_netcdf \
		../../tests/libs/superlu_dist/tests/rm_execution \
		../../tests/libs/superlu_dist/tests/test_module \
		../../tests/libs/scalapack/ohpc-tests/test_mpi_families \
		../../tests/libs/scalapack/tests/rm_execution \
		../../tests/libs/scalapack/tests/test_module \
		../../tests/libs/ptscotch/tests/rm_execution \
		../../tests/libs/ptscotch/tests/test_module \
		../../tests/libs/boost-mpi/tests/mpi/test/rm_execution \
		../../tests/libs/plasma/tests/rm_execution \
		../../tests/libs/plasma/tests/test_module \
		../../tests/perf-tools/scorep/tests/instrumenter_test \
		../../tests/perf-tools/scorep/tests/rm_execution \
		../../tests/perf-tools/scorep/tests/test_module

shfmt-lint:
	@echo "Running 'shfmt' on selected shell scripts"
	shfmt \
		-w -d -ln bats \
		../../tests/ci/test_parse_doc.bats \
		../../tests/apps/hpcg/run \
		../../tests/dev-tools/cuda/cuda \
		../../tests/dev-tools/cmake/test_cmake \
		../../tests/dev-tools/cmake/run \
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
		../../tests/perf-tools/omb/tests/rm_execution \
		../../tests/perf-tools/omb/tests/test_module \
		../../tests/perf-tools/omb/ohpc-tests/test_mpi_families \
		../../tests/libs/superlu_dist/tests/rm_execution \
		../../tests/libs/superlu_dist/tests/test_module \
		../../tests/libs/phdf5/tests/rm_execution \
		../../tests/libs/phdf5/tests/test_module \
		../../tests/libs/netcdf/ohpc-tests/test_mpi_families \
		../../tests/libs/netcdf/ohpc-tests/netcdf_all_test_mpi_families \
		../../tests/libs/netcdf/tests-parallel/test_pnetcdf \
		../../tests/libs/netcdf/tests/test_C_module \
		../../tests/libs/netcdf/tests/test_CXX_module \
		../../tests/libs/netcdf/tests/test_Fortran_module \
		../../tests/libs/netcdf/tests/test_netcdf \
		../../tests/libs/scalapack/ohpc-tests/test_mpi_families \
		../../tests/libs/scalapack/tests/rm_execution \
		../../tests/libs/scalapack/tests/test_module \
		../../tests/libs/ptscotch/tests/rm_execution \
		../../tests/libs/ptscotch/tests/test_module \
		../../tests/libs/boost-mpi/tests/mpi/test/rm_execution \
		../../tests/libs/plasma/tests/rm_execution \
		../../tests/libs/plasma/tests/test_module
	shfmt -w -d \
		../../tests/ci/prepare-ci-environment.sh \
		../../tests/common/functions \
		../../tests/libs/hdf5/ohpc-tests/test_compiler_families \
		../../tests/libs/phdf5/ohpc-tests/test_mpi_families \
		../../components/compiler-families/intel-compilers-devel/SOURCES/ohpc-update-modules-intel \
		../../components/mpi-families/impi-devel/SOURCES/ohpc-update-modules-impi \
		../../components/rms/slurm/SOURCES/slurm.epilog.clean \
		../../containers/*.sh \
		../../containers/examples/mpi/*.sh

clang-format-lint:
	@echo "Running 'clang-format' on selected source code files"
	clang-format --dry-run --Werror \
		../libs/boost-mpi/tests/mpi/test/*.cpp \
		../libs/boost-mpi/tests/mpi/test/*.hpp \
		../libs/netcdf/tests/*.cpp \
		../libs/netcdf/tests/*.c \
		../libs/ptscotch/tests/*.c \
		../libs/netcdf/tests/*.h \
		../libs/netcdf/tests-parallel/*.c \
		../libs/netcdf/tests-parallel/*.h \
		../libs/phdf5/tests/C_mpi_test.c \
		../dev-tools/cuda/add.cu \
		../../tests/libs/plasma/tests/*.c \
		../../tests/libs/plasma/tests/*.h \
		../../containers/examples/mpi/mpi.c
