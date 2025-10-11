#!/usr/bin/env -S bats --report-formatter junit

bats_require_minimum_version 1.5.0

setup() {
	TMPDIR=$(mktemp -d)
	TEST_TEX_FILE="${TMPDIR}/test.tex"
	INCLUDED_FILE="${TMPDIR}/included.tex"
	PARSE_DOC_PERL="$(pwd)/docs/recipes/install/parse_doc.pl"
	PARSE_DOC_PYTHON="$(pwd)/docs/recipes/install/parse_doc.py"

	# Determine which implementation to test based on environment variable
	# Default to Perl if PARSE_DOC_TEST_IMPL is not set
	if [ "${PARSE_DOC_TEST_IMPL:-perl}" = "python" ]; then
		PARSE_DOC_SCRIPT="${PARSE_DOC_PYTHON}"
	else
		PARSE_DOC_SCRIPT="${PARSE_DOC_PERL}"
	fi
}

teardown() {
	rm -rf "${TMPDIR}"
}

@test "Test input handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\input{included.tex}
% begin_ohpc_run
[sms](*\\#*) echo "Main file command"
% end_ohpc_run
EOF

	cat >"${INCLUDED_FILE}" <<EOF
% begin_ohpc_run
[sms](*\\#*) echo "Included file command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "Main file command"'
	echo "${output}" | grep -q 'echo "Included file command"'
}

@test "Test HERE document handling" {
	echo -e "\\\\newcommand{\\\\install}{'install'}\n\\\\newcommand{\\\\chrootinstall}{'chrootinstall'}\n\\\\newcommand{\\\\groupinstall}{'groupinstall'}\n\\\\newcommand{\\\\groupchrootinstall}{'groupchrootinstall'}\n% begin_ohpc_run\n[sms](*\\\\#*) cat <<-EOF > /tmp/test.txt\nline 1\nline 2\nEOF\n% end_ohpc_run" >"${TEST_TEX_FILE}"

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -Fq "cat <<-EOF > /tmp/test.txt"
	echo "${output}" | grep -q "line 1"
	echo "${output}" | grep -q "line 2"
	echo "${output}" | grep -q "EOF"
}

@test "Test line continuation handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[sms](*\\#*) echo \\
"Hello, \\
World!"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q "echo \"Hello, \\\\"
}

@test "Test ARCH handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\newcommand{\\arch}{x86_64}
% begin_ohpc_run
[sms](*\\#*) echo "ARCH is ARCH"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "x86_64 is x86_64"'
}

@test "Test TAG handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\newcommand{\\OSTag}{4.0}
% begin_ohpc_run
[sms](*\\#*) echo "OpenHPC version TAG"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "OpenHPC version 4.0"'
}

@test "Test OSTREE handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\newcommand{\\OSTree}{EL_10}
% begin_ohpc_run
[sms](*\\#*) echo "Repository path OSTREE"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "Repository path EL_10"'
}

@test "Test BASEURL handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\newcommand{\\baseurl}{http://example.com/repo}
% begin_ohpc_run
[sms](*\\#*) echo "Base URL is BASEURL"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "Base URL is http://example.com/repo"'
}

@test "Test OSNAME handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\newcommand{\\OS}{Rocky Linux}
% begin_ohpc_run
[sms](*\\#*) echo "Operating system is OSNAME"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "Operating system is Rocky Linux"'
}

# Test command-line arguments
@test "Test --repo argument" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[sms](*\\#*) echo "test"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" --repo=testrepo "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "test"'
}

@test "Test --ci_run argument excludes CI commands by default" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_ci_comment This is a CI only command
[sms](*\\#*) echo "normal command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "normal command"'
	[ "$(echo "${output}" | grep -c 'This is a CI only command')" -eq 0 ]
}

@test "Test --ci_run argument includes CI commands when enabled" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_ci_comment This is a CI only command
[sms](*\\#*) echo "normal command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" --ci_run "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "normal command"'
	echo "${output}" | grep -q 'This is a CI only command'
}

@test "Test error handling for missing required macros" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
% Missing other required macros
% begin_ohpc_run
[sms](*\\#*) echo "test"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 1 ]
	echo "${output}" | grep -q "Error: package manager macros not defined"
}

@test "Test all macro substitutions" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{dnf -y install}
\\newcommand{\\chrootinstall}{dnf -y --installroot=\\\$CHROOT install}
\\newcommand{\\groupinstall}{dnf -y groupinstall}
\\newcommand{\\groupchrootinstall}{dnf -y --installroot=\\\$CHROOT groupinstall}
\\newcommand{\\remove}{dnf -y remove}
\\newcommand{\\addrepo}{dnf -y config-manager --add-repo}
\\newcommand{\\chrootaddrepo}{dnf --installroot=\\\$CHROOT config-manager --add-repo}
\\newcommand{\\beegfsrepo}{https://www.beegfs.io/release/beegfs\\_7.4.5/dists/beegfs-rhel9.repo}
\\newcommand{\\cudarepo}{https://developer.download.nvidia.com/compute/cuda/repos/rhel8/x86\\_64/cuda-rhel8.repo}
\\newcommand{\\confluentprofile}{confluent\\_os}
\\newcommand{\\baseos}{rocky-9}
\\newcommand{\\baseosshort}{rocky9}
\\newcommand{\\osimage}{rockylinux:9}
\\newcommand{\\OSTag}{el9}
\\newcommand{\\OSTree}{EL\\_9}
\\newcommand{\\arch}{x86\\_64}
\\newcommand{\\VERLONG}{4.0.0}
\\newcommand{\\installimage}{container-runtime}
\\newcommand{\\baseurl}{http://example.com/repo}
\\newcommand{\\OS}{Rocky Linux}
% begin_ohpc_run
[sms](*\\#*) (*\\install*) package
[sms](*\\#*) (*\\chrootinstall*) package
[sms](*\\#*) (*\\groupinstall*) "Development Tools"
[sms](*\\#*) (*\\groupchrootinstall*) "Development Tools"
[sms](*\\#*) (*\\remove*) package
[sms](*\\#*) (*\\addrepo*) http://example.com/repo.repo
[sms](*\\#*) (*\\chrootaddrepo*) http://example.com/repo.repo
[sms](*\\#*) (*\\beegfsrepo*)
[sms](*\\#*) (*\\cudarepo*)
[sms](*\\#*) (*\\confluentprofile*)
[sms](*\\#*) echo BOSVER BOSSHORT OSIMAGE TAG OSTREE ARCH VERLONG IMAGE BASEURL OSNAME
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'dnf -y install package'
	echo "${output}" | grep -q "dnf -y --installroot=\$CHROOT install package"
	echo "${output}" | grep -q 'dnf -y groupinstall "Development Tools"'
	echo "${output}" | grep -q "dnf -y --installroot=\$CHROOT groupinstall \"Development Tools\""
	echo "${output}" | grep -q 'dnf -y remove package'
	echo "${output}" | grep -q 'dnf -y config-manager --add-repo http://example.com/repo.repo'
	echo "${output}" | grep -q "dnf --installroot=\$CHROOT config-manager --add-repo http://example.com/repo.repo"
	echo "${output}" | grep -q 'https://www.beegfs.io/release/beegfs_7.4.5/dists/beegfs-rhel9.repo'
	echo "${output}" | grep -q 'https://developer.download.nvidia.com/compute/cuda/repos/rhel8/x86_64/cuda-rhel8.repo'
	echo "${output}" | grep -q 'confluent_os'
	echo "${output}" | grep -q 'echo rocky-9 rocky9 rockylinux:9 el9 EL_9 x86_64 4.0.0 container-runtime http://example.com/repo Rocky Linux'
}

@test "Test comment handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_comment_header Install Base Packages
% ohpc_validation_comment This is a validation comment
% ohpc_validation_newline
[sms](*\\#*) echo "test"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q '# Install Base Packages'
	echo "${output}" | grep -q '# This is a validation comment'
	echo "${output}" | grep -q 'echo "test"'
}

@test "Test indentation handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_indent 4
[sms](*\\#*) echo "indented command"
% ohpc_indent 0
[sms](*\\#*) echo "normal command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q '    echo "indented command"'
	echo "${output}" | grep -q '^echo "normal command"'
}

@test "Test ohpc_command directive" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_command echo "direct command"
[sms](*\\#*) echo "prompt command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "direct command"'
	echo "${output}" | grep -q 'echo "prompt command"'
}

@test "Test do loop handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[sms](*\\#*) for i in 1 2 3; do
   echo \$i
   # comment in loop
done
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'for i in 1 2 3; do'
	echo "${output}" | grep -q "   echo \$i"
	echo "${output}" | grep -q '   # comment in loop'
	echo "${output}" | grep -q '^done'
}

@test "Test postgres prompt handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[postgres]\$ CREATE DATABASE test;
[postgres]\$ \\q
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'CREATE DATABASE test;'
	echo "${output}" | grep -q '\\q'
}

@test "Test disabled lines with %%" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[sms](*\\#*) echo "enabled command"
%% [sms](*\\#*) echo "disabled command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "enabled command"'
	[ "$(echo "${output}" | grep -c 'echo "disabled command"')" -eq 0 ]
}

@test "Test CI-only command exclusion with % prefix" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[sms](*\\#*) echo "normal command"
% [sms](*\\#*) echo "ci only command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "normal command"'
	[ "$(echo "${output}" | grep -c 'echo "ci only command"')" -eq 0 ]
}

@test "Test CI-only command inclusion with --ci_run and % prefix" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
[sms](*\\#*) echo "normal command"
% [sms](*\\#*) echo "ci only command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "--ci_run" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "normal command"'
	echo "${output}" | grep -q 'echo "ci only command"'
}

@test "Test nested includes" {
	SECOND_INCLUDE="${TMPDIR}/second.tex"
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\input{included.tex}
% begin_ohpc_run
[sms](*\\#*) echo "Main file command"
% end_ohpc_run
EOF

	cat >"${INCLUDED_FILE}" <<EOF
\\input{second.tex}
% begin_ohpc_run
[sms](*\\#*) echo "First included file command"
% end_ohpc_run
EOF

	cat >"${SECOND_INCLUDE}" <<EOF
% begin_ohpc_run
[sms](*\\#*) echo "Second included file command"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "Main file command"'
	echo "${output}" | grep -q 'echo "First included file command"'
	echo "${output}" | grep -q 'echo "Second included file command"'
}

@test "Test LaTeX escape sequence handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\newcommand{\\arch}{x86\\_64}
\\newcommand{\\beegfsrepo}{beegfs\\_7.4.5}
\\newcommand{\\OSTree}{EL\\_9}
% begin_ohpc_run
[sms](*\\#*) echo "ARCH is ARCH"
[sms](*\\#*) echo "(*\\beegfsrepo*)"
[sms](*\\#*) echo "OSTREE"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q 'echo "x86_64 is x86_64"'
	echo "${output}" | grep -q 'echo "beegfs_7.4.5"'
	echo "${output}" | grep -q 'echo "EL_9"'
}

@test "Test missing input file handling" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
\\input{nonexistent.tex}
% begin_ohpc_run
[sms](*\\#*) echo "test"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -ne 0 ]
	echo "${output}" | grep -q "Cannot open file"
}

@test "Test usage message for missing arguments" {
	run "${PARSE_DOC_SCRIPT}"
	[ "${status}" -eq 1 ]
	echo "${output}" | grep -q "Usage:"
}

@test "Test section reference replacement with aux file" {
	AUX_FILE="${TMPDIR}/test.aux"
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_comment_header Install packages \\ref{sec:install_packages}
[sms](*\\#*) echo "test"
% end_ohpc_run
EOF

	cat >"${AUX_FILE}" <<EOF
\\relax
\\@writefile{toc}{\\contentsline {section}{\\numberline {1}Install packages}{1}}
\\newlabel{sec:install_packages}{{1}{1}}
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q "Install packages (Section 1)"
}

@test "Test section reference without aux file" {
	cat >"${TEST_TEX_FILE}" <<EOF
\\newcommand{\\install}{'install'}
\\newcommand{\\chrootinstall}{'chrootinstall'}
\\newcommand{\\groupinstall}{'groupinstall'}
\\newcommand{\\groupchrootinstall}{'groupchrootinstall'}
% begin_ohpc_run
% ohpc_comment_header Install packages \\ref{sec:install_packages}
[sms](*\\#*) echo "test"
% end_ohpc_run
EOF

	run "${PARSE_DOC_SCRIPT}" "${TEST_TEX_FILE}"
	[ "${status}" -eq 0 ]
	echo "${output}" | grep -q "Install packages"
	[ "$(echo "${output}" | grep -c "Section")" -eq 0 ]
}
