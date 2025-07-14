#!/usr/bin/env -S bats --report-formatter junit

setup() {
	TMPDIR=$(mktemp -d)
	TEST_TEX_FILE="${TMPDIR}/test.tex"
	INCLUDED_FILE="${TMPDIR}/included.tex"
	PARSE_DOC_SCRIPT="$(pwd)/docs/recipes/install/parse_doc.pl"
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
