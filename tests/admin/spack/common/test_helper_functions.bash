# Test convenience functions for use with Bats Automated Testing System
# 
# routines modified from:
# https://github.com/sstephenson/rbenv/blob/master/test/test_helper.bash

flunk() {
    { 
	if [ "$#" -eq 0 ]; then 
	    cat -
        else
	    echo "$@"
     	fi
    } | sed "s:LOCAL_TEST_DIR:TEST_DIR:g" >&2
    return 1
}

assert_success() {
    if [ "$status" -ne 0 ]; then
	flunk "command failed with exit status $status"
    elif [ "$#" -gt 0 ]; then
	assert_output "$1"
    fi
}

assert_failure() {
  if [ "$status" -eq 0 ]; then
    flunk "expected failed exit status"
  elif [ "$#" -gt 0 ]; then
    assert_output "$1"
  fi
}

assert_equal() {
    if [ "$1" != "$2" ]; then
	{ 
	    echo "output expected: $1"
	    echo "output observed: $2"
	} | flunk 
    fi
}

assert_output() {
    local expected
    if [ $# -eq 0 ]; then 
	expected="$(cat -)"
    else 
	expected="$1"
    fi
    assert_equal "$expected" "$output"
}
