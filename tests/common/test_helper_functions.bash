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

###assert_success() {
###    if [ "$status" -ne 0 ]; then
###	flunk "command failed with exit status $status"
###    elif [ "$#" -gt 0 ]; then
###	assert_output "$1"
###    fi
###}

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

assert_success() {
  if (( status != 0 )); then
    { local -ir width=6
      batslib_print_kv_single "$width" 'status' "$status"
      batslib_print_kv_single_or_multi "$width" 'output' "$output"
    } | batslib_decorate 'command failed' \
      | fail
  fi
}

batslib_decorate() {
  echo
  echo "-- $1 --"
  cat -
  echo '--'
  echo
}

batslib_print_kv_single() {
  local -ir col_width="$1"; shift
  while (( $# != 0 )); do
    printf '%-*s : %s\n' "$col_width" "$1" "$2"
    shift 2
  done
}

fail() {
  (( $# == 0 )) && batslib_err || batslib_err "$@"
  return 1
}


batslib_err() {
  { if (( $# > 0 )); then
      echo "$@"
    else
      cat -
    fi
  } >&2
}

batslib_print_kv_single_or_multi() {
  local -ir width="$1"; shift
  local -a pairs=( "$@" )

  local -a values=()
  local -i i
  for (( i=1; i < ${#pairs[@]}; i+=2 )); do
    values+=( "${pairs[$i]}" )
  done

  if batslib_is_single_line "${values[@]}"; then
    batslib_print_kv_single "$width" "${pairs[@]}"
  else
    local -i i
    for (( i=1; i < ${#pairs[@]}; i+=2 )); do
      pairs[$i]="$( batslib_prefix < <(printf '%s' "${pairs[$i]}") )"
    done
    batslib_print_kv_multi "${pairs[@]}"
  fi
}

batslib_print_kv_multi() {
  while (( $# != 0 )); do
    printf '%s (%d lines):\n' "$1" "$( batslib_count_lines "$2" )"
    printf '%s\n' "$2"
    shift 2
  done
}


batslib_is_single_line() {
  for string in "$@"; do
    (( $(batslib_count_lines "$string") > 1 )) && return 1
  done
  return 0
}

batslib_count_lines() {
  local -i n_lines=0
  local line
  while IFS='' read -r line || [[ -n $line ]]; do
    (( ++n_lines ))
  done < <(printf '%s' "$1")
  echo "$n_lines"
}

batslib_prefix() {
  local -r prefix="${1:-  }"
  local line
  while IFS='' read -r line || [[ -n $line ]]; do
    printf '%s%s\n' "$prefix" "$line"
  done
}

