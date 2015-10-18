#! /bin/sh

cat > test.exp.1.tmp <<EOF
1 2 1
2 3 2
3 4 0
4 5 1
EOF

echo 1 2 2.5 4 | gsl-histogram 1 5 4 | tr -d '\r' > test.obs.1.tmp

cmp test.exp.1.tmp test.obs.1.tmp
STATUS=$?
rm test.exp.1.tmp test.obs.1.tmp

exit $STATUS
