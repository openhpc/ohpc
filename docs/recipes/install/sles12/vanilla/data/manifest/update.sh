#!/bin/bash

./listfsp
./listpatterns
./listchanges

./build_tables.pl
./build_patterns.pl
./build_changelog.pl
