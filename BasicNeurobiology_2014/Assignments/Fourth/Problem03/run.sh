#!/usr/bin/env bash
# --embed adds main function .
cython --embed ./solution_nerst.pyx
gcc `python-config --cflags` `python-config --ldflags` ./solution_nerst.c -o answer && ./answer

