#!/bin/bash
set -e

git clone https://github.com/GeorgeKT/cobra-runtime.git
mkdir cobra-runtime/build
(cd cobra-runtime/build && cmake .. && make && sudo make install && sudo ldconfig)
