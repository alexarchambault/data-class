#!/usr/bin/env bash
set -e

project/cs-setup.sh
./cs install sbt-launcher:1.2.22 scalafmt:2.7.3
rm -f cs
