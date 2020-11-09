#!/usr/bin/env bash
set -e

.github/scripts/cs-setup.sh

mkdir -p bin
export PATH="$(pwd)/bin:$PATH"
echo "$(pwd)/bin" >> "$GITHUB_PATH"

eval "$(cs java --jvm "${JDK:-8}" --env)"
echo "JAVA_HOME=$JAVA_HOME" >> "$GITHUB_ENV"
echo "$JAVA_HOME/bin" >> "$GITHUB_PATH"

./cs install --install-dir bin \
  sbt-launcher:1.2.22 \
  scalafmt:2.7.5

rm -f cs
