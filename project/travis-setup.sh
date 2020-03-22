#!/usr/bin/env bash
set -e

CS_BIN_PATH="$HOME/.local/share/coursier/bin"
CS_VERSION="2.0.0-RC3-4"

mkdir -p "$CS_BIN_PATH"

if [[ ! -x "$CS_BIN_PATH/cs-$CS_VERSION" ]]; then
  curl -Lo "$CS_BIN_PATH/cs-$CS_VERSION" "https://github.com/coursier/coursier/releases/download/v$CS_VERSION/cs-x86_64-pc-linux"
  chmod +x "$CS_BIN_PATH/cs-$CS_VERSION"
  cp -a "$CS_BIN_PATH/cs-$CS_VERSION" "$CS_BIN_PATH/cs"
fi

export COURSIER_EXPERIMENTAL=1

cs install sbt-launcher:1.2.17
cs install scalafmt:2.4.2
