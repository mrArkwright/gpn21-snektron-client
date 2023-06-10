#!/bin/sh

DOCKER_CONTEXT=$(cat docker-context)

read_file_if_exists() {
  if [ -f "$1" ]; then
    cat "$1"
  fi
}

task() {
  TASK_NAME=$1
  echo "- $TASK_NAME -"
  shift # drop $1 so $@ contains the command to be executed
  set -x
  "$@" || { { set +x; } 2> /dev/null; error "$TASK_NAME failed"; }
  { set +x; } 2> /dev/null
  echo
}

error() {
  echo
  echo error: "$1"
  exit 1
}

go() {
  case $1 in
    deploy)
      deploy
      ;;
    run)
      run
      ;;
    build)
      build
      ;;
    docker-with-context)
      shift # drop $1 so $@ contains the command to be executed
      docker_with_context "$@"
      ;;
    "")
      print_usage
      exit 1
      ;;
    *)
      echo invalid command: "$1"
      print_usage
      exit 1
      ;;
  esac
}

deploy() {
  build
  task "deploy" docker --context $DOCKER_CONTEXT compose up --build -d
}

build() {
  task "build" sbt assembly
}

docker_with_context() {
  docker --context $DOCKER_CONTEXT "$@"
}

print_usage() {
  echo usage: "$0" \<command\>
}

go "$@"
