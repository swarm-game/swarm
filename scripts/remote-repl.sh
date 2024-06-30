#!/usr/bin/env bash

PORT="5357"
HOST="localhost"

function help {
  echo "Swarm remote REPL"
  echo
  echo "This is a shortcut for sending (TBD: and recieving) to Swarm REPL via cURL:"
  echo "curl --header \"Content-Type: text/plain;charset=utf-8\" --data \"build {}\" $HOST:$PORT/code/run"
  echo
  echo "Options:"
  echo " -p PORT  --port PORT        Specify the port, default is $PORT."
  echo " -n NAME  --hostaname NAME   Specify the hostaname, default is $HOST."
  echo " -h       --help             Show this helpful text."
}

function parse_args {
  while [[ $# -gt 0 ]]; do
    case $1 in
      -p|--port)
        EXTENSION="$2"
        shift # past argument
        shift # past value
        ;;
      -n|--hostname)
        HOST="$2"
        shift # past argument
        shift # past value
        ;;
      -h|--help)
        help
        exit 0
        ;;
      *)
        echo "Unknown argument $1"
        shift # past argument
        ;;
    esac
  done
}

function repl {
  while true; do
    read -p "> " expr
    curl --header "Content-Type: text/plain;charset=utf-8" --data "$expr" $HOST:$PORT/code/run
  done
}

function main {
  parse_args
  repl
}

main