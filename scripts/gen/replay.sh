#!/usr/bin/env bash

PORT="5357"
HOST="localhost"

function help {
  echo "Save Swarm REPL history for replay"
  echo
  echo "This is a shortcut for saving Swarm REPL via cURL as JSON:"
  echo "curl --silent $HOST:$PORT/repl/history/current"
  echo
  echo "You probably want to pipe it to a file, like:"
  echo "$0 > history.json"
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
        PORT="$2"
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

function replay {
  curl -s "$HOST:$PORT/repl/history/current"
}

function main {
  parse_args "$@"
  replay
}

main "$@"