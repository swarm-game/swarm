#!/usr/bin/env bash

PORT="5357"
HOST="localhost"

function help {
  echo "Swarm remote REPL"
  echo
  echo "This is a shortcut for sending to Swarm REPL via cURL and recieving responses:"
  echo "curl -XGET --header \"Content-Type: text/plain;charset=utf-8\" --data \"build {}\" $HOST:$PORT/code/run"
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

IN_PROGRESS=""
function print_elipsis {
  echo -n "..."
  IN_PROGRESS="\r   \r"
}

function remove_elipsis {
  echo -ne "$IN_PROGRESS"
  IN_PROGRESS=""
}

function repl {
  while true; do
    remove_elipsis
    read -r -e -p "> " expr
    curl --silent -XGET --header "Content-Type: text/plain;charset=utf-8" --data "$expr" "$HOST:$PORT/code/run" | while read -r line ; do
      remove_elipsis
      if jq -e 'has("InProgress")' <<< "$line" > /dev/null; then
        print_elipsis
      elif jq -e 'has("Complete")' <<< "$line" > /dev/null; then
        RESULT="$(jq -r '.Complete' <<< "$line")"
        [ -n "$RESULT" ] && echo "$RESULT";
      elif jq -e 'has("Rejected")' <<< "$line" > /dev/null; then
        jq -C '.Rejected' <<< "$line"
      else
        echo "$line"
      fi
    done
  done
}

function main {
  parse_args "$@"
  repl
}

main "$@"