#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

DEFAULT_COOKIE_JAR_PATH=/tmp/cookies.txt
COOKIE_JAR_PATH=${1:-$DEFAULT_COOKIE_JAR_PATH}

PORT=8080
BASE_API_URL=http://localhost:$PORT

LOGIN_URL=$BASE_API_URL/api/private/login/local

curl --silent --include --cookie-jar $COOKIE_JAR_PATH $LOGIN_URL
