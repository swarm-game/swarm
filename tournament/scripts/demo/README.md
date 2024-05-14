# Running in local development environment

The `client.sh` script can be run with the `server-native.sh` script as the host.

Running the server application natively is the simplest option and connects to the local database file.

## Database setup

One may populate the database using the committed schema with a script:

    tournament/scripts/database/recreate-local-database.sh
