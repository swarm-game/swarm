# Running in local development environment

The `client.sh` script can be run with either the `server-docker.sh` or the `server-native.sh` script as the host.

Running the server application natively is the simplest option and connects to the local Postgres database via a socket.

Running the server inside a local Docker image requires supplying the Postgres password as an environment variable.

## Database setup

One first needs to install a local Postgres server.

After configuring logins and users, one may populate the database using the stored `schema-local.sql` schema with a script:

    tournament/scripts/database/recreate-local-database.sh

### Configuring database access from Docker

See this answer: https://stackoverflow.com/a/58015643/105137

To summarize:

* Edit `postgresql.conf`, uncomment and set `listen_addresses = '*'`
* Edit `pg_hba.conf`, add the line:
    ```
    host   all              all             172.17.0.0/16           password
    ```
