# Usage

## Installation prerequisites:

Install sqlite:
```
sudo apt install sqlite3
```

## Deployment

Run this script (requires Docker):
```
tournament/scripts/docker/build-static-binary.sh
```

# Testing

## Unit tests

```
scripts/test/run-tests.sh swarm:test:tournament-host
```