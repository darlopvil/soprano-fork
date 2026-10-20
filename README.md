# Soprano
Soprano is a privacy-respecting frontend to Tenor

This project is incomplete. The only supported endpoints are /view (gif) and /search (search).

# Support
Join our [Matrix room](https://mto.vern.cc/#/#cobra-frontends:vern.cc) for support and other things related to Soprano

# Instances
See instances.json

# Run your own instance
## Dependencies
This program is written in Guile Scheme.

You will need need `guile`, `guile-gnutls`, and `guile-lib`.

## Running
### Bare metal
0. Install the dependencies
1. Clone the repository using `git clone https://github.com/darlopvil/soprano-fork.git`
2. Run `guile soprano.scm` from the repository root
3. Connect to http://localhost:8005 (or point your reverse proxy to it)
4. Profit

### Docker
1. Clone the repository and `cd` into it
2. `cp docker-compose.yml.example docker-compose.yml` and adapt it to your setup
3. Run `docker compose up -d --build`

## Environment
`PATCHES_URL` - Link to any patches that were applied. Necessary if there are any. Do not set if there aren't.

The following are optional.

`PORT` - What port to run on (default `8005`).
