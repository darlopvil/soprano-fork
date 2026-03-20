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
0. Install the dependencies
1. Clone the repository using `git clone --recurse-submodules https://git.vern.cc/cobra/Soprano`
2. Run `guile -L . soprano.scm`
3. Connect to http://localhost:8005 (or point your reverse proxy to it)
4. Profit

## Environment
`PATCHES_URL` - Link to any patches that were applied. Necessary if there are any. Do not set if there aren't.

The following are optional.

`PORT` - What port to run on (default `8005`).
