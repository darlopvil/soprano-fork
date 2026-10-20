# Soprano

A privacy-respecting frontend to [Tenor](https://tenor.com): browse and search GIFs without your browser ever talking to Tenor or Google, and without JavaScript.

This is a personal fork of [Soprano](https://codeberg.org/cobra/Soprano) by Skylar Astaroth. It is public to keep the code in sync and to leave a record of the changes; there is no list of public instances and no support channel. Like the original, it is licensed under the GNU AGPL v3 or later.

## What it does

- **GIF pages** (`/view/...`): the GIF as a light animated WebP, its title, tags, uploader, a link back to Tenor, download links (GIF, MP4, WebM) and related GIFs.
- **Search** (`/search/<terms>-gifs`, or the search box): a grid of results, with a "Next page" link past Tenor's first 50.
- **Locales**: Tenor's locale prefixes work too (`/es/search/...`, `/es-es/view/...`), and searching from a localized page stays in that locale.

Replacing `tenor.com` with the instance's domain in any Tenor link opens the same page.

Every image goes through the instance's own media proxy, which only fetches from Tenor's media hosts. Pages carry no scripts, no cookies and no third-party requests. `robots.txt` keeps crawlers out, since every page they fetched would become a request to Tenor.

## Changes from the original

- Reads the JSON data Tenor embeds in its pages instead of walking their DOM, which broke whenever Tenor changed its layout.
- Thumbnails and GIFs are animated WebP images: much lighter than GIF, and never videos, so they play in browsers that block autoplay without asking anyone to change their settings.
- GIF pages show tags, credits, related GIFs and download links.
- Search results have "Next page" links (see below).
- Rendered pages are cached in memory (GIF pages for an hour, searches for ten minutes), so reloads and repeated visits do not reach Tenor.
- One slow answer from Tenor no longer freezes the instance: requests are served in parallel, with a 5 s connect timeout and a 15 s deadline for every request to Tenor. Clients that disconnect mid-response no longer take the server down.
- Failures become proper error pages (400, 404, 502, 503, 504) instead of blank 500s, and unknown routes answer 404.
- Pages render in standards mode, with a layout that fits phones, alt texts and image sizes.
- The media proxy encodes the URLs it is given and follows redirects only within Tenor's media hosts.
- The Docker image is built from local source, runs as an unprivileged user and stops cleanly.

The commit history explains each change in detail.

## About "Next page"

A Tenor search page only holds its first 50 results. Tenor's public API is discontinued: it answers 403 to everyone except Tenor's own web client, which tenor.com uses to load more results as you scroll. Soprano reads that client's key from the config Tenor's pages publish and asks for the next page the same way tenor.com does, one request per click.

This may stop working at any time. If a call fails, "Next page" disappears for an hour and everything else keeps working.

## Running

### Dependencies

Guile 3.0 with `guile-gnutls`, `guile-lib` and `guile-json`. On Debian or Ubuntu:

```
sudo apt install guile-3.0 guile-library guile-gnutls guile-json
```

### Bare metal

1. Install the dependencies.
2. `git clone https://github.com/darlopvil/soprano-fork.git && cd soprano-fork`
3. `guile soprano.scm`
4. Open http://localhost:8005, or point your reverse proxy at it.

### Docker

1. Clone the repository and `cd` into it.
2. `cp docker-compose.yml.example docker-compose.yml` and adapt it to your setup. `docker-compose.yml` is ignored by git, so your setup stays out of the repository.
3. `docker compose up -d --build`

Run `docker compose up -d --build` again after pulling changes.

### Environment

| Variable | Default | Purpose |
| --- | --- | --- |
| `PORT` | `8005` | Port to listen on. |
| `SOURCE_URL` | this repository | Where the "Source Code" link in the footer points. If you run a modified copy, the AGPL requires you to offer its source to your users, so point this at it. |
| `PATCHES_URL` | unset | Link to patches applied on top of the source, shown in the footer. Set it only if there are any. |

## License

Soprano was written by Skylar Astaroth. This fork keeps the original copyright notices and adds its own to the files it changes.

Soprano is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. See [LICENSE](LICENSE).
