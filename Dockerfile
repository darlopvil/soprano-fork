FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        guile-3.0 \
        guile-library \
        guile-gnutls \
        guile-json \
        ca-certificates \
        netbase \
        tini \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --create-home --shell /usr/sbin/nologin soprano

WORKDIR /app
COPY --chown=soprano:soprano soprano.scm ./
COPY --chown=soprano:soprano soprano ./soprano
COPY --chown=soprano:soprano static ./static

USER soprano

ENV PORT=8005
EXPOSE 8005

ENTRYPOINT ["tini", "--"]
CMD ["guile", "soprano.scm"]
