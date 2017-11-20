
FROM alpine:latest

RUN apk update && apk add libffi gmp libpq

RUN mkdir -p /site/ &&                                 \
    mkdir -p /site/bin &&                              \
    mkdir -p /site/static

COPY run /site/bin/

COPY css /site/static/css
COPY svg /site/static/svg
COPY js /site/static/js
COPY fonts /site/static/fonts

ENV POSTGRES_PASSWORD_TEST="HS$zAfDF!E@767"

ENTRYPOINT ["/site/bin/run"]
