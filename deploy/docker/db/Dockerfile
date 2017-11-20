
FROM postgres:10-alpine
MAINTAINER "jeff.scott.wise@gmail.com"



RUN mkdir -p /docker-entrypoint-initdb.d/


ENV POSTGRES_PASSWORD="HS$zAfDF!E@767"

COPY schema.sql /docker-entrypoint-initdb.d/schema.sql

