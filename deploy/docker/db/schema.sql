

CREATE TABLE article (
  id            serial PRIMARY KEY,
  title         text         NOT NULL,
  time_created  timestamptz  NOT NULL,
  summary       text         NOT NULL,
  body          text         NOT NULL
);


CREATE TABLE image (
  id     serial PRIMARY KEY,
  image  bytea  NOT NULL
);

