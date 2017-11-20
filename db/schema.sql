

CREATE TABLE article (
  id      serial PRIMARY KEY,
  title   text   NOT NULL,
  date    date   NOT NULL,
  body    text   NOT NULL
);


CREATE TABLE image (
  id     serial PRIMARY KEY,
  image  bytea  NOT NULL
);

