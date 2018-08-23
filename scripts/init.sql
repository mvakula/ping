CREATE TABLE IF NOT EXISTS endpoints (
  id serial PRIMARY KEY,
  url text UNIQUE,
  timestamp timestamp DEFAULT current_timestamp
);

CREATE TABLE IF NOT EXISTS pings (
  id serial PRIMARY KEY,
  endpoint_id integer REFERENCES endpoints ON DELETE CASCADE,
  latency smallint NOT NULL,
  status_code smallint NOT NULL,
  timestamp timestamp DEFAULT current_timestamp
);
