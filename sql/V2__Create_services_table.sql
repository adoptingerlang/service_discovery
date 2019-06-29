CREATE EXTENSION hstore;

CREATE TABLE services
    (
        name TEXT NOT NULL,
        attributes HSTORE NOT NULL
    );

CREATE TABLE endpoints
    (
        service_name TEXT NOT NULL,
        ip INET NOT NULL,
        port INT2 NOT NULL,
        tags TEXT[] NOT NULL
   );
