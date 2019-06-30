CREATE EXTENSION hstore;

CREATE TABLE named_ports
    (
        service_name TEXT,
        protocol TEXT,
        port_name TEXT,
        port INT2
    );

CREATE TABLE services
    (
        name TEXT NOT NULL,
        attributes HSTORE NOT NULL
    );

CREATE TABLE endpoints
    (
        service_name TEXT NOT NULL,
        ip INET NOT NULL,
        tags TEXT[] NOT NULL
   );
