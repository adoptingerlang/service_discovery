CREATE EXTENSION hstore;

CREATE TABLE named_ports
    (
        service_name TEXT,
        protocol TEXT,
        port_name TEXT,
        port INT2,
        PRIMARY KEY (service_name, port_name)
    );

CREATE TABLE services
    (
        name TEXT NOT NULL,
        attributes HSTORE NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (name)
    );

CREATE TABLE endpoints
    (
        service_name TEXT NOT NULL,
        ip INET NOT NULL,
        tags TEXT[] NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (service_name, ip)
    );

CREATE INDEX idx_service_attributes ON services USING GIN(attributes);
CREATE INDEX idx_endpoint_tags ON endpoints USING GIN(tags);
