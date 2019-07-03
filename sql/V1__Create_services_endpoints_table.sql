CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE services
    (
        id UUID NOT NULL DEFAULT gen_random_uuid(),
        name TEXT UNIQUE NOT NULL,
        attributes HSTORE NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (id)
    );

CREATE TABLE named_ports
    (
        service_id UUID NOT NULL REFERENCES services (id) ON DELETE CASCADE,
        port_name TEXT,
        protocol TEXT,
        port INT2,
        PRIMARY KEY (service_id, port_name)
    );


CREATE TABLE endpoints
    (
        service_id UUID NOT NULL REFERENCES services (id) ON DELETE CASCADE,
        ip INET NOT NULL,
        tags TEXT[] NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (service_id, ip)
    );

CREATE INDEX idx_service_attributes ON services USING GIN(attributes);
CREATE INDEX idx_endpoint_tags ON endpoints USING GIN(tags);
