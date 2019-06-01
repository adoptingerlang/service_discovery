CREATE TABLE zones (
        name TEXT NOT NULL,
        version TEXT NOT NULL,
        authority JSONB[] NOT NULL,
        records JSONB[] NOT NULL,
        keysets JSONB[] NOT NULL);
