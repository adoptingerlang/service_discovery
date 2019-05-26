-- :select_zones
SELECT
    name,
    version,
    authority,
    records,
    keysets
FROM zones
WHERE name = $1

-- :insert_zone
INSERT INTO zones (name, version, authority, records, keysets) VALUES ($1, $2, $3, $4, $5)
