-- :select_service
SELECT
    name,
    attributes
FROM services
WHERE name = $1

-- :insert_service
INSERT INTO services (name, attributes) VALUES ($1, $2)

-- :select_endpoints
SELECT
    ip,
    port,
    tags
FROM endpoints
WHERE service_name = $1
