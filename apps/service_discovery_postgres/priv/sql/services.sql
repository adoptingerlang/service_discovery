-- :select_service
SELECT
    name,
    attributes
FROM services
WHERE name = $1

-- :select_all_services
SELECT
    name,
    attributes
FROM services

-- :insert_service
INSERT INTO services (name, attributes) VALUES ($1, $2)

-- :insert_endpoint
INSERT INTO endpoints (service_name, ip, port, tags) VALUES ($1, $2, $3, $4)

-- :select_endpoints
SELECT
    ip,
    port,
    tags
FROM endpoints
WHERE service_name = $1
