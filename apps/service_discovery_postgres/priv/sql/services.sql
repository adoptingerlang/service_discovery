-- :select_service
SELECT
    services.name,
    services.attributes,
    COALESCE(array_agg(named_ports) FILTER (WHERE named_ports IS NOT NULL), '{}')
FROM services
LEFT OUTER JOIN named_ports ON named_ports.service_name = services.name
WHERE services.name = $1
GROUP BY services.name, services.attributes


-- :select_all_services
SELECT
    name,
    attributes
FROM services

-- :insert_service
INSERT INTO services (name, attributes) VALUES ($1, $2)

-- :insert_endpoint
INSERT INTO endpoints (service_name, ip, port, port_name, tags) VALUES ($1, $2, $3, $4, $5)

-- :select_endpoints
SELECT
    ip,
    port,
    port_name,
    tags
FROM endpoints
WHERE service_name = $1
