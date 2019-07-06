-- :select_service
SELECT
    services.name,
    services.attributes,
    COALESCE(array_agg(named_ports) FILTER (WHERE named_ports IS NOT NULL), '{}'),
    COALESCE(array_agg(endpoints) FILTER (WHERE endpoints IS NOT NULL), '{}')
FROM services
LEFT OUTER JOIN named_ports ON named_ports.service_id = services.id
LEFT OUTER JOIN endpoints ON endpoints.service_id = services.id
WHERE services.name = $1
GROUP BY services.name, services.attributes


-- :select_all_services
SELECT
    name,
    attributes
FROM services

-- :insert_service
INSERT INTO services (name, attributes) VALUES (lower($1), $2) RETURNING (id)

-- :insert_endpoint
INSERT INTO endpoints (service_id, ip, tags)
VALUES ((SELECT id FROM services WHERE name = $1), $2, $3)

-- :insert_named_ports
INSERT INTO named_ports (service_id, port_name, protocol, port)
SELECT (SELECT id FROM services WHERE name = $1), lower(p.port_name), lower(p.protocol), p.port
FROM UNNEST($2::named_ports[]) AS p(service_id, port_name, protocol, port)

-- :select_endpoints
SELECT
    ip,
    tags
FROM endpoints
JOIN services ON services.id = endpoints.service_id
WHERE services.name = $1
