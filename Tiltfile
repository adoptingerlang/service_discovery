allow_k8s_contexts("microk8s")
default_registry('127.0.0.1:32000')

custom_build(
    'service_discovery_sql',
    'docker buildx build -o type=docker --target dev_sql --tag $EXPECTED_REF .',
    ['apps/service_discovery_postgres/priv/migrations'],
    entrypoint="cp /app/sql/* /flyway/sql"
)

custom_build(
    'service_discovery',
    'docker buildx build -o type=docker --target dev_release --tag $EXPECTED_REF .',
    ['.'],
    live_update=[
        sync('apps', '/app/src/apps'),
        run('rebar3 as tilt compile'),
        run('/app/_build/tilt/rel/service_discovery/bin/service_discovery restart')
    ],
    ignore=["rebar.lock", "apps/service_discovery_postgres/priv/migrations/"]
)

k8s_yaml(local('kustomize build deployment/overlays/dev'))

watch_file('deployment/')
