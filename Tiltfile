allow_k8s_contexts("microk8s")
default_registry('127.0.0.1:32000')

custom_build(
    'service_discovery-sql',
    'docker build --target builder --tag $EXPECTED_REF .',
    ['apps/service_discovery_postgres/priv/migrations'],
    live_update=[
        sync('apps/service_discovery_postgres/priv/migrations', '/app/src/sql'),
    ],
    entrypoint="cp /app/src/sql/* /flyway/sql"
)

custom_build(
    'service_discovery',
    'docker build --target devrel --tag $EXPECTED_REF .',
    ['.'],
    live_update=[
        sync('apps', '/app/src/apps'),
        run('./rebar3 compile'),
        run('/app/src/_build/default/rel/service_discovery/bin/service_discovery-dev restart')
    ],
    ignore=["rebar.lock"]
)

k8s_yaml(kustomize('deployment/overlays/dev'))

watch_file('deployment/')
