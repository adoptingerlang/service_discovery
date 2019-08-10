allow_k8s_contexts("microk8s")
default_registry('127.0.0.1:32000')

custom_build(
    'service_discovery-sql',
    'docker build --target builder --tag $EXPECTED_REF .',
    ['sql'],
    live_update=[
        sync('sql', '/app/src/sql'),
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
    #entrypoint="/opt/rel/bin/service_discovery foreground",
    ignore=["rebar.lock"]
)

k8s_yaml(kustomize('deployment/overlays/dev'))

watch_file('deployment/')
