allow_k8s_contexts("microk8s")
default_registry('127.0.0.1:32000')

custom_build(
    'service_discovery_sql',
    'docker buildx build -o type=docker --target dev_sql --tag $EXPECTED_REF .',
    ['apps/service_discovery_postgres/priv/migrations'],
    entrypoint="cp /app/sql/* /flyway/sql"
)

# custom_build(
#     'service_discovery',
#     'docker buildx build -o type=docker --target dev_release --tag $EXPECTED_REF .',
#     ['.'],
#     live_update=[
#         sync('rebar.config', '/app/src/rebar.config'),
#         sync('apps', '/app/src/apps'),
#         run('rebar3 as tilt compile'),
#         run('/app/_build/tilt/rel/service_discovery/bin/service_discovery restart')
#     ],
#     ignore=["rebar.lock", "apps/service_discovery_postgres/priv/migrations/"]
# )

local_resource('release',
    cmd='rebar3 as tilt release',
    deps=['apps']
)

custom_build(
    'service_discovery',
    'docker buildx build -o type=docker --target runner --tag $EXPECTED_REF .',
    ['_build/tilt/rel/service_discovery'],
    live_update=[
        sync('_build/tilt/rel/service_discovery', '/opt/service_discovery'),
        run('/opt/service_discovery/bin/service_discovery restart')
    ],
    ignore=["rebar.lock", "_build/default", "_build/tilt/lib",
            "apps",
            "apps/service_discovery_postgres/priv/migrations/"]
)

k8s_yaml(kustomize('deployment/overlays/dev'))

watch_file('deployment/')
