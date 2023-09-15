allow_k8s_contexts("kind-adoptingerlang")
default_registry('127.0.0.1:5002')

docker_build('service_discovery_sql',
             '.',
             only=['apps/service_discovery_postgres/priv/migrations'],
             dockerfile='./Dockerfile',
             target='dev_sql',
             entrypoint="cp /app/sql/* /flyway/sql")

docker_build('service_discovery',
             '.',
             # live_update=[
             #     sync('apps', '/app/src/apps'),
             #     run('rebar3 as tilt compile'),
             #     run('/app/_build/tilt/rel/service_discovery/bin/service_discovery restart')
             # ],
             target='dev_release',
             # ignore=["apps/service_discovery_postgres/priv/migrations/"]
             )

k8s_yaml(local('kustomize build deployment/overlays/dev'))

watch_file('deployment/')
