x = local('git rev-parse --short HEAD')
print(x)
custom_build(
  'tsloughter/service_discovery',
  "docker build --target {} -t $EXPECTED_REF .".format("runner"),
  ['.'],
)
#docker_build("tsloughter/service_discovery", ".")
docker_compose("./docker-compose.yml")
#k8s_yaml(kustomize('deployment/dev'))
