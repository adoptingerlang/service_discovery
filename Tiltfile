default_registry('127.0.0.1:32000')
chksum = local("cat rebar.config rebar.lock | cksum | awk '{print $1}' | tr -d '\n\n'")
ref = local("git rev-parse --short HEAD | tr -d '\n'")
custom_build(
  'service_discovery',
  """docker build \
     --cache-from tsloughter/service_discovery_builder:{} \
     --cache-from tsloughter/service_discovery_releaser:{} \
     --cache-from tsloughter/service_discovery_runner:{} \
     --target runner \
     -t $EXPECTED_REF .""".format(chksum, ref, ref),
  ['.'],
)

# docker_compose("./docker-compose.yml")
k8s_yaml(kustomize('deployment/overlays/dev'))
