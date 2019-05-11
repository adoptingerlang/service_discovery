custom_build(
  'tsloughter/service_discovery',
  'docker build -t $EXPECTED_REF --target runner .',
  ['.'],
  disable_push=False]
)

k8s_yaml(kustomize('deployment/dev'))
