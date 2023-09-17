allow_k8s_contexts("kind-adoptingerlang")
default_registry('127.0.0.1:5002')

docker_build('service_discovery', '.', target='dev_release')

k8s_yaml(local('kustomize build deployment/overlays/dev'))

watch_file('deployment/')
