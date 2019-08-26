service_discovery
=====

This project contains applications for creating and updating services over HTTP and grpc that can be queried through DNS. The provided backing storage is through Postgres.

**Disclaimer:** The purpose of creating this project was the need [Adopting Erlang](https://adoptingerlang.org/) had for a "real-world-esque" project to show the development and production patterns, structure and workflow we use. However, some decisions are made for the purpose of demonstration only, and not because we sat down and determined a certain strategy was optimal for some use case. The use of Postgres as the default storage backend is an example of this. This doesn't mean Postgres isn't a perfectly valid solution for a service discovery backend, and for many use cases even an optimal backend. The point is that it was chosen so that working with Postgres connections, queries and migrations from Erlang could be covered.

## Running Locally

A Postgres instance can be started and have the migrations in the directory `sql/` run on it with `docker-compose`:

``` shell
$ docker-compose up
```

This will bring up the database and run the migrations in the foreground, you can use `-d` to start them in the background.

With the database running the `service_discovery` applications can be started and drop you into a shell with:

``` shell
$ rebar3 shell
```

This will be running an HTTP interface on port `3000` and grpc on `8081`. The following `curl` commands will add a service named `webapp` with a single endpoint `127.0.0.1` and port named `http` with value `8000` :

``` shell
$ curl -v -XPUT http://localhost:3000/service -d '{"name": "webapp", "attributes": {"attr-1": "value-1"}}'
$ curl -v -XPUT http://localhost:3000/service/webapp/register -d '{"ip": "127.0.0.1", "tags": []}'
$ curl -v -XPUT http://localhost:3000/service/webapp/port -d '{"http": {"protocol": "tcp", "port": 8000}}'
```

`service_discovery` will also be running a DNS server on port `8053` and after adding the endpoint and port it can be queried with `dig` for the new service `webapp`:

``` shell
$ dig -p8053 @127.0.0.1 _http._tcp.webapp.svc.cluster.local SRV
...
_http._tcp.webapp.svc.cluster.local. 3600 IN SRV 1 1 8000 webapp.svc.cluster.local.
...
$ dig -p8053 @127.0.0.1 webapp.svc.cluster.local A
...
webapp.svc.cluster.local. 3600 IN	A	127.0.0.1
...
```

    
## Running in Kubernetes Locally

``` shell
$ tilt up
```
