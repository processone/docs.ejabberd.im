# Install ejabberd using a Container Image

There are two official container images of ejabberd that you can install using docker (or podman):

## `ejabberd` Container Image

The [`"ejabberd"`](https://github.com/processone/ejabberd/pkgs/container/ejabberd) container image is available in the GitHub Container Registry.
It is available for x64 and arm64, both for stable ejabberd releases and the `master` branch.
Check the [`"ejabberd"` container documentation](../../../CONTAINER.md).

For example, download the latest stable ejabberd release:

``` sh
docker pull ghcr.io/processone/ejabberd
```

If you use Microsoft Windows 7, 10, or similar operating systems, check those tutorials:

- [Install ejabberd on Windows 10 using Docker Desktop](https://www.process-one.net/blog/install-ejabberd-on-windows-10-using-docker-desktop/)
- [Install ejabberd on Windows 7 using Docker Toolbox](https://www.process-one.net/blog/install-ejabberd-on-windows-7-using-docker-toolbox/)

For bug reports and improvement suggestions, if you use the `"ecs"` container image please go to the [docker-ejabberd GitHub Issues](https://github.com/processone/docker-ejabberd/issues),
if using the `"ejabberd"` container image from github please go to the [ejabberd GitHub Issues](https://github.com/processone/ejabberd/issues?q=is%3Aopen+is%3Aissue+label%3APackaging%3AContainer)
## `ecs` Container Image

The [`"ecs"`](https://hub.docker.com/r/ejabberd/ecs) container image allows to get ejabberd stable releases in x64 machines.
Check the [`"ecs"` container documentation](../../../README-ECS.md).

Download ejabberd with:

``` sh
docker pull docker.io/ejabberd/ecs
```

