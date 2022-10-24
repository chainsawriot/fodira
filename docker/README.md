# Docker image for setting up a worker

1. Please install Docker according to [this guide](https://docs.docker.com/get-started/). Also, please make sure that you have setup the envvar `FODIRA_HOST` and you have your own SSH key at the host (default to ed25519 format).

2. Clone this repo and `cd` to this directory (`fodira/docker`)

3. Run this (depending on the way Docker was installed, you may need to `sudo` on some Linux setup)

```bash
docker build -t fodira --build-arg SSH_PRIVATE_KEY="$(cat ~/.ssh/id_ed25519)" --build-arg FODIRA_IP=`echo $FODIRA_HOST | sed 's/[[:alpha:]@]//g'` .
```

4. Lanuch the container

```bash
docker run -e FODIRA_HOST=$FODIRA_HOST --rm --name "fodiratest" -ti fodira
```

5. Now you are at the shell within the container. Give it a test

```bash
/fodira/page_dl/worker_dl --verbose
exit
```

6. There is no step 6
