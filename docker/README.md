# Docker image for setting up a worker

1. Please install Docker according to [this guide](https://docs.docker.com/get-started/).

2. Clone this repo and `cd` to this directory (`fodira/docker`)

3. Run this (depending on the way Docker was installed, you may need to `sudo` on some Linux setup; on Windows, you probably don't need to)

```bash
docker build -t fodira .  
```

4. Lanuch the container

```bash
docker run --rm --name "fodiratest" -ti fodira
```

5. Now you are at the shell within the container. Give it a test

```bash
cd worker
./worker_dl sometestlinks.RDS jobs.tar.gz html
exit
```

6. There is no step 6
