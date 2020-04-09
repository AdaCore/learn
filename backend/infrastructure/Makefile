LXC_IMAGE      = images:alpine/3.11
CONTAINER_NAME = safecontainer

CLOUD_CONFIG   = cloud-config.yml

DOCKER_URL     = .future
DOCKERFILE     = $(DOCKER_URL)/Dockerfile

PROCESS_LIMIT  = 300

all: lxc docker

docker:
	# destroy previous container if it exists
	docker container ls | grep $(CONTAINER_NAME) && docker rm --force $(CONTAINER_NAME) || true
	# Build docker image
	docker build -f $(DOCKERFILE) -t "$(CONTAINER_NAME):Dockerfile" $(DOCKER_URL)
	# Run image as a container
	docker run --detach --name $(CONTAINER_NAME) "$(CONTAINER_NAME):Dockerfile"
	# set process limit
	docker container run --pids-limit $(PROCESS_LIMIT) $(CONTAINER_NAME)

lxc:
	# destroy previous container if it exists
	lxc list | grep $(CONTAINER_NAME) && lxc delete --force $(CONTAINER_NAME) || true
	# create the container
	lxc launch $(LXC_IMAGE) $(CONTAINER_NAME) -s default -c user.user-data="`cat $(CLOUD_CONFIG)`"
	# wait a bit to allow network to come up
	sleep 5
	# set process limit
	lxc config set $(CONTAINER_NAME) limits.processes $(PROCESS_LIMIT)
