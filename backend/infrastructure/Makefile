CONTAINER_NAME = safecontainer

DOCKER_URL     = context
DOCKERFILE     = $(DOCKER_URL)/Dockerfile

all: docker

docker:
	# destroy previous container if it exists
	docker container ls | grep $(CONTAINER_NAME) && docker rm --force $(CONTAINER_NAME) || true
	# remove all images on system
	docker system prune -a -f
	# Build docker image
	docker build -f $(DOCKERFILE) -t "$(CONTAINER_NAME):Dockerfile" $(DOCKER_URL)
