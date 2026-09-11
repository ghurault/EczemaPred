IMAGE_NAME := eczemapred-dev
R_VERSION := 4.4.1

.PHONY: docker-build docker-run

docker-build:
	docker build --build-arg R_VERSION=$(R_VERSION) -t $(IMAGE_NAME) .

docker-run: docker-build
	docker run --rm -it \
		-p 8787:8787 \
		-e DISABLE_AUTH=true \
		-v "$(CURDIR):/home/rstudio/EczemaPred" \
		$(IMAGE_NAME)
