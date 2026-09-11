IMAGE_NAME := eczemapred-dev

.PHONY: docker-build docker-run

docker-build:
	docker build -t $(IMAGE_NAME) .

docker-run: docker-build
	docker run --rm -it \
		-p 8787:8787 \
		-e DISABLE_AUTH=true \
		-v "$(CURDIR):/home/rstudio/EczemaPred" \
		$(IMAGE_NAME)
