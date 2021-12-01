ROJECT_ID:= bank-306009
SERVICE_NAME:=dashboard-dev
IMAGE:=gcr.io/${ROJECT_ID}/${SERVICE_NAME}
REGION:=europe-north1

build: 
	docker build . -t ${IMAGE}
	
bash: build 
	docker run -it --rm -p 8080:8080 ${IMAGE} /bin/bash
	
test: build
	docker run -it --rm -p 8080:8080 ${IMAGE}