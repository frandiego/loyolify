ROJECT_ID:= dashbaord-331416
SERVICE_NAME:= loyolify-dev
IMAGE:= gcr.io/${ROJECT_ID}/${SERVICE_NAME}
REGION:= europe-north1
INSTANCES:= 1
MEMORY:= 2G
SERVICE_ACCOUNT:= loyolify

build: 
	docker build . -t ${IMAGE}
	
bash: build 
	docker run -it --rm -p 8080:8080 ${IMAGE} /bin/bash
	
test: build
	docker run -it --rm -p 8080:8080 ${IMAGE}
	
push:
	docker push ${IMAGE}
	
deploy:
	gcloud run deploy ${SERVICE_NAME} --image ${IMAGE} --region ${REGION} --platform managed --max-instances ${INSTANCES} --memory ${MEMORY}
	
create_service_account:
	gcloud iam service-accounts create ${SERVICE_ACCOUNT}

set_service_account:
	gcloud projects add-iam-policy-binding ${ROJECT_ID} --member "serviceAccount:${SERVICE_ACCOUNT}@${ROJECT_ID}.iam.gserviceaccount.com" --role "roles/owner"

create_service_account_key:
	gcloud iam service-accounts keys create keyfile.json --iam-account ${SERVICE_ACCOUNT}@${ROJECT_ID}.iam.gserviceaccount.com
	
service_account_auth:
	cat keyfile.json | docker login -u _json_key --password-stdin https://gcr.io
	
service_account: create_service_account set_service_account create_service_account_key service_account_auth

run: build push deploy
