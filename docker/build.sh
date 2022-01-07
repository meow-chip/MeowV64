#!/bin/sh
IMAGE_NAME=jiegec/meowv64
sudo docker build -t $IMAGE_NAME .
sudo docker push $IMAGE_NAME

