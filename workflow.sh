#!/bin/bash
export IMAGE_DATAVALIDATOR=appsilon/datavalidator
export TAG_DATAVALIDATOR=1.7
REPO_HOST_DIR=`pwd`

remove_and_stop_rstudio () {
  container_id=$(docker ps -q -a -f name=rstudio)
  container_id_length=${#container_id}
  if [[ $container_id_length > 0 ]]; then
    docker stop rstudio
    docker rm rstudio
  fi
}

echo "Using image $IMAGE_DATAVALIDATOR:$TAG_DATAVALIDATOR"

if [ "$1" == "build" ]; then

  cd environment
  docker build -f Dockerfile -t $IMAGE_DATAVALIDATOR:$TAG_DATAVALIDATOR .
  cd ..

elif [ "$1" == "pull" ]; then

  docker pull $IMAGE_DATAVALIDATOR:$TAG_DATAVALIDATOR

elif [ "$1" == "push" ]; then

  docker push $IMAGE_DATAVALIDATOR:$TAG_DATAVALIDATOR

elif [ "$1" == "rstudio" ]; then

  remove_and_stop_rstudio

  docker run -d --name rstudio -p 8787:8787 -v $REPO_HOST_DIR:/mnt $IMAGE_DATAVALIDATOR:$TAG_DATAVALIDATOR
  echo "Rstudio running on: http://localhost:8787"
  xdg-open http://localhost:8787 > /dev/null

elif [ "$1" == "stop" ]; then

  remove_and_stop_rstudio
  echo "Container stopped and removed"

elif [ "$1" == "test" ]; then

  echo "Running unit tests..."
  docker run -v $REPO_HOST_DIR:/mnt -t $IMAGE_DATAVALIDATOR:$TAG_DATAVALIDATOR bash -c "cd /mnt/src; Rscript tests/testthat.R"

else
  echo "Usage: ./workflow.sh [param]"
  echo
  echo "Params:"
  echo
  echo "   build - quick build of new image from Dockerfile based on base image"
  echo "   pull - get image from Docker Hub"
  echo "   push - push image to Docker Hub"
  echo "   rstudio - run image as daemon and start Rstudio in a browser"
  echo "   stop - stop running container"
  echo "   test - run linter and unittests"
  echo
fi
