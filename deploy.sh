#!/bin/bash

# Define the current path
HOST_PATH=$(pwd)
echo $HOST_PATH

# Define container image name and port
container_image="dh_sediment_monitoring"
container_port=3838

# Run Docker container with port mapping
# docker run -it --rm -p $container_port:$container_port --entrypoint /bin/bash $container_image
# docker run -it --rm -p $container_port:$container_port --entrypoint /bin/bash -v "$(pwd)"/input:/home/project/input $container_image
# docker run -it --rm -p $container_port:$container_port --entrypoint R -v "$(pwd)"/input:/home/project/input $container_image
docker run -it --rm -p $container_port:$container_port -v "$(pwd)"/input:/home/project/input $container_image

# # Get the host IP address
# host_ip=$(hostname -I | awk '{print $1}')

# Open Shiny app URL in browser
xdg-open "http://localhost:$container_port"
