docker build . -t finalproject611
docker run -it \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -e PASSWORD=1234 \
  -v $(pwd):/home/rstudio/work \
  -v $HOME/.ssh:/home/rstudio/.ssh \
  -v $HOME/.gitconfig:/home/rstudio/.gitconfig \
  -p 8797:8787 \
  finalproject611

