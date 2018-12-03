FROM ubuntu
RUN apt-get update -q
RUN apt-get install libgmp10 -q
COPY .stack-work/install/x86_64-linux/lts-12.17/8.4.4/bin/cloud-haskell-test-exe /bin/

#ENTRYPOINT ["cloud-haskell-test-exe","--worker","8080"]

#CMD cloud-haskell-test-exe worker localhost 8080 &
