# Example of Kubernetes and Cloud Haskell - Prime Factors

This example was made to test common functions and utilities of both Cloud Haskell and Kubernetes.

[Prime Factors Example](https://github.com/oren-atidot/cloud_haskell_test)

### Build
Before you stert you need to check that you have the following installed: GHC, Stack (Haskell), Docker, VirtualBox, Minikube, Kubectl.
for further installation instructions you can check in google, matching this to the relevant OS.

You can use the `makefile` to create docker image and Kubernetes deployments or use the same docker/Kubernetes commands Manually

To check if Kubernetes runs properly I recommend using

```
kubectl get pods
```
and
```
kubectl logs <pod_name>
```

### Cloud Haskell
The Cloud Haskell part on the example is the code you can find `app/Main.hs` and on `src/MasterSlave.hs` and it was taken from the example in the link below:

[Master Slave examples](https://github.com/haskell-distributed/distributed-process-demos)

The explanation for the calculation can be found [here](https://www.well-typed.com/blog/71/)
Notice that I used `hPtint stderr` instead of `print` since the logging system of Kubernetes gets the stderr immediately while the regular print suffers from laziness or something alike and will not show you the logs of the Kubernetes pods while they are running

#### different models on different branches
There are 3 different models that can be find on 3 different branches of this repo:
1. `master` - is actually a standard model of communication between master machine and slave machines, AKA the master slave model.

    This model can be extended to multiple masters, whereas every master can "live" on its own zone with a bunch of slaves and a queue of tasks of his own that will be assigned to them.

2. `actor_model` - is an attempt to implement the model that is defined [here](https://www.brianstorti.com/the-actor-model/). It is though incomplete Haskellwise, to understand where this stands you can consult Berko.

3. `with_queue` - an in between model that all the masters use one queue to rule them all.

Notice that the masters here are called `builders` and the slaves `workers` since its not really a master slave model anymore when there are multiple masters (unlike the ancient Meereen, Yunkai and Astapor)

### Kubernetes
This repo was also used for Kubernetes experiments. On any problem and isnufficient case I Would reccomend for the formal documentation of [Kubernetes](https://kubernetes.io/)
