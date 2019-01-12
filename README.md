# Example of Kubernetes and Cloud Haskell - Prime Factors

This example was made to test common functions and utilities of both Cloud Haskell and Kubernetes.

[Prime Factors Example](https://github.com/oren-atidot/cloud_haskell_test)

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
This repo was also used for Kubernetes experiments. Except for the attempt to deploy on Azure and not minikube that is described below the things are pretty simple. There is a `makefile`, there is a stack project with regular build and there are two `.yaml` files of kubernetes deployments (which are, surprisingly `workers.yaml` and `builders.yaml`, not `stack.yaml` or `package.yaml`)

#### The incomplete attempt for deploying on AKS
The last but most important incomplete part of the Kubernetes project is to run on **A**zure **K**ubernetes **S**ervice, and not on the local minikube based on Vbox. That attempt was done on this toy example and it is now the ***last commit*** of the branch ***actor_model***.

The relevant things to notice on this incomplete mission are:
1. The secret is not the same secret used on minikube.
2. The same might apply to some other tools like the container-registry, docker image etc.
3. There's already an existing AKS cluster on our Azure active directory and resource group. Oh yes, there's a chance we are still paying Azure money for this one...
4. The Logging system might differ. I do not remember exactly how but I think I spent a few minutes to figure how to get the `stderr` from the Haskell code on AKS. 
5. The reason this did not work eventually is a Cloud Haskell reason: There are some functions in the backend I used that works through UDP multicast that cannot apply on real Kubernetes cluster in the cloud. Check it out in the [Cloud Haskell page](https://github.com/Atidot/atidot/wiki/distributed-Process).
