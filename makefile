docker:
	stack install --fast
	docker build -t bo .
	docker tag bo orenm13/test1
	docker push orenm13/test1

kubernetes:
	minikube start
	kubectl apply -f workers.yaml
	kubectl apply -f builders.yaml

clean:
	kubectl delete deployments --grace-period=0 --force builders-deployment workers-deployment
