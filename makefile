docker:
	stack install --fast
	docker build -t test .
	docker tag test faminiacr.azurecr.io/ch_test
	docker push faminiacr.azurecr.io/ch_test

kube:
	kubectl apply -f workers.yaml
	kubectl apply -f builders.yaml

clean:
	kubectl delete deployments --grace-period=0 --force builders-deployment workers-deployment
