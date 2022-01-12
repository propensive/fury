
niveau:
	mkdir -p niveau/mod
	rsync --delete -a ../niveau/mod/* niveau/mod/

.image: niveau src/*/*.scala Dockerfile
	docker build --tag=vex .
	touch .image

vex: .image
	docker run --name vex vex /bin/true
	docker cp vex:/vex/vex vex
	docker rm vex

.PHONY: niveau
