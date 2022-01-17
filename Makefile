VERSION = 0.2.0

distribution: dist/vex-$(VERSION) dist/launcher-$(VERSION)

local-niveau:
	rsync -a --delete --delete-excluded --exclude 'mod/*/out' --exclude '.*' --exclude scala ../niveau ./

.image: niveau src/*/*.scala Dockerfile
	docker build --tag=vex .
	touch .image

dist:
	mkdir dist

quick:
	VERSION="$(VERSION)" etc/build

dist/vex-$(VERSION): dist .image
	docker run --name vex vex /bin/true
	docker cp vex:/vex/vex dist/vex-$(VERSION)
	docker rm vex

dist/launcher-$(VERSION): dist etc/launcher
	VERSION="$(VERSION)" envsubst < etc/launcher > dist/launcher-$(VERSION)

install: dist/vex-$(VERSION)
	sudo cp dist/vex-$(VERSION) /usr/local/bin/vex

release: distribution
	gh release upload --clobber "v$(VERSION)" vex-$(VERSION)
	gh release upload --clobber "v$(VERSION)" launcher-$(VERSION)

.PHONY: local-niveau niveau quick distribution