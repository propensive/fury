VERSION = $(shell cat doc/.version)

distribution: dist/irk-$(VERSION) dist/launcher-$(VERSION)

one.jar:
	rsync -a --delete --delete-excluded --exclude 'mod/*/out' --exclude '.*' --exclude scala ../one ./
	zip -q -r one.jar one && rm -r one


.image: one.jar src/*/*.scala Dockerfile
	docker build --tag=irk .
	touch .image

dist:
	mkdir dist

quick:
	VERSION="$(VERSION)" etc/build

dist/irk-$(VERSION): dist .image
	docker rm irk || true
	docker run --name irk irk /bin/true
	docker cp irk:/irk/irk dist/irk-$(VERSION)
	docker rm irk

dist/launcher-$(VERSION): dist etc/launcher
	VERSION="$(VERSION)" envsubst < etc/launcher > dist/launcher-$(VERSION)

install: dist/irk-$(VERSION)
	sudo cp dist/irk-$(VERSION) /usr/local/bin/irk

release: distribution
	cd dist && gh release upload --clobber "v$(VERSION)" irk-$(VERSION)
	cd dist && gh release upload --clobber "v$(VERSION)" launcher-$(VERSION)

test:
	for DIR in test/* ; do \
	  cd $$DIR ; \
	  ./script > ./out ; \
	  diff ./pos ./out || echo 'Output does not match' && exit 1 ; \
	  cd ../../ ; \
	done

.PHONY: one quick distribution test
