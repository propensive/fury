VERSION = $(shell jq -r '.modules[0].version' build.irk)

distribution: dist/fury-$(VERSION) dist/launcher-$(VERSION)

one.zip:
	rsync -a --delete --delete-excluded --exclude 'mod/*/out' --exclude '.*' --exclude scala ../one ./
	zip -q -r one.zip one && rm -r one


.image: one.zip src/*/*.scala Dockerfile
	docker build --tag=fury .
	#docker build --tag=fury . --no-cache
	touch .image

dist:
	mkdir dist

quick:
	VERSION="$(VERSION)" etc/build

dist/fury-$(VERSION): dist .image
	docker rm fury || true
	docker run --name fury fury /bin/true
	docker cp fury:/fury/fury dist/fury-$(VERSION)
	docker rm fury

dist/launcher-$(VERSION): dist etc/launcher
	VERSION="$(VERSION)" envsubst < etc/launcher > dist/launcher-$(VERSION)

install: dist/fury-$(VERSION)
	sudo cp dist/fury-$(VERSION) /usr/local/bin/fury

release: distribution
	cd dist && gh release upload --clobber "v$(VERSION)" fury-$(VERSION)
	cd dist && gh release upload --clobber "v$(VERSION)" launcher-$(VERSION)

test:
	for DIR in test/* ; do \
	  cd $$DIR ; \
	  ./script > ./out ; \
	  diff ./pos ./out || echo 'Output does not match' && exit 1 ; \
	  cd ../../ ; \
	done

.PHONY: one quick distribution test
