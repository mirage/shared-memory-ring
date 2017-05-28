
.PHONY: build clean test

build:
	jbuilder build @install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

.PHONY: docker
docker:
	docker build -t xen-gnt .

clean:
	rm -rf _build
