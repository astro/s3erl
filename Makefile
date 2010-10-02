main:	clean
	mkdir -p ebin
	erlc -o ebin -I include src/*.erl
	cp src/*.app ebin/

clean:
	mkdir -p ebin
	rm -f ebin/*

VERSION=s3erl-0.1
release:
	mkdir disttmp
	svn export `svn info . | grep '^URL:'| cut -d' ' -f2` disttmp/${VERSION}
	tar -Cdisttmp -zcvf  ${VERSION}.tar.gz ${VERSION}
	rm -rf disttmp
	echo Distribution is ${VERSION}.tar.gz
