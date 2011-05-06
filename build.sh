#!/bin/bash

[ -e /tmp/sbt-launch-0.7.5.jar ] || \
	(cd /tmp && wget http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.5.jar)

SUM=`openssl sha1 /tmp/sbt-launch-0.7.5.jar | sed 's!SHA1(/tmp/sbt-launch-0.7.5.jar)= !!g'`

if [ "${SUM}" == "b398555a69ef9317f7840020fa57bd8f23eaf60e" ]; then
	exec java -Xmx1024m -jar /tmp/sbt-launch-0.7.5.jar update test-compile package doc
else
	echo 'Downloaded SBT launcher 0.7.5 but did not match the checksum on record!'
	exit -1
fi
