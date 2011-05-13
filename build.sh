#!/bin/bash

TEMPLATE=conf/template.html
TARGET=deploy

require() {
	REQUIRES="$REQUIRES $1"
}

check() {
	for c in $REQUIRES; do
		if [ ! -e "`which $c`" ]; then
			echo "ERROR: $c must be installed" 1>&2
			ERROR=true
		fi
	done
	
	if [ $ERROR ]; then
		exit 100
	fi
}

require ack
require rst2html
check

cd `dirname $0`

[ -e $TARGET ] || mkdir $TARGET
for f in src/*.txt; do
	fname=`echo $f | sed 's!^src/!!g' | sed 's/.txt$//g'`.html
	rst2html --template=$TEMPLATE $f $TARGET/$fname
	
	ack '^<title>([^<]*)</title>$' --output '<title>Anti-XML: $1</title>' --passthru deploy/$fname > /tmp/$fname
	mv /tmp/$fname deploy/$fname
	
	ack '^<title>Anti-XML: </title>$' --output '<title>Anti-XML</title>' --passthru deploy/$fname > /tmp/$fname
	mv /tmp/$fname deploy/$fname
done

cp static/* deploy

