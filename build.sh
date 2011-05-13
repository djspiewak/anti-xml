#!/bin/bash

TEMPLATE=conf/template.html
TARGET=deploy

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

