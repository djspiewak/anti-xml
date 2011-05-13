#!/bin/bash

TEMPLATE=conf/template.html
TARGET=deploy
ACTIONS=build
CURRENT_ACTION=

require() {
	REQUIRES="$REQUIRES $1"
}

check() {
	for c in $REQUIRES; do
		if [ ! -e "`which $c`" ]; then
			echo "ERROR: $c must be installed" 1>&2
			MISSING=true
		fi
	done
	
	if [ $MISSING ]; then
		exit 100
	fi
}

log() {
	echo -e "[\033[1m${CURRENT_ACTION}\033[0m] " $* 1>&2
}

error() {
	echo -e "[\033[31m${CURRENT_ACTION}\033[0m] " $* 1>&2
}


require ack
require rst2html
check

cd `dirname $0`

[ "$1" == "" ] || ACTIONS="$*"

ERROR=false
for action in $ACTIONS; do
	CURRENT_ACTION=$action
	case $action in
		build)
			log 'Building all source files...'
			[ -e $TARGET ] || mkdir $TARGET
			for f in src/*.txt; do
				fname=`echo $f | sed 's!^src/!!g' | sed 's/.txt$//g'`.html
				
				log "    $f"
				rst2html --template=$TEMPLATE $f $TARGET/$fname
				
				if [ $? -ne 0 ]; then
					ERROR=true
				fi
				
				ack '^<title>([^<]*)</title>$' --output '<title>Anti-XML: $1</title>' --passthru $TARGET/$fname > /tmp/$fname
				mv /tmp/$fname $TARGET/$fname
				
				ack '^<title>Anti-XML: </title>$' --output '<title>Anti-XML</title>' --passthru $TARGET/$fname > /tmp/$fname
				mv /tmp/$fname $TARGET/$fname
			done
			
			log 'Copying static files...'
			cp static/* $TARGET
			log 'Complete'
		;;
		
		deploy)
			log 'Not yet implemented!'
			ERROR=true
		;;
		
		clean)
			log "Removing target directory..."
			rm -rf $TARGET
			log 'Complete'
		;;
		
		*)
			error 'Unrecognized action!'
			exit -1
		;;
	esac
	
	if [ "$ERROR" == true ]; then
		error 'One or more tasks exited with an error status...'
		exit -1
	fi
done

echo -e '[\033[32msuccess\033[0m] All done!' 1>&2
