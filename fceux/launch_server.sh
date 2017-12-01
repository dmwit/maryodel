#!/bin/sh

case $# in
	0)
		echo >&2 "USAGE: $0 FILE [ARGS]"
		echo >&2
		echo >&2 "Launch the Dr. Mario server, and print the paths of the two communication"
		echo >&2 "pipes (server to client and client to server, respectively) to stdout on the"
		echo >&2 "first two lines. The required FILE argument is the path to the Dr. Mario ROM."
		echo >&2 "Any remaining ARGS are passed unaltered to fceux."
		exit 1
		;;
esac

here=`dirname "$0"`
script="$here/dr_mario_server.lua"
if [ ! -r "$script" ]
then
	echo "Could not find server script at $script"
	exit 2
fi

pipe_dir=`mktemp -d`
if [ $? -ne 0 ]
then
	exit $?
fi

s2c="$pipe_dir/s2c"
c2s="$pipe_dir/c2s"

if ! mkfifo "$s2c"
then
	rm -rf "$pipe_dir"
	exit 3
fi

if ! mkfifo "$c2s"
then
	rm -rf "$pipe_dir"
	exit 4
fi

echo "$s2c"
echo "$c2s"

trap "rm -rf \"$pipe_dir\"; exit 5" INT
printf "%s\n%s\n" "$s2c" "$c2s" | fceux --loadlua "$script" "$@"

rm -rf "$pipe_dir"
