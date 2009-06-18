#!/bin/sh

case "$2" in
  vcc2/* )
    case "$1" in
      *.c )
        chmod 644 "$2"
        cat "$1" >> "$2"
        echo '`' >> "$2"
        echo '`' >> "$2"
	sleep 1
	perl -p -i -e 's/\r//g' "$2"
	sleep 1
	perl -p -i -e 's/\n/\r\n/g' "$2"
	rm -f "$2".bak
        ./fix "$2"
        ;;
    esac
esac
