# Mini-driver for Alex

# needs the following variables:
#	ALEXLIB
#	ALEXBIN

case $* in
*--template*) $ALEXBIN $*;;
*)            $ALEXBIN --template $ALEXLIB $*;;
esac
