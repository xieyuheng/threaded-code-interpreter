.ONESHELL:

help:
	@
	echo -e "\e[33;1m"
	echo "* makefile operations"
	echo "  * clean"
	echo "* I wish you happy making ^-^"
	echo "  please read the makefile for more informations"
	echo -e "\e[0m"

tangle:
	@
	./tangle.el threaded-code-interpreter.org

run:
	guile threaded-code-interpreter.scm

dev:
	make clean
	make tangle
	make run

clean:
	@
	echo -e "\e[33;1m"
	echo "* clean"
	echo -e "\e[0m"
	rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~  */*/*/*/*/*~
