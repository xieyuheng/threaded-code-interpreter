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
	ocaml threaded-code-interpreter.ml

bin:
	ocamlopt.opt threaded-code-interpreter.ml -o threaded-code-interpreter

run-bin:
	./threaded-code-interpreter

dev:
	make clean
	make tangle
	make run

clean:
	@
	echo -e "\e[33;1m"
	echo "* clean"
	echo -e "\e[0m"
	rm -f threaded-code-interpreter
	rm -f *.o *.cmi *.cmo *.cmx
	rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~  */*/*/*/*/*~
