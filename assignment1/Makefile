all: solver generator

solver: solver.hs
	ghc -o solver -O2 solver.hs

generator: generator.hs
	ghc -o generator -O2 generator.hs

test_solver: solver
	for i in boards/*; do echo $$i; cat $$i; echo "\nSolution:"; ./solver < $$i; echo "~~~~~~~~~~~~~~~~~~~~~~"; done

test_generator: generator
	echo "3 3 5 21383" | ./generator
