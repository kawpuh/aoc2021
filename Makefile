watch:
	echo "src/aoc2021/aoc2021.clj" | entr -cr clojure -M:run-m

run:
	clojure -M:run-m

start-server:
	clj -X:repl-server

watch-server:
	echo "src/aoc2021/aoc2021.clj" | entr -c sh -c "echo \"(load-file \\\"src/aoc2021/aoc2021.clj\\\")\n(use 'aoc2021.aoc2021)\n(-main)\" | nc -N localhost 5555"
