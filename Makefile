watch:
	echo "src/aoc2021/aoc2021.clj" | entr -cr clojure -M:run-m

run:
	clojure -M:run-m

start-server:
	clj -X:repl-server

watch-server:
	echo "(require '[clojure.tools.namespace.repl :refer [refresh]]) (load-file \"src/aoc2021/aoc2021.clj\") (use 'aoc2021.aoc2021) (-main)" | nc -N localhost 5555;
	echo "src/aoc2021/aoc2021.clj" | entr -c sh -c "echo \"(refresh) (-main)\" | nc -N localhost 5555";
