.PHONY: zarith_bench
zarith_bench:
	@jbuilder build src/zarith_bench.exe
	@cp _build/default/src/zarith_bench.exe zarith_bench

.PHONY: clean
clean:
	rm zarith_bench
	jbuilder clean
