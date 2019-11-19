BIN=pgoc
NAMES=hubrecht
report=Rapport

all:
	@$(MAKE) -C src/
	@cp -u src/$(BIN) ./

ship: clean
	@$(MAKE) -C rapport/
	mkdir -p $(NAMES)
	mv rapport/$(report).pdf ./
	\cp -f src/* $(NAMES)/
	tar -czf $(NAMES).tgz $(NAMES)/ $(report).pdf

clean:
	@$(MAKE) -C src/ clean
	@$(MAKE) -C rapport/ clean
	rm -rf $(NAMES) $(NAMES).tgz $(report).pdf

tests: all
	@cd tests && ./test -all ../$(BIN)

test_1: all
	@cd tests && ./test -1 ../$(BIN)

test_2: all
	@cd tests && ./test -2 ../$(BIN)

test_3: all
	@cd tests && ./test -3 ../$(BIN)

.PHONY: all clean clean_all ship test test_1 test_2 test_3

