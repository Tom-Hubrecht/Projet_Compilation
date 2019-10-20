BIN=pgoc
NAMES=hubrecht
report=Rapport

all:
	@$(MAKE) -C src/
	mv src/$(BIN) ./

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

.PHONY: all clean clean_all ship

