SOURCES = src/final.ml src/main.ml

all: final

clean:
	rm -f final
	for X in ./src; do \
      for Y in cmo cmi output; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

final: $(SOURCES)
	ocamlc -o final -g -I src/ str.cma $(SOURCES)
