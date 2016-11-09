DOC=grammar
OUT=$(DOC).pdf

all: $(OUT)

$(OUT): $(DOC).tex
	pdflatex $<

clean:
	rm -f $(OUT)
	rm -f *.aux
	rm -f *.log

.PHONY: clean
