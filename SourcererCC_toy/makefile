.PHONY: all
all: tokenizer clone_detector

tokenizer: tokenizer.l
	flex $<;
	clang++ lex.yy.c -o $@

clone_detector: clone_detector.cpp
	clang++ -Wall $< -o $@

.PHONY: clean
clean:
	-rm lex.yy.c tokenizer clone_detector
