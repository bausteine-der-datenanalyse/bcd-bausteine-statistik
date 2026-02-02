clean:
	rm -rf _output .quarto
	rm -rf m-*/skript/*.tex
	rm -rf m-*/skript/*.aux
	rm -rf m-*/skript/*.idx
	rm -rf m-*/skript/*.log
	rm -rf m-*/skript/out
	rm -rf m-*/skript/skript_files

render:
	for folder in m-*; do \
		quarto render $$folder/skript -t html; \
		quarto render $$folder/skript -t pdf; \
	done

