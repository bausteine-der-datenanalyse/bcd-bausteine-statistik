clean:
	rm -rf __output .quarto
	rm -rf m-*/skript/*.tex
	rm -rf m-*/skript/*.aux
	rm -rf m-*/skript/*.idx
	rm -rf m-*/skript/*.log
	rm -rf m-*/skript/.Rproj.user
	rm -rf m-*/skript/out
	rm -rf m-*/skript/skript_files

html:
	for folder in m-*; do \
		echo $$folder; \
		quarto render $$folder -t html || exit 1; \
	done

pdf:
	for folder in m-*; do \
		echo $$folder; \
		quarto render $$folder -t pdf || exit 1; \
	done

render: html pdf

