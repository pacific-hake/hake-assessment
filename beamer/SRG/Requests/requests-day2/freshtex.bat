# Clean up the directory after a LaTeX build. Windows version
del *.aux
del *.bbl
del *.blg
del *.dvi
del *.log
del *.lof
del *.lot
del *.ps
del *.toc
del *.txt
del *.out
del *.nav
del *.snm
del *.synctex.gz
del beamer-hake-requests-day1.tex
del beamer-hake-requests-day1.pdf
rmdir /S /Q knitr-cache
