;; latex.eld
;; Tempel templates for latex mode

latex-mode LaTeX-mode

(abstract "\\begin{abstract}\n" r> n> "\\end{abstract}" >)
(align "\\begin{align}\n" r> n> "\\end{align}" >)
(aligns "\\begin{align*}\n" r> n> "\\end{align*}" >)
(gather "\\begin{gather}\n" r> n> "\\end{gather}" >)
(gathers "\\begin{gather*}\n" r> n> "\\end{gather*}" >)
(appendix "\\begin{appendix}\n" r> n> "\\end{appendix}" >)
(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env >) "}" >)
(center "\\begin{center}\n" r> n> "\\end{center}" >)
(displaymath "\\begin{displaymath}\n" r> n> "\\end{displaymath}" >)
(document "\\begin{document}\n" r> n> "\\end{document}" >)
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}" >)
(equation "\\begin{equation}" r> n> "\\end{equation}" >)
(flushleft "\\begin{flushleft}" r> n> "\\end{flushleft}" >)
(flushright "\\begin{flushright}" r> n> "\\end{flushright}" >)
(frac "\\frac{" p "}{" q "}" >)
(fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}" >)
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}" >)
(letter "\\begin{letter}\n" r> n> "\\end{letter}" >)
(math "\\begin{math}\n" r> n> "\\end{math}" >)
(minipage "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}" >)
(quotation "\\begin{quotation}\n" r> n> "\\end{quotation}" >)
(quote "\\begin{quote}\n" r> n> "\\end{quote}" >)
(sloppypar "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}" >)
(theindex "\\begin{theindex}\n" r> n> "\\end{theindex}" >)
(theorem "\\begin{theorem}\n" r> n> "\\end{theorem}" >)
(theoremm "\\begin{theorem}[" q "]\n" r> n> "\\end{theorem}" >)
(trivlist "\\begin{trivlist}\n" r> n> "\\end{trivlist}" >)
(verbatim "\\begin{verbatim}\n" r> n> "\\end{verbatim}" >)
(verbatims "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}" >)

;;; latex.eld ends here
