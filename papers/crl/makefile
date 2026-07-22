# コンパイラと結合ツールの設定
LATEX = lualatex
PDFTK = pdftk
LATEX_FLAGS = -interaction=nonstopmode

# ソースファイルのリスト（指定された順番を維持）
SOURCES = \
	../logic/qrb.tex \
	../math/amphigory.tex \
	crl.tex

# 中間生成物および最終的なPDFのリスト
PDFS = $(SOURCES:.tex=.pdf)
TARGET = _crl.pdf

# デフォルトターゲット
all: $(TARGET)

# 最終PDFの結合
$(TARGET): $(PDFS)
	$(PDFTK) $(PDFS) cat output $(TARGET)

# 個別のPDF生成ルール
%.pdf: %.tex
	$(LATEX) $(LATEX_FLAGS) $<
	$(LATEX) $(LATEX_FLAGS) $<

txt: $(SOURCES)
	@rm -f _crl.txt
	for f in $(SOURCES) ../ref.bib; do \
		cat $$f >> _crl.txt; \
		printf '\n\n' >> _crl.txt; \
	done

# 掃除用
clean:
	rm -f $(PDFS) $(TARGET) *.aux *.log *.out

.PHONY: all clean
