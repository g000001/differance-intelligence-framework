# コンパイラと結合ツールの設定
LATEX = lualatex
PDFTK = pdftk
LATEX_FLAGS = -interaction=nonstopmode

# ソースファイルのリスト（指定された順番を維持）
SOURCES = \
	qrb.tex \
	avyakata.tex \
	pov-proof.tex \
	lhagma-semantics.tex \
	il-ait.tex \
	math-catuskoti.tex


# 中間生成物および最終的なPDFのリスト
PDFS = $(SOURCES:.tex=.pdf)
TARGET = _logic.pdf

# デフォルトターゲット
all: $(TARGET)

# 最終PDFの結合
$(TARGET): $(PDFS)
	$(PDFTK) $(PDFS) cat output $(TARGET)

# 個別のPDF生成ルール
%.pdf: %.tex
	$(LATEX) $(LATEX_FLAGS) $<
	# 目次や参照がある場合、2回実行する必要がある場合は以下を有効にしてください
	# $(LATEX) $(LATEX_FLAGS) $<

txt: $(SOURCES)
	@rm -f _logic.txt
	for f in icat-common.sty $(SOURCES); do \
		cat $$f >> _logic.txt; \
		echo '' >> _logic.txt; \
	done

# 掃除用
clean:
	rm -f $(PDFS) $(TARGET) *.aux *.log *.out

.PHONY: all clean
