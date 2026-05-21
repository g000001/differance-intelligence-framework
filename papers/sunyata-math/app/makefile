# コンパイラと結合ツールの設定
LATEX = lualatex
PDFTK = pdftk
LATEX_FLAGS = -interaction=nonstopmode

# ソースファイルのリスト（指定された順番を維持）
SOURCES = dimensional_metamathematics.tex \
          8d-collatz.tex \
          bintree.tex \
          16-truth-functions-and-catuskoti.tex \
          catuskoti_truth_functions.tex \
          \
          titotp-collatz.tex \
          twdt-beta-reduction.tex

# 中間生成物および最終的なPDFのリスト
PDFS = $(SOURCES:.tex=.pdf)
TARGET = icat-app.pdf

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

bigtex: $(SOURCES)
	@rm -f icat-app.txt
	for f in $(SOURCES); do \
		cat $$f >> icat-app.txt; \
		echo '' >> icat-app.txt; \
	done

# 掃除用
clean:
	rm -f $(PDFS) $(TARGET) *.aux *.log *.out

.PHONY: all clean
