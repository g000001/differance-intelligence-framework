# コンパイラと結合ツールの設定
LATEX = lualatex
PDFTK = pdftk
LATEX_FLAGS = -interaction=nonstopmode

# ソースファイルのリスト（指定された順番を維持）
SOURCES = \
	temporal-boolean-algebra.tex \
	../logic/avyakata.tex \
	../logic/qrb.tex \
	../logic/pov-proof.tex \
	pov-herbrand-space.tex \
	../sunyata-math/titotp.tex \
	../sunyata-math/titotp-mebius.tex \
	../logic/math-observer-elimination.tex \
	../logic/math-limit-equivalence.tex \
	amphigory.tex 

# 中間生成物および最終的なPDFのリスト
PDFS = $(SOURCES:.tex=.pdf)
TARGET = gttt.pdf

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
	@rm -f gttt.txt
	for f in icat-common.sty $(SOURCES); do \
		cat $$f >> gttt.txt; \
		printf '\n\n' >> gttt.txt; \
	done

# 掃除用
clean:
	rm -f $(PDFS) $(TARGET) *.aux *.log *.out

.PHONY: all clean
