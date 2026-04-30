# コンパイラと結合ツールの設定
LATEX = lualatex
PDFTK = pdftk
LATEX_FLAGS = -interaction=nonstopmode

# ソースファイルのリスト（指定された順番を維持）
SOURCES = icat-perception.tex \
          icat-leges-fractales.tex \
          icat-ideal.tex \
          icat-truth-preservation.tex \
          icat-principles.tex \
          icat-dfix0.tex \
          icat-cl.tex \
          icat-catuskoti-skandha.tex \
          icat-sunyata-theory.tex \
          icat-lk.tex \
          icat-ymwcomb.tex \
          boundary-theory-core.tex \
          aletheic-proof-theory.tex \
          boundary-category.tex \
          titotp.tex \
          titotp-mebius.tex
          #avyakata-mnonad.tex \
          #lax-catuskoti.tex
          # icat-tta.tex \
          #icat-ifrd.tex


# 中間生成物および最終的なPDFのリスト
PDFS = $(SOURCES:.tex=.pdf)
TARGET = icat.pdf

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
	cat $(SOURCES) > icat.tex

# 掃除用
clean:
	rm -f $(PDFS) $(TARGET) *.aux *.log *.out

.PHONY: all clean
