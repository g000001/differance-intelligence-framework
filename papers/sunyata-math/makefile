# コンパイラと結合ツールの設定
LATEX = lualatex
PDFTK = pdftk
LATEX_FLAGS = -interaction=nonstopmode

# ソースファイルのリスト（指定された順番を維持）
SOURCES = icat-perception.tex \
	  avyakata.tex \
          icat-leges-fractales.tex \
          fractal-id.tex \
	  aletheics-observer-elimination.tex \
	\
	  boundary-theory-core.tex \
	  lhagma-avyakata-dfix0.tex \
          icat-dfix0.tex \
          icat-truth-preservation.tex \
          titotp.tex \
          titotp-mebius.tex \
	  math-limit-equivalence.tex \
	\
	  pov-proof.tex \
	  exploitation-monad.tex \
	  double-negation-monad.tex \
	  refinement-monad.tex \
	  cut-elimination-monad.tex \
	  5skandha.tex \
          icat-cl.tex \
          icat-catuskoti-skandha.tex \
          icat-ymwcomb.tex \
	\
	  icat-sunyata-theory.tex \
          icat-lk.tex \
	  aletheic-proof-theory.tex \
	\
          boundary-category.tex \
          icat-twdt.tex \
	  emb-twdt.tex \
	  bau-cat-em-kl.tex \
	  lhagma-semantics.tex \
	\
	  icat-calibration.tex

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
	@rm -f icat.txt
	for f in icat-common.sty $(SOURCES); do \
		cat $$f >> icat.txt; \
		echo '' >> icat.txt; \
	done

# 掃除用
clean:
	rm -f $(PDFS) $(TARGET) *.aux *.log *.out

.PHONY: all clean
