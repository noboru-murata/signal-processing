# 1. フォルダ内のすべての .tex ファイルを検索
SRCS = $(wildcard *.tex)

# 2. .tex の一覧から、生成すべき .pdf と .png の一覧を作る
PDFS = $(SRCS:.tex=.pdf)
PNGS = $(SRCS:.tex=.png)

# デフォルトターゲット：すべてのPNGを生成する
all: $(PNGS)

# ルールA: PDFからPNGを生成する（パターンルール）
%.png: %.pdf
	magick -density 300 $< -quality 100 $@

# ルールB: TEXからPDFを生成する（パターンルール）
%.pdf: %.tex
	latexmk $<

# 掃除ルール
clean:
	rm -f *.aux *.log *.toc *.out *.fdb_latexmk *.fls

fclean: clean
	rm -f $(PDFS) $(PNGS)

.PHONY: all clean fclean

