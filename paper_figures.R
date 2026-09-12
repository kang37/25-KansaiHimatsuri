# ==============================================================================
# 論文用图表（印刷サイズ版）
# ------------------------------------------------------------------------------
# main2.R が作った12枚の図を、A4二段組の片段（約半ページ幅）に直接貼り込む
# 前提で作り直す。方針（図表20260910.pptxの備考に基づく）：
#   1. 図タイトル・サブタイトル・キャプションは全て削除する
#      （本文側にキャプションとして別途記載するため）。
#   2. 軸ラベル・地名・数値・凡例など、図に残る文字は全て拡大する
#      （最終的な貼り込み幅＝約3.4〜5.0インチを基準に、実寸ptで指定）。
#   3. 図17bは「上位3外の非0セル」が薄灰で塗られているが凡例に出ていない
#      ため、灰色の凡例スウォッチを追加する。
# 出力先: data_proc/20260902/paper_figures/（新設）
# ------------------------------------------------------------------------------
# 使い方: source("main2.R") の全変数がある状態で実行する必要があるため、
# 本スクリプト自身が source("main2.R") する。
# ==============================================================================

source("main2.R")

suppressMessages(library(ggnewscale))

if (!exists("PAPER_DIR")) PAPER_DIR <- file.path(OUTPUT_DIR, "paper_figures")
dir.create(PAPER_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 印刷用の文字サイズ（pt、最終貼り込み幅で実際に読める大きさ） -----------
AX_TEXT   <- 8.2   # 軸目盛（地名・植物名・カテゴリー名）
AX_TITLE  <- 9.2   # 軸タイトル
LG_TEXT   <- 8.2   # 凡例項目
LG_TITLE  <- 8.8   # 凡例タイトル
ST_TEXT   <- 8.5   # facet（小図）見出し
GT_SM     <- 3.0   # geom_text（セル内数値）小
GT_MD     <- 3.4   # geom_text（セル内数値）中
GT_LG     <- 3.8   # geom_text（セル内数値）大

pth <- theme(
  axis.text    = element_text(size = AX_TEXT),
  axis.title   = element_text(size = AX_TITLE),
  legend.text  = element_text(size = LG_TEXT),
  legend.title = element_text(size = LG_TITLE),
  strip.text   = element_text(size = ST_TEXT),
  plot.margin  = margin(3, 5, 3, 3)
)

noti <- labs(title = NULL, subtitle = NULL, caption = NULL)

save_paper <- function(p, name, width, height, dpi = 600) {
  path <- file.path(PAPER_DIR, name)
  ggsave(path, p, width = width, height = height, dpi = dpi, limitsize = FALSE)
  # ggsave()が書き出すPNGにはDPI情報（pHYsチャンク）が埋め込まれないため、
  # Word等に貼り付けると画面用の96dpiとして解釈され、実寸よりはるかに
  # 大きく表示されてしまう。Pillowで正しいDPIを書き込み直す。
  py <- sprintf(
    "from PIL import Image; im = Image.open('%s'); im.save('%s', dpi=(%d, %d))",
    path, path, dpi, dpi
  )
  status <- system2("python3", args = c("-c", shQuote(py)))
  if (status != 0) warning("DPI埋め込みに失敗しました: ", name)
  cat("saved:", name, sprintf("(%.2f x %.2f in, %d dpi embedded)\n", width, height, dpi))
}

# ==============================================================================
# 図-1 = 01_profile_age_and_resources
# ------------------------------------------------------------------------------
# 【2026-09-12改訂】左パネル（協力者年齢）を削除し、右パネル（資源数）のみ
# を残す。祭り名を表示し、文字を拡大、全体の高さを縮小する。カウント基準を
# 2種類併記する：
#   ・植物口径＝祭り×植物分類群のユニーク数（従来のn_resources、濃灰）
#   ・資源口径＝祭り×植物×使用部位のレコード数（resource_dfの行数、灰）
# ==============================================================================

resource_count_records <- resource_df %>%
  count(festival, name = "n_records")

BASIS_LEVELS <- c("植物種類数", "資源種類数")
BASIS_PAL <- setNames(c("gray35", "gray75"), BASIS_LEVELS)

# 【デザイン方針】資源口径（n_records）は植物口径（n_resources）以上に
# なる（同一祭り内で同じ植物の複数部位が記録される場合のみ差が出る）ため、
# 縦に並べる（dodge）のではなく、太い灰色バー（資源口径）の上に細い濃灰色
# バー（植物口径）を重ねる「バレットチャート」方式にする。dodgeだと30祭り
# 分の行高が低いため2本のバーが視覚的に潰れて重なり、ラベルも衝突する。
resource_count_df <- festival_profile %>%
  left_join(resource_count_records, by = "festival") %>%
  mutate(n_records = replace_na(n_records, 0))

count_max_01 <- max(resource_count_df$n_records, resource_count_df$n_resources, na.rm = TRUE)

# 【2026-09-12改訂】バー上のn/n数値ラベルを削除（データはバーの長さと
# 凡例の色だけで示す）。ラベル分の余白が不要になったのでx軸の拡大率を
# 縮小し、その分だけ全体の高さを拡大する。
p01b_paper <- ggplot(resource_count_df, aes(x = festival)) +
  geom_col(aes(y = n_records, fill = BASIS_LEVELS[2]), width = 0.68, na.rm = TRUE) +
  geom_col(aes(y = n_resources, fill = BASIS_LEVELS[1]), width = 0.32, na.rm = TRUE) +
  coord_flip() +
  facet_pref +
  scale_fill_manual(values = BASIS_PAL, name = NULL, breaks = BASIS_LEVELS) +
  scale_y_continuous(limits = c(0, count_max_01 * 1.05), expand = c(0, 0)) +
  labs(x = NULL, y = "件数（植物口径・資源口径）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid.major.y = element_blank(),
        strip.text.y = element_text(size = ST_TEXT, angle = -90),
        legend.position = "bottom") +
  pth

save_paper(p01b_paper, "図-1_01_profile_age_and_resources.png",
           width = 4.4, height = max(5.5, length(festival_order) * 0.22))

# ---- 8cm幅版 ----
w8 <- 8 / 2.54
AX_TEXT_S1 <- 5.2

p01b_s <- p01b_paper +
  theme(axis.text = element_text(size = AX_TEXT_S1),
        axis.title = element_text(size = AX_TEXT_S1 + 0.4),
        strip.text.y = element_text(size = AX_TEXT_S1, angle = -90),
        legend.text = element_text(size = AX_TEXT_S1 - 0.4))

save_paper(p01b_s, "図-1_01_profile_age_and_resources_8cm.png",
           width = w8, height = max(4.5, length(festival_order) * 0.19), dpi = 900)

# ==============================================================================
# 図-2 = 03a_plant_prevalence_weighted
# ==============================================================================

# 図-2は再度の拡大要望があったため、共通サイズ（pth）よりさらに一回り大きくする
AX_TEXT_2 <- AX_TEXT + 2.2
AX_TITLE_2 <- AX_TITLE + 2.2
LG_TEXT_2 <- LG_TEXT + 2.2
LG_TITLE_2 <- LG_TITLE + 2.2

# 【2026-09-12追加】各植物に対応する火祭り数（raw_n、素の集計）を
# y軸ラベルに「（n）」として付記する。
taxon_n_lookup_02 <- setNames(as.character(prev_plot$raw_n), as.character(prev_plot$taxon))

p03a_main_paper <- p03a_main + noti + pth +
  theme(plot.margin = margin(3, 10, 3, 3), axis.ticks.y = element_blank(),
        axis.text = element_text(size = AX_TEXT_2),
        axis.title = element_text(size = AX_TITLE_2)) +
  labs(x = "使用する火祭りの割合") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.70),
                      expand = expansion(mult = c(0, 0.03))) +
  scale_y_discrete(labels = function(x) paste0(x, "（", taxon_n_lookup_02[x], "）"))
# 棒棒糖の色を青からダークグレーへ
p03a_main_paper$layers[[1]]$aes_params$colour <- "gray30"  # geom_segment
p03a_main_paper$layers[[2]]$aes_params$colour <- "gray30"  # geom_point

# 日常利用の凡例を1行に収めるため、ラベル先頭の番号（1/2/3）を除去
daily_label_nonum   <- sub("^[0-9]\\s*", "", daily_label_lv)
daily_colors_nonum  <- setNames(daily_colors, daily_label_nonum)
daily_share_03_pp <- daily_share_03 %>%
  mutate(daily_label = factor(sub("^[0-9]\\s*", "", as.character(daily_label)),
                              levels = daily_label_nonum))

p03a_daily_paper <- ggplot(daily_share_03_pp, aes(x = pct, y = resource_taxon, fill = daily_label)) +
  geom_col(position = "stack", width = 0.72, na.rm = TRUE) +
  scale_fill_manual(values = daily_colors_nonum, name = "日常利用", drop = FALSE) +
  scale_x_continuous(labels = scales::percent,
                      expand = expansion(mult = c(0, 0.06))) +
  labs(x = "日常利用の割合", y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(3, 8, 3, 10),
        axis.text.x = element_text(size = AX_TEXT_2)) +
  pth

# 凡例（legend.text/legend.title）も拡大
p03a_legend_theme <- theme(legend.text = element_text(size = LG_TEXT_2),
                           legend.title = element_text(size = LG_TITLE_2))

# 左図の横軸範囲を70%までに絞った分、左を狭く・右（日常利用）を広く配分。
# 右図はexpandを少し足して「100%」の目盛文字が右端で切れないようにする。
p03a_paper <- (p03a_main_paper + p03a_daily_paper +
  patchwork::plot_layout(widths = c(1.7, 1.5), guides = "collect")) &
  theme(legend.position = "bottom") &
  p03a_legend_theme &
  guides(fill = guide_legend(nrow = 1))

save_paper(p03a_paper, "図-2_03a_plant_prevalence_weighted.png",
           width = 5.8, height = max(5, nrow(prev_plot) * 0.20))

# ---- テスト出力: 幅8cm（論文の1カラム幅相当）・高解像度版 ----
# 縦横比は元の5.8in x 6.8inと同じに保つ（w8は図-1の節で定義済み）
save_paper(p03a_paper, "図-2_03a_plant_prevalence_weighted_8cm.png",
           width = w8, height = w8 * (6.8 / 5.8), dpi = 900)

# ---- テスト2: 幅8cmで文字を縮小し、かつ高さは幅に連動させず
#      34行分の行間を確保する（前回は高さも比例縮小したため縦方向に
#      文字が重なった。今回は高さを元の6.8inのまま据え置く） ----
AX_TEXT_S  <- 4.6
AX_TITLE_S <- 5.0
LG_TEXT_S  <- 4.2
LG_TITLE_S <- 4.6

p03a_main_s <- p03a_main_paper +
  theme(axis.text = element_text(size = AX_TEXT_S),
        axis.title = element_text(size = AX_TITLE_S))
# 棒棒糖の線・点を縮小（行間を詰めても図形同士がぶつからないように）
p03a_main_s$layers[[1]]$aes_params$linewidth <- 0.4  # geom_segment
p03a_main_s$layers[[2]]$aes_params$size      <- 1.4  # geom_point

p03a_daily_s <- p03a_daily_paper +
  theme(axis.text.x = element_text(size = AX_TEXT_S))
# 積み上げ棒を細く
p03a_daily_s$layers[[1]]$aes_params$width <- 0.5

p03a_paper_s <- (p03a_main_s + p03a_daily_s +
  patchwork::plot_layout(widths = c(1.7, 1.5), guides = "collect")) &
  theme(legend.position = "bottom",
        legend.box.spacing = unit(2, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(8, "pt")) &
  theme(legend.text = element_text(size = LG_TEXT_S),
        legend.title = element_text(size = LG_TITLE_S)) &
  guides(fill = guide_legend(nrow = 1))

save_paper(p03a_paper_s, "図-2_03a_plant_prevalence_weighted_8cm_smallfont.png",
           width = w8, height = 3.4, dpi = 900)

# ==============================================================================
# 図-3 = 19d_plant_x_part
# ==============================================================================

# 【2026-09-12改訂】軸・凡例の文字を、他図のセル内数値と同程度の大きさ
# まで拡大し、軸文字と凡例文字を揃える。凡例タイトルは「資源の割合」に
# 変更し、キー（丸）の下に配置する。
AX_TEXT_3  <- 13.5
LG_TEXT_3  <- 13.5
LG_TITLE_3 <- 14.25

p19d_paper <- p19d + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT_3, angle = 35, hjust = 1),
        axis.text.y = element_text(size = AX_TEXT_3),
        legend.text = element_text(size = LG_TEXT_3),
        legend.title = element_text(size = LG_TITLE_3),
        legend.position = "bottom") +
  guides(size = guide_legend(nrow = 1, title.position = "left")) +
  scale_size_area(max_size = 6, labels = scales::percent,
                   name = "資源の割合") +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.6, 1.0)))
# 百分比ラベル（layers[[2]]）を削除。点を「黒縁・グレー塗り」に変更。
p19d_paper$layers[[2]] <- NULL
p19d_paper$layers[[1]]$aes_params$shape  <- 21
p19d_paper$layers[[1]]$aes_params$colour <- "black"
p19d_paper$layers[[1]]$aes_params$fill   <- "gray65"
p19d_paper$layers[[1]]$aes_params$alpha  <- NULL
p19d_paper$layers[[1]]$aes_params$stroke <- 0.7

# 【2026-09-12改訂】軸文字を1.5倍に拡大した分、列同士のラベルが重ならない
# よう幅も同程度拡大する（6.2in→9.3in）。
save_paper(p19d_paper, "図-3_19d_plant_x_part.png",
           width = 9.3, height = max(6, length(part_taxon_order) * 0.34))

# 【8cm版は省略】x軸の「部位」カテゴリーが21列あり、8cm幅では列ごとに
# 1.5mm程度しか割り当てられず、文字サイズを極限まで縮めても軸ラベルが
# 判読不能になる。半ページ幅に収める案（6.2in）を維持する。

# ==============================================================================
# 図-4 = 19c_plant_x_use
# ==============================================================================

p19c_paper <- p19c + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT + 1.3, angle = 40, hjust = 1),
        axis.text.y = element_text(size = AX_TEXT + 1.3),
        legend.text = element_text(size = LG_TEXT + 1),
        legend.title = element_text(size = LG_TITLE + 1))
p19c_paper$layers[[2]]$aes_params$size <- 2.2   # 百分比文字は逆に縮小
# 【注】17列の用途カテゴリーを持つため、半ページ幅（3.4in程度）には
# 収まらない。文字を判読できる最低限の幅として8.2inを確保する
# （他の図より広いが、詰めると数値ラベルが列間で衝突し判読不能になる）。
save_paper(p19c_paper, "図-4_19c_plant_x_use.png",
           width = 8.2, height = max(6, n_distinct(mat_19c$resource_taxon) * 0.20))

# 【8cm版は省略】用途カテゴリーが17列あり、図-3と同じ理由で8cm幅では
# 判読不能になる。8.2inを維持する。

# ==============================================================================
# 図-5 = 17b_reason_by_plant
# ------------------------------------------------------------------------------
# 【2026-09-12改訂：資源口径への変更】セルの割合（share）の計算を、
# 「祭り×植物分類群」単位（plant_festival、部位ごとの理由を和集合に集約）
# から「資源レコード」単位（resource_df、部位ごとに独立した1件として扱う）
# に変更した。同じ植物の複数部位が同じ理由を挙げている場合、資源口径では
# 部位の数だけ数える（図-4・図-7と同じ「府県ウェイトなし・観測標本の記述」
# という資源口径の方針に揃えた）。
# 行ラベルの「（n）」は従来通り taxon_denom（植物口径・祭り数）のままとし、
# 括弧内は数字のみ表示する（他図との対応づけ用の識別番号であり、セル内%の
# 分母ではない。分母の定義は図左上に注記する）。
# 【2026-09-11改訂】上位3セルのみ着色する方式をやめ、0%以外は全セルを
# 連続グラデーション（他図と同じ青系）で着色。灰色は使わない代わりに、
# セル罫線をグレーにして行・列のラベル対応を追いやすくする。
# ==============================================================================

reason_long_res <- resource_df %>%
  filter(!is.na(reason_types)) %>%
  separate_rows(reason_types, sep = "\\|") %>%
  rename(rtype = reason_types) %>%
  mutate(rlabel = factor(unname(REASON_LABELS[rtype]), levels = unname(REASON_LABELS)))

taxon_n_rec_17b <- resource_df %>%
  filter(!is.na(reason_types)) %>%
  count(resource_taxon, name = "n_rec")

# 行ラベルは植物口径（taxon_denom$n_fes、祭り数）のまま。括弧内は数字のみ。
label_order_17b_num <- taxon_denom %>%
  mutate(resource_taxon = factor(resource_taxon, levels = levels(taxon_order_17b))) %>%
  arrange(resource_taxon) %>%
  mutate(taxon_label = paste0(resource_taxon, "（", n_fes, "）")) %>%
  pull(taxon_label)

reason_by_taxon_res <- reason_long_res %>%
  filter(resource_taxon %in% taxon_denom$resource_taxon) %>%
  count(resource_taxon, rtype, rlabel, name = "n") %>%
  left_join(taxon_n_rec_17b, by = "resource_taxon") %>%
  left_join(taxon_denom %>% select(resource_taxon, n_fes), by = "resource_taxon") %>%
  mutate(share = n / n_rec,
         taxon_label = paste0(resource_taxon, "（", n_fes, "）"))

reason_grid_res <- expand_grid(
  taxon_label = label_order_17b_num,
  rlabel      = factor(unname(REASON_LABELS), levels = unname(REASON_LABELS))
) %>%
  left_join(reason_by_taxon_res %>% select(taxon_label, rlabel, share),
            by = c("taxon_label", "rlabel")) %>%
  mutate(share = ifelse(is.na(share), 0, share),
         taxon_label = factor(taxon_label, levels = rev(label_order_17b_num)))

p17b_main_paper <- ggplot(reason_grid_res, aes(x = rlabel, y = taxon_label)) +
  geom_tile(aes(fill = ifelse(share > 0, share, NA)), color = "gray75", linewidth = 0.35) +
  geom_text(aes(label = ifelse(share > 0, scales::percent(share, accuracy = 1, suffix = ""), "")),
            size = 2.9, family = "HiraginoSans-W3",
            color = ifelse(reason_grid_res$share > 0.5, "white", "gray20")) +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      limits = c(0, 1), breaks = c(0, 0.5, 1),
                      labels = scales::percent, name = "各理由の割合　",
                      guide = guide_colorbar(title.position = "left",
                                             barwidth = unit(70, "pt"),
                                             barheight = unit(6, "pt"))) +
  # 【2026-09-12改訂】「植物（祭り数）」はplot.tag（plot全体基準＝軸ラベルを
  # 含めた左端に配置）、「割合（%）」はplot.title（デフォルトのpanel基準＝
  # タイル部分の左端に配置）に分けることで、前者は軸ラベルの上、後者は
  # パネルの左端の上にそれぞれ揃う。
  labs(x = NULL, y = NULL, title = "割合（%）", tag = "植物（祭り数）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(size = AX_TEXT, face = "plain", hjust = 0),
        plot.title.position = "panel",
        plot.tag = element_text(size = AX_TEXT, face = "plain", hjust = 0),
        plot.tag.position = c(0, 1),
        plot.tag.location = "plot") +
  pth + theme(plot.margin = margin(14, 10, 3, 3))

# --- 代替可能性の内訳（右パネル）---
# 【2026-09-11改訂】代替可能性が空値（NA）の資源は、元々「代替不可」等の
# 4類型のどこにも属さないため除外されていたが、確認したところこれらは
# 「他の資源の代替品として使われている植物」（is_substitute_material=TRUE、
# 結果2の代替可能性区分コード4）であり、独自の意味を持つカテゴリーである。
# そこで「代替品」という4つ目の区分を追加し、着色・凡例に含める。
SUBST4 <- c("代替可", "代替困難", "代替不可", "代替品")
SUBST4_PAL <- c("代替可" = "#4DAF4A", "代替困難" = "#FF7F00",
                "代替不可" = "#E41A1C", "代替品" = "#6A51A3")

subst_share_17b_pp <- resource_df %>%
  filter(resource_taxon %in% taxon_denom$resource_taxon) %>%
  filter(!is.na(subst_score) | is_substitute_material) %>%
  mutate(subst_label = case_when(
    is_substitute_material ~ "代替品",
    subst_score == 1        ~ "代替可",
    subst_score == 2        ~ "代替困難",
    subst_score == 3        ~ "代替不可",
    TRUE                     ~ NA_character_
  )) %>%
  filter(!is.na(subst_label)) %>%
  mutate(subst_label = factor(subst_label, levels = SUBST4)) %>%
  count(resource_taxon, subst_label) %>%
  complete(resource_taxon = taxon_denom$resource_taxon, subst_label = SUBST4,
           fill = list(n = 0)) %>%
  group_by(resource_taxon) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(pct = ifelse(is.nan(pct), NA_real_, pct)) %>%
  left_join(taxon_denom %>% select(resource_taxon, n_fes), by = "resource_taxon") %>%
  mutate(taxon_label = factor(paste0(resource_taxon, "（", n_fes, "）"), levels = rev(label_order_17b_num)))

p17b_subst_paper <- ggplot(subst_share_17b_pp, aes(x = pct, y = taxon_label, fill = subst_label)) +
  geom_col(position = "stack", width = 0.72, na.rm = TRUE) +
  scale_fill_manual(values = SUBST4_PAL, name = "代替可能性", drop = FALSE,
                     guide = guide_legend(nrow = 1, title.position = "left")) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  pth + theme(plot.margin = margin(3, 5, 3, 10))

# 【2026-09-12改訂】左右2つの凡例（理由の割合＝連続グラデーション、
# 代替可能性＝離散4区分）を1行に横並びで統合する。キーと文字の間の
# 余白を詰め、省スペース化する。
p17b_legend_theme <- theme(
  legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.spacing.x = unit(26, "pt"),
  legend.key.spacing.x = unit(0, "pt"),
  legend.text = element_text(margin = margin(l = 0)),
  # title.position="left"では凡例タイトルとキー（バー/スウォッチ）の間隔が
  # デフォルトでほぼ0になるため、タイトル右側に余白を追加する。
  legend.title = element_text(margin = margin(r = 6)),
  legend.margin = margin(0, 2, 0, 2)
)

p17b_paper <- (p17b_main_paper + p17b_subst_paper +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect")) &
  theme(legend.position = "bottom") &
  p17b_legend_theme

# 【注】10列の選定理由カテゴリーがあるため、半ページ幅には収まらない。
save_paper(p17b_paper, "図-5_17b_reason_by_plant.png",
           width = 7.8, height = max(7.0, nrow(taxon_denom) * 0.22 + 0.6))

# 【8cm版は省略】選定理由10列＋右側の代替可能性内訳パネルがあり、
# 図-3・図-4と同じ理由で8cm幅では判読不能になる。7.8inを維持する。

# ==============================================================================
# 図-6 = 03b_plant_prevalence_by_pref
# ==============================================================================

p03b_paper <- p03b + noti + pth
p03b_paper$layers[[1]]$aes_params$colour <- "gray75"   # タイル罫線を白→灰に
p03b_paper$layers[[2]]$aes_params$size <- GT_MD

save_paper(p03b_paper, "図-6_03b_plant_prevalence_by_pref.png",
           width = 3.8, height = max(5, nrow(prev_plot) * 0.20))

# ---- 8cm幅版（列は6府県のみなので縮小の余地あり）----
p03b_s <- p03b_paper +
  theme(axis.text = element_text(size = 6.2),
        axis.title = element_text(size = 6.8),
        legend.text = element_text(size = 6.0),
        legend.title = element_text(size = 6.4))
p03b_s$layers[[2]]$aes_params$size <- 2.0   # セル内「n/n_sample」文字

save_paper(p03b_s, "図-6_03b_plant_prevalence_by_pref_8cm.png",
           width = w8, height = 6.8, dpi = 900)

# ==============================================================================
# 図-7 = 23c_method_type_by_plant
# ==============================================================================

p23c_paper <- p23c + noti + pth +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = METHOD_TYPE_PAL, name = "調達方式", drop = FALSE,
                     labels = function(x) sub("^[①②③④⑤]\\s*", "", x)) +
  guides(fill = guide_legend(nrow = 1))
# 【注】n=件数は行ラベル（taxon_label）に埋め込み済みなので専用のgeom_textはない。
# 凡例から番号を外して短くしたので1行に収まる。

save_paper(p23c_paper, "図-7_23c_method_type_by_plant.png",
           width = 6.2, height = max(6, n_distinct(method_type_long$taxon_label) * 0.17))

# ---- 8cm幅版 ----
p23c_s <- p23c_paper +
  theme(axis.text = element_text(size = 4.6),
        axis.title = element_text(size = 5.0),
        legend.text = element_text(size = 4.2),
        legend.title = element_text(size = 4.6),
        legend.box.spacing = unit(2, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(7, "pt")) +
  guides(fill = guide_legend(nrow = 2))

save_paper(p23c_s, "図-7_23c_method_type_by_plant_8cm.png",
           width = w8, height = 3.3, dpi = 900)

# ==============================================================================
# 図-8 = 28_procurement_change_by_plant
# ==============================================================================

# 【2026-09-11改訂】並びを「変化が大きい順」からTAXON_ORDER（生活形）順に
# 変更し、他の植物別の図（図03a・17b・19c・19d・23c・29）と揃える。
p28_taxon_order <- rev(levels(order_taxon(change_denom$resource_taxon)))
p28_label_levels <- paste0(p28_taxon_order, "（",
  change_denom$n_rec[match(p28_taxon_order, change_denom$resource_taxon)], "件）")

change_summary_pp <- change_summary %>%
  mutate(taxon_label = factor(paste0(resource_taxon, "（", n_rec, "件）"),
                              levels = p28_label_levels))

p28_paper <- ggplot(change_summary_pp, aes(x = taxon_label, y = pct, fill = change_cat)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = CHANGE_PAL, name = "調達地の変化", drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "資源レコードの割合") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid.major.y = element_blank()) +
  pth + theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

save_paper(p28_paper, "図-8_28_procurement_change_by_plant.png",
           width = 4.7, height = max(5, nrow(change_denom) * 0.17))

# ---- 8cm幅版 ----
p28_s <- p28_paper +
  theme(axis.text = element_text(size = 4.6),
        axis.title = element_text(size = 5.0),
        legend.text = element_text(size = 4.2),
        legend.title = element_text(size = 4.6),
        legend.box.spacing = unit(2, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(7, "pt"))

save_paper(p28_s, "図-8_28_procurement_change_by_plant_8cm.png",
           width = w8, height = 3.4, dpi = 900)

# ==============================================================================
# 図-9 = 29_plant_x_method_x_change
# ==============================================================================

p29_paper <- p29 + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT - 0.5, angle = 45, hjust = 1)) +
  facet_wrap(~ method_type, nrow = 1,
             labeller = as_labeller(function(x) sub("^[①②③④⑤]\\s*", "", x)))
p29_paper$layers[[2]]$aes_params$size <- 2.9
# 【注】5つの小図（調達方式）×各3列（調達地の変化）を並べるため、
# 半ページ幅には収まらない。列同士のラベルが衝突しない最低限の幅を確保する。
save_paper(p29_paper, "図-9_29_plant_x_method_x_change.png",
           width = 9.0, height = max(6, length(taxon_order_29) * 0.19))

# 【8cm版は省略】5つの小図（調達方式）を横に並べるため、8cm幅では
# 小図1つあたり1.6cm程度しか割り当てられず、各小図内の3列（調達地の
# 変化）とその軸ラベルが判読不能になる。9.0inを維持する。

# ==============================================================================
# 図-10 = 24a_plant_x_landscape
# ==============================================================================

# 【2026-09-11改訂】着色を「件数」から「その植物を使う祭りのうち、その
# 景観に由来する祭りの割合」に変更（例：14祭りが使用、うち12祭りが水田
# 由来なら12/14）。セルの文字も件数ではなく「n/分母」の分数表記にする。
# 分母は「調達地の景観が記録された、その植物を使う祭り数」（重複祭りは
# 1件に集約）。列は景観タイプの出現頻度順、行はTAXON_ORDER順（他図と統一）。
taxon_festival_denom_24 <- landscape_records %>%
  distinct(festival, resource_taxon) %>%
  count(resource_taxon, name = "n_taxon_fest")

mat_24a_pp <- landscape_records %>%
  distinct(festival, resource_taxon, landscape_type) %>%
  count(resource_taxon, landscape_type, name = "n") %>%
  left_join(taxon_festival_denom_24, by = "resource_taxon") %>%
  mutate(pct = n / n_taxon_fest, cell_label = paste0(n, "/", n_taxon_fest))

land_order_24_pp  <- mat_24a_pp %>% count(landscape_type, wt = n, sort = TRUE) %>% pull(landscape_type)
taxon_order_24_pp <- rev(levels(order_taxon(mat_24a_pp$resource_taxon)))

mat_24a_pp <- mat_24a_pp %>%
  mutate(resource_taxon = factor(resource_taxon, levels = taxon_order_24_pp),
         landscape_type = factor(landscape_type, levels = land_order_24_pp))

p24a_paper <- ggplot(mat_24a_pp, aes(x = landscape_type, y = resource_taxon, fill = pct)) +
  geom_tile(color = "gray75", linewidth = 0.3) +
  geom_text(aes(label = cell_label), size = 2.5, family = "HiraginoSans-W3", color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      labels = scales::percent, name = "その植物を使う祭りのうち\nその景観に由来する割合") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  pth

# 【注】10列の生態景観類型があるため、半ページ幅には収まらない。
save_paper(p24a_paper, "図-10_24a_plant_x_landscape.png",
           width = 6.6, height = max(6, n_distinct(mat_24a_pp$resource_taxon) * 0.19))

# ---- 8cm幅版（試作。9列あるため図-3等より厳しいが、列数が少なめなので
#      試す価値はある）----
p24a_s <- p24a_paper +
  theme(axis.text = element_text(size = 4.0),
        axis.title = element_text(size = 4.4),
        legend.text = element_text(size = 4.0),
        legend.title = element_text(size = 4.4, lineheight = 0.9),
        legend.box.spacing = unit(2, "pt"),
        legend.margin = margin(0, 0, 0, 0))
p24a_s$layers[[2]]$aes_params$size <- 1.6   # セル内「n/n_taxon_fest」文字

save_paper(p24a_s, "図-10_24a_plant_x_landscape_8cm.png",
           width = w8, height = 4.5, dpi = 900)

# ==============================================================================
# 図-11 = 28b_procurement_change_by_landscape
# ==============================================================================

# 【2026-09-11改訂】並びを「変化が大きい順」から、図24aと同じ「景観タイプの
# 出現頻度順」に変更（coord_flipのため昇順にすると多い方が上に来る）。
landscape_freq_order <- landscape_change_denom %>% arrange(n_rec) %>% pull(landscape_type)
landscape_label_levels <- paste0(landscape_freq_order, "（",
  landscape_change_denom$n_rec[match(landscape_freq_order, landscape_change_denom$landscape_type)], "件）")

landscape_change_summary_pp <- landscape_change_summary %>%
  mutate(land_label = factor(paste0(landscape_type, "（", n_rec, "件）"),
                             levels = landscape_label_levels))

p28b_paper <- ggplot(landscape_change_summary_pp, aes(x = land_label, y = pct, fill = change_cat)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = CHANGE_PAL, name = "調達地の変化", drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "資源レコードの割合") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid.major.y = element_blank()) +
  pth + theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

save_paper(p28b_paper, "図-11_28b_procurement_change_by_landscape.png",
           width = 4.7, height = max(4, nrow(landscape_change_denom) * 0.30))

# ---- 8cm幅版（10行のみなので図-8より短く収まる）----
p28b_s <- p28b_paper +
  theme(axis.text = element_text(size = 4.6),
        axis.title = element_text(size = 5.0),
        legend.text = element_text(size = 4.2),
        legend.title = element_text(size = 4.6),
        legend.box.spacing = unit(2, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(7, "pt"))

save_paper(p28b_s, "図-11_28b_procurement_change_by_landscape_8cm.png",
           width = w8, height = 1.9, dpi = 900)

# ==============================================================================
# 図-12 = 27a_topic_x_management
# ==============================================================================

# str_wrap()は単語間のスペースで改行するため、スペースのない日本語では
# 機能しない（見た目上ラベルが一切折り返されず重なる）。文字数ベースで
# 強制的に改行する自前の関数を使う。
wrap_cjk <- function(x, width = 7) {
  vapply(x, function(s) {
    chars <- strsplit(s, "")[[1]]
    idx <- seq_along(chars)
    groups <- split(chars, ceiling(idx / width))
    paste(vapply(groups, paste, character(1), collapse = ""), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

p27a_paper <- p27a + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT - 0.7, lineheight = 0.9)) +
  scale_x_discrete(labels = function(x) wrap_cjk(x, 5))
p27a_paper$layers[[2]]$aes_params$size <- GT_LG

save_paper(p27a_paper, "図-12_27a_topic_x_management.png",
           width = 4.4, height = 4.3)

# ---- 8cm幅版（4行×3列の小さなクロス表なので余裕がある）----
p27a_s <- p27a_paper +
  theme(axis.text.x = element_text(size = 5.0, lineheight = 0.9),
        axis.text.y = element_text(size = 5.0),
        axis.title = element_text(size = 5.4),
        legend.text = element_text(size = 4.6),
        legend.title = element_text(size = 5.0))
p27a_s$layers[[2]]$aes_params$size <- 2.6

save_paper(p27a_s, "図-12_27a_topic_x_management_8cm.png",
           width = w8, height = 3.0, dpi = 900)

cat("\n=== 論文用図表", length(list.files(PAPER_DIR, pattern = "\\.png$")), "枚を",
    PAPER_DIR, "に出力 ===\n")

# ==============================================================================
# 最終セット: 12図を新フォルダに統合出力
# ------------------------------------------------------------------------------
# 8cm幅に収まる図（1,2,6,7,8,10,11,12）はその版を、収まらない図
# （3,4,5,9）は元の幅の版を採用。全12図の文字サイズを図-11の8cm版に統一し、
# ヒートマップ系6図（4,5,6,9,10,12：geom_tileを使う図）はセル罫線を細い黒に
# 変更、外枠（panel.border）と目盛線（axis.ticks）を除去する。
# ==============================================================================

if (!exists("FINAL_DIR")) FINAL_DIR <- file.path(OUTPUT_DIR, "paper_figures_8cm")
dir.create(FINAL_DIR, showWarnings = FALSE, recursive = TRUE)

# 図-11の8cm版と同じ文字サイズに統一
F_AX_TEXT  <- 4.6
F_AX_TITLE <- 5.0
F_LG_TEXT  <- 4.2
F_LG_TITLE <- 4.6
F_ST_TEXT  <- 4.6

final_text_theme <- theme(
  axis.text    = element_text(size = F_AX_TEXT),
  axis.title   = element_text(size = F_AX_TITLE),
  legend.text  = element_text(size = F_LG_TEXT),
  legend.title = element_text(size = F_LG_TITLE),
  strip.text   = element_text(size = F_ST_TEXT)
)

# ヒートマップ共通の枠スタイル：外枠と目盛線を消す
heat_frame_theme <- theme(panel.border = element_blank(), axis.ticks = element_blank())

# geom_tileレイヤーの罫線を細い黒に変更（図-6のスタイルを基に、さらに細く・黒く）
recolor_tiles <- function(p, colour = "black", linewidth = 0.15) {
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomTile")) {
      p$layers[[i]]$aes_params$colour   <- colour
      p$layers[[i]]$aes_params$linewidth <- linewidth
    }
  }
  p
}

save_final <- function(p, name, width, height, dpi = 900) {
  path <- file.path(FINAL_DIR, name)
  ggsave(path, p, width = width, height = height, dpi = dpi, limitsize = FALSE)
  py <- sprintf(
    "from PIL import Image; im = Image.open('%s'); im.save('%s', dpi=(%d, %d))",
    path, path, dpi, dpi
  )
  status <- system2("python3", args = c("-c", shQuote(py)))
  if (status != 0) warning("DPI埋め込みに失敗しました: ", name)
  cat("final saved:", name, sprintf("(%.2f x %.2f in)\n", width, height))
}

# ---- 図-1（8cm版、非ヒートマップ）文字をさらに拡大。1パネルのみに
#      なったため高さを大幅に縮小する ----
p01_final <- p01b_s +
  theme(axis.text = element_text(size = 6.4),
        axis.title = element_text(size = 6.8),
        strip.text = element_text(size = 6.2),
        legend.text = element_text(size = 6.0))
save_final(p01_final, "図-1_01_profile_age_and_resources.png",
           width = w8, height = max(4.8, length(festival_order) * 0.20))

# ---- 図-2（8cm版、非ヒートマップ）----
p02_final <- p03a_paper_s + final_text_theme
save_final(p02_final, "図-2_03a_plant_prevalence_weighted.png",
           width = w8, height = 3.4)

# ---- 図-3（8cm不可、幅を拡大。非ヒートマップ＝散布バブル図）----
# p19d_paper側で既に軸・凡例文字を拡大済み（AX_TEXT_3等）なのでそのまま使う。
p03_final <- p19d_paper
save_final(p03_final, "図-3_19d_plant_x_part.png",
           width = 9.3, height = max(6, length(part_taxon_order) * 0.34))

# ---- 図-4（8cm不可、元の幅のまま。ヒートマップ）----
# 値がないセルにも罫線を表示するため、行×列の全組み合わせに展開してから描画
mat_19c_complete <- use_long %>%
  count(resource_taxon, use_cat) %>%
  complete(resource_taxon = taxon_order_19c, use_cat = use_order_19c, fill = list(n = 0)) %>%
  left_join(taxon_denom_19c, by = "resource_taxon") %>%
  mutate(pct = n / n_taxon,
         resource_taxon = factor(resource_taxon, levels = rev(taxon_order_19c)),
         use_cat = factor(use_cat, levels = use_order_19c))

# 【2026-09-12改訂】final_text_theme（8cm幅図用の縮小サイズ）ではなく、
# 図-4自身の幅（8.2in）に見合う大きさを別途定義する（現状比2倍）。
# 図左上に分母の注記を追加し、plot.title.position="plot"でy軸ラベルの
# 左端とおおよそ揃える。
F4_AX_TEXT  <- F_AX_TEXT * 2
F4_AX_TITLE <- F_AX_TITLE * 2
F4_LG_TEXT  <- F_LG_TEXT * 2
F4_LG_TITLE <- F_LG_TITLE * 2

p04_final <- ggplot(mat_19c_complete, aes(x = use_cat, y = resource_taxon,
                                           fill = ifelse(pct > 0, pct, NA))) +
  geom_tile(color = "black", linewidth = 0.15) +
  geom_text(aes(label = ifelse(pct > 0, scales::percent(pct, accuracy = 1, suffix = ""), "")),
            size = 3.0, family = "HiraginoSans-W3", color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      labels = scales::percent, name = "割合",
                      guide = guide_colorbar(title.position = "left")) +
  # 「植物（祭り数）」はplot.tag（plot全体基準）、「割合（%）」はplot.title
  # （panel基準）に分け、後者をタイル部分の左端と揃える。
  labs(x = NULL, y = NULL, title = "割合（%）", tag = "植物（祭り数）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  heat_frame_theme +
  theme(axis.text = element_text(size = F4_AX_TEXT),
        axis.title = element_text(size = F4_AX_TITLE),
        legend.text = element_text(size = F4_LG_TEXT),
        legend.title = element_text(size = F4_LG_TITLE),
        plot.title = element_text(size = F4_AX_TEXT, face = "plain", hjust = 0),
        plot.title.position = "panel",
        plot.tag = element_text(size = F4_AX_TEXT, face = "plain", hjust = 0),
        plot.tag.position = c(0, 1),
        plot.tag.location = "plot",
        plot.margin = margin(14, 5.5, 5.5, 5.5))
save_final(p04_final, "図-4_19c_plant_x_use.png",
           width = 8.2, height = max(6, n_distinct(mat_19c$resource_taxon) * 0.20))

# ---- 図-5（8cm不可、元の幅のまま。左パネルのみヒートマップ）----
# 文字サイズをセル内の百分比（geom_textのsize=2.9mm≒8.3pt）に揃える。
# 代替可能性の凡例は「代替品」を最後に固定表示（breaksを明示し、
# position_stack由来の並び崩れを防ぐ）。凡例は1行に横並び統合。
F5_TEXT  <- 8.3
F5_TITLE <- 8.8
p17b_main_final <- recolor_tiles(p17b_main_paper) + heat_frame_theme +
  theme(axis.text = element_text(size = F5_TEXT),
        axis.title = element_text(size = F5_TITLE),
        legend.text = element_text(size = F5_TEXT),
        legend.title = element_text(size = F5_TITLE),
        plot.title = element_text(size = F5_TEXT, face = "plain", hjust = 0),
        plot.title.position = "panel",
        plot.tag = element_text(size = F5_TEXT, face = "plain", hjust = 0),
        plot.tag.position = c(0, 1),
        plot.tag.location = "plot")
p17b_subst_final <- p17b_subst_paper +
  scale_fill_manual(values = SUBST4_PAL, name = "代替可能性", drop = FALSE, breaks = SUBST4,
                     guide = guide_legend(nrow = 1, title.position = "left")) +
  theme(axis.text = element_text(size = F5_TEXT),
        axis.title = element_text(size = F5_TITLE),
        legend.text = element_text(size = F5_TEXT),
        legend.title = element_text(size = F5_TITLE))

p05_final <- (p17b_main_final + p17b_subst_final +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect")) &
  theme(legend.position = "bottom") &
  p17b_legend_theme
save_final(p05_final, "図-5_17b_reason_by_plant.png",
           width = 7.8, height = max(7.0, nrow(taxon_denom) * 0.22 + 0.6))

# ---- 図-6（8cm版、ヒートマップ）文字を拡大 ----
p06_final <- recolor_tiles(p03b_s) + heat_frame_theme +
  theme(axis.text = element_text(size = 6.5),
        axis.title = element_text(size = 7.0),
        legend.text = element_text(size = 6.2),
        legend.title = element_text(size = 6.6))
save_final(p06_final, "図-6_03b_plant_prevalence_by_pref.png",
           width = w8, height = 6.8)

# ---- 図-7（8cm版、非ヒートマップ）----
p07_final <- p23c_s + final_text_theme
save_final(p07_final, "図-7_23c_method_type_by_plant.png",
           width = w8, height = 3.3)

# ---- 図-8（8cm版、非ヒートマップ）「吉祥草」（TAXON_ORDER外の1件のみの
#      記録で、他図には登場しない）を除外 ----
p08_final <- p28_s + final_text_theme
p08_final$data <- p08_final$data %>% filter(resource_taxon != "吉祥草")
save_final(p08_final, "図-8_28_procurement_change_by_plant.png",
           width = w8, height = 3.4)

# ---- 図-9（8cm不可、元の幅のまま。ヒートマップ）----
# 値がないセルにも罫線を表示するため、行×小図×列の全組み合わせに展開
mat_29_complete <- change_method_df %>%
  count(resource_taxon, method_type, change_cat, name = "n") %>%
  complete(resource_taxon = taxon_order_29,
           method_type = levels(change_method_df$method_type),
           change_cat = levels(change_method_df$change_cat),
           fill = list(n = 0)) %>%
  # complete()はfactorの列順を保持しないことがあるため、明示的に再設定
  mutate(resource_taxon = factor(resource_taxon, levels = rev(taxon_order_29)),
         method_type = factor(method_type, levels = levels(change_method_df$method_type)),
         change_cat = factor(change_cat, levels = levels(change_method_df$change_cat)))

p09_final <- ggplot(mat_29_complete, aes(x = change_cat, y = resource_taxon, fill = ifelse(n > 0, n, NA))) +
  geom_tile(color = "black", linewidth = 0.15) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 2.9, family = "HiraginoSans-W3", color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white", name = "記録数") +
  scale_y_discrete(drop = FALSE) +
  facet_wrap(~ method_type, nrow = 1) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  heat_frame_theme + final_text_theme
save_final(p09_final, "図-9_29_plant_x_method_x_change.png",
           width = 9.0, height = max(6, length(taxon_order_29) * 0.19))

# ---- 図-10（8cm版、ヒートマップ）文字を拡大 + 図-6と同じく
#      値がないセルにも罫線を表示（行×列を完全展開）----
mat_24a_complete <- landscape_records %>%
  distinct(festival, resource_taxon, landscape_type) %>%
  count(resource_taxon, landscape_type, name = "n") %>%
  complete(resource_taxon = unique(landscape_records$resource_taxon),
           landscape_type = unique(landscape_records$landscape_type),
           fill = list(n = 0)) %>%
  left_join(taxon_festival_denom_24, by = "resource_taxon") %>%
  mutate(pct = ifelse(!is.na(n_taxon_fest) & n_taxon_fest > 0, n / n_taxon_fest, NA_real_),
         cell_label = ifelse(n > 0, paste0(n, "/", n_taxon_fest), ""),
         resource_taxon = factor(resource_taxon, levels = taxon_order_24_pp),
         landscape_type = factor(landscape_type, levels = land_order_24_pp))

p10_final <- ggplot(mat_24a_complete, aes(x = landscape_type, y = resource_taxon, fill = pct)) +
  geom_tile(color = "black", linewidth = 0.15) +
  geom_text(aes(label = cell_label), size = 1.6, family = "HiraginoSans-W3", color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      labels = scales::percent, name = "その植物を使う祭りのうち\nその景観に由来する割合") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  heat_frame_theme +
  theme(axis.text = element_text(size = 5.0),
        axis.title = element_text(size = 5.4),
        legend.text = element_text(size = 4.6),
        legend.title = element_text(size = 5.0, lineheight = 0.9))
# 【注】文字拡大により軸ラベルが幅を取るため、9列のタイル部分を確保する
# には8cm(w8)では足りない。字が重ならない最低限としてやや幅を広げる。
save_final(p10_final, "図-10_24a_plant_x_landscape.png",
           width = 3.7, height = 4.8)

# ---- 図-11（8cm版、非ヒートマップ。文字サイズの基準そのもの）----
p11_final <- p28b_s + final_text_theme
save_final(p11_final, "図-11_28b_procurement_change_by_landscape.png",
           width = w8, height = 1.9)

# ---- 図-12（8cm版、ヒートマップ）高さを低くする ----
p12_final <- recolor_tiles(p27a_s) + heat_frame_theme + final_text_theme
save_final(p12_final, "図-12_27a_topic_x_management.png",
           width = w8, height = 2.5)

cat("\n=== 最終統合図表", length(list.files(FINAL_DIR, pattern = "\\.png$")), "枚を",
    FINAL_DIR, "に出力 ===\n")

