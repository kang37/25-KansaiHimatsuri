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

PAPER_DIR <- file.path(OUTPUT_DIR, "paper_figures")
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
  ggsave(file.path(PAPER_DIR, name), p, width = width, height = height,
         dpi = dpi, limitsize = FALSE)
  cat("saved:", name, sprintf("(%.2f x %.2f in)\n", width, height))
}

# ==============================================================================
# 図-1 = 01_profile_age_and_resources
# ==============================================================================

p01a_paper <- p01a_age + noti +
  theme(axis.text.x = element_text(size = AX_TEXT),
        axis.text.y = element_text(size = AX_TEXT))
p01a_paper$layers[[5]]$aes_params$size <- 3.0   # age_label_df の年齢数値
# 文字を拡大した分、年齢ラベルと点が重ならないよう右側の余白を広げる
# （元は limits=c(25,100), ラベルはy=99）。範囲を広げすぎると30〜90の
# 実データ部分が圧縮されて軸目盛が読みにくくなるため、控えめに広げる。
p01a_paper$layers[[5]]$data <- age_label_df
p01a_paper$layers[[5]]$mapping <- aes(x = festival, y = 103, label = age_label)
p01a_paper <- p01a_paper +
  scale_y_continuous(limits = c(25, 107), breaks = seq(30, 90, 20))

p01b_paper <- p01b_div + noti +
  theme(axis.text.x = element_text(size = AX_TEXT))
p01b_paper$layers[[2]]$aes_params$size <- GT_MD   # n_resources 数値

p01_paper <- (p01a_paper + p01b_paper +
  patchwork::plot_layout(widths = c(1, 0.62))) &
  pth & theme(strip.text.y = element_text(size = ST_TEXT, angle = 0))

save_paper(p01_paper, "図-1_01_profile_age_and_resources.png",
           width = 4.6, height = 9.2)

# ==============================================================================
# 図-2 = 03a_plant_prevalence_weighted
# ==============================================================================

p03a_main_paper <- p03a_main + noti + pth +
  theme(plot.margin = margin(3, 10, 3, 3))
p03a_daily_paper <- p03a_daily + noti + pth +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = margin(3, 5, 3, 10))

p03a_paper <- (p03a_main_paper + p03a_daily_paper +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect")) &
  theme(legend.position = "bottom") &
  guides(fill = guide_legend(nrow = 2))

save_paper(p03a_paper, "図-2_03a_plant_prevalence_weighted.png",
           width = 5.4, height = max(5, nrow(prev_plot) * 0.20))

# ==============================================================================
# 図-3 = 19d_plant_x_part
# ==============================================================================

p19d_paper <- p19d + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT, angle = 35, hjust = 1),
        legend.position = "bottom") +
  guides(size = guide_legend(nrow = 1)) +
  scale_size_area(max_size = 6, labels = scales::percent,
                   name = "その植物の部位記録に占める割合") +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.6, 1.0)))
# geom_text（%ラベル）はlayers[[2]]。バブルを縮小し、ラベルは点の真上
# すぐ近くに小さめのオフセットで置くことで、上の行との衝突を避ける。
p19d_paper$layers[[2]]$aes_params$size <- 2.9
p19d_paper$layers[[2]]$aes_params$vjust <- -0.9

save_paper(p19d_paper, "図-3_19d_plant_x_part.png",
           width = 6.2, height = max(6, length(part_taxon_order) * 0.34))

# ==============================================================================
# 図-4 = 19c_plant_x_use
# ==============================================================================

p19c_paper <- p19c + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT, angle = 40, hjust = 1))
p19c_paper$layers[[2]]$aes_params$size <- 2.9
# 【注】17列の用途カテゴリーを持つため、半ページ幅（3.4in程度）には
# 収まらない。文字を判読できる最低限の幅として8.2inを確保する
# （他の図より広いが、詰めると数値ラベルが列間で衝突し判読不能になる）。
save_paper(p19c_paper, "図-4_19c_plant_x_use.png",
           width = 8.2, height = max(6, n_distinct(mat_19c$resource_taxon) * 0.20))

# ==============================================================================
# 図-5 = 17b_reason_by_plant（灰色の凡例を追加）
# ==============================================================================

p17b_main_paper <- ggplot(reason_grid, aes(x = rlabel, y = taxon_label)) +
  geom_tile(data = ~ filter(.x, cell_kind %in% c("zero", "mid")),
            aes(fill = cell_kind), color = "white", linewidth = 0.4) +
  scale_fill_manual(
    values = c(zero = "white", mid = "#F0F0F0"),
    breaks = "mid", labels = c(mid = "上位3外の非0セル"),
    name = NULL
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(data = ~ filter(.x, cell_kind == "top"),
            aes(fill = share), color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(share > 0, scales::percent(share, accuracy = 1), "")),
            size = 2.9, family = "HiraginoSans-W3",
            color = ifelse(reason_grid$cell_kind == "top" & reason_grid$share > 0.5,
                           "white", "gray20")) +
  scale_fill_gradient(low = "#FDD8C0", high = "#B30000",
                      labels = scales::percent, name = "その理由を挙げた割合（上位3セル）") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.box = "vertical") +
  pth

p17b_subst_paper <- p17b_subst + noti + pth +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = margin(3, 5, 3, 10))

p17b_main_paper <- p17b_main_paper + theme(plot.margin = margin(3, 10, 3, 3))

p17b_paper <- (p17b_main_paper + p17b_subst_paper +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect")) &
  theme(legend.position = "bottom", legend.box = "vertical")

# 【注】10列の選定理由カテゴリーがあるため、半ページ幅には収まらない。
# 凡例を縦に3段（灰/理由の濃淡/代替可能性）積むことで、幅を無理に
# 広げずに全項目を収める。
save_paper(p17b_paper, "図-5_17b_reason_by_plant.png",
           width = 7.6, height = max(7.0, nrow(taxon_denom) * 0.22 + 0.6))

# ==============================================================================
# 図-6 = 03b_plant_prevalence_by_pref
# ==============================================================================

p03b_paper <- p03b + noti + pth
p03b_paper$layers[[2]]$aes_params$size <- GT_MD

save_paper(p03b_paper, "図-6_03b_plant_prevalence_by_pref.png",
           width = 3.8, height = max(5, nrow(prev_plot) * 0.20))

# ==============================================================================
# 図-7 = 23c_method_type_by_plant
# ==============================================================================

p23c_paper <- p23c + noti + pth +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))
# 【注】n=件数は行ラベル（taxon_label）に埋め込み済みなので専用のgeom_textはない。
# 5類型の凡例が3.4inには収まらないため少し広げる。

save_paper(p23c_paper, "図-7_23c_method_type_by_plant.png",
           width = 5.6, height = max(6, n_distinct(method_type_long$taxon_label) * 0.17))

# ==============================================================================
# 図-8 = 28_procurement_change_by_plant
# ==============================================================================

p28_paper <- p28 + noti + pth +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

save_paper(p28_paper, "図-8_28_procurement_change_by_plant.png",
           width = 4.7, height = max(5, nrow(change_denom) * 0.17))

# ==============================================================================
# 図-9 = 29_plant_x_method_x_change
# ==============================================================================

p29_paper <- p29 + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT - 0.5, angle = 45, hjust = 1))
p29_paper$layers[[2]]$aes_params$size <- 2.9
# 【注】5つの小図（調達方式）×各3列（調達地の変化）を並べるため、
# 半ページ幅には収まらない。列同士のラベルが衝突しない最低限の幅を確保する。
save_paper(p29_paper, "図-9_29_plant_x_method_x_change.png",
           width = 9.0, height = max(6, length(taxon_order_29) * 0.19))

# ==============================================================================
# 図-10 = 24a_plant_x_landscape
# ==============================================================================

p24a_paper <- p24a + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT, angle = 30, hjust = 1))
p24a_paper$layers[[2]]$aes_params$size <- 2.9
# 【注】10列の生態景観類型があるため、半ページ幅には収まらない。
save_paper(p24a_paper, "図-10_24a_plant_x_landscape.png",
           width = 6.4, height = max(6, n_distinct(mat_24a$resource_taxon) * 0.19))

# ==============================================================================
# 図-11 = 28b_procurement_change_by_landscape
# ==============================================================================

p28b_paper <- p28b + noti + pth +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

save_paper(p28b_paper, "図-11_28b_procurement_change_by_landscape.png",
           width = 4.7, height = max(4, nrow(landscape_change_denom) * 0.30))

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

cat("\n=== 論文用図表", length(list.files(PAPER_DIR, pattern = "\\.png$")), "枚を",
    PAPER_DIR, "に出力 ===\n")
