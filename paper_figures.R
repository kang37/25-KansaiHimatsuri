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
# 年齢数値ラベル（layers[[5]]）を削除。中央（左パネル）の府県名は
# strip.text.y=element_blank()（p01a_age自身の設定を継承）のまま維持。
p01a_paper$layers[[5]] <- NULL
# 点を「黒縁・内部半透明」に変更（shape=21で塗りと縁を分離）
p01a_paper$layers[[4]]$aes_params$shape  <- 21
p01a_paper$layers[[4]]$aes_params$colour <- "black"
p01a_paper$layers[[4]]$aes_params$fill   <- scales::alpha("black", 0.35)
p01a_paper$layers[[4]]$aes_params$alpha  <- NULL
p01a_paper$layers[[4]]$aes_params$stroke <- 0.9

p01b_paper <- p01b_div + noti +
  theme(axis.text.x = element_text(size = AX_TEXT),
        # 右パネルにのみ府県名を残し、時計回りに90度回転して省スペース化
        strip.text.y = element_text(size = ST_TEXT, angle = -90)) +
  labs(x = NULL, y = "植物種類")
p01b_paper$layers[[2]]$aes_params$size <- GT_MD   # n_resources 数値

p01_paper <- (p01a_paper + p01b_paper +
  patchwork::plot_layout(widths = c(1, 0.62))) &
  pth

save_paper(p01_paper, "図-1_01_profile_age_and_resources.png",
           width = 4.6, height = 9.2)

# ==============================================================================
# 図-2 = 03a_plant_prevalence_weighted
# ==============================================================================

p03a_main_paper <- p03a_main + noti + pth +
  theme(plot.margin = margin(3, 10, 3, 3)) +
  labs(x = "使用する火祭りの割合") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.75),
                      expand = expansion(mult = c(0, 0.03)))
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
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(3, 5, 3, 10)) +
  pth

# 左図の横軸範囲を75%までに絞った分、右図（日常利用）に幅を多めに配分
p03a_paper <- (p03a_main_paper + p03a_daily_paper +
  patchwork::plot_layout(widths = c(2.1, 1.3), guides = "collect")) &
  theme(legend.position = "bottom") &
  guides(fill = guide_legend(nrow = 1))

save_paper(p03a_paper, "図-2_03a_plant_prevalence_weighted.png",
           width = 5.8, height = max(5, nrow(prev_plot) * 0.20))

# ==============================================================================
# 図-3 = 19d_plant_x_part
# ==============================================================================

p19d_paper <- p19d + noti + pth +
  theme(axis.text.x = element_text(size = AX_TEXT + 1, angle = 35, hjust = 1),
        axis.text.y = element_text(size = AX_TEXT + 1),
        legend.text = element_text(size = LG_TEXT + 1),
        legend.title = element_text(size = LG_TITLE + 1),
        legend.position = "bottom") +
  guides(size = guide_legend(nrow = 1)) +
  scale_size_area(max_size = 6, labels = scales::percent,
                   name = "その植物の部位記録に占める割合") +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.6, 1.0)))
# 百分比ラベル（layers[[2]]）を削除。点を「黒縁・グレー塗り」に変更。
p19d_paper$layers[[2]] <- NULL
p19d_paper$layers[[1]]$aes_params$shape  <- 21
p19d_paper$layers[[1]]$aes_params$colour <- "black"
p19d_paper$layers[[1]]$aes_params$fill   <- "gray65"
p19d_paper$layers[[1]]$aes_params$alpha  <- NULL
p19d_paper$layers[[1]]$aes_params$stroke <- 0.7

save_paper(p19d_paper, "図-3_19d_plant_x_part.png",
           width = 6.2, height = max(6, length(part_taxon_order) * 0.34))

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

# ==============================================================================
# 図-5 = 17b_reason_by_plant
# ------------------------------------------------------------------------------
# 【百分比の定義の確認】reason_gridのshareは「資源レコード単位」ではなく
# 「祭り×植物分類群」単位（plant_festival）で計算されている。1つの祭りで
# 同じ植物の複数部位が記録されていても、その祭り・その植物の選定理由は
# 「記録された全部位の理由類型の和集合」として1単位に集約済み（つまり
# 部位ごとの提及回数は数えない）。したがってshareは「その植物を使う
# 祭り数（府県ウェイト補正後）のうち、その理由が（部位を問わず）1回でも
# 挙げられた祭りの割合」であり、部位単位の延べ提及回数の比率ではない。
# 【2026-09-11改訂】上位3セルのみ着色する方式をやめ、0%以外は全セルを
# 連続グラデーション（他図と同じ青系）で着色。灰色は使わない代わりに、
# セル罫線をグレーにして行・列のラベル対応を追いやすくする。
# ==============================================================================

p17b_main_paper <- ggplot(reason_grid, aes(x = rlabel, y = taxon_label)) +
  geom_tile(aes(fill = ifelse(share > 0, share, NA)), color = "gray75", linewidth = 0.35) +
  geom_text(aes(label = ifelse(share > 0, scales::percent(share, accuracy = 1), "")),
            size = 2.9, family = "HiraginoSans-W3",
            color = ifelse(reason_grid$share > 0.5, "white", "gray20")) +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      labels = scales::percent, name = "その理由を挙げた割合") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  pth + theme(plot.margin = margin(3, 10, 3, 3))

# --- 代替可能性の内訳（右パネル）---
# 【2026-09-11改訂】代替可能性が空値（NA）の資源は、元々「代替不可」等の
# 4類型のどこにも属さないため除外されていたが、確認したところこれらは
# 「他の資源の代替品として使われている植物」（is_substitute_material=TRUE、
# 結果2の代替可能性区分コード4）であり、独自の意味を持つカテゴリーである。
# そこで「代替品として使用」という4つ目の区分を追加し、着色・凡例に含める。
SUBST4 <- c("代替可", "代替困難", "代替不可", "代替品として使用")
SUBST4_PAL <- c("代替可" = "#4DAF4A", "代替困難" = "#FF7F00",
                "代替不可" = "#E41A1C", "代替品として使用" = "#6A51A3")

subst_share_17b_pp <- resource_df %>%
  filter(resource_taxon %in% taxon_denom$resource_taxon) %>%
  filter(!is.na(subst_score) | is_substitute_material) %>%
  mutate(subst_label = case_when(
    is_substitute_material ~ "代替品として使用",
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
  mutate(taxon_label = factor(paste0(resource_taxon, "（", n_fes, "祭り）"), levels = rev(label_order_17b)))

p17b_subst_paper <- ggplot(subst_share_17b_pp, aes(x = pct, y = taxon_label, fill = subst_label)) +
  geom_col(position = "stack", width = 0.72, na.rm = TRUE) +
  scale_fill_manual(values = SUBST4_PAL, name = "代替可能性", drop = FALSE) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  pth + theme(plot.margin = margin(3, 5, 3, 10))

p17b_paper <- (p17b_main_paper + p17b_subst_paper +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect")) &
  theme(legend.position = "bottom", legend.box = "vertical")

# 【注】10列の選定理由カテゴリーがあるため、半ページ幅には収まらない。
save_paper(p17b_paper, "図-5_17b_reason_by_plant.png",
           width = 7.8, height = max(7.0, nrow(taxon_denom) * 0.22 + 0.6))

# ==============================================================================
# 図-6 = 03b_plant_prevalence_by_pref
# ==============================================================================

p03b_paper <- p03b + noti + pth
p03b_paper$layers[[1]]$aes_params$colour <- "gray75"   # タイル罫線を白→灰に
p03b_paper$layers[[2]]$aes_params$size <- GT_MD

save_paper(p03b_paper, "図-6_03b_plant_prevalence_by_pref.png",
           width = 3.8, height = max(5, nrow(prev_plot) * 0.20))

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
