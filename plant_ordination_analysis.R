# ==============================================================================
# 火祭りが使用する植物組成の多変量解析（NMDS・PCA）
# ------------------------------------------------------------------------------
# 【位置づけ】main2.R・paper_figures.R とは独立した新規スクリプト。それらを
# source せず、data_raw/raw_data_agg.xlsx と data_raw/分析内容まとめ.xlsx を
# 直接読み込んで完結させる（他スクリプトとの副作用の分離、同時編集の衝突を
# 避けるため）。
#
# 【目的】
#   1. 祭り×植物（在/不在）行列から、祭りが植物組成の上でいくつかの
#      クラスタに分かれるかを NMDS（Jaccard非類似度）と PCA の両方で見る。
#   2. 見えたクラスタ/序列構造が、既知の変数（府県・調達嵌入度・景観多様性・
#      祭りの規模タイプ）のどれかと対応するかを PERMANOVA で検定する。
#
# 【NMDS vs PCA について】
#   植物在/不在行列は「多くの種が1〜2祭りにしか出現しない」非常に疎な
#   データで、共有ゼロ（両方に無い＝類似とみなされてしまう）の影響を強く
#   受けるユークリッド距離ベースのPCAには本来不向き。群集生態学の標準は
#   Jaccard/Bray-Curtis非類似度に基づくNMDSまたはPCoAだが、本スクリプトは
#   比較のためPCA（Hellinger変換後）も併記する。
# ==============================================================================

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
  library(vegan)
})

RAW_AGG_PATH <- "data_raw/raw_data_agg.xlsx"
MATOME_PATH  <- "data_raw/分析内容まとめ.xlsx"
OUTPUT_DIR   <- "data_proc/plant_ordination"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

clean <- function(x) str_squish(str_replace_all(as.character(x), "[\r\n]+", " "))

PREF_ORDER <- c("滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県")
PREF_PAL <- c(
  "滋賀県"   = "#1B9E77", "京都府"   = "#D95F02", "大阪府"   = "#7570B3",
  "兵庫県"   = "#E7298A", "奈良県"   = "#66A61E", "和歌山県" = "#E6AB02"
)
NONPLANT_TAXA <- c("アルミ・灯油", "ワタ（綿）", "タオル（綿）", "布類（材質不明）")

# ------------------------------------------------------------------------------
# 1. 資源レコード読み込み（raw_data_agg.xlsx、20列。2026-09-14時点の構成）
# ------------------------------------------------------------------------------
agg_raw <- suppressMessages(read_excel(RAW_AGG_PATH, sheet = 1, col_names = TRUE))
stopifnot("raw_data_agg.xlsxの列数が想定(20列)と異なる。列構成を確認すること" = ncol(agg_raw) == 20)
names(agg_raw) <- c(
  "festival", "taxon_kind", "part_category", "part", "daily_class", "daily_note",
  "current_use", "in_scope", "use_class", "use_note", "reason_raw",
  "subst_class", "subst_note", "method_class", "method_note", "timing_raw",
  "change_class", "change_note", "landscape_raw", "landscape_note"
)

resource_df <- agg_raw %>%
  mutate(across(c(festival, taxon_kind, method_class, landscape_raw), clean),
         current_use = suppressWarnings(as.integer(current_use)),
         in_scope    = suppressWarnings(as.integer(in_scope))) %>%
  filter(in_scope == 1, current_use == 1, !(taxon_kind %in% NONPLANT_TAXA))

cat("=== 資源レコード:", nrow(resource_df), "件 /",
    n_distinct(resource_df$festival), "祭り /",
    n_distinct(resource_df$taxon_kind), "分類群 ===\n")

# ------------------------------------------------------------------------------
# 2. 府県マッピング（分析内容まとめ.xlsx 結果3）
# ------------------------------------------------------------------------------
pref_map <- read_excel(MATOME_PATH, sheet = 4, col_names = TRUE)[, 1:2]
names(pref_map) <- c("pref", "festival")
pref_map <- pref_map %>% mutate(across(everything(), clean))
FESTIVAL_PREF <- setNames(pref_map$pref, pref_map$festival)

# ------------------------------------------------------------------------------
# 3. 調達嵌入度（結果4 method_classから平均自給度スコア 1〜3）
# ------------------------------------------------------------------------------
bracket_cat <- function(x) {
  vapply(as.character(x), function(z) {
    if (is.na(z)) return(NA_character_)
    m <- str_match_all(z, "【([^】]+)】")[[1]]
    if (nrow(m) == 0) return(NA_character_)
    paste(m[, 2], collapse = "|")
  }, character(1), USE.NAMES = FALSE)
}
EMBED_LEVELS <- list(
  "3" = c("氏子・保存会採取", "氏子・保存会栽培", "協働採取", "協働栽培", "地域内採取"),
  "2" = c("地域住民提供", "地元農家提供", "地元農家委託栽培", "地域内寺社提供",
          "地域内事業者提供", "副産物・再利用", "寄付・奉納"),
  "1" = c("地域外購入", "地域内購入", "購入", "外部業者委託", "外部協力者提供",
          "外部協力者採取", "外部協力者仲介", "地域外農家提供",
          "地域外農家委託栽培", "農家提供")
)
winning_method_cat <- function(cs) {
  if (is.na(cs)) return(NA_character_)
  parts <- str_trim(str_split(cs, "[／|]")[[1]])
  parts <- str_replace_all(parts, "[（(].*?[）)]", "")
  parts <- parts[parts != ""]
  if (!length(parts)) return(NA_character_)
  sc <- vapply(parts, function(p) {
    lv <- names(EMBED_LEVELS)[vapply(EMBED_LEVELS, function(v) p %in% v, logical(1))]
    if (length(lv)) as.integer(lv[1]) else NA_integer_
  }, integer(1))
  if (all(is.na(sc))) parts[1] else parts[which.max(sc)]
}
code_embeddedness <- function(x) {
  cat_str <- bracket_cat(x)
  vapply(cat_str, function(cs) {
    winner <- winning_method_cat(cs)
    if (is.na(winner)) return(NA_integer_)
    for (lv in names(EMBED_LEVELS)) if (winner %in% EMBED_LEVELS[[lv]]) return(as.integer(lv))
    NA_integer_
  }, integer(1), USE.NAMES = FALSE)
}

embed_by_festival <- resource_df %>%
  mutate(embed_score = code_embeddedness(method_class)) %>%
  filter(!is.na(embed_score)) %>%
  group_by(festival) %>%
  summarise(mean_embed = mean(embed_score), .groups = "drop")

# ------------------------------------------------------------------------------
# 4. 景観多様性（結果5 landscape_rawから調達先景観タイプ数）
# ------------------------------------------------------------------------------
LANDSCAPE_VOCAB <- c("二次林", "人工林", "竹林", "神社林", "海岸防災林", "庭園",
                     "水田", "湿地", "畑", "荒地", "木材流通")
code_landscape <- function(x) {
  vapply(as.character(x), function(z) {
    if (is.na(z)) return(NA_character_)
    hit <- LANDSCAPE_VOCAB[str_detect(z, fixed(LANDSCAPE_VOCAB))]
    if (!length(hit)) return(NA_character_)
    paste(sort(unique(hit)), collapse = "|")
  }, character(1), USE.NAMES = FALSE)
}
habitat_diversity <- resource_df %>%
  mutate(landscape_all = code_landscape(landscape_raw)) %>%
  filter(!is.na(landscape_all)) %>%
  separate_rows(landscape_all, sep = "\\|") %>%
  group_by(festival) %>%
  summarise(n_habitats = n_distinct(landscape_all), .groups = "drop")

# ------------------------------------------------------------------------------
# 5. 祭りの規模タイプ（関係者数・観光客数。図10と同じ手作業表を再掲）
# ------------------------------------------------------------------------------
scale_df <- tribble(
  ~festival,                    ~participants, ~tourists,
  "巽神社松明",                  36,           0,
  "鞍馬の火祭",                  136,          5000,
  "三栖の火祭",                  150,          3500,
  "松明を次世代に送る会",        1000,         0,
  "雄琴学区ヨシ松明一斉点火",    500,          NA,
  "太郎坊宮の火祭り",            100,          0,
  "信楽の火祭り",                180,          3000,
  "勝部の火祭り",                150,          2500,
  "近江八幡左義長祭り",          600,          80000,
  "大文字送り火",                700,          350000,
  "八幡祭り",                    550,          16500,
  "王の浜若宮神社",              30,           30,
  "小田神社",                    60,           400,
  "大嶋奥津嶋神社",              50,           20,
  "往馬大社",                    200,          5000,
  "熊野速玉大社",                2000,         NA,
  "稲むらの火祭り",              200,          600,
  "熊野那智",                    120,          3000,
  "嵯峨のお松明式",              40,           1000,
  "広河原松上げ",                80,           1000,
  "がんがら火祭り",              53,           10000,
  "まんどろ火祭り",              16,           4000,
  "麦わら松明",                  10,           100,
  "東光寺鬼会",                  21,           200,
  "吉祥草寺茅原大とんど",        170,          3500,
  "稲引き樽引き神事",            20,           60,
  "花背松上げ",                  30,           300,
  "雲ケ畑松上げ",                10,           30,
  "湯村火祭り",                  53,           300,
  "ほうらんや火祭り",            10,           100
) %>%
  mutate(
    participants_class = ifelse(participants >= 200, "大規模", "小規模"),
    tourists_class      = case_when(
      is.na(tourists)  ~ "地域型",
      tourists >= 1000 ~ "観光型",
      TRUE             ~ "地域型"
    ),
    festival_type = paste0(participants_class, "・", tourists_class)
  )

# ------------------------------------------------------------------------------
# 6. 祭り×植物 在/不在行列
# ------------------------------------------------------------------------------
pa_long <- resource_df %>% distinct(festival, taxon_kind)
festivals <- sort(unique(pa_long$festival))
taxa      <- sort(unique(pa_long$taxon_kind))

pa_mat <- table(pa_long$festival, pa_long$taxon_kind)
pa_mat <- (pa_mat > 0) * 1
pa_mat <- pa_mat[festivals, taxa]

cat("\n=== 祭り×植物 行列:", nrow(pa_mat), "祭り ×", ncol(pa_mat), "分類群 ===\n")
cat("行列の密度（1の割合）:", round(mean(pa_mat), 3), "\n")
cat("1祭りにしか出現しない植物の数:", sum(colSums(pa_mat) == 1), "/", ncol(pa_mat), "\n")

# 補助変数を祭り順に整列
meta <- tibble(festival = festivals) %>%
  mutate(pref = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER)) %>%
  left_join(embed_by_festival,   by = "festival") %>%
  left_join(habitat_diversity,   by = "festival") %>%
  left_join(scale_df %>% select(festival, participants, tourists, festival_type),
            by = "festival") %>%
  left_join(pa_long %>% count(festival, name = "n_taxa"), by = "festival")

stopifnot(all(meta$festival == festivals))

write.csv(as.data.frame(unclass(pa_mat)) %>% tibble::rownames_to_column("festival"),
          file.path(OUTPUT_DIR, "festival_x_plant_matrix.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(meta, file.path(OUTPUT_DIR, "festival_metadata.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 7. NMDS（Jaccard非類似度）
# ==============================================================================
set.seed(20260914)
jac <- vegdist(pa_mat, method = "jaccard")
nmds <- metaMDS(jac, k = 2, trymax = 100, trace = FALSE)
cat("\n=== NMDS (Jaccard) ===\nstress =", round(nmds$stress, 3),
    "（目安：<0.10 良好, 0.10-0.20 許容, >0.20 解釈に注意。祭り数30と少なく",
    "疎な行列のためstressはやや高めに出やすい）\n")

nmds_scores <- as.data.frame(scores(nmds, display = "sites")) %>%
  tibble::rownames_to_column("festival") %>%
  left_join(meta, by = "festival")

# ------------------------------------------------------------------------------
# 7a. NMDS散布図：府県で色分け
# ------------------------------------------------------------------------------
p_nmds_pref <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = pref, size = n_taxa), alpha = 0.85) +
  geom_text_repel(aes(label = festival), size = 2.7, max.overlaps = 30,
                  family = "HiraginoSans-W3") +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  scale_size_continuous(range = c(2, 7), name = "植物種数") +
  labs(
    title = "火祭りの植物組成 NMDS（Jaccard非類似度）— 府県別",
    subtitle = paste0("stress = ", round(nmds$stress, 3),
                      "　点が近いほど使用する植物の組み合わせが似ている"),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "nmds_by_pref.png"), p_nmds_pref, width = 10, height = 7.5, dpi = 150)

# ------------------------------------------------------------------------------
# 7b. NMDS散布図：嵌入度・景観多様性・祭りタイプ
# ------------------------------------------------------------------------------
p_nmds_embed <- ggplot(nmds_scores %>% filter(!is.na(mean_embed)),
                       aes(x = NMDS1, y = NMDS2, color = mean_embed)) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_text_repel(aes(label = festival), size = 2.5, max.overlaps = 30,
                  family = "HiraginoSans-W3", color = "gray20") +
  scale_color_gradient(low = "#D62728", high = "#1A6A1A", name = "調達嵌入度") +
  labs(title = "NMDS — 調達嵌入度", x = "NMDS1", y = "NMDS2") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 11, face = "bold"))

p_nmds_habitat <- ggplot(nmds_scores %>% filter(!is.na(n_habitats)),
                         aes(x = NMDS1, y = NMDS2, color = n_habitats)) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_text_repel(aes(label = festival), size = 2.5, max.overlaps = 30,
                  family = "HiraginoSans-W3", color = "gray20") +
  scale_color_gradient(low = "#F7FBFF", high = "#08519C", name = "景観タイプ数") +
  labs(title = "NMDS — 景観多様性", x = "NMDS1", y = "NMDS2") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 11, face = "bold"))

p_nmds_type <- ggplot(nmds_scores %>% filter(!is.na(festival_type)),
                      aes(x = NMDS1, y = NMDS2, color = festival_type)) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_text_repel(aes(label = festival), size = 2.5, max.overlaps = 30,
                  family = "HiraginoSans-W3", color = "gray20") +
  scale_color_brewer(palette = "Set1", name = "祭りタイプ") +
  labs(title = "NMDS — 祭りタイプ（規模×観光）", x = "NMDS1", y = "NMDS2") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 11, face = "bold"))

p_nmds_grid <- patchwork::wrap_plots(p_nmds_embed, p_nmds_habitat, p_nmds_type, ncol = 2) +
  patchwork::plot_annotation(
    title = "NMDS散布図 — 既知の変数との対応",
    theme = theme(plot.title = element_text(face = "bold", family = "HiraginoSans-W3"))
  )
ggsave(file.path(OUTPUT_DIR, "nmds_by_covariates.png"), p_nmds_grid, width = 12, height = 10, dpi = 150)

# ==============================================================================
# 8. 階層クラスタリング（Jaccard非類似度、Ward法）でクラスタを定義
# ==============================================================================
hc <- hclust(jac, method = "ward.D2")
# クラスタ数はデンドログラムを見て決めるのが本来だが、ここでは平均シルエット
# 幅が最大になるkを機械的に選ぶ（k=2..6の範囲で探索）。
sil_width <- sapply(2:6, function(k) {
  cl <- cutree(hc, k = k)
  mean(cluster::silhouette(cl, jac)[, 3])
})
best_k <- (2:6)[which.max(sil_width)]
cat("\n=== 階層クラスタリング（Ward法, Jaccard）===\n")
cat("シルエット幅:", paste(sprintf("k=%d:%.3f", 2:6, sil_width), collapse = "  "), "\n")
cat("採用クラスタ数 k =", best_k, "（シルエット幅最大）\n")

cluster_id <- cutree(hc, k = best_k)
nmds_scores$cluster <- factor(cluster_id[nmds_scores$festival])

# factoextraは未導入のためbase Rのplot.hclustで代用
png(file.path(OUTPUT_DIR, "dendrogram.png"), width = 2400, height = 1500, res = 200)
par(family = "HiraginoSans-W3")
plot(hc, main = paste0("植物組成に基づく階層クラスタリング（Ward法, Jaccard, k=", best_k, "）"),
     xlab = "", sub = "", cex = 0.75)
rect.hclust(hc, k = best_k, border = RColorBrewer::brewer.pal(max(best_k, 3), "Set1"))
dev.off()

p_nmds_cluster <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.7, linetype = "dashed") +
  geom_point(aes(size = n_taxa), alpha = 0.85) +
  geom_text_repel(aes(label = festival), size = 2.7, max.overlaps = 30,
                  family = "HiraginoSans-W3") +
  scale_color_brewer(palette = "Set1", name = paste0("クラスタ (k=", best_k, ")")) +
  scale_size_continuous(range = c(2, 7), name = "植物種数") +
  labs(title = "NMDS — 植物組成に基づくクラスタ",
       subtitle = paste0("階層クラスタリング（Ward法、Jaccard非類似度）による ", best_k, " 群"),
       x = "NMDS1", y = "NMDS2") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(OUTPUT_DIR, "nmds_clusters.png"), p_nmds_cluster, width = 10, height = 7.5, dpi = 150)

cat("\n=== クラスタごとの祭り ===\n")
for (k in sort(unique(cluster_id))) {
  cat(sprintf("クラスタ%d: %s\n", k, paste(names(cluster_id)[cluster_id == k], collapse = "、")))
}

# クラスタ×府県 のクロス集計（カイ二乗/Fisher）
cross_pref <- table(cluster = nmds_scores$cluster, pref = nmds_scores$pref)
cat("\n=== クラスタ × 府県 クロス表 ===\n"); print(cross_pref)
ft_pref <- tryCatch(fisher.test(cross_pref, simulate.p.value = TRUE, B = 10000),
                    error = function(e) NULL)
if (!is.null(ft_pref)) cat("Fisher検定（シミュレーション） p =", round(ft_pref$p.value, 4), "\n")

# クラスタ×連続変数（embed, n_habitats, participants）のKruskal-Wallis検定
for (v in c("mean_embed", "n_habitats", "participants", "n_taxa")) {
  d <- nmds_scores %>% filter(!is.na(.data[[v]]))
  if (n_distinct(d$cluster) >= 2 && nrow(d) >= 5) {
    kw <- kruskal.test(d[[v]] ~ d$cluster)
    cat(sprintf("Kruskal-Wallis クラスタ別 %s: chi2=%.2f, df=%d, p=%.4f\n",
                v, kw$statistic, kw$parameter, kw$p.value))
  }
}

# ==============================================================================
# 9. PERMANOVA：植物組成（Jaccard）を各変数がどれだけ説明するか
# ------------------------------------------------------------------------------
# クラスタに切ってから検定する上のKruskal-Wallisとは異なり、PERMANOVAは
# 群集組成の非類似度行列を直接、連続変数・カテゴリ変数で説明する分散分析。
# サンプルが揃う祭りのみ使う（NAを含む変数は個別に検定）。
# ==============================================================================
cat("\n=== PERMANOVA（adonis2, 999置換）：各変数が植物組成の分散を説明する割合 ===\n")

run_permanova <- function(var_name) {
  d <- meta %>% filter(!is.na(.data[[var_name]]))
  if (nrow(d) < 5 || n_distinct(d[[var_name]]) < 2) {
    cat(sprintf("  %-16s スキップ（データ不足）\n", var_name)); return(invisible())
  }
  sub_mat <- pa_mat[d$festival, , drop = FALSE]
  sub_mat <- sub_mat[, colSums(sub_mat) > 0, drop = FALSE]
  sub_jac <- vegdist(sub_mat, method = "jaccard")
  form <- as.formula(paste("sub_jac ~", var_name))
  res <- adonis2(form, data = d, permutations = 999)
  cat(sprintf("  %-16s R2=%.3f  F=%.2f  p=%.4f  (n=%d)\n",
              var_name, res$R2[1], res$F[1], res$`Pr(>F)`[1], nrow(d)))
  res
}

perm_results <- lapply(c("pref", "mean_embed", "n_habitats", "festival_type", "participants"),
                       run_permanova)

# ==============================================================================
# 10. PCA（比較用。Hellinger変換 + prcomp）
# ==============================================================================
pa_hell <- decostand(pa_mat, method = "hellinger")
pca <- prcomp(pa_hell, center = TRUE, scale. = FALSE)
var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
cat("\n=== PCA（Hellinger変換）寄与率 ===\nPC1:", var_exp[1], "%  PC2:", var_exp[2], "%",
    " 累積:", round(sum(var_exp[1:2]), 1), "%\n")

pca_scores <- as.data.frame(pca$x[, 1:2]) %>%
  tibble::rownames_to_column("festival") %>%
  left_join(meta, by = "festival") %>%
  left_join(nmds_scores %>% select(festival, cluster), by = "festival")

p_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = pref)) +
  geom_point(aes(size = n_taxa), alpha = 0.85) +
  geom_text_repel(aes(label = festival), size = 2.7, max.overlaps = 30,
                  family = "HiraginoSans-W3") +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  scale_size_continuous(range = c(2, 7), name = "植物種数") +
  labs(
    title = "火祭りの植物組成 PCA（Hellinger変換）— 府県別",
    subtitle = paste0("PC1 ", var_exp[1], "% + PC2 ", var_exp[2], "% = 累積 ",
                      round(sum(var_exp[1:2]), 1), "%（NMDSとの比較用）"),
    x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)")
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(OUTPUT_DIR, "pca_by_pref.png"), p_pca, width = 10, height = 7.5, dpi = 150)

write.csv(nmds_scores, file.path(OUTPUT_DIR, "nmds_scores_with_clusters.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 11. 滋賀・京都・大阪の3府県のみでNMDS（凸包で囲む）
# ------------------------------------------------------------------------------
# 兵庫・奈良・和歌山は各3祭りしかなく、全体解析ではnが少ない府県の効果が
# 見えにくい可能性がある。標本数が相対的に多い3府県（滋賀11・京都7・大阪3）
# に絞り、Jaccard距離を作り直してNMDSを取り直す（全体のNMDS座標をそのまま
# 使うと、除外した祭りとの距離関係が残ったままになるため、非類似度の計算
# からやり直す必要がある）。
# ==============================================================================

convex_hull_df <- function(scores_df, x = "NMDS1", y = "NMDS2", group = "pref") {
  scores_df %>%
    group_by(.data[[group]]) %>%
    filter(n() >= 3) %>%   # 凸包を描くには最低3点必要
    slice(chull(.data[[x]], .data[[y]])) %>%
    ungroup()
}

sub_pref <- c("滋賀県", "京都府", "大阪府")
meta_sub <- meta %>% filter(pref %in% sub_pref)
pa_sub   <- pa_mat[meta_sub$festival, , drop = FALSE]
pa_sub   <- pa_sub[, colSums(pa_sub) > 0, drop = FALSE]

cat("\n=== 3府県サブセット（滋賀・京都・大阪）===\n")
cat("祭り数:", nrow(pa_sub), " 分類群数:", ncol(pa_sub), "\n")
print(table(meta_sub$pref))

set.seed(20260914)
jac_sub  <- vegdist(pa_sub, method = "jaccard")
nmds_sub <- metaMDS(jac_sub, k = 2, trymax = 100, trace = FALSE)
cat("stress =", round(nmds_sub$stress, 3), "\n")

nmds_sub_scores <- as.data.frame(scores(nmds_sub, display = "sites")) %>%
  tibble::rownames_to_column("festival") %>%
  left_join(meta_sub, by = "festival") %>%
  mutate(pref = droplevels(pref))

hulls_sub <- convex_hull_df(nmds_sub_scores)

p_nmds_sub <- ggplot(nmds_sub_scores, aes(x = NMDS1, y = NMDS2, color = pref, fill = pref)) +
  geom_polygon(data = hulls_sub, alpha = 0.12, linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(size = n_taxa), alpha = 0.9) +
  geom_text_repel(aes(label = festival), size = 3, max.overlaps = 30,
                  family = "HiraginoSans-W3", color = "gray15") +
  scale_color_manual(values = PREF_PAL[sub_pref], name = "都道府県") +
  scale_fill_manual(values = PREF_PAL[sub_pref], name = "都道府県") +
  scale_size_continuous(range = c(2.5, 8), name = "植物種数") +
  labs(
    title = "植物組成 NMDS — 滋賀・京都・大阪のみ（凸包で囲む）",
    subtitle = paste0("stress = ", round(nmds_sub$stress, 3),
                      "　祭り数: 滋賀", sum(meta_sub$pref == "滋賀県"),
                      "・京都", sum(meta_sub$pref == "京都府"),
                      "・大阪", sum(meta_sub$pref == "大阪府"),
                      "（大阪は3件のみのため凸包は三角形）"),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "nmds_3pref_hulls.png"), p_nmds_sub, width = 10.5, height = 8, dpi = 150)

perm_sub <- adonis2(jac_sub ~ pref, data = meta_sub %>% mutate(pref = droplevels(pref)),
                    permutations = 999)
cat("\nPERMANOVA（3府県のみ）: R2=", round(perm_sub$R2[1], 3),
    " F=", round(perm_sub$F[1], 2), " p=", round(perm_sub$`Pr(>F)`[1], 4), "\n")

# ==============================================================================
# 12. 植物分類を data_raw/plant_category.xlsx の6区分に集約して再解析
# ------------------------------------------------------------------------------
# 30分類群（taxon_kind）を「農作物／タケ・ササ類／草本／針葉樹／広葉樹／
# つる性木本」の6区分に集約する。広葉樹だけで13分類群を吸収するため情報量は
# 大きく失われるが、疎行列（1祭りだけの分類群が15/30）の影響を減らし、
# 「植物の種類」よりも粗い「生活形」のレベルで見た場合に構造が変わるかを見る。
# ==============================================================================

plant_cat <- read_excel("data_raw/plant_category.xlsx") %>%
  rename(category = 分類, taxon_kind = 植物) %>%
  mutate(across(everything(), clean))

.unmapped_taxa <- setdiff(colnames(pa_mat), plant_cat$taxon_kind)
if (length(.unmapped_taxa) > 0) {
  cat("\n【警告】plant_category.xlsxに対応がない分類群（集約から除外）:",
      paste(.unmapped_taxa, collapse = "、"), "\n")
}

cat_long <- resource_df %>%
  distinct(festival, taxon_kind) %>%
  inner_join(plant_cat, by = "taxon_kind")

cat("\n=== 6区分への集約 ===\n")
print(as.data.frame(plant_cat %>% count(category, name = "元の分類群数")))

pa_cat <- table(cat_long$festival, cat_long$category)
pa_cat <- (pa_cat > 0) * 1
pa_cat <- pa_cat[festivals, , drop = FALSE]

cat("\n祭り×生活形区分 行列:", nrow(pa_cat), "祭り ×", ncol(pa_cat), "区分\n")
cat("行列の密度:", round(mean(pa_cat), 3), "（分類群レベルでは0.148だった）\n")

set.seed(20260914)
jac_cat  <- vegdist(pa_cat, method = "jaccard")
nmds_cat <- metaMDS(jac_cat, k = 2, trymax = 100, trace = FALSE)
cat("stress =", round(nmds_cat$stress, 3), "\n")

nmds_cat_scores <- as.data.frame(scores(nmds_cat, display = "sites")) %>%
  tibble::rownames_to_column("festival") %>%
  left_join(meta, by = "festival")

hulls_cat <- convex_hull_df(nmds_cat_scores)

p_nmds_cat <- ggplot(nmds_cat_scores, aes(x = NMDS1, y = NMDS2, color = pref, fill = pref)) +
  geom_polygon(data = hulls_cat, alpha = 0.12, linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(size = n_taxa), alpha = 0.9) +
  geom_text_repel(aes(label = festival), size = 2.7, max.overlaps = 30,
                  family = "HiraginoSans-W3", color = "gray15") +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  scale_fill_manual(values = PREF_PAL, name = "都道府県") +
  scale_size_continuous(range = c(2, 7), name = "植物種数（元の分類群数）") +
  labs(
    title = "植物組成 NMDS — 生活形6区分に集約（農作物/タケ類/草本/針葉樹/広葉樹/つる性木本）",
    subtitle = paste0("stress = ", round(nmds_cat$stress, 3),
                      "　行列密度 ", round(mean(pa_cat), 3),
                      "（元の分類群レベルは0.148）"),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "nmds_category_by_pref.png"), p_nmds_cat, width = 11, height = 8, dpi = 150)

cat("\n=== PERMANOVA（生活形6区分, 全30祭り）===\n")
run_permanova_mat <- function(var_name, mat) {
  d <- meta %>% filter(!is.na(.data[[var_name]]))
  if (nrow(d) < 5 || n_distinct(d[[var_name]]) < 2) {
    cat(sprintf("  %-16s スキップ（データ不足）\n", var_name)); return(invisible())
  }
  sub_mat <- mat[d$festival, , drop = FALSE]
  sub_mat <- sub_mat[, colSums(sub_mat) > 0, drop = FALSE]
  sub_jac <- vegdist(sub_mat, method = "jaccard")
  form <- as.formula(paste("sub_jac ~", var_name))
  res <- adonis2(form, data = d, permutations = 999)
  cat(sprintf("  %-16s R2=%.3f  F=%.2f  p=%.4f  (n=%d)\n",
              var_name, res$R2[1], res$F[1], res$`Pr(>F)`[1], nrow(d)))
}
invisible(lapply(c("pref", "mean_embed", "n_habitats", "festival_type", "participants"),
                 run_permanova_mat, mat = pa_cat))

write.csv(as.data.frame(unclass(pa_cat)) %>% tibble::rownames_to_column("festival"),
          file.path(OUTPUT_DIR, "festival_x_category_matrix.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(nmds_sub_scores, file.path(OUTPUT_DIR, "nmds_3pref_scores.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(nmds_cat_scores, file.path(OUTPUT_DIR, "nmds_category_scores.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n完了。出力先:", OUTPUT_DIR, "\n")
cat("  nmds_by_pref.png / nmds_by_covariates.png / nmds_clusters.png / dendrogram.png\n")
cat("  pca_by_pref.png\n")
cat("  nmds_3pref_hulls.png              滋賀・京都・大阪のみ、凸包つき\n")
cat("  nmds_category_by_pref.png         植物を6区分に集約、凸包つき\n")
cat("  festival_x_plant_matrix.csv / festival_x_category_matrix.csv\n")
cat("  nmds_scores_with_clusters.csv / nmds_3pref_scores.csv / nmds_category_scores.csv\n")
