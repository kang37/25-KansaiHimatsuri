# ==============================================================================
# 神社・祭礼組織の生物多様性/自然保全関連の管理措置 — 独立分析
# ------------------------------------------------------------------------------
# 【位置づけ】本スクリプトは main2.R とは独立した新規ファイルであり、
# main2.R には一切手を加えない（ユーザー指示）。data_raw/分析内容まとめ.xlsx
# から必要なシートだけを直接読み込み、独自に完結させる。main2.R の
# resource_df 等は一切参照しない。
#
# 【目的】
#   1. 結果6（話題頻度・保全管理活動）の自由記述 mgmt_note を読み、各祭りの
#      管理措置を4段階に分類する（conservation_tier）:
#        A: 生態系・環境保全と明示的に紐づく措置（外部保全団体との連携、
#           湖岸整備・里山保全・植樹等）
#        B: 祭礼専用の計画的栽培（菜種・小麦・赤米等の農業的栽培）
#        C: 受動的な資源管理（境内・社叢の日常的資産管理、輪採り採取地の
#           分散など、保全を明示的に意図しない経験知）
#        D: 計画的な措置なし（購入依存・行き当たりばったりの採取のみ）
#      いずれにも該当しない場合は "other" とする。
#   2. 各祭りの景観多様性（結果5の調達地景観タイプ数）と嵌入度（結果4の
#      調達方法から求めた平均自給度）を独自に集計する。
#   3. conservation_tier と 景観多様性・嵌入度 を突き合わせ、「明示的に
#      保全と紐づく祭り」が「複数景観・高自給」の祭りと重なるかを検証する。
#
# 【簡略化した点（main2.R との差分）】
#   - 現時点で使われていない資源（結果1 current_use=0、全155件中13件）を
#     景観多様性・嵌入度の集計から除外していない。結果4/結果5の単一シート
#     読み込みで完結させるための簡略化で、影響は最大でも数%程度と見積もる。
#     厳密に揃えたい場合は main2.R の resource_df を参照すること。
#   - conservation_tier はキーワードに基づく機械的分類であり、分類根拠に
#     使った本文の抜粋を必ず併記して出力する。目視での検証を前提とする。
# ==============================================================================

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
})

MATOME_PATH <- "data_raw/分析内容まとめ.xlsx"
OUTPUT_DIR  <- "data_proc/conservation_measures"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# シート順（2026-09時点の構成。"受访者信息"が先頭に入っているため
# 旧結果1〜6は2〜7にシフトしている）
MT_SHEET <- c(informant = 1, daily = 2, use = 3, pref = 4,
             method = 5, landscape = 6, engagement = 7)

PREF_ORDER <- c("滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県")
PREF_PAL <- c(
  "滋賀県"   = "#1B9E77", "京都府"   = "#D95F02", "大阪府"   = "#7570B3",
  "兵庫県"   = "#E7298A", "奈良県"   = "#66A61E", "和歌山県" = "#E6AB02"
)
NONPLANT_TAXA <- c("アルミ・灯油", "ワタ（綿）", "タオル（綿）", "布類（材質不明）")
LANDSCAPE_VOCAB <- c("二次林", "人工林", "竹林", "神社林", "海岸防災林", "庭園",
                     "水田", "湿地", "畑", "荒地", "木材流通")

clean <- function(x) str_squish(str_replace_all(as.character(x), "[\r\n]+", " "))
read_mt <- function(name) read_excel(MATOME_PATH, sheet = MT_SHEET[[name]], col_names = TRUE)

# ------------------------------------------------------------------------------
# 1. 府県マッピング（結果3）
# ------------------------------------------------------------------------------
pref_map <- read_mt("pref") %>%
  rename(pref = 1, festival = 2) %>%
  transmute(festival = clean(festival), pref = factor(clean(pref), levels = PREF_ORDER))

# ------------------------------------------------------------------------------
# 2. 植物資源種数（結果1。現在使用中のみ）
# ------------------------------------------------------------------------------
n_resources <- read_mt("daily") %>%
  rename(festival = 1, taxon_kind = 2, resource_raw = 3,
         daily_class = 4, daily_note = 5, current_use = 6) %>%
  mutate(festival = clean(festival), taxon_kind = clean(taxon_kind),
         current_use = suppressWarnings(as.integer(current_use))) %>%
  filter(current_use == 1, !(taxon_kind %in% NONPLANT_TAXA)) %>%
  group_by(festival) %>%
  summarise(n_resources = n_distinct(taxon_kind), .groups = "drop")

# ------------------------------------------------------------------------------
# 3. 嵌入度（結果4 調達方法 → 平均自給度スコア 1〜3）
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

embed_by_festival <- read_mt("method") %>%
  rename(festival = 1, taxon_kind = 2, part = 3, method_class = 4,
         method_note = 5, timing_raw = 6) %>%
  mutate(festival = clean(festival), method_class = clean(method_class),
         embed_score = code_embeddedness(method_class)) %>%
  filter(!is.na(embed_score)) %>%
  group_by(festival) %>%
  summarise(mean_embed = mean(embed_score), n_embed_records = n(), .groups = "drop")

# ------------------------------------------------------------------------------
# 4. 景観多様性（結果5 調達地の景観 → 祭りごとの景観タイプ数）
# ------------------------------------------------------------------------------
code_landscape <- function(x) {
  vapply(as.character(x), function(z) {
    if (is.na(z)) return(NA_character_)
    hit <- LANDSCAPE_VOCAB[str_detect(z, fixed(LANDSCAPE_VOCAB))]
    if (!length(hit)) return(NA_character_)
    ord <- order(vapply(hit, function(h) str_locate(z, fixed(h))[1, 1], numeric(1)))
    paste(hit[ord], collapse = "|")
  }, character(1), USE.NAMES = FALSE)
}

habitat_diversity <- read_mt("landscape") %>%
  rename(festival = 1, taxon_kind = 2, part = 3, change_class = 4,
         change_note = 5, landscape_raw = 6) %>%
  mutate(festival = clean(festival), landscape_all = code_landscape(clean(landscape_raw))) %>%
  filter(!is.na(landscape_all)) %>%
  separate_rows(landscape_all, sep = "\\|") %>%
  group_by(festival) %>%
  summarise(n_habitats = n_distinct(landscape_all),
            habitats = paste(sort(unique(landscape_all)), collapse = "・"),
            .groups = "drop")

# ------------------------------------------------------------------------------
# 5. 保全措置の4段階分類（結果6 mgmt_note の自由記述をキーワードで分類）
# ------------------------------------------------------------------------------
# 判定は上から順に試し、最初にヒットした段階を採用する（A→B→C→D→other）。
# A・Bを先に見るのは、1つのmgmt_noteに複数資源の記述が併記され、片方に
# 「計画栽培は行っていない」という否定文言があっても、別の資源で実際に
# 保全・栽培をしている場合はそちらを優先させるため
# （例：稲引き樽引き神事＝赤米は特別栽培＝B、赤松は保全せず＝別記述）。
CONS_RULES <- list(
  A = "保全|環境保護|環境教育|里山|ネットワークが.*(刈|保護|整備)|湖岸整備|間伐|植樹|育樹|鹿防護|アドプト|山麓保全|水質浄化",
  B = "栽培|植栽|播種|施肥|植替え|育成|種まき|苗",
  C = "社叢管理|境内管理|資産管理|採取地.*分散|回復.*年程度|少なくとも.*年"
)
NEG_PAT <- "計画栽培|計画的.*栽培|保全活動は行|植林.*行わず|行っていない|^ない$|^なし$"

# 【重要】「栽培・保全する活動は…行っていない」のように、A/Bの陽性キーワード
# （保全・環境・栽培等）が同じ節の中で明示的に否定されているケースがある
# （例：吉祥草寺茅原大とんど「植物を計画栽培・保全する活動は基本的に行って
# いない」、東光寺鬼会「計画的に栽培・育成する活動は現在行っていない」、
# ほうらんや火祭り「資源保全制度はなく」）。素朴なキーワード一致だとこれらを
# 誤って陽性判定してしまうため、判定の前に「陽性キーワード＋（15字以内）＋
# 否定語」というパターンをまず本文から削除し、否定された節のキーワードが
# 後続のA/B判定に紛れ込まないようにする。
NEGATED_SPAN <- "(保全|環境保護|環境教育|里山|植樹|育樹|栽培|植栽|育成)[^。；]{0,15}(行わず|行っていない|活動は行|はなく|はない|ではなく|していない|というより)"

code_conservation_tier <- function(note) {
  s_orig <- clean(note)
  if (is.na(s_orig) || s_orig %in% c("", "NA", "ない", "なし"))
    return(list(tier = "D", hit = "(記述なし)"))
  s <- str_remove_all(s_orig, NEGATED_SPAN)
  m <- str_extract(s, CONS_RULES[["A"]]); if (!is.na(m)) return(list(tier = "A", hit = m))
  m <- str_extract(s, CONS_RULES[["B"]]); if (!is.na(m)) return(list(tier = "B", hit = m))
  m <- str_extract(s, CONS_RULES[["C"]]); if (!is.na(m)) return(list(tier = "C", hit = m))
  m <- str_extract(s_orig, NEG_PAT); if (!is.na(m)) return(list(tier = "D", hit = m))
  list(tier = "other", hit = NA_character_)
}

engagement <- read_mt("engagement") %>%
  rename(festival = 1, topic_class = 2, topic_note = 3,
         mgmt_class = 4, mgmt_note = 5) %>%
  mutate(festival = clean(festival), mgmt_note = clean(mgmt_note))

tier_result <- lapply(engagement$mgmt_note, code_conservation_tier)
engagement$conservation_tier <- vapply(tier_result, function(z) z$tier, character(1))
engagement$tier_evidence      <- vapply(tier_result, function(z) z$hit, character(1))

TIER_LABELS <- c(
  A = "A: 生態系・環境保全と明示的に紐づく措置",
  B = "B: 祭礼専用の計画的栽培",
  C = "C: 受動的な資源管理（経験知ベース）",
  D = "D: 計画的な措置なし",
  other = "分類不能（要目視確認）"
)
engagement <- engagement %>%
  mutate(conservation_tier = factor(conservation_tier, levels = names(TIER_LABELS)),
         tier_label = factor(unname(TIER_LABELS[as.character(conservation_tier)]),
                             levels = unname(TIER_LABELS)))

cat("\n=== 保全措置の4段階分類（祭り別） ===\n")
print(as.data.frame(engagement %>%
  select(festival, conservation_tier, tier_evidence) %>%
  arrange(conservation_tier)))

cat("\n=== 段階別の件数 ===\n")
print(table(engagement$tier_label))

# ------------------------------------------------------------------------------
# 6. 突き合わせ：保全段階 × 景観多様性 × 嵌入度
# ------------------------------------------------------------------------------
joined <- engagement %>%
  select(festival, conservation_tier, tier_label, tier_evidence) %>%
  left_join(pref_map, by = "festival") %>%
  left_join(n_resources, by = "festival") %>%
  left_join(embed_by_festival, by = "festival") %>%
  left_join(habitat_diversity, by = "festival")

cat("\n=== 段階別の景観多様性・嵌入度（平均） ===\n")
print(as.data.frame(joined %>%
  group_by(tier_label) %>%
  summarise(n = n(),
            景観タイプ数_平均 = round(mean(n_habitats, na.rm = TRUE), 2),
            嵌入度_平均       = round(mean(mean_embed, na.rm = TRUE), 2),
            .groups = "drop")))

write.csv(joined, file.path(OUTPUT_DIR, "conservation_tier_by_festival.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 図: 景観多様性 × 嵌入度、色 = 保全措置の段階
# ------------------------------------------------------------------------------
TIER_COLORS <- c(
  "A: 生態系・環境保全と明示的に紐づく措置" = "#1A6A1A",
  "B: 祭礼専用の計画的栽培"                 = "#74C476",
  "C: 受動的な資源管理（経験知ベース）"     = "#FDB863",
  "D: 計画的な措置なし"                     = "#D62728",
  "分類不能（要目視確認）"                   = "gray60"
)

p1 <- joined %>%
  filter(!is.na(n_habitats), !is.na(mean_embed)) %>%
  ggplot(aes(x = n_habitats, y = mean_embed)) +
  annotate("rect", xmin = 3.5, xmax = Inf, ymin = 2, ymax = Inf,
           fill = "#FFF3CD", alpha = 0.5) +
  geom_jitter(aes(color = tier_label, size = n_resources),
              width = 0.08, height = 0.05, alpha = 0.9) +
  geom_text_repel(aes(label = festival), size = 2.7, max.overlaps = 25,
                  family = "HiraginoSans-W3") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(breaks = 1:3, limits = c(0.8, 3.2)) +
  scale_color_manual(values = TIER_COLORS, name = "保全措置の段階") +
  scale_size_continuous(range = c(2, 7), name = "植物資源種数") +
  labs(
    title = "保全措置の段階は「複数景観・高自給」の祭りと重なるか",
    subtitle = paste0("背景の黄色帯 = 景観タイプ4以上 かつ 嵌入度2以上の領域\n",
                      "conservation_tier はmgmt_noteのキーワード分類（要目視検証、tier_evidence列参照）"),
    x = "景観タイプ数（調達先の多様性）", y = "嵌入度（平均自給度、3=自ら採取・栽培）"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "conservation_tier_x_habitat_x_embed.png"), p1,
       width = 10.5, height = 7.5, dpi = 150)

# ------------------------------------------------------------------------------
# 図: 段階別の景観タイプ数・嵌入度の分布（箱ひげ + 実データ点）
# ------------------------------------------------------------------------------
p2a <- joined %>% filter(!is.na(n_habitats)) %>%
  ggplot(aes(x = tier_label, y = n_habitats)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = "gray92") +
  geom_jitter(aes(color = pref), width = 0.12, height = 0.05, size = 2.6, alpha = 0.85) +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  scale_y_continuous(breaks = 1:6) +
  labs(title = "保全措置の段階別・景観タイプ数", x = NULL, y = "景観タイプ数") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        plot.title = element_text(size = 11, face = "bold"))

p2b <- joined %>% filter(!is.na(mean_embed)) %>%
  ggplot(aes(x = tier_label, y = mean_embed)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = "gray92") +
  geom_jitter(aes(color = pref), width = 0.12, height = 0.05, size = 2.6, alpha = 0.85) +
  scale_color_manual(values = PREF_PAL, guide = "none") +
  scale_y_continuous(breaks = 1:3, limits = c(0.8, 3.2)) +
  labs(title = "保全措置の段階別・嵌入度", x = NULL, y = "嵌入度（平均自給度）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        plot.title = element_text(size = 11, face = "bold"))

p2 <- patchwork::wrap_plots(p2a, p2b, ncol = 2, guides = "collect")
ggsave(file.path(OUTPUT_DIR, "conservation_tier_boxplots.png"), p2,
       width = 11, height = 5.5, dpi = 150)

cat("\n完了。出力先:", OUTPUT_DIR, "\n")
cat("  conservation_tier_by_festival.csv      祭り別の分類結果・根拠・景観多様性・嵌入度\n")
cat("  conservation_tier_x_habitat_x_embed.png 散布図\n")
cat("  conservation_tier_boxplots.png          段階別の箱ひげ図\n")
