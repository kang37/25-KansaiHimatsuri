# ==============================================================================
# 火祭り資源と組織調査結果 - 統計分析スクリプト（v3）
# データ: data_raw/火祭の資源と組織調査結果.xlsx
# 作成日: 2026-06-27
# 出力先: data_proc/
#
# 分析項目:
#   01 協力者年齢（複数協力者を個別表示）
#   02 関係者数・観光客数トレンド（横ばい=黄色）
#   03 植物資源出現頻度（全件）
#   04 植物資源種数（祭り別）
#   05 組織の有無
#   06 祭り目的キーワード頻度
#   07 信仰キーワード頻度
#   08 近年の課題（自由記述のキーワード分類）
#   09 植物資源 × 祭り マトリクス
#   10 関係者数 vs 観光客数 散布図
#   11 規模（関係者数） vs 植物資源種数
#   12 生息地多様性 vs 植物資源種数
#   13 植物資源 × 信仰 クロス分析
#   14 植物資源 × 祭り目的 クロス分析
#   --- 生物文化多様性分析 ---
#   15 文化的关键种（代替可能性 × 出現頻度）
#   16 文化-生態嵌入度（調達方法の地域性）
#   17 伝統生態知識（TEK）の深さ（植物の選定理由）
#   18 生息地依存ネットワーク（祭り × 景観タイプ）
#   19 文化-生態脆弱性（嵌入度変化 × 課題）
# ==============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(ggrepel)   # install.packages("ggrepel") if needed
library(patchwork) # 図01の左右並置に使用
library(writexl)  # 選定理由コーディング仕様書の出力に使用

# 2026-09-02 更新：30シート版（新規16祭り追加）に切替
# 旧: DATA_PATH <- "data_raw/火祭の資源と組織調査結果.xlsx"  (14シート, 出力先 data_proc/)
DATA_PATH   <- "data_raw/火祭の資源と組織調査結果_20260830.xlsx"
# 資源レベルの変数（日常利用・利用方法・選定理由・代替可能性・調達方法・
# 調達時期・調達地の変化・調達地の景観）は、原票の表記ゆれとコード揺れを
# 整理し終えた「分析内容まとめ.xlsx」から読む。
# 原票（DATA_PATH）から読むのは祭りレベルの変数のみ：
#   協力者年齢／関係者数・観光客数とその変化／信仰／祭り目的／保存会・氏子組織／
#   火祭り中心世代／近年の課題／社会意義
MATOME_PATH <- "data_raw/分析内容まとめ.xlsx"
OUTPUT_DIR  <- "data_proc/20260902"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 祭り所在府県（図の並び順のグループ化に使用）
# ------------------------------------------------------------------------------
# 出典：data_raw/近畿地方火祭り.xlsx（都道府県・市町村欄）および調査票の
#       「関係社寺名」「氏子地域範囲」欄。順序は本研究の近畿圏の定義に合わせる。
PREF_ORDER <- c("滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県")

# 府県は 分析内容まとめ.xlsx の「結果3」府県欄を出典とする（読み込みは 1c の後）。
# 手作業で作った旧マッピングと30件すべて一致することを確認済み。
FESTIVAL_PREF <- NULL   # 1c の読み込み後に設定する

# ------------------------------------------------------------------------------
# 抽出調査の府県別ウェイト（事後層化）
# ------------------------------------------------------------------------------
# 【なぜ必要か】
# 30祭りの調査対象は府県ごとの抽出率が大きく異なる。素の「利用祭り数」は
# 抽出の多い府県で使われる植物を過大に、抽出の少ない府県に偏在する植物を
# 過小に見せてしまう。そこで母集団（近畿地方火祭り.xlsx で把握した156件）の
# 府県構成に合わせて事後層化ウェイト w = N_府県 / n_府県 を与える。
#
# POP_FRAME は data_raw/近畿地方火祭り.xlsx の都道府県欄の集計。
# 同ファイルで都道府県が空欄の3件（箕面市・堺市・河内長野市）は市町村から
# 大阪府に算入している。
#
# 【限界（結果の解釈時に必ず併記すること）】
#   1. 156件の母集団自体が悉皆ではない（本文でも「網羅するものではない」と明記）。
#      特に大阪府は4件と極端に少なく、実態より過小に把握されている可能性が高い。
#      → 大阪府のウェイトは小さくなりすぎている恐れがある。
#   2. 府県内の抽出は無作為ではない（有意抽出）。ウェイトが補正するのは
#      府県間の構成比のみで、府県内の選択バイアスは補正できない。
#   3. 府県あたりの標本が3〜11件と小さく、府県内の比率は粗い（3件なら0/⅓/⅔/1）。
#      → 層化ブートストラップで区間を併記する。
POP_FRAME <- c("滋賀県" = 79, "京都府" = 30, "大阪府" = 4,
               "兵庫県" = 21, "奈良県" = 12, "和歌山県" = 10)

pref_weights <- function(sample_pref) {
  n_smp <- table(factor(sample_pref, levels = PREF_ORDER))
  w <- POP_FRAME[PREF_ORDER] / as.numeric(n_smp)
  tibble(pref = factor(PREF_ORDER, levels = PREF_ORDER),
         n_sample = as.numeric(n_smp),
         n_pop    = as.numeric(POP_FRAME[PREF_ORDER]),
         w        = as.numeric(w))
}

# ------------------------------------------------------------------------------
# 0. ユーティリティ関数
# ------------------------------------------------------------------------------

# 行ラベルに対応するセル値をシートから取得
get_row_values <- function(sheet_df, label_pat) {
  col1_clean <- str_replace_all(as.character(sheet_df[[1]]), "\\s+", "")
  match_rows <- which(str_detect(col1_clean, label_pat))
  if (length(match_rows) == 0) return(NA_character_)
  vals <- as.character(sheet_df[match_rows[1], -1])
  vals <- vals[!is.na(vals) & vals != "NULL" & vals != "NA"]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = " / ")
}

# 変化傾向の正規化
parse_trend <- function(x) {
  x <- str_replace_all(as.character(x), "\\s+", "")
  case_when(
    str_detect(x, "大量増加")         ~ "大幅増加",
    str_detect(x, "増加")             ~ "増加",
    str_detect(x, "ほぼ同じ|変化なし")~ "横ばい",
    str_detect(x, "大量減少")         ~ "大幅減少",
    str_detect(x, "減少")             ~ "減少",
    str_detect(x, "^なし$|^0$")       ~ "なし/0",
    TRUE                              ~ "不明"
  )
}

# 【2026-09-06】協力者年齢の抽出は「受访者信息」シート（extract_age_simple）に一本化。旧 extract_ages() は原票の自由記述向けだったため廃止。

# ------------------------------------------------------------------------------
# 1. 全シート読み込み（祭りレベルの変数。まとめ.xlsxには無いため原票のまま）
# ------------------------------------------------------------------------------

sheet_ids <- excel_sheets(DATA_PATH)          # 読み込み用（原文のまま）
sheets    <- str_trim(sheet_ids)                # 表示・結合キー用（前後空白を除去）
# 旧版では「小田神社 」の末尾空白により scale_df との結合が外れ、
# n_resources が NA になっていた。ここで統一する。
cat("シート数:", length(sheets), "\n")

survey_list <- lapply(sheet_ids, function(sh) {
  df <- read_excel(DATA_PATH, sheet = sh, col_names = FALSE)
  list(
    festival         = str_trim(sh),
    age_raw          = get_row_values(df, "^年齢$"),
    belief           = get_row_values(df, "^信仰$"),
    purpose          = get_row_values(df, "^祭り目的$"),
    participants_r7  = get_row_values(df, "祭り関係者数"),
    participants_chg = get_row_values(df, "祭り関係者数変化"),
    tourists_r7      = get_row_values(df, "祭り観光客数"),
    tourists_chg     = get_row_values(df, "祭り観光客数変化"),
    preservation     = get_row_values(df, "^保存会$"),
    ujiko_org        = get_row_values(df, "^氏子組織$"),
    admin_org        = get_row_values(df, "^関係する行政"),
    company          = get_row_values(df, "^企業・財団$"),
    school           = get_row_values(df, "^学校$"),
    core_generation  = get_row_values(df, "^火祭り中心世代$"),
    challenges       = get_row_values(df, "^近年の課題"),
    social_meaning   = get_row_values(df, "^火祭りの社会意義$")
  )
})

survey_df <- bind_rows(lapply(survey_list, as.data.frame, stringsAsFactors = FALSE)) %>%
  mutate(
    participants_trend = parse_trend(participants_chg),
    tourists_trend     = parse_trend(tourists_chg)
  )

# ------------------------------------------------------------------------------
# 1b. 協力者年齢 — 分析内容まとめ.xlsx「受访者信息」シートから読み込み
# ------------------------------------------------------------------------------
# 【2026-09-06 改訂】原票の自由記述セルから正規表現で年齢を抜き出す方式
# （生年・世代表記の誤読が繰り返し発生していた）をやめ、まとめ側で人手整理
# 済みの「協力者名×年齢」表を使う。54名（1〜5名/祭り）、年齢は52/54が
# 「NN歳」の統一形式。
# 【2026-09-06 再改訂】残り2件は個人ではなく集団エントリー（まんどろ火祭り
# 「その他（氏名・人数未記載）」、東光寺鬼会「複数名の保存会メンバー・
# 上万願寺町役員等」）で、氏名も特定できず年齢欄も「50～70歳」という範囲
# 表記。図01は「個別協力者」の年齢分布を示す図であり、正体不明の集団を
# 1個人として1点扱いすると分布の意味が変わってしまうため、中央値で代表
# させるのではなく、この2件は協力者年齢の集計から除外する
# （＝個人が特定できる52名のみを対象とする）。
extract_age_simple <- function(x) {
  if (!str_detect(as.character(x), "^[0-9]+歳$")) return(NA_real_)
  as.numeric(str_extract(x, "[0-9]+"))
}

# ------------------------------------------------------------------------------
# 1c. 植物資源 — 分析内容まとめ.xlsx から読み込み
# ------------------------------------------------------------------------------
# 【2026-09-06 全面改訂】
# 分析内容まとめ.xlsx が再整理され、以下の点で「正規表現による推測」が
# 不要になった：
#   ・植物の種類・材質（taxon_kind）: 36分類が人手で確定済み。
#     旧 normalize_taxon()/CANON_RESOURCE/TAXON_RULES による独自の正規表現
#     グルーピングは不要になったため全廃した。
#   ・使用部位等（part）: 資源名から部位を正規表現で分離する必要がなくなった。
#   ・利用方法・代替可能性・調達方法・調達地の変化: いずれも「◯◯区分」列に
#     分離済み（自由記述の「補足説明」と別列）。
#   ・話題頻度・保全管理活動（結果6）: 唯一まだ「チェック選択肢＋自由記述」
#     が同一セルに残っている（分離されていない）。code_topic()/code_mgmt()
#     による先頭一致の抽出が引き続き必要（"推測"ではなく、統制語彙の
#     先頭一致というだけなので誤読の余地はない）。
#   ・選定理由（reason）: 自由記述のまま（意図的にコード化されていない）。
#     10類型への帰納的コーディング（REASON_RULES）は今後も必要。
#   ・調達地の景観（landscape）: 大部分は単一の統制語（二次林・人工林等）に
#     なったが、約6/205行はなお「二次林 過去：…」のように後続の自由記述が
#     付く。LANDSCAPE_VOCAB とのキーワード一致は残す必要がある。
#
# 新たに追加された変数:
#   現時点の祭り利用（1=有,0=無）: 過去には使われたが現在は使われていない
#     資源が13件ある。本スクリプトの分析は「現在使われている資源」を
#     対象とするため current_use == 1 に絞る。除外分は discontinued_resources
#     として別途CSV出力する（資源基盤の変容を論じる際の追加材料）。
#
# シートの並びが変わった点に注意：新設の「受访者信息」が先頭に挿入された
# ため、旧結果1〜6は結果2〜7にシフトしている。読み違いを避けるため
# 名前付きベクトルで固定する。
MT_SHEET <- c(informant = 1, daily = 2, use = 3, pref = 4,
             method = 5, landscape = 6, engagement = 7)

# 非植物資材（結果1の植物の種類・材質に人手で記録されている）
NONPLANT_TAXA <- c("アルミ・灯油", "ワタ（綿）", "布類（材質不明）")

matome_clean <- function(x) str_squish(str_replace_all(as.character(x), "[\r\n]+", " "))

read_matome <- function(i) suppressMessages(read_excel(MATOME_PATH, sheet = i, col_names = TRUE))

# --- 0. 受访者信息（協力者×年齢）---
mt0 <- read_matome(MT_SHEET["informant"]) %>%
  rename(festival = 1, informant_name = 2, informant_role = 3,
         age_raw = 4, source_cell = 5) %>%
  mutate(across(c(festival, informant_name, informant_role, age_raw), matome_clean))

age_long <- mt0 %>%
  mutate(age = vapply(age_raw, extract_age_simple, numeric(1))) %>%
  filter(!is.na(age)) %>%
  select(festival, age)

# --- 1. 結果1（日常利用の有無 + 現時点の祭り利用）---
mt1 <- read_matome(MT_SHEET["daily"]) %>%
  rename(festival = 1, taxon_kind = 2, resource_raw = 3,
         daily_class = 4, daily_note = 5, current_use = 6) %>%
  mutate(across(c(festival, taxon_kind, resource_raw, daily_class, daily_note), matome_clean),
         current_use = suppressWarnings(as.integer(current_use)))

# --- 2. 結果2（利用方法・選定理由・代替可能性）---
mt2 <- read_matome(MT_SHEET["use"]) %>%
  rename(taxon_kind = 1, part = 2, festival = 3, use_class = 4, use_note = 5,
         reason_raw = 6, subst_class = 7, subst_note = 8) %>%
  mutate(across(c(taxon_kind, part, festival, use_class, use_note,
                  reason_raw, subst_class, subst_note), matome_clean),
         # 結合キー用：「稲穂」(結果2) と「穂」(結果4) の表記差のみ吸収する
         # （もち米・赤米の2件。他に「稲」で始まる part 値はない）。
         join_part = coalesce(str_replace(part, "^稲", ""), ""))

# --- 3. 結果3（府県×資源グループ）---
mt3 <- read_matome(MT_SHEET["pref"]) %>% rename(pref = 1, festival = 2) %>%
  mutate(across(c(pref, festival), matome_clean))

# 府県マッピングを結果3から確定させる
FESTIVAL_PREF <- setNames(mt3$pref, mt3$festival)
stopifnot(all(sheets %in% names(FESTIVAL_PREF)))
stopifnot(all(FESTIVAL_PREF %in% PREF_ORDER))

# 結果3 の資源グループ（祭り×グループ10列、セルに資源名が空白区切り）を
# (祭り, 資源名) → グループ の対応表に展開する。図17bで使用する粗い9分類。
MATOME_TAXON_COLS <- c("稲・米・藁類", "麦・菜種類", "ヨシ・カヤ・ススキ類",
                       "タケ・ササ類", "マツ類", "スギ・ヒノキ類",
                       "その他樹木・柴類", "蔓・縄・繊維類", "その他植物・供物類")

taxon_matome_map <- mt3 %>%
  select(festival, all_of(MATOME_TAXON_COLS)) %>%
  pivot_longer(-festival, names_to = "taxon_matome", values_to = "res_list") %>%
  filter(!is.na(res_list), matome_clean(res_list) != "") %>%
  mutate(res_list = matome_clean(res_list)) %>%
  separate_rows(res_list, sep = "[[:space:]]+") %>%
  filter(res_list != "") %>%
  distinct(festival, res_list, taxon_matome)

# --- 4. 結果4（調達方法・調達時期）— 資源レコードの軸（スパイン）---
mt4 <- read_matome(MT_SHEET["method"]) %>%
  rename(festival = 1, taxon_kind = 2, part = 3,
         method_class = 4, method_note = 5, timing_raw = 6) %>%
  mutate(across(c(festival, taxon_kind, part, method_class, method_note), matome_clean),
         join_part = coalesce(str_replace(part, "^稲", ""), ""))
stopifnot(!anyDuplicated(paste(mt4$festival, mt4$taxon_kind, mt4$join_part)))

# --- 5. 結果5（調達地の変化・景観）---
# 結果4と行順・(祭り,植物の種類・材質)が完全一致することを確認済みのため、
# 位置対応で結合する（キー一致による突合は不要）。
mt5 <- read_matome(MT_SHEET["landscape"]) %>%
  rename(festival = 1, taxon_kind = 2, part = 3,
         change_class = 4, change_note = 5, landscape_raw = 6) %>%
  mutate(across(c(festival, taxon_kind, part, change_class, change_note, landscape_raw),
                matome_clean))
stopifnot(nrow(mt4) == nrow(mt5),
          all(mt4$festival == mt5$festival),
          all(mt4$taxon_kind == mt5$taxon_kind))

# --- 6. 結果6（話題頻度・保全管理活動、祭りレベル）---
# ここだけチェック選択肢＋自由記述が未分離のため、統制語彙の先頭一致で
# スコア化する（コーディング表は下の code_topic()/code_mgmt() 定義を参照）。
mt6 <- read_matome(MT_SHEET["engagement"]) %>%
  rename(festival = 1, topic_class = 2, topic_note = 3,
         mgmt_class = 4, mgmt_note = 5) %>%
  mutate(across(everything(), matome_clean))

# ------------------------------------------------------------------------------
# コード化関数（分析内容まとめ.xlsx の統制語彙をスコアに変換）
# ------------------------------------------------------------------------------

# 先頭の全角/半角数字コードを取り出す（「２　ほとんどない」「2B　以前より広い」）
lead_code <- function(x) {
  v <- chartr("０１２３４５６７８９", "0123456789", as.character(x))
  str_match(str_squish(v), "^([0-9][AB]?)")[, 2]
}

# 【 】内の分類ラベルを取り出す
bracket_cat <- function(x) str_match(as.character(x), "^【([^】]+)】")[, 2]

# 日常利用スコア：1=日常的に使う, 2=ほとんどない, 3=全くない
code_daily <- function(x) suppressWarnings(as.integer(lead_code(x)))

# 代替可能性：1=代用できる, 2=ある程度代用できる, 3=代用できない
#   コード4「代替用材料」は"その資源自体が他資源の代替として使われている"
#   という別次元の情報であり、1〜3の順序尺度には乗らない。順序尺度は NA とし、
#   is_substitute_material フラグで保持する。「未確認」もNA（1件）。
code_substitutability <- function(x) {
  v <- suppressWarnings(as.integer(lead_code(x)))
  ifelse(is.na(v) | v > 3, NA_integer_, v)
}

# 調達地の変化：1=変化なし, 2A=以前より近い, 2B=以前より広い, 3=使用停止, 9=不明
code_change <- function(x) {
  v <- lead_code(x)
  factor(ifelse(v == "9", NA_character_, v),
         levels = c("1", "2A", "2B", "3"),
         labels = c("変化なし", "以前より近い", "以前より広い", "使用停止"))
}

# 調達方法の地域嵌入度（結果4 の【 】カテゴリーから）
#   3 = 共同体が自ら採取・栽培
#   2 = 地域内の他者から無償で得る（農家・住民・寺社・事業者、副産物の再利用）
#   1 = 市場・地域外に依存（購入・業者委託・地域外の提供者）
#   NA = 現行調達なし／調達方法不明
# 1つのセルに複数カテゴリーが「／」で並ぶ場合は最も高い嵌入度を採る。
EMBED_LEVELS <- list(
  "3" = c("氏子・保存会採取", "氏子・保存会栽培", "協働採取", "協働栽培"),
  "2" = c("地域住民提供", "地元農家提供", "地元農家委託栽培", "地域内寺社提供",
          "地域内事業者提供", "副産物・再利用", "寄付・奉納"),
  "1" = c("地域外購入", "地域内購入", "購入", "外部業者委託", "外部協力者提供",
          "外部協力者採取", "外部協力者仲介", "地域外農家提供",
          "地域外農家委託栽培", "農家提供")
)

code_embeddedness <- function(x) {
  cat_str <- bracket_cat(x)
  vapply(cat_str, function(cs) {
    if (is.na(cs)) return(NA_integer_)
    parts <- str_trim(str_split(cs, "／")[[1]])
    parts <- str_replace_all(parts, "[（(].*?[）)]", "")   # （旧来）（推定）を落とす
    sc <- integer(0)
    for (lv in names(EMBED_LEVELS))
      if (any(parts %in% EMBED_LEVELS[[lv]])) sc <- c(sc, as.integer(lv))
    if (!length(sc)) return(NA_integer_)
    max(sc)
  }, integer(1), USE.NAMES = FALSE)
}

# 利用方法（結果2 の【 】カテゴリー）。「／」区切りの複数カテゴリーと、
# （旧来）（代替材）（代替試行・不採用）（推定）という状態注記を分離する。
code_use <- function(x) {
  cs <- bracket_cat(x)
  vapply(cs, function(z) {
    if (is.na(z)) return(NA_character_)
    parts <- str_trim(str_split(z, "／")[[1]])
    parts <- unique(str_replace_all(parts, "[（(].*?[）)]", ""))
    parts <- parts[parts != ""]
    if (!length(parts)) return(NA_character_)
    paste(parts, collapse = "|")
  }, character(1), USE.NAMES = FALSE)
}

# 利用方法18分類の機能グループ（クロス分析でセル数を確保するため）
#   結果2 の【 】は18カテゴリーに細分されているが、n<5 のものが多い。
#   松明内での機能を基準に6群へまとめる。詳細分類は use_types に保持。
USE_GROUPS <- c(
  "主要燃焼材" = "燃焼材系", "燃焼材" = "燃焼材系", "着火材" = "燃焼材系",
  "構造材" = "構造材系", "芯棒" = "構造材系", "充填材" = "構造材系",
  "補強材" = "構造材系", "担ぎ棒" = "構造材系",
  "化粧材" = "化粧・装飾系", "装飾材" = "化粧・装飾系", "造形材" = "化粧・装飾系",
  "結束材" = "結束材",
  "祭具材" = "祭具・供物系", "供物" = "祭具・供物系", "装束材" = "祭具・供物系",
  "発煙材" = "その他機能", "防火材" = "その他機能", "非祭具" = "その他機能",
  "芯材（燃焼部）" = "燃焼材系", "被覆材" = "化粧・装飾系"
)
USE_GROUP_ORDER <- c("燃焼材系", "構造材系", "化粧・装飾系", "結束材",
                     "祭具・供物系", "その他機能")

# 利用の位置づけ：現行 / 旧来 / 代替材 / 代替試行・不採用 / 推定
code_use_status <- function(x) {
  cs <- bracket_cat(x)
  case_when(
    is.na(cs)                              ~ NA_character_,
    str_detect(cs, "代替試行・不採用")     ~ "代替試行・不採用",
    str_detect(cs, "代替候補")             ~ "代替候補",
    str_detect(cs, "代替材")               ~ "代替材",
    str_detect(cs, "旧来")                 ~ "旧来",
    str_detect(cs, "推定")                 ~ "推定",
    TRUE                                   ~ "現行"
  )
}

# 調達地の景観（結果5）。大半は単一の統制語だが、なお約6/205行は
# 「二次林 過去：…」のように後続の自由記述が付く。統制語彙に含まれる
# 語だけを拾う（＝残る唯一のキーワード一致の必要箇所）。
LANDSCAPE_VOCAB <- c("二次林", "人工林", "竹林", "神社林", "海岸防災林", "庭園",
                     "水田", "湿地", "畑", "荒地", "木材流通")

# 景観タイプの配色（図18・図24で共有）。系統でまとめる：
# 森林系＝緑の階調、水系＝青、畑＝橙、荒地＝灰、庭園＝桃、木材流通＝茶
LANDSCAPE_PAL <- c(
  "二次林"     = "#238B45",
  "人工林"     = "#74C476",
  "竹林"       = "#A1D99B",
  "神社林"     = "#00441B",
  "海岸防災林" = "#66C2A4",
  "水田"       = "#3182BD",
  "湿地"       = "#6BAED6",
  "畑"         = "#FD8D3C",
  "荒地"       = "#969696",
  "庭園"       = "#DE77AE",
  "木材流通"   = "#8C6D31"
)

# ------------------------------------------------------------------------------
# 植物分類群（taxon_kind）の表示順（図03a/03b・図17bで共有）
# ------------------------------------------------------------------------------
# 【2026-09-06 追加の理由】従来は出現頻度（w_prev等）でソートしていたが、
# それだと近縁種・同じ生活形の植物が図の離れた位置に散らばってしまう
# （例：アカマツとクロマツが遠く離れる）。生活形・分類群でまとめた順序に
# 変更し、同系統の植物が隣り合って読めるようにする。
#   草本・水辺（ヨシ原・湿地・荒地）→ 穀物・畑作物 → 竹・ササ →
#   針葉樹（マツ科・ヒノキ科）→ 広葉樹・柴（低木含む）→ 蔓性 → その他
# 実データに存在しない分類群が含まれていても実害はない（描画時に
# intersect() で絞り込む）。新しい taxon_kind が増えた場合はここに追記する。
TAXON_ORDER <- c(
  # 草本・水辺
  "ヨシ", "カヤ", "ススキ",
  # 穀物・畑作物
  "稲", "稲（もち米）", "稲（赤米）", "小麦", "菜種", "麻",
  # 竹・ササ
  "タケ類", "ササ",
  # 針葉樹
  "アカマツ", "クロマツ", "マツ類・タケ類・ウメ", "スギ", "ヒノキ",
  # 広葉樹・柴（低木含む）
  "クリ", "クロモジ", "コバノミツバツツジ", "サカキ", "シイノキ", "シキミ",
  "ソヨゴ", "ツツジ", "ツバキ", "ヌルデ", "ハンノキ", "他の広葉樹類",
  "雑木", "樹木（樹種不明）",
  # 蔓性
  "フジ", "ツツラフジ",
  # その他（供物・装飾等）
  "ヒオウギ", "吉祥草", "食材各種"
)

# resource_taxon の水準を TAXON_ORDER に沿って並べる共通ヘルパー
# （実データにのみ存在し、TAXON_ORDERにない値は末尾に追加＝取りこぼし防止）
order_taxon <- function(x) {
  present <- unique(as.character(x))
  ord <- intersect(TAXON_ORDER, present)
  extra <- setdiff(present, TAXON_ORDER)
  if (length(extra) > 0)
    warning("TAXON_ORDER未登録の分類群: ", paste(extra, collapse = "、"))
  factor(x, levels = c(ord, extra))
}

code_landscape <- function(x) {
  vapply(as.character(x), function(z) {
    if (is.na(z)) return(NA_character_)
    hit <- LANDSCAPE_VOCAB[str_detect(z, fixed(LANDSCAPE_VOCAB))]
    if (!length(hit)) return(NA_character_)
    ord <- order(vapply(hit, function(h) str_locate(z, fixed(h))[1, 1], numeric(1)))
    paste(hit[ord], collapse = "|")
  }, character(1), USE.NAMES = FALSE)
}

# ------------------------------------------------------------------------------
# 選定理由の類型（結果2「植物の選定理由（要点）」に適用。唯一まだ純粋な
# 自由記述として残っている列——意図的にコード化されていないため、
# 帰納的な10類型コーディングは今後も必要）
# ------------------------------------------------------------------------------
# 159件の原文から帰納的に立てた10類型。1記録が複数類型を持つ。
#   burn   燃焼特性       燃えやすい・火力・持続時間・油分・煙
#   phys   物理/加工特性  まっすぐ・軽い・強度・しなやか・加工しやすい・寸法
#   sens   感覚/美的      色・香り・見た目・音・緑・清浄感・装飾性
#   avail  入手容易性     手に入りやすい・地域に多い・身近・調達が容易
#   byprod 生業副産物     農林業の副産物・裏作・間伐材・不要材の循環利用
#   trad   伝統/慣習      昔からの材料・伝統・由来・継承
#   symb   象徴/宗教      縁起・神聖・魔除け・奉納・豊穣の象徴・伝承
#   subst  代替/制約      本命が確保できない/高価/技術低下のための代替選択
#   social 社会的機能     子供の参加・世代継承・安全・村同士の競い合い
#   env    環境保全       水質浄化・環境保護政策・里山保全を目的とする選択
REASON_RULES <- list(
  c("burn",  "燃えやす|燃えにく|火力|火勢|火付|火つき|着火|燃焼|長持ち|燃え残|持続|油分|松脂|樹脂|煙|温度|よく燃え|最後まで燃|燃やしやす|派手に燃え|消えにく|火に強|燃え方|火よけ|燃え上が"),
  c("phys",  "まっすぐ|真っ直ぐ|直材|直線性|軽い|軽く|軽さ|重い|強度|強さ|丈夫|しなやか|しなり|柔軟|柔らか|加工しやす|扱いやす|使いやす|割り加工|太さ|太く|長さ|長く|径|折れにく|粘り|中空|細く|形状|曲がりが少な|寸法|かさ|束ねやす|締めやす|切れにく|構造材|骨組み|芯|支柱|型崩れ"),
  c("sens",  "色|綺麗|きれい|美し|香り|見た目|肌が|緑|音|白っぽ|清浄|外観|見栄え|装飾|飾り"),
  c("avail", "手に入りやす|入手|調達|身近|近くに|地域に多い|豊富|得やす|確保できる|多く生え|自生|地域内|周辺で|供給|地域から|地域産|地域で得|地域にある|容易"),
  c("byprod","副産物|裏作|二毛作|間伐|循環利用|不要材|廃棄|再利用|余った|農産物|農作物|無駄なく"),
  c("trad",  "昔から|伝統|慣習|昔の|継承|由来|古くから|歴史|従来|旧来|昔ぼ|開始時|定着"),
  c("symb",  "縁起|神聖|めでた|魔除|奉納|象徴|祈願|神事|神が宿|常若|信仰|伝承|清め|しめ縄|注連縄|御幣|紙垂|供物|五穀豊穣|豊作|神に|神前|祈るための木"),
  c("subst", "代替|代用|代わり|確保できない|不足|安価|コスト|補強|技術力|技術が弱く|現実的|貴重|模倣"),
  c("social","子供|子ども|教育|次世代|関心|対抗意識|見せ場|競う|遊び|楽しみ|安全|ささくれ|危険"),
  c("env",   "水質|浄化|環境保護|環境保全|里山保全")
)

REASON_LABELS <- c(
  burn = "燃焼特性", phys = "物理・加工特性", sens = "感覚・美的",
  avail = "入手容易性", byprod = "生業副産物", trad = "伝統・慣習",
  symb = "象徴・宗教", subst = "代替・制約", social = "社会的機能",
  env = "環境保全"
)

code_reason <- function(x) {
  s <- str_replace_all(as.character(x), "[\r\n]+", " ")
  if (is.na(s) || str_trim(s) %in% c("", "NA", "NULL")) return(NA_character_)
  if (str_detect(s, "^不明$|^理由なし|^未確認")) return(NA_character_)
  # 「田遊び」は農耕儀礼の名称であり social の「遊び」ではない（誤検出の回避）
  s <- str_replace_all(s, "田遊び", "田儀礼")
  ty <- character(0)
  for (r in REASON_RULES) if (str_detect(s, r[2])) ty <- c(ty, r[1])
  if (!length(ty)) return(NA_character_)
  paste(ty, collapse = "|")
}

# 話題頻度（4段階）と保全管理活動（3段階）— 結果6のみ未分離のため必要
TOPIC_LABELS <- c("1" = "話題にあがらない", "2" = "詳しくは話さない",
                  "3" = "時々話す",         "4" = "よく話す")
MGMT_LABELS  <- c("1" = "行っていない",
                  "2" = "採取・管理のみ（栽培なし）",
                  "3" = "全面的な生産（栽培・植栽あり）")

code_topic <- function(x) case_when(
  str_detect(x, "^□\\s*よくこの種について話す")             ~ 4L,
  str_detect(x, "^□\\s*時々この種について話すことがある")   ~ 3L,
  str_detect(x, "^□\\s*話題に上がるが詳しくは話さない")     ~ 2L,
  str_detect(x, "^□\\s*話題にあがらない")                   ~ 1L,
  TRUE ~ NA_integer_
)
code_mgmt <- function(x) case_when(
  str_detect(x, "^□\\s*全面的に行われている")       ~ 3L,
  str_detect(x, "^□\\s*採取・管理が行われている")   ~ 2L,
  str_detect(x, "^□\\s*行っていない")               ~ 1L,
  TRUE ~ NA_integer_
)

festival_engagement <- mt6 %>%
  mutate(
    pref        = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER),
    topic_score = code_topic(topic_class),
    mgmt_score  = code_mgmt(mgmt_class),
    topic_label = factor(unname(TOPIC_LABELS[as.character(topic_score)]),
                         levels = unname(TOPIC_LABELS)),
    mgmt_label  = factor(unname(MGMT_LABELS[as.character(mgmt_score)]),
                         levels = unname(MGMT_LABELS))
  )

cat("\n=== 結果6 話題・管理活動のコード化 ===\n")
cat("未分類 話題:", sum(is.na(festival_engagement$topic_score)),
    " 管理活動:", sum(is.na(festival_engagement$mgmt_score)), "（各30件中）\n")

# ------------------------------------------------------------------------------
# 資源レコードの統合（結果4を軸に）
# ------------------------------------------------------------------------------
# 結果4=軸（159行）。結果1・結果5は結果4と行順・(祭り,taxon_kind)が一致する
# ことを確認済みなので位置対応（bind_cols）で結合する——結果1には part 列が
# なく、キー結合では同一(祭り,taxon_kind)内に複数部位がある場合に多重一致
# してしまうため、位置対応が正しい。結果2のみ行順が異なる（資源名でソート
# されている）ため、(祭り,taxon_kind,part) のキー結合を使う。
stopifnot(nrow(mt1) == nrow(mt4), all(matome_clean(mt1$festival) == mt4$festival))

resource_raw <- bind_cols(
  mt4 %>% select(festival, taxon_kind, part, join_part, method_class, method_note, timing_raw),
  mt1 %>% select(resource_orig = resource_raw, daily_class, daily_note, current_use)
) %>%
  left_join(mt2 %>% select(festival, taxon_kind, join_part, use_class, use_note,
                           reason_raw, subst_class, subst_note),
            by = c("festival", "taxon_kind", "join_part")) %>%
  left_join(mt5 %>% select(festival, taxon_kind, part, change_class, change_note, landscape_raw),
            by = c("festival", "taxon_kind", "part")) %>%
  left_join(taxon_matome_map %>% rename(resource_orig = res_list),
            by = c("festival", "resource_orig")) %>%
  select(-join_part)

.unmapped_use <- resource_raw %>% filter(is.na(use_class))
cat("結果2（利用方法等）に対応しない記録:", nrow(.unmapped_use), "件",
    if (nrow(.unmapped_use) > 0) paste0("（", paste(unique(.unmapped_use$taxon_kind), collapse = "、"), "）") else "", "\n")

.unmapped_taxon <- resource_raw %>% filter(is.na(taxon_matome), !(taxon_kind %in% NONPLANT_TAXA))
if (nrow(.unmapped_taxon) > 0) {
  cat("結果3の資源グループに対応しない記録:", nrow(.unmapped_taxon), "件\n")
  print(as.data.frame(.unmapped_taxon %>% select(festival, taxon_kind, resource_orig)))
}

cat("資源レコード:", nrow(resource_raw), "件 /",
    n_distinct(resource_raw$festival), "祭り /",
    n_distinct(resource_raw$taxon_kind), "分類群\n")

# ------------------------------------------------------------------------------
# 資源レベルの解析用データ
# ------------------------------------------------------------------------------
# resource_taxon は taxon_kind をそのまま使う（人手コード済みの正式分類、
# 独自の正規表現グルーピングは行わない）。resource_norm は過去バージョンの
# 「部位を分けたまま数えた名称」との互換のために残すが、現在は
# resource_taxon と同一の値を持つエイリアスに過ぎない（結果4・結果5に
# 部位別の重複がある場合の粒度差は resource_raw / part 列で見ること）。
resource_df_full <- resource_raw %>%
  mutate(
    resource_raw   = resource_orig,
    resource_taxon = taxon_kind,
    resource_norm  = taxon_kind,
    landscape_all  = code_landscape(landscape_raw),
    landscape_norm = str_replace(landscape_all, "\\|.*$", ""),
    subst_score    = code_substitutability(subst_class),
    is_substitute_material = str_detect(replace_na(lead_code(subst_class), ""), "^4$"),
    embed_score    = code_embeddedness(method_class),
    method_cat     = bracket_cat(method_class),
    daily_score    = code_daily(daily_class),
    use_types      = code_use(use_class),
    use_status     = code_use_status(use_class),
    change_cat     = code_change(change_class),
    reason_types   = vapply(reason_raw, code_reason, character(1))
  ) %>%
  filter(!(taxon_kind %in% NONPLANT_TAXA))

n_discontinued <- sum(resource_df_full$current_use == 0, na.rm = TRUE)
cat("\n現時点で使われていない資源（current_use=0）:", n_discontinued,
    "件 — discontinued_resources.csv に出力し、主分析からは除外\n")

# 主分析は「現在使われている植物資源」を対象とする
resource_df <- resource_df_full %>% filter(current_use == 1)

cat("\n=== 分析内容まとめ由来のコード化の網羅率（植物資源", nrow(resource_df), "件）===\n")
for (v in c("subst_score", "embed_score", "daily_score", "use_types",
            "landscape_norm", "reason_types")) {
  cat(sprintf("  %-15s 有効 %3d 件（欠測 %d）\n", v,
              sum(!is.na(resource_df[[v]])), sum(is.na(resource_df[[v]]))))
}
cat("  うち代替用材料（代替可能性コード4）:", sum(resource_df$is_substitute_material, na.rm = TRUE), "件\n")

# ==============================================================================
# 植物 × 祭り の解析単位と、府県ウェイトによる推定
# ==============================================================================
# 【解析単位】 祭り × 植物分類群（festival × taxon）
#   ・同じ祭りで同じ植物の部位が複数記録されていても（ヒノキの丸太／薪／葉）
#     1件として数える（利用の有無を見るため）
#   ・選定理由は、その祭り・その植物について記録された全部位の理由類型の和集合
# 【推定量】
#   raw_prev   素の割合          = 使用祭り数 / 30
#   w_prev     母集団推定割合    = Σ w_i·u_i / Σ w_i  （Σw_i = 156）
#   eq_prev    府県均等化割合    = 6府県の府県内使用率の単純平均
#                                  （母集団件数の推定を使わない感度分析用）
#   w_n        母集団での使用祭り数の推定値 = Σ w_i·u_i
#   CI         府県内で祭りを復元抽出する層化ブートストラップ（2000回）の
#              パーセンタイル区間。標本が府県3〜11件と小さいため区間は広い。
# ------------------------------------------------------------------------------

# 祭り単位の標本設計表（府県とウェイト）
fest_design <- tibble(festival = sheets) %>%
  mutate(pref = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER)) %>%
  left_join(pref_weights(unname(FESTIVAL_PREF[sheets])), by = "pref")

# 祭り × 植物分類群（部位を統合、理由類型は和集合）
plant_festival <- resource_df %>%
  filter(!is.na(resource_taxon), resource_taxon != "非植物資材") %>%
  group_by(festival, resource_taxon) %>%
  summarise(
    n_parts      = n(),
    parts        = paste(unique(resource_raw), collapse = " / "),
    reason_types = {
      ty <- unique(unlist(strsplit(na.omit(reason_types), "\\|")))
      if (length(ty) == 0) NA_character_ else paste(sort(ty), collapse = "|")
    },
    # 同一(祭り,分類群)内で部位ごとにスコアが異なる場合があるため（例：9件で
    # 代替可能性が部位間で不一致）、まず祭り内で単純平均してから祭り間を
    # 府県ウェイトで平均する（図22で使用）。
    mean_subst_fest = mean(subst_score, na.rm = TRUE),
    mean_daily_fest = mean(daily_score, na.rm = TRUE),
    mean_embed_fest = mean(embed_score, na.rm = TRUE),
    has_subst_material = any(is_substitute_material, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_subst_fest, mean_daily_fest, mean_embed_fest),
                ~ ifelse(is.nan(.x), NA_real_, .x))) %>%
  left_join(fest_design, by = "festival")

# 祭り × まとめ資源グループ（結果3の分類。図17bで使う）
plant_festival_mt <- resource_df %>%
  filter(!is.na(taxon_matome)) %>%
  group_by(festival, taxon_matome) %>%
  summarise(
    n_parts      = n(),
    parts        = paste(unique(resource_raw), collapse = " / "),
    reason_types = {
      ty <- unique(unlist(strsplit(na.omit(reason_types), "\\|")))
      if (length(ty) == 0) NA_character_ else paste(sort(ty), collapse = "|")
    },
    .groups = "drop"
  ) %>%
  left_join(fest_design, by = "festival")

# --- 推定関数 ---------------------------------------------------------------
# u_mat: 祭り(行) × 植物(列) の 0/1 利用行列
usage_matrix <- function(df, unit_col) {
  tab <- table(factor(df$festival, levels = fest_design$festival),
               df[[unit_col]])
  (tab > 0) * 1
}

# 素・均等化・母集団推定の3種の割合をまとめて計算
estimate_prevalence <- function(u_mat, design = fest_design, n_boot = 2000, seed = 20260902) {
  w  <- design$w
  pr <- design$pref
  W  <- sum(w)
  raw_n  <- colSums(u_mat)
  w_n    <- as.numeric(t(u_mat) %*% w)
  # 府県均等化：府県内使用率の単純平均
  eq <- sapply(levels(pr), function(g) colMeans(u_mat[pr == g, , drop = FALSE]))
  eq_prev <- rowMeans(eq)

  set.seed(seed)
  idx_by_pref <- split(seq_len(nrow(u_mat)), pr)
  boot <- replicate(n_boot, {
    idx <- unlist(lapply(idx_by_pref, function(ii) sample(ii, length(ii), replace = TRUE)))
    as.numeric(t(u_mat[idx, , drop = FALSE]) %*% w[idx]) / W
  })
  tibble(
    taxon    = colnames(u_mat),
    raw_n    = as.numeric(raw_n),
    raw_prev = as.numeric(raw_n) / nrow(u_mat),
    eq_prev  = as.numeric(eq_prev),
    w_n      = w_n,
    w_prev   = w_n / W,
    lo       = apply(boot, 1, quantile, 0.025),
    hi       = apply(boot, 1, quantile, 0.975)
  )
}

# ==============================================================================
# 図01: 調査対象の基本プロファイル（協力者年齢 × 植物資源種数）
# ------------------------------------------------------------------------------
# 左：協力者の年齢（点＝個別協力者、横棒＝平均）
# 右：使用している植物資源の種類数（分類群ベース）
# 祭りの並び順は両図で共通。所属府県でグループ化し、府県内は平均年齢の高い順（上ほど高齢）。
# 右図は左図と同じ行に対応するため、縦軸のラベルを省略している。
# （旧 図04「植物資源種数」は本図の右パネルに統合した）
# ==============================================================================

# --- 植物資源の種類数（分類群ベース。図10・図12でも使用） ---
resource_diversity <- resource_df %>%
  group_by(festival) %>%
  summarise(n_resources = n_distinct(resource_taxon), .groups = "drop")

age_summary <- age_long %>%
  group_by(festival) %>%
  summarise(mean_age = mean(age), n_inf = n(), .groups = "drop")

# --- 全30祭りを含む並び順の土台（年齢・資源数が欠測の祭りも行を残す） ---
festival_profile <- tibble(festival = sheets) %>%
  mutate(pref = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER)) %>%
  left_join(age_summary,        by = "festival") %>%
  left_join(resource_diversity, by = "festival")

if (any(is.na(festival_profile$pref))) {
  warning("府県未登録の祭り: ",
          paste(festival_profile$festival[is.na(festival_profile$pref)], collapse = ", "))
}

# 府県ごとにまとめ、府県内は平均年齢の昇順（coord_flip 後は上ほど高齢）
# coord_flip 後は最初の水準が下に来るため、昇順で並べると上ほど高齢になる
festival_order <- festival_profile %>%
  arrange(pref, mean_age) %>%
  pull(festival)

fct_fes <- function(x) factor(x, levels = festival_order)

festival_profile <- festival_profile %>% mutate(festival = fct_fes(festival))

age_long_f <- age_long %>%
  left_join(festival_profile %>% select(festival, pref) %>%
              mutate(festival = as.character(festival)), by = "festival") %>%
  mutate(festival = fct_fes(festival))

age_summary_f <- festival_profile %>% filter(!is.na(mean_age))

age_multi <- age_long_f %>%
  group_by(festival) %>%
  filter(n() > 1) %>%
  ungroup()

# 各祭りの協力者年齢を「53, 76, 77」の形にまとめた注記用ラベル
age_label_df <- age_long_f %>%
  filter(!is.na(age)) %>%
  group_by(festival, pref) %>%
  summarise(age_label = paste(sort(age), collapse = ", "), .groups = "drop")

# 府県ごとの行数に応じてパネル高さを可変にする共通設定
facet_pref <- facet_grid(pref ~ ., scales = "free_y", space = "free_y")

# --- 左：協力者年齢 ---
p01a_age <- ggplot(age_long_f, aes(x = festival, y = age)) +
  # 【重要】全30祭りを含む geom_blank を最初のレイヤーに置く。
  # これがないと離散軸は最初のレイヤー（複数協力者のみを含む age_multi）で
  # 訓練され、右パネルと行の並びがずれる。年齢が欠測の祭りの行もこれで残る。
  geom_blank(data = festival_profile, aes(x = festival, y = 60)) +
  geom_line(data = age_multi, aes(group = festival),
            color = "gray60", linewidth = 0.5) +
  geom_errorbar(data = age_summary_f,
                aes(x = festival, ymin = mean_age, ymax = mean_age),
                width = 0.6, color = "#444444", linewidth = 1.0,
                inherit.aes = FALSE) +
  geom_point(size = 3, alpha = 0.9, color = "black") +
  # 年齢の数値は点の脇ではなくパネル右端にまとめて表示（近い年齢の重なりを回避）
  geom_text(data = age_label_df, aes(x = festival, y = 99, label = age_label),
            hjust = 1, size = 2.9, color = "gray30", inherit.aes = FALSE) +
  coord_flip() +
  facet_pref +
  scale_y_continuous(limits = c(25, 100), breaks = seq(30, 90, 20)) +
  labs(title = "協力者年齢", x = NULL, y = "年齢（歳）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.text.y = element_blank(),
        strip.background.y = element_blank())

# --- 右：植物資源種数（縦軸ラベルなし） ---
p01b_div <- festival_profile %>%
  ggplot(aes(x = festival, y = n_resources)) +
  geom_col(fill = "gray40", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = n_resources), hjust = -0.35, size = 2.9, color = "gray30") +
  coord_flip() +
  facet_pref +
  scale_y_continuous(limits = c(0, max(festival_profile$n_resources, na.rm = TRUE) + 1.5),
                     breaks = seq(0, 12, 2)) +
  labs(title = "植物資源種数", x = NULL, y = "植物資源の種類数（分類群）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p01_profile <- p01a_age + p01b_div +
  patchwork::plot_layout(widths = c(1, 0.62)) +
  patchwork::plot_annotation(
    title = "調査対象30火祭りの基本プロファイル",
    subtitle = paste0("左：協力者年齢（点＝個別協力者、縦線＝平均、右端の数字＝全協力者の年齢） ／ 右：植物資源の種類数",
                      "\n※祭りの並びは府県別、府県内は平均年齢の高い順"),
    caption = paste0("協力者 全", nrow(age_long), "名（複数名の祭りは点を縦線で連結）"),
    theme = theme(plot.title = element_text(face = "bold", family = "HiraginoSans-W3"),
                  plot.subtitle = element_text(family = "HiraginoSans-W3"),
                  plot.caption = element_text(family = "HiraginoSans-W3"))
  )

ggsave(file.path(OUTPUT_DIR, "01_profile_age_and_resources.png"), p01_profile,
       width = 11, height = 8.5, dpi = 150)

# プロファイル表（府県・平均年齢・資源種数）
write.csv(
  festival_profile %>%
    arrange(pref, mean_age) %>%
    select(pref, festival, mean_age, n_informants = n_inf, n_resources),
  file.path(OUTPUT_DIR, "festival_profile.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図02: 関係者数・観光客数トレンド（横ばい = 黄色）
# ==============================================================================

trend_order  <- c("大幅増加", "増加", "横ばい", "減少", "大幅減少", "なし/0", "不明")
trend_colors <- c(
  "大幅増加" = "#1B7837",
  "増加"     = "#7FBF7B",
  "横ばい"   = "#FFD700",   # 黄色
  "減少"     = "#F4A582",
  "大幅減少" = "#D6604D",
  "なし/0"   = "#BDBDBD",
  "不明"     = "#E0E0E0"
)

trend_long <- bind_rows(
  survey_df %>% count(trend = participants_trend) %>% mutate(type = "祭り関係者数"),
  survey_df %>% count(trend = tourists_trend)     %>% mutate(type = "観光客数")
) %>%
  mutate(trend = factor(trend, levels = trend_order))

p02_trend <- trend_long %>%
  ggplot(aes(x = type, y = n, fill = trend)) +
  geom_col(position = "stack", width = 0.5) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            size = 3.5, color = "gray20") +
  scale_fill_manual(values = trend_colors, name = "変化方向",
                    drop = FALSE) +
  labs(title = "関係者数・観光客数の変化傾向",
       subtitle = paste0("全", nrow(survey_df), "祭り"),
       x = NULL, y = "祭り件数") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "02_trend_participants_tourists.png"), p02_trend,
       width = 7, height = 5, dpi = 150)

# ==============================================================================
# 図03: 植物の利用頻度 — 抽出の府県偏りを補正した推定
# ------------------------------------------------------------------------------
# 03a 素の利用祭り数（部位重複を除いた祭り数）と母集団推定割合の比較
# 03b 府県別の利用率ヒートマップ（順位が動く理由を示す）
# ==============================================================================

u_taxon  <- usage_matrix(plant_festival, "resource_taxon")
prev_tax <- estimate_prevalence(u_taxon)

cat("\n=== 府県別の抽出率とウェイト ===\n")
print(as.data.frame(pref_weights(unname(FESTIVAL_PREF[sheets]))))

cat("\n=== 植物の利用頻度（素 vs 補正）===\n")
print(as.data.frame(
  prev_tax %>%
    arrange(desc(w_prev)) %>%
    transmute(taxon, raw_n,
              素の割合 = round(raw_prev, 3),
              府県均等化 = round(eq_prev, 3),
              母集団推定割合 = round(w_prev, 3),
              CI = paste0("[", round(lo, 2), ", ", round(hi, 2), "]"),
              推定使用祭り数 = round(w_n, 1))
))

# --- 03a: 素 vs 母集団推定 --------------------------------------------------
# 【2026-09-06改訂】全34分類群を表示する（従来はraw_n>=2でフィルタしていた）。
# 1祭りのみで使われる植物は層化ブートストラップCIが不安定（区間が非常に
# 広い、または単一府県の値に張り付く）ことに変わりはないため、そのまま
# 解釈しないよう注意。
prev_plot <- prev_tax %>%
  mutate(taxon = order_taxon(taxon))
# coord_flipなしの横棒(geom_point+y=taxon)なので、上から見たい順に並べるには
# 逆順にする（factorの最初の水準が下に来るため）。
prev_plot <- prev_plot %>% mutate(taxon = factor(taxon, levels = rev(levels(taxon))))

p03a <- ggplot(prev_plot, aes(y = taxon)) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                color = "gray70", linewidth = 0.8) +
  geom_segment(aes(x = raw_prev, xend = w_prev, y = taxon, yend = taxon),
               color = "gray45", linewidth = 0.4,
               arrow = arrow(length = unit(0.10, "cm"), type = "closed")) +
  geom_point(aes(x = raw_prev, color = "素の割合（30祭りのうち）"), size = 2.6) +
  geom_point(aes(x = w_prev,   color = "母集団推定割合（府県ウェイト）"), size = 3.2) +
  scale_color_manual(values = c("素の割合（30祭りのうち）" = "#999999",
                                "母集団推定割合（府県ウェイト）" = "#D62728"),
                     name = NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "植物ごとの利用の広がり — 抽出の府県偏りを補正",
    subtitle = paste0("解析単位＝祭り×植物分類群（同一祭り内の部位重複は1件に集約） ／ ",
                      "全", nrow(prev_plot), "分類群\n",
                      "灰＝素の割合、赤＝母集団156件に事後層化した推定割合、横棒＝層化ブートストラップ95%区間"),
    x = "その植物を使用する火祭りの割合", y = NULL,
    caption = "矢印は補正による移動方向。1祭りのみで使われる植物は区間が特に不安定。"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(OUTPUT_DIR, "03a_plant_prevalence_weighted.png"), p03a,
       width = 9.5, height = max(5, nrow(prev_plot) * 0.42), dpi = 150)

# --- 03b: 府県別の利用率ヒートマップ ----------------------------------------
pref_prev <- plant_festival %>%
  count(pref, resource_taxon) %>%
  right_join(expand_grid(pref = factor(PREF_ORDER, levels = PREF_ORDER),
                         resource_taxon = prev_plot$taxon),
             by = c("pref", "resource_taxon")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  left_join(pref_weights(unname(FESTIVAL_PREF[sheets])) %>% select(pref, n_sample),
            by = "pref") %>%
  mutate(prev = n / n_sample,
         resource_taxon = factor(resource_taxon, levels = levels(prev_plot$taxon)))

p03b <- ggplot(pref_prev, aes(x = pref, y = resource_taxon, fill = prev)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, paste0(n, "/", n_sample), "")),
            size = 2.8, color = "gray20", family = "HiraginoSans-W3") +
  scale_fill_gradient(low = "white", high = "#08519C",
                      labels = scales::percent, name = "府県内の利用率") +
  labs(title = "府県別の植物利用率",
       subtitle = paste0("セル内は「使用した祭り数／その府県の調査祭り数」。\n",
                         "府県ごとの標本数の差が図03aの補正量を決める。"),
       x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "03b_plant_prevalence_by_pref.png"), p03b,
       width = 8.5, height = max(5, nrow(prev_plot) * 0.36), dpi = 150)

write.csv(
  prev_tax %>% arrange(desc(w_prev)),
  file.path(OUTPUT_DIR, "plant_prevalence_weighted.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図04: 植物資源種数（祭り別）→ 図01 の右パネルに統合（2026-09-02）
# resource_diversity の計算は図01のブロックに移動した。
# ==============================================================================

# ==============================================================================
# 図05: 組織の有無
# ==============================================================================

org_summary <- survey_df %>%
  mutate(
    `保存会あり`         = !is.na(preservation) &
                           !str_detect(str_replace_all(preservation, "\\s+",""), "^なし$"),
    `氏子組織あり`       = !is.na(ujiko_org) &
                           !str_detect(str_replace_all(ujiko_org, "\\s+",""), "^なし$"),
    `行政・自治組織あり` = !is.na(admin_org) &
                           !str_detect(str_replace_all(admin_org, "\\s+",""), "^なし$"),
    `企業・財団あり`     = !is.na(company) &
                           !str_detect(str_replace_all(company, "\\s+",""), "^なし$"),
    `学校あり`           = !is.na(school) &
                           !str_detect(str_replace_all(school, "\\s+",""), "^なし$")
  ) %>%
  summarise(across(ends_with("あり"), sum)) %>%
  pivot_longer(everything(), names_to = "組織種別", values_to = "件数")

p05_org <- org_summary %>%
  mutate(組織種別 = fct_reorder(組織種別, 件数)) %>%
  ggplot(aes(x = 組織種別, y = 件数)) +
  geom_col(fill = "#984EA3", alpha = 0.85) +
  geom_text(aes(label = paste0(件数, "/", nrow(survey_df))),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, nrow(survey_df) + 1)) +
  labs(title = "火祭りに関わる組織の有無",
       x = NULL, y = "件数") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "05_organization_presence.png"), p05_org,
       width = 8, height = 4, dpi = 150)

# ==============================================================================
# 図06: 祭り目的キーワード
# ==============================================================================

purpose_kw <- c(
  "五穀豊穣|豊作|農業" = "豊作祈願",
  "無病息災"           = "無病息災",
  "家内安全"           = "家内安全",
  "地域安全"           = "地域安全",
  "先祖供養"           = "先祖供養",
  "伝統.*継承|文化.*継承" = "伝統継承",
  "火伏|火除"          = "火伏せ/火除け",
  "神恩感謝"           = "神恩感謝",
  "商売繁盛"           = "商売繁盛",
  "疫病退散"           = "疫病退散",
  "環境保護|水質"      = "環境保護",
  "交通安全"           = "交通安全"
)

purpose_count <- lapply(names(purpose_kw), function(pat) {
  data.frame(keyword = purpose_kw[[pat]],
             n = sum(str_detect(survey_df$purpose, regex(pat)), na.rm = TRUE))
}) %>% bind_rows() %>% filter(n > 0) %>% arrange(desc(n))

p06_purpose <- purpose_count %>%
  mutate(keyword = fct_reorder(keyword, n)) %>%
  ggplot(aes(x = keyword, y = n)) +
  geom_col(fill = "#E6550D", alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(purpose_count$n) + 0.5)) +
  labs(title = "祭り目的のキーワード出現頻度",
       x = NULL, y = "件数") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "06_festival_purpose.png"), p06_purpose,
       width = 8, height = 5, dpi = 150)

# ==============================================================================
# 図07: 信仰キーワード
# ==============================================================================

belief_kw <- c(
  "氏神"              = "氏神信仰",
  "愛宕"              = "愛宕信仰",
  "祖霊|お盆"         = "祖霊/お盆信仰",
  "八幡"              = "八幡信仰",
  "春祭"              = "春祭信仰",
  "琵琶湖"            = "琵琶湖信仰",
  "伝統.*継承|文化遺産" = "伝統継承（現代）",
  "歳神"              = "歳神信仰",
  "不動明王|愛宕"     = "仏教系信仰",
  "稲荷"              = "稲荷信仰"
)

belief_count <- lapply(names(belief_kw), function(pat) {
  data.frame(keyword = belief_kw[[pat]],
             n = sum(str_detect(survey_df$belief, regex(pat)), na.rm = TRUE))
}) %>% bind_rows() %>% distinct(keyword, .keep_all = TRUE) %>%
  filter(n > 0) %>% arrange(desc(n))

p07_belief <- belief_count %>%
  mutate(keyword = fct_reorder(keyword, n)) %>%
  ggplot(aes(x = keyword, y = n)) +
  geom_col(fill = "#3182BD", alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(belief_count$n) + 0.5)) +
  labs(title = "信仰の種類と出現頻度",
       x = NULL, y = "件数") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "07_belief_types.png"), p07_belief,
       width = 8, height = 5, dpi = 150)

# ==============================================================================
# 図08: 近年の課題（自由記述 → キーワード分類）
# ------------------------------------------------------------------------------
# 【処理方針の注記】
# 原問卷の回答形式は自由記述であり、祭りによって粒度が大きく異なる。
#   - 短答型：「若者が少ない」「ない」など1〜2語
#   - 長文型：王の浜・小田神社・大嶋奥津嶋神社は3〜5文の段落で記述
# 処理方法：正規表現によるキーワードマッチング（主題コーディング）
#   各キーワードパターンに対して str_detect() でマッチするかをチェック。
#   1件の回答が複数のカテゴリにヒットすることがある（例：小田神社は
#   「若者不足」「費用」「資源調達」を同時に言及）。
#   → グラフは「件数」ではなく「延べ言及件数」になる点に注意。
#   長文型の回答は完全にはカバーできないため、必要に応じて
#   human coding との照合推奨。
# ==============================================================================

challenge_kw <- c(
  "若者|若年|後継|担い手|参加者.*減|人手"       = "若者不足・担い手問題",
  "少子化|高齢化|高齢|老年"                     = "少子高齢化",
  "資源|植物|調達|入手|刈|購入|材料"             = "植物資源の調達難",
  "費用|資金|コスト|経費|お金|財政"             = "費用・資金問題",
  "コロナ|COVID|疫病|感染"                       = "コロナ禍の影響",
  "技術|技法|作り方|製法|伝承方法"               = "技術継承",
  "外来種|環境|生態|消滅|減少.*植"               = "植生環境の変化",
  "過疎|人口減|転出|農家.*減"                    = "過疎化・人口減少"
)

challenge_count <- lapply(names(challenge_kw), function(pat) {
  data.frame(
    keyword = challenge_kw[[pat]],
    n = sum(str_detect(survey_df$challenges, regex(pat, ignore_case = TRUE)),
            na.rm = TRUE)
  )
}) %>% bind_rows() %>% filter(n > 0) %>% arrange(desc(n))

cat("\n=== 近年の課題（キーワード分類 — 延べ言及件数） ===\n")
print(challenge_count)

p08_challenge <- challenge_count %>%
  mutate(keyword = fct_reorder(keyword, n)) %>%
  ggplot(aes(x = keyword, y = n)) +
  geom_col(fill = "#D62728", alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(challenge_count$n) + 0.5)) +
  labs(
    title = "近年の課題（キーワード頻度）",
    subtitle = "自由記述をキーワードで分類（1件が複数カテゴリに該当する場合あり）",
    x = NULL, y = "言及件数（延べ）"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8, color = "gray40"))

ggsave(file.path(OUTPUT_DIR, "08_recent_challenges.png"), p08_challenge,
       width = 9, height = 5, dpi = 150)

# ==============================================================================
# 図09a: 植物資源 × 祭り — 代替可能性マトリクス
# 図09b: 植物資源 × 祭り — 調達嵌入度マトリクス
# ==============================================================================

# 図03の resource_count は廃止。2祭り以上で使われた資源名（記録どおりの粒度）
resource_count <- resource_df %>%
  distinct(festival, resource_norm) %>%
  count(resource_norm, sort = TRUE)
top_resources <- resource_count %>% filter(n >= 2) %>% pull(resource_norm)

matrix_theme <- theme_bw(base_family = "HiraginoSans-W3") +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    panel.grid  = element_blank(),
    legend.position = "right"
  )

# --- 09a: 代替可能性（1=代替可・緑, 3=代替不可・赤, NA=灰=不使用） ---
matrix_subst <- resource_df %>%
  filter(resource_norm %in% top_resources, !is.na(subst_score)) %>%
  group_by(festival, resource_norm) %>%
  summarise(subst_score = mean(subst_score, na.rm = TRUE), .groups = "drop") %>%
  complete(festival = sheets, resource_norm = top_resources)

p09a_subst <- matrix_subst %>%
  ggplot(aes(x = resource_norm, y = festival, fill = subst_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(!is.na(subst_score),
                               c("可","困","不")[round(subst_score)], "")),
            size = 2.5, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#74C476", high = "#D62728", na.value = "#EEEEEE",
                      limits = c(1, 3),
                      breaks = 1:3,
                      labels = c("1 代替可", "2 困難", "3 不可"),
                      name = "代替可能性") +
  labs(title = "植物資源の使用状況 × 代替可能性",
       subtitle = "灰色 = 当該祭りで使用なし",
       x = NULL, y = NULL) +
  matrix_theme

ggsave(file.path(OUTPUT_DIR, "09a_resource_matrix_substitutability.png"),
       p09a_subst, width = 11, height = 7, dpi = 150)

# --- 09b: 調達嵌入度（1=購入・赤, 3=自採・緑, NA=灰=不使用） ---
matrix_embed <- resource_df %>%
  filter(resource_norm %in% top_resources, !is.na(embed_score)) %>%
  group_by(festival, resource_norm) %>%
  summarise(embed_score = mean(embed_score, na.rm = TRUE), .groups = "drop") %>%
  complete(festival = sheets, resource_norm = top_resources)

p09b_embed <- matrix_embed %>%
  ggplot(aes(x = resource_norm, y = festival, fill = embed_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(!is.na(embed_score),
                               c("購","農","採")[round(embed_score)], "")),
            size = 2.5, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#D62728", high = "#1A6A1A", na.value = "#EEEEEE",
                      limits = c(1, 3),
                      breaks = 1:3,
                      labels = c("1 購入・地域外", "2 地域内から無償", "3 共同体が自ら"),
                      name = "調達嵌入度") +
  labs(title = "植物資源の使用状況 × 調達嵌入度",
       subtitle = "灰色 = 当該祭りで使用なし",
       x = NULL, y = NULL) +
  matrix_theme

ggsave(file.path(OUTPUT_DIR, "09b_resource_matrix_embeddedness.png"),
       p09b_embed, width = 11, height = 7, dpi = 150)

# ==============================================================================
# 図10: 関係者数 vs 観光客数 — 散布図
# ------------------------------------------------------------------------------
# 【分析の注記】
# 原データは文字列混じりのため、以下の方針で数値を手動設定。
#   - 関係者数：複数記載があれば最大値を採用（保存会+氏子+ボランティア等）
#   - 観光客数：ライブ配信等の間接視聴は除外し、現地参集人数を採用
#   - 「不明」はNA扱い（グラフ外）
# ==============================================================================

scale_df <- tribble(
  ~festival,                    ~participants, ~tourists,
  "巽神社松明",                  36,           0,
  "鞍馬の火祭",                  136,          5000,
  "三栖の火祭",                  150,          3500,
  "松明を次世代に送る会",        1000,         0,
  "雄琴学区ヨシ松明一斉点火",    500,          NA,    # 不明
  "太郎坊宮の火祭り",            100,          0,
  "信楽の火祭り",                180,          3000,
  "勝部の火祭り",                150,          2500,  # 現地のみ
  "近江八幡左義長祭り",          600,          80000,
  "大文字送り火",                700,          350000,
  "八幡祭り",                    550,          16500,
  "王の浜若宮神社",              30,           30,
  "小田神社",                    60,           400,
  "大嶋奥津嶋神社",              50,           20,
  # ---- 2026-09-02 追加：30シート版の新規16祭り ------------------------------
  # 上と同じ方針（関係者数=複数記載なら最大値、観光客数=現地参集人数）で
  # 自由記述から読み取った値。★ は原文が幅・感覚値・未計数のため要確認。
  "往馬大社",                    200,          5000,
  "熊野速玉大社",                2000,         NA,     # ★ 運営100人＋上り子・本番参加者1,500〜2,000人／観光客数の明示なし
  "稲むらの火祭り",              200,          600,    # ★ 「参加者600人程」＝見物客とほぼ同義との説明
  "熊野那智",                    120,          3000,
  "嵯峨のお松明式",              40,           1000,   # ★ 「1,000人を大きく上回る」正確な計数なし → 下限値
  "広河原松上げ",                80,           1000,
  "がんがら火祭り",              53,           10000,  # ★ 関係者数はLINE登録53名のみ（当日協力者は未計数）
  "まんどろ火祭り",              16,           4000,   # ★ 関係者数は実行委員会15〜16名のみ
  "麦わら松明",                  10,           100,    # ★ 中核10人前後／観光客は未計数「100人以上」
  "東光寺鬼会",                  21,           200,
  "吉祥草寺茅原大とんど",        170,          3500,
  "稲引き樽引き神事",            20,           60,
  "花背松上げ",                  30,           300,    # ★ 「二、三百人」「五百人ぐらい」複数の感覚値
  "雲ケ畑松上げ",                10,           30,
  "湯村火祭り",                  53,           300,
  "ほうらんや火祭り",            10,           100     # ★ 代表層約10名のみ／2026年は神事のみで大幅減
) %>%
  left_join(resource_diversity, by = "festival")

# 祭り分類 2×2：
#   関係者数 ≥ 200 → 大規模、< 200 → 小規模
#   観光客数 ≥ 1000 → 観光集客型、< 1000（0含む）→ 地域内向型
#   雄琴学区（観光客数不明）は関係者数のみで判定 → 大規模・地域型とみなす
scale_df <- scale_df %>%
  mutate(
    participants_class = ifelse(participants >= 200, "大規模", "小規模"),
    tourists_class     = case_when(
      is.na(tourists)    ~ "地域型",   # 不明は地域型扱い
      tourists >= 1000   ~ "観光型",
      TRUE               ~ "地域型"
    ),
    festival_type = paste0(participants_class, "・", tourists_class)
  )

type_colors <- c(
  "大規模・観光型" = "#D62728",   # 赤：大文字・左義長・八幡
  "大規模・地域型" = "#FF7F0E",   # 橙：松明を次世代・雄琴
  "小規模・観光型" = "#9467BD",   # 紫：鞍馬・三栖・信楽・勝部
  "小規模・地域型" = "#1F77B4"    # 青：巽神社・太郎坊・王の浜・小田・大嶋
)

p10_scatter <- scale_df %>%
  filter(!is.na(tourists)) %>%
  ggplot(aes(x = participants, y = tourists + 1,
             color = festival_type, size = n_resources)) +
  # 2×2 象限の区切り線
  geom_vline(xintercept = 200, linetype = "dashed", color = "gray60", linewidth = 0.5) +
  geom_hline(yintercept = 1001, linetype = "dashed", color = "gray60", linewidth = 0.5) +
  # 象限ラベル
  annotate("text", x = 80,  y = 500000, label = "小規模・観光型", size = 2.8,
           color = "#9467BD", alpha = 0.7, fontface = "italic",
           family = "HiraginoSans-W3") +
  annotate("text", x = 800, y = 500000, label = "大規模・観光型", size = 2.8,
           color = "#D62728", alpha = 0.7, fontface = "italic",
           family = "HiraginoSans-W3") +
  annotate("text", x = 80,  y = 2,      label = "小規模・地域型", size = 2.8,
           color = "#1F77B4", alpha = 0.7, fontface = "italic",
           family = "HiraginoSans-W3") +
  annotate("text", x = 800, y = 2,      label = "大規模・地域型", size = 2.8,
           color = "#FF7F0E", alpha = 0.7, fontface = "italic",
           family = "HiraginoSans-W3") +
  geom_point(alpha = 0.85) +
  geom_text_repel(aes(label = festival), size = 2.8, max.overlaps = 20,
                  family = "HiraginoSans-W3") +
  scale_x_continuous(transform = "log10", labels = scales::comma,
                     breaks = c(30, 100, 200, 500, 1000)) +
  scale_y_continuous(transform = "log10",
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c("0", "10", "100", "1千", "1万", "10万", "100万")) +
  scale_color_manual(values = type_colors, name = "祭りタイプ（2×2）") +
  scale_size_continuous(range = c(3, 9), name = "植物資源種数") +
  labs(
    title = "関係者数 vs 観光客数（2×2 類型）",
    subtitle = "縦破線: 関係者数200人、横破線: 観光客1000人 | 点の大きさ = 植物資源種数",
    x = "祭り関係者数（人）[対数]",
    y = "観光客数（人）[対数]",
    caption = "雄琴学区ヨシ松明一斉点火・熊野速玉大社は観光客数不明のため除外"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "10_participants_vs_tourists.png"), p10_scatter,
       width = 10, height = 7, dpi = 150)

# 相関係数（対数変換後）
cor_data <- scale_df %>%
  filter(!is.na(tourists) & tourists > 0 & participants > 0)
if (nrow(cor_data) >= 4) {
  cor_result <- cor.test(log10(cor_data$participants), log10(cor_data$tourists),
                         method = "pearson")
  cat("\n=== 関係者数 vs 観光客数 相関（対数変換, n =", nrow(cor_data), ") ===\n")
  cat("Pearson r =", round(cor_result$estimate, 3),
      ", p =", round(cor_result$p.value, 3), "\n")
}

# 図11は削除

# ==============================================================================
# 図12: 生息地多様性 vs 植物資源種数
# ------------------------------------------------------------------------------
# 生息地多様性 = 各祭りで使用される植物の調達先景観タイプの数
# （水田・湿地・森林・畑・荒地・庭などのユニーク数）
# ==============================================================================

habitat_diversity <- resource_df %>%
  filter(!is.na(landscape_all), landscape_all != "なし") %>%
  select(-landscape_norm) %>%
  separate_rows(landscape_all, sep = "\\|") %>%
  rename(landscape_norm = landscape_all) %>%
  group_by(festival) %>%
  summarise(
    n_habitats  = n_distinct(landscape_norm),
    habitats    = paste(sort(unique(landscape_norm)), collapse = "・"),
    .groups = "drop"
  )

cat("\n=== 生息地多様性（祭り別） ===\n")
print(habitat_diversity %>% arrange(desc(n_habitats)))

div_join <- resource_diversity %>%
  left_join(habitat_diversity, by = "festival") %>%
  left_join(scale_df %>% select(festival, festival_type), by = "festival")

p12_habitat <- div_join %>%
  filter(!is.na(n_habitats)) %>%
  ggplot(aes(x = n_habitats, y = n_resources,
             color = festival_type)) +
  geom_jitter(size = 4, alpha = 0.85, width = 0.05, height = 0.05) +
  geom_smooth(method = "lm", se = TRUE, color = "gray50",
              linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE,
              aes(x = n_habitats, y = n_resources), data = div_join) +
  geom_text_repel(aes(label = festival), size = 2.8, max.overlaps = 20,
                  family = "HiraginoSans-W3") +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = type_colors, name = "祭りタイプ") +
  labs(
    title = "生息地多様性と植物資源種数の関係",
    subtitle = "生息地多様性 = 調達先景観タイプの数（水田・湿地・森林等）",
    x = "生息地タイプ数（景観の多様性）",
    y = "植物資源種数"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "12_habitat_diversity_vs_resources.png"),
       p12_habitat, width = 9, height = 6, dpi = 150)

cor12 <- cor.test(div_join$n_habitats, div_join$n_resources,
                  method = "pearson", use = "complete.obs")
cat("\n=== 生息地多様性 vs 植物資源種数 相関 ===\n")
cat("r =", round(cor12$estimate, 3), ", p =", round(cor12$p.value, 3), "\n")

# ==============================================================================
# 図13: 植物資源 × 信仰 クロス分析
# ==============================================================================

# 信仰カテゴリを各祭りに付与
belief_cats <- c(
  "氏神"              = "氏神信仰",
  "愛宕"              = "愛宕信仰",
  "祖霊|お盆"         = "祖霊/お盆信仰",
  "八幡"              = "八幡信仰",
  "琵琶湖"            = "琵琶湖信仰",
  "伝統.*継承|文化遺産" = "伝統継承（現代）"
)

festival_belief <- survey_df %>%
  select(festival, belief) %>%
  rowwise() %>%
  mutate(
    belief_cats = list(
      names(belief_cats)[
        sapply(names(belief_cats),
               function(pat) str_detect(belief %||% "", regex(pat, ignore_case = TRUE)))
      ]
    ),
    belief_label = paste(belief_cats[belief_cats != ""], collapse = " / ")
  ) %>%
  ungroup() %>%
  unnest(belief_cats) %>%
  filter(belief_cats != "") %>%
  mutate(belief_label_mapped = belief_cats[belief_cats])

# 資源 × 信仰の共起マトリクス
resource_belief <- resource_df %>%
  filter(resource_norm %in% top_resources) %>%
  distinct(festival, resource_norm) %>%
  left_join(festival_belief %>% select(festival, belief_cats), by = "festival") %>%
  filter(!is.na(belief_cats)) %>%
  group_by(resource_norm, belief_cats) %>%
  summarise(n = n(), .groups = "drop")

p13_belief <- resource_belief %>%
  ggplot(aes(x = belief_cats, y = resource_norm, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3) +
  scale_fill_gradient(low = "#EFF3FF", high = "#08519C",
                      name = "共起件数") +
  labs(
    title = "植物資源 × 信仰 共起マトリクス",
    subtitle = "セル値 = その信仰をもつ祭りで当該植物資源が使われた件数",
    x = "信仰カテゴリ", y = "植物資源"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid  = element_blank())

ggsave(file.path(OUTPUT_DIR, "13_resource_x_belief.png"), p13_belief,
       width = 10, height = 7, dpi = 150)

# ==============================================================================
# 図14: 植物資源 × 祭り目的 クロス分析
# ==============================================================================

purpose_cats <- c(
  "五穀豊穣|豊作"      = "豊作祈願",
  "無病息災"           = "無病息災",
  "家内安全"           = "家内安全",
  "先祖供養"           = "先祖供養",
  "伝統.*継承|文化.*継承" = "伝統継承",
  "火伏|火除"          = "火伏せ/火除け",
  "環境保護|水質"      = "環境保護",
  "地域安全"           = "地域安全"
)

festival_purpose <- survey_df %>%
  select(festival, purpose) %>%
  rowwise() %>%
  mutate(
    purpose_cats = list(
      names(purpose_cats)[
        sapply(names(purpose_cats),
               function(pat) str_detect(purpose %||% "", regex(pat, ignore_case = TRUE)))
      ]
    )
  ) %>%
  ungroup() %>%
  unnest(purpose_cats) %>%
  filter(purpose_cats != "") %>%
  mutate(purpose_label = purpose_cats[purpose_cats])

resource_purpose <- resource_df %>%
  filter(resource_norm %in% top_resources) %>%
  distinct(festival, resource_norm) %>%
  left_join(festival_purpose %>% select(festival, purpose_cats), by = "festival") %>%
  filter(!is.na(purpose_cats)) %>%
  group_by(resource_norm, purpose_cats) %>%
  summarise(n = n(), .groups = "drop")

p14_purpose <- resource_purpose %>%
  ggplot(aes(x = purpose_cats, y = resource_norm, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3) +
  scale_fill_gradient(low = "#FFF5EB", high = "#D94801",
                      name = "共起件数") +
  labs(
    title = "植物資源 × 祭り目的 共起マトリクス",
    subtitle = "セル値 = その目的をもつ祭りで当該植物資源が使われた件数",
    x = "祭り目的カテゴリ", y = "植物資源"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid  = element_blank())

ggsave(file.path(OUTPUT_DIR, "14_resource_x_purpose.png"), p14_purpose,
       width = 10, height = 7, dpi = 150)

# ==============================================================================
# CSV出力
# ==============================================================================

# ==============================================================================
# keystone_df: 文化的関鍵種の指標データ（2026-09-06: 図としては削除。
#   代替可能性×日常利用×調達方法の関係は図22で植物ごとに詳しく見られる
#   ため、同じ情報を1枚のバブルチャートに圧縮した本図は不要と判断）。
#   数値自体は keystone_species.csv として引き続き出力する。
# ==============================================================================

keystone_df <- resource_df %>%
  filter(!is.na(subst_score)) %>%
  group_by(resource_norm) %>%
  summarise(
    n_festivals  = n_distinct(festival),
    mean_subst   = mean(subst_score, na.rm = TRUE),
    mean_embed   = mean(embed_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    keystone_score = n_festivals * mean_subst,
    label_flag = mean_subst >= 2.5 | n_festivals >= 4
  )

cat("\n=== 文化的関鍵種候補（代替不可能性 × 出現頻度） ===\n")
print(as.data.frame(keystone_df %>% arrange(desc(keystone_score))))

# ==============================================================================
# 図16: 文化-生態嵌入度（調達方法の地域性）
#   各祭りの嵌入度分布：3=共同体が自ら採取・栽培、2=地域内の他者から無償で得る
#   （農家・住民・寺社・事業者、副産物の再利用）、1=購入・地域外に依存
# ==============================================================================

embed_festival <- resource_df %>%
  filter(!is.na(embed_score)) %>%
  group_by(festival) %>%
  summarise(
    mean_embed   = mean(embed_score, na.rm = TRUE),
    n_high       = sum(embed_score == 3, na.rm = TRUE),
    n_mid        = sum(embed_score == 2, na.rm = TRUE),
    n_low        = sum(embed_score == 1, na.rm = TRUE),
    n_total      = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_embed))

cat("\n=== 文化-生態嵌入度（祭り別） ===\n")
print(as.data.frame(embed_festival))

embed_long <- resource_df %>%
  filter(!is.na(embed_score)) %>%
  mutate(
    embed_label = factor(embed_score,
                         levels = c(3, 2, 1),
                         labels = c("3: 共同体が自ら採取・栽培",
                                    "2: 地域内から無償で入手",
                                    "1: 購入・地域外に依存"))
  ) %>%
  group_by(festival, embed_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(festival) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

festival_embed_order <- embed_festival %>% pull(festival)

p16_embed <- embed_long %>%
  mutate(festival = factor(festival, levels = festival_embed_order)) %>%
  ggplot(aes(x = festival, y = pct, fill = embed_label)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    data = embed_festival %>%
      mutate(festival = factor(festival, levels = festival_embed_order)),
    aes(x = festival, y = 1.05, label = sprintf("%.1f", mean_embed)),
    inherit.aes = FALSE, size = 3, color = "gray30"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("3: 共同体が自ら採取・栽培" = "#1A6A1A",
               "2: 地域内から無償で入手"   = "#74C476",
               "1: 購入・地域外に依存"     = "#D62728"),
    name = "調達方法"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.12)) +
  labs(
    title = "文化-生態嵌入度：植物資源の調達方法（祭り別）",
    subtitle = "右の数値 = 嵌入度スコア平均（3=最高、1=最低）",
    x = NULL, y = "植物資源の割合"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "16_procurement_embeddedness.png"), p16_embed,
       width = 10, height = 7, dpi = 150)

# ==============================================================================
# 図17: 植物の選定理由の類型（旧TEK分析を全面改訂）
# ------------------------------------------------------------------------------
# 17a 理由類型の全体分布（素 vs 府県ウェイト補正）
# 17b 主要植物ごとの理由構成
# 解析単位は 祭り×植物分類群。1単位が複数の理由類型を持つため、
# 割合の合計は100%を超える（多重ラベル）。
# ==============================================================================

reason_long <- plant_festival %>%
  filter(!is.na(reason_types)) %>%
  separate_rows(reason_types, sep = "\\|") %>%
  rename(rtype = reason_types) %>%
  mutate(rlabel = factor(unname(REASON_LABELS[rtype]),
                         levels = unname(REASON_LABELS)))

n_unit      <- nrow(plant_festival)
n_unit_coded<- sum(!is.na(plant_festival$reason_types))
cat("\n=== 選定理由の解析単位 ===\n")
cat("祭り×植物分類群:", n_unit, "単位（うち理由が読み取れたもの", n_unit_coded, "）\n")
cat("1単位あたりの理由類型数:\n")
print(table(vapply(plant_festival$reason_types,
                   function(z) if (is.na(z)) 0L else length(strsplit(z, "\\|")[[1]]),
                   integer(1))))

# --- 17a: 理由類型の全体分布（素 vs 補正）-----------------------------------
# 「その理由を挙げた 祭り×植物 単位」の割合を、ウェイト付きでも計算する
u_reason <- table(factor(reason_long$festival, levels = fest_design$festival),
                  reason_long$rtype)
# ※ ここでは祭り単位ではなく単位（祭り×植物）を数えるので行列は使わず直接集計
reason_share <- reason_long %>%
  count(rtype, rlabel, name = "raw_n") %>%
  left_join(
    reason_long %>%
      group_by(rtype) %>%
      summarise(w_n = sum(w), .groups = "drop"),
    by = "rtype"
  ) %>%
  mutate(
    raw_share = raw_n / n_unit_coded,
    w_share   = w_n / sum(plant_festival$w[!is.na(plant_festival$reason_types)])
  ) %>%
  arrange(desc(w_share))

cat("\n=== 選定理由の類型別シェア（素 vs 府県ウェイト補正）===\n")
print(as.data.frame(reason_share %>%
  transmute(類型 = rlabel, 件数 = raw_n,
            素のシェア = round(raw_share, 3),
            補正シェア = round(w_share, 3))))

p17a <- reason_share %>%
  mutate(rlabel = fct_reorder(rlabel, w_share)) %>%
  select(rlabel, raw_share, w_share) %>%
  pivot_longer(-rlabel, names_to = "kind", values_to = "share") %>%
  mutate(kind = factor(kind, levels = c("raw_share", "w_share"),
                       labels = c("素のシェア（30祭りの標本）",
                                  "府県ウェイト補正後"))) %>%
  ggplot(aes(x = share, y = rlabel, fill = kind)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.68, alpha = 0.9) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)),
            position = position_dodge(width = 0.72), hjust = -0.15, size = 2.8,
            family = "HiraginoSans-W3") +
  scale_fill_manual(values = c("#BDBDBD", "#D62728"), name = NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.62)) +
  labs(
    title = "植物を選ぶ理由の類型",
    subtitle = paste0("解析単位＝祭り×植物分類群（n = ", n_unit_coded,
                      "、部位の重複は集約、理由は和集合）\n",
                      "1単位が複数類型を持つため合計は100%を超える"),
    x = "その理由を挙げた単位の割合", y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(OUTPUT_DIR, "17a_reason_types_overall.png"), p17a,
       width = 9, height = 6, dpi = 150)

# --- 17b: 植物（個別分類群）ごとの理由構成（ウェイト付き）-------------------
# 【2026-09-06 改訂】結果3の粗い資源グループ（9分類）ではなく、taxon_kind
#   （人手コード済みの個別植物、TAXON_ORDERで表示順を統一）を行に使う。
#   記録数2件未満の分類群は割合が不安定なため除外する。
# 【選定理由】結果2「植物の選定理由（要点）」に code_reason() の10類型を適用。
# 【着色】行ごとに割合の高い上位3セルだけをグラデーションで着色する
#   （固定の分位点しきい値だと行によって着色数がばらつくため、順位方式に
#   変更）。0%のセルは白、上位3に入らない非0セルは薄灰にして区別する。

REASON_MIN_N <- 2

reason_long <- plant_festival %>%
  filter(!is.na(reason_types)) %>%
  separate_rows(reason_types, sep = "\\|") %>%
  rename(rtype = reason_types) %>%
  mutate(rlabel = factor(unname(REASON_LABELS[rtype]), levels = unname(REASON_LABELS)))

taxon_denom <- plant_festival %>%
  filter(!is.na(reason_types)) %>%
  group_by(resource_taxon) %>%
  summarise(w_tot = sum(w), n_fes = n(), .groups = "drop") %>%
  filter(n_fes >= REASON_MIN_N)

reason_by_taxon <- reason_long %>%
  filter(resource_taxon %in% taxon_denom$resource_taxon) %>%
  group_by(resource_taxon, rtype, rlabel) %>%
  summarise(w_n = sum(w), .groups = "drop") %>%
  left_join(taxon_denom, by = "resource_taxon") %>%
  mutate(share = w_n / w_tot,
         taxon_label = paste0(resource_taxon, "（", n_fes, "祭り）"))

taxon_order_17b <- order_taxon(taxon_denom$resource_taxon)
label_order_17b <- taxon_denom %>%
  mutate(resource_taxon = factor(resource_taxon, levels = levels(taxon_order_17b))) %>%
  arrange(resource_taxon) %>%
  mutate(taxon_label = paste0(resource_taxon, "（", n_fes, "祭り）")) %>%
  pull(taxon_label)

cat("\n=== 選定理由コーディング対象（植物別、n>=", REASON_MIN_N, "）===\n")
print(as.data.frame(taxon_denom %>% arrange(desc(n_fes))))

# 「その植物を使う祭りのうち、その理由を挙げた割合」をヒートマップで示す。
reason_grid <- expand_grid(
  taxon_label = label_order_17b,
  rlabel      = factor(unname(REASON_LABELS), levels = unname(REASON_LABELS))
) %>%
  left_join(reason_by_taxon %>% select(taxon_label, rlabel, share),
            by = c("taxon_label", "rlabel")) %>%
  mutate(share = ifelse(is.na(share), 0, share),
         taxon_label = factor(taxon_label, levels = rev(label_order_17b))) %>%
  # 行ごとに上位3セルのみ着色対象とする
  group_by(taxon_label) %>%
  mutate(rank_in_row = rank(-share, ties.method = "min")) %>%
  ungroup() %>%
  mutate(cell_kind = case_when(
    share == 0        ~ "zero",
    rank_in_row <= 3   ~ "top",
    TRUE               ~ "mid"
  ))

p17b <- ggplot(reason_grid, aes(x = rlabel, y = taxon_label)) +
  # 背景：0%セルは白、上位3に入らない非0セルは薄灰
  geom_tile(data = ~ filter(.x, cell_kind == "zero"),
            fill = "white", color = "white", linewidth = 0.5) +
  geom_tile(data = ~ filter(.x, cell_kind == "mid"),
            fill = "#F0F0F0", color = "white", linewidth = 0.5) +
  # 前景：行ごとの上位3セルのみグラデーション着色（凡例はこの層から生成）
  geom_tile(data = ~ filter(.x, cell_kind == "top"),
            aes(fill = share), color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(share > 0, scales::percent(share, accuracy = 1), "")),
            size = 2.8, family = "HiraginoSans-W3",
            color = ifelse(reason_grid$cell_kind == "top" & reason_grid$share > 0.5,
                           "white", "gray20")) +
  scale_fill_gradient(low = "#FDD8C0", high = "#B30000",
                      labels = scales::percent, name = "その理由を挙げた割合") +
  labs(
    title = "植物ごとの選定理由の構成",
    subtitle = paste0("2祭り以上で理由が記録された", nrow(taxon_denom), "分類群。",
                      "縦軸はTAXON_ORDER（生活形）順\n",
                      "セル＝その植物を使う祭りのうちその理由が語られた割合",
                      "（行ごとの割合、府県ウェイト補正後）\n",
                      "着色は行ごとの上位3セルのみ。白＝0%、薄灰＝上位3外の非0セル\n",
                      "1単位が複数類型を持つため行の合計は100%を超える"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "17b_reason_by_plant.png"), p17b,
       width = 10, height = max(6.5, nrow(taxon_denom) * 0.42), dpi = 150)

write.csv(
  plant_festival %>%
    select(pref, festival, resource_taxon, n_parts, parts, reason_types, w),
  file.path(OUTPUT_DIR, "plant_festival_units.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  reason_share %>% select(rtype, rlabel, raw_n, raw_share, w_n, w_share),
  file.path(OUTPUT_DIR, "reason_type_share.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図19a: 利用方法カテゴリーの分布
# 図19b: 利用方法 × 選定理由 の関連
# ------------------------------------------------------------------------------
# 【解析単位】 資源レコード（祭り × 資源名。部位を分けたまま扱う）
#   利用方法は部位ごとに異なる（ヒノキの丸太＝構造材／ヒノキの葉＝装飾材）ため、
#   図03・図17のような分類群への集約はせず、記録のまま扱う。
#   利用方法・選定理由ともに多重ラベル（1記録が複数カテゴリーを持つ）。
# 【ウェイト】 図03・図17と同じ府県事後層化ウェイトを適用する。
# ==============================================================================

use_long <- resource_df %>%
  filter(!is.na(use_types)) %>%
  left_join(fest_design %>% select(festival, pref, w), by = "festival") %>%
  separate_rows(use_types, sep = "\\|") %>%
  filter(use_types != "") %>%
  rename(use_cat = use_types) %>%
  mutate(use_group = factor(unname(USE_GROUPS[use_cat]), levels = USE_GROUP_ORDER))

if (any(is.na(use_long$use_group)))
  warning("グループ未登録の利用方法: ",
          paste(unique(use_long$use_cat[is.na(use_long$use_group)]), collapse = ", "))

n_rec_use <- n_distinct(paste(use_long$festival, use_long$resource_raw))
w_rec_use <- resource_df %>% filter(!is.na(use_types)) %>%
  left_join(fest_design %>% select(festival, w), by = "festival") %>% pull(w) %>% sum()

use_share <- use_long %>%
  distinct(festival, resource_raw, use_cat, use_group, w) %>%
  group_by(use_group, use_cat) %>%
  summarise(raw_n = n(), w_n = sum(w), .groups = "drop") %>%
  mutate(raw_share = raw_n / n_rec_use, w_share = w_n / w_rec_use)

cat("\n=== 利用方法カテゴリーの分布（資源レコード", n_rec_use, "件）===\n")
print(as.data.frame(use_share %>% arrange(use_group, desc(w_share)) %>%
  transmute(機能群 = use_group, 分類 = use_cat, 件数 = raw_n,
            素のシェア = round(raw_share, 3), 補正シェア = round(w_share, 3))))
cat("\n=== 利用の位置づけ ===\n")
print(table(resource_df$use_status, useNA = "ifany"))

p19a <- use_share %>%
  mutate(use_cat = fct_reorder(use_cat, w_share)) %>%
  select(use_group, use_cat, raw_share, w_share) %>%
  pivot_longer(c(raw_share, w_share), names_to = "kind", values_to = "share") %>%
  mutate(kind = factor(kind, levels = c("raw_share", "w_share"),
                       labels = c("素のシェア", "府県ウェイト補正後"))) %>%
  ggplot(aes(x = share, y = use_cat, fill = kind)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.68, alpha = 0.9) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)),
            position = position_dodge(width = 0.72), hjust = -0.15, size = 2.6,
            family = "HiraginoSans-W3") +
  facet_grid(use_group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("#BDBDBD", "#08519C"), name = NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.40)) +
  labs(
    title = "松明の中で植物が担う役割（利用方法）",
    subtitle = paste0("解析単位＝資源レコード（祭り×資源名、部位を分けたまま） n = ",
                      n_rec_use, "\n",
                      "1記録が複数の役割を持つため合計は100%を超える"),
    x = "その役割で使われた記録の割合", y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        strip.text.y = element_text(angle = 0))

ggsave(file.path(OUTPUT_DIR, "19a_use_types_overall.png"), p19a,
       width = 9.5, height = 8, dpi = 150)

# ------------------------------------------------------------------------------
# 図19b: 利用方法 × 選定理由
# ------------------------------------------------------------------------------
# 【利用方法の分類】分析内容まとめ.xlsx 結果2 の【 】カテゴリーをそのまま行に
#   使う（スクリプト側の機能グループ USE_GROUPS は使わない）。
#   記録数5件未満のカテゴリーは割合が不安定なため除外する。
# 【選定理由】結果2「選定理由（要点）」に code_reason() の10類型を適用。
# セル = P(その理由 | その利用方法)：その利用方法で使われる記録のうち、
#        その理由が語られた割合（府県ウェイト補正後）。色もこの割合。
# ------------------------------------------------------------------------------

USE_MIN_N <- 5

use_cat_denom <- use_long %>%
  filter(!is.na(reason_types)) %>%
  distinct(festival, resource_raw, use_cat, w) %>%
  group_by(use_cat) %>%
  summarise(w_tot = sum(w), n_rec = n(), .groups = "drop")

cat("\n=== 図19bから除外した利用方法（記録数", USE_MIN_N, "件未満）===\n")
print(as.data.frame(use_cat_denom %>% filter(n_rec < USE_MIN_N) %>% arrange(desc(n_rec))))

use_cat_keep <- use_cat_denom %>% filter(n_rec >= USE_MIN_N)

use_reason <- use_long %>%
  filter(!is.na(reason_types), use_cat %in% use_cat_keep$use_cat) %>%
  distinct(festival, resource_raw, use_cat, reason_types, w) %>%
  separate_rows(reason_types, sep = "\\|") %>%
  rename(rtype = reason_types) %>%
  group_by(use_cat, rtype) %>%
  summarise(raw_n = n(), w_n = sum(w), .groups = "drop") %>%
  right_join(expand_grid(use_cat = use_cat_keep$use_cat,
                         rtype = names(REASON_LABELS)),
             by = c("use_cat", "rtype")) %>%
  mutate(raw_n = replace_na(raw_n, 0L), w_n = replace_na(w_n, 0)) %>%
  left_join(use_cat_keep, by = "use_cat") %>%
  mutate(
    p_cond = w_n / w_tot,
    rlabel = factor(unname(REASON_LABELS[rtype]), levels = unname(REASON_LABELS)),
    use_label = paste0(use_cat, "（", n_rec, "件）")
  )

use_ord <- use_cat_keep %>% arrange(n_rec) %>% pull(use_cat)
use_reason <- use_reason %>%
  mutate(use_label = factor(use_label,
    levels = unique(use_label[order(match(use_cat, use_ord))])))

cat("\n=== 利用方法 × 選定理由：P(理由|利用方法) ===\n")
print(as.data.frame(
  use_reason %>% select(use_cat, n_rec, rlabel, p_cond) %>%
    mutate(p_cond = round(p_cond, 2)) %>%
    pivot_wider(names_from = rlabel, values_from = p_cond) %>%
    arrange(desc(n_rec))
))

# 【2026-09-06 改訂】着色を「行ごとの上位3セルのみ」に変更。0%は白、
# 上位3外の非0セルは薄灰にして区別する（図17bと同じ方式）。
# ＝このヒートマップの読み方：セルは「その用途（例：主要燃焼材）で使われる
#   記録のうち、その理由が語られた割合」＝ P(理由|用途)。例えば主要燃焼材の
#   行で「燃焼特性」列が49%なら、「主要燃焼材として使われる記録の49%で
#   燃焼特性が選定理由に挙がっている」という意味（用途全体のうち49%が
#   燃焼特性由来、ではない）。
use_reason <- use_reason %>%
  group_by(use_label) %>%
  mutate(rank_in_row = rank(-p_cond, ties.method = "min")) %>%
  ungroup() %>%
  mutate(cell_kind = case_when(
    p_cond == 0       ~ "zero",
    rank_in_row <= 3   ~ "top",
    TRUE               ~ "mid"
  ))

p19b <- ggplot(use_reason, aes(x = rlabel, y = use_label)) +
  geom_tile(data = ~ filter(.x, cell_kind == "zero"),
            fill = "white", color = "white", linewidth = 0.5) +
  geom_tile(data = ~ filter(.x, cell_kind == "mid"),
            fill = "#F0F0F0", color = "white", linewidth = 0.5) +
  geom_tile(data = ~ filter(.x, cell_kind == "top"),
            aes(fill = p_cond), color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(p_cond > 0,
                               scales::percent(p_cond, accuracy = 1), "")),
            size = 2.8, family = "HiraginoSans-W3",
            color = ifelse(use_reason$cell_kind == "top" & use_reason$p_cond > 0.55,
                           "white", "gray20")) +
  scale_fill_gradient(low = "#FFF7EC", high = "#B30000",
                      labels = scales::percent, name = "その理由を挙げた割合") +
  labs(
    title = "利用方法と選定理由の関係",
    subtitle = paste0("利用方法は分析内容まとめ.xlsx 結果2 のカテゴリー",
                      "（記録数", USE_MIN_N, "件未満は除外）\n",
                      "セル＝P(理由｜用途)＝その利用方法で使われる記録のうちその理由が",
                      "語られた割合（行ごとの割合、府県ウェイト補正後）\n",
                      "着色は行ごとの上位3セルのみ。白＝0%、薄灰＝上位3外の非0セル\n",
                      "1記録が複数の理由を持つため行の合計は100%を超える"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "19b_use_x_reason.png"), p19b,
       width = 10, height = 6, dpi = 150)

write.csv(use_share, file.path(OUTPUT_DIR, "use_type_share.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  use_reason %>% select(use_cat, n_rec, rtype, rlabel, raw_n, w_n, p_cond),
  file.path(OUTPUT_DIR, "use_x_reason.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図20: 利用方法カテゴリー × 代替可能性
#   松明内での植物の役割（燃焼材・構造材・化粧材・装飾材・結束材など）ごとに
#   代替可能性スコアの分布を積み上げ比率バーで示す。
#   「装飾・化粧材は代替不可が多い」等の仮説を検証。
# ==============================================================================

subst_colors_20 <- c(
  "代替可（1）"    = "#4DAF4A",
  "代替困難（2）"  = "#FF7F00",
  "代替不可（3）"  = "#E41A1C"
)

use_subst_df <- resource_df %>%
  filter(!is.na(use_types), !is.na(subst_score)) %>%
  mutate(use_split = str_split(use_types, "\\|")) %>%
  tidyr::unnest(use_split) %>%
  filter(use_split != "", use_split != "その他") %>%
  mutate(
    # 2026-09-03: まとめ移行で利用方法が18分類になったため、旧6分類の
    # レベル指定では大半の記録が落ちていた。機能グループに差し替える。
    use_label  = factor(unname(USE_GROUPS[use_split]), levels = USE_GROUP_ORDER),
    subst_label = factor(subst_score, levels = 1:3,
                         labels = c("代替可（1）", "代替困難（2）", "代替不可（3）"))
  ) %>%
  count(use_label, subst_label) %>%
  group_by(use_label) %>%
  mutate(total = sum(n), pct = n / total * 100) %>%
  ungroup() %>%
  mutate(use_label = fct_reorder(use_label,
                                  ifelse(subst_label == "代替不可（3）", pct, 0),
                                  .fun = sum))

p20_use_subst <- ggplot(use_subst_df,
       aes(x = use_label, y = pct, fill = subst_label)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(aes(label = ifelse(pct >= 8, paste0(round(pct), "%\n(n=", n, ")"), "")),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white", fontface = "bold",
            family = "HiraginoSans-W3") +
  coord_flip() +
  scale_fill_manual(values = subst_colors_20, name = "代替可能性") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = c(0, 0), limits = c(0, 102)) +
  labs(
    title    = "利用方法カテゴリー別 代替可能性の分布",
    subtitle = "バーは「代替不可（3）の割合」で降順ソート。バー内に割合と件数を表示。",
    x = "松明における利用方法",
    y = "割合（%）",
    caption = paste0("N = ", nrow(resource_df %>% filter(!is.na(use_types), !is.na(subst_score))),
                     " レコード（利用方法・代替可能性データ両方あり）")
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(
    plot.title  = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "20_use_vs_substitutability.png"), p20_use_subst,
       width = 9, height = 5, dpi = 150)

# ==============================================================================
# 図21a: 日常利用スコアの全体分布
#   1=日常的に使う / 2=ほとんどない / 3=全くない の件数棒グラフ
# ==============================================================================

daily_label_lv <- c("1 日常的に使う", "2 ほとんどない", "3 全くない")
daily_colors   <- c("1 日常的に使う" = "#2CA02C",
                    "2 ほとんどない"  = "#FF7F0E",
                    "3 全くない"      = "#D62728")

daily_overall <- resource_df %>%
  filter(!is.na(daily_score)) %>%
  mutate(daily_label = factor(
    case_when(daily_score == 1 ~ "1 日常的に使う",
              daily_score == 2 ~ "2 ほとんどない",
              TRUE             ~ "3 全くない"),
    levels = daily_label_lv)) %>%
  count(daily_label)

p21a_daily_dist <- ggplot(daily_overall, aes(x = daily_label, y = n, fill = daily_label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 4,
            family = "HiraginoSans-W3") +
  scale_fill_manual(values = daily_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "植物資源の日常利用スコア分布",
    subtitle = paste0("N = ", sum(daily_overall$n), " レコード（日常利用データあり）"),
    x = "日常利用スコア", y = "植物使用レコード数"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

ggsave(file.path(OUTPUT_DIR, "21a_daily_use_distribution.png"), p21a_daily_dist,
       width = 7, height = 5, dpi = 150)

# ==============================================================================
# 図21b: 日常利用 × 代替可能性（クロス集計・積み上げ比率バー）
# ==============================================================================

daily_subst <- resource_df %>%
  filter(!is.na(daily_score), !is.na(subst_score)) %>%
  mutate(
    daily_label = factor(
      case_when(daily_score == 1 ~ "1 日常的に使う",
                daily_score == 2 ~ "2 ほとんどない",
                TRUE             ~ "3 全くない"),
      levels = daily_label_lv),
    subst_label = factor(subst_score, levels = 1:3,
                         labels = c("代替可（1）", "代替困難（2）", "代替不可（3）"))
  ) %>%
  count(daily_label, subst_label) %>%
  group_by(daily_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p21b_daily_subst <- ggplot(daily_subst,
       aes(x = daily_label, y = pct, fill = subst_label)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = ifelse(pct >= 8, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold",
            family = "HiraginoSans-W3") +
  scale_fill_manual(
    values = c("代替可（1）" = "#4DAF4A", "代替困難（2）" = "#FF7F00", "代替不可（3）" = "#E41A1C"),
    name = "代替可能性") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = c(0, 0), limits = c(0, 102)) +
  labs(
    title    = "日常利用スコア別 代替可能性の分布",
    subtitle = "日常利用あり（1）の植物は代替可が多いか？",
    x = "日常利用スコア", y = "割合（%）"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.major.x = element_blank())

ggsave(file.path(OUTPUT_DIR, "21b_daily_vs_substitutability.png"), p21b_daily_subst,
       width = 8, height = 5, dpi = 150)

# ==============================================================================
# 図21c: 植物種別の日常利用スコア（頻出植物のみ）
# ==============================================================================

daily_by_resource <- resource_df %>%
  filter(!is.na(daily_score), resource_norm %in% top_resources) %>%
  group_by(resource_norm) %>%
  summarise(mean_daily = mean(daily_score, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  mutate(resource_norm = fct_reorder(resource_norm, mean_daily))

p21c_daily_resource <- ggplot(daily_by_resource,
       aes(x = resource_norm, y = mean_daily, fill = mean_daily)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0("n=", n)), hjust = -0.1, size = 2.8,
            family = "HiraginoSans-W3") +
  coord_flip() +
  scale_fill_gradient2(low = "#2CA02C", mid = "#FF7F0E", high = "#D62728",
                       midpoint = 2, limits = c(1, 3),
                       name = "平均スコア\n（1=有 3=無）") +
  scale_y_continuous(limits = c(0, 3.5),
                     breaks = 1:3,
                     labels = c("1\n日常的に使う", "2\nほとんどない", "3\n全くない")) +
  labs(
    title    = "植物種別 日常利用スコア（平均）",
    subtitle = "2祭り以上で使用された植物のみ。スコア低い（緑）ほど日常利用あり",
    x = NULL, y = "日常利用スコア（平均）"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "21c_daily_by_resource.png"), p21c_daily_resource,
       width = 9, height = 6, dpi = 150)

# ==============================================================================
# 図21d: 日常利用 × TEK利用タイプ（積み上げ比率バー）
# ==============================================================================

daily_tek <- resource_df %>%
  filter(!is.na(daily_score), !is.na(reason_types)) %>%
  rename(tek_types = reason_types) %>%
  mutate(
    daily_label = factor(
      case_when(daily_score == 1 ~ "1 日常的に使う",
                daily_score == 2 ~ "2 ほとんどない",
                TRUE             ~ "3 全くない"),
      levels = daily_label_lv),
    tek_split = str_split(tek_types, "\\|")
  ) %>%
  tidyr::unnest(tek_split) %>%
  filter(tek_split != "") %>%
  # 2026-09-03: 旧TEK5類型から新しい選定理由10類型に差し替え
  mutate(tek_label = unname(REASON_LABELS[tek_split])) %>%
  filter(!is.na(tek_label)) %>%
  count(tek_label, daily_label) %>%
  group_by(tek_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(tek_label = fct_reorder(tek_label,
                                  ifelse(daily_label == "3 全くない", pct, 0),
                                  .fun = sum))

p21d_daily_tek <- ggplot(daily_tek,
       aes(x = tek_label, y = pct, fill = daily_label)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(aes(label = ifelse(pct >= 8, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold",
            family = "HiraginoSans-W3") +
  coord_flip() +
  scale_fill_manual(values = daily_colors, name = "日常利用") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = c(0, 0), limits = c(0, 102)) +
  labs(
    title    = "TEK利用タイプ別 日常利用スコアの分布",
    subtitle = "バーは「日常利用なし（3）の割合」で降順ソート",
    x = "TEK利用タイプ", y = "割合（%）"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(OUTPUT_DIR, "21d_daily_vs_tek.png"), p21d_daily_tek,
       width = 9, height = 5, dpi = 150)

# ==============================================================================
# 図21e: 日常利用 × 府県
# ------------------------------------------------------------------------------
# 21a〜21dは全体・代替可能性・植物別・選定理由との関係を見てきたが、
# 府県別の日常利用の違いは未確認だった。図23a等と同じ方針で府県ウェイトは
# 適用しない（観測された標本の記述であり、母集団への一般化ではないため）。
# 府県の並びは「日常的に使う＋ほとんどない」の割合が高い順（日常利用が
# 残っている県が上に来る）。
# ==============================================================================

daily_pref <- resource_df %>%
  filter(!is.na(daily_score)) %>%
  mutate(pref = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER),
         daily_label = factor(
           case_when(daily_score == 1 ~ "1 日常的に使う",
                     daily_score == 2 ~ "2 ほとんどない",
                     TRUE             ~ "3 全くない"),
           levels = daily_label_lv))

pref_retain <- daily_pref %>%
  count(pref, daily_label, .drop = FALSE) %>%
  group_by(pref) %>%
  mutate(n_tot = sum(n), pct = n / n_tot) %>%
  ungroup()

pref_order_21e <- pref_retain %>%
  filter(daily_label != "3 全くない") %>%
  group_by(pref) %>%
  summarise(retain = sum(pct), .groups = "drop") %>%
  arrange(retain) %>%
  pull(pref)

cat("\n=== 府県別 日常利用の残存率（1+2の割合） ===\n")
print(as.data.frame(pref_retain %>% filter(daily_label != "3 全くない") %>%
  group_by(pref) %>% summarise(残存率 = round(sum(pct), 2), n = unique(n_tot))))

p21e_daily_pref <- pref_retain %>%
  mutate(pref = factor(pref, levels = pref_order_21e)) %>%
  ggplot(aes(x = pref, y = pct, fill = daily_label)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(data = pref_retain %>% distinct(pref, n_tot) %>%
              mutate(pref = factor(pref, levels = pref_order_21e)),
            aes(x = pref, y = 1.06, label = paste0("n=", n_tot)),
            inherit.aes = FALSE, size = 2.8, color = "gray30") +
  coord_flip() +
  scale_fill_manual(values = daily_colors, name = "日常利用") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  labs(
    title = "府県別 日常利用スコアの分布",
    subtitle = "並びは「日常的に使う＋ほとんどない」の残存率が高い順。府県ウェイトなし（観測された標本の記述）",
    x = NULL, y = "植物資源レコードの割合"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "21e_daily_by_pref.png"), p21e_daily_pref,
       width = 8, height = 4.5, dpi = 150)

write.csv(pref_retain, file.path(OUTPUT_DIR, "daily_use_by_pref.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 図21f: 日常利用スコア × 現在の使用状況（現役／使用停止）
# ------------------------------------------------------------------------------
# 日常利用が失われた資源ほど、その後「使用停止」になりやすいか？という
# 資源基盤の脆弱性の指標。現在使われていない資源（現時点の祭り利用=0）を
# 含める必要があるため resource_df ではなく resource_df_full を使う
# （図28と同じ理由）。
# ==============================================================================

daily_status <- resource_df_full %>%
  filter(!is.na(daily_score)) %>%
  mutate(
    daily_label = factor(
      case_when(daily_score == 1 ~ "1 日常的に使う",
                daily_score == 2 ~ "2 ほとんどない",
                TRUE             ~ "3 全くない"),
      levels = daily_label_lv),
    status_label = factor(ifelse(current_use == 1, "現役", "使用停止"),
                          levels = c("現役", "使用停止"))
  ) %>%
  count(daily_label, status_label, .drop = FALSE) %>%
  group_by(daily_label) %>%
  mutate(n_tot = sum(n), pct = n / n_tot) %>%
  ungroup()

cat("\n=== 日常利用スコア別 使用停止の割合 ===\n")
print(as.data.frame(daily_status %>% filter(status_label == "使用停止") %>%
  transmute(daily_label, 使用停止件数 = n, 使用停止割合 = round(pct, 3), n = n_tot)))

p21f_daily_status <- ggplot(daily_status, aes(x = daily_label, y = pct, fill = status_label)) +
  geom_col(position = "stack", width = 0.55) +
  geom_text(aes(label = ifelse(n > 0, paste0(n, "件"), "")),
            position = position_stack(vjust = 0.5), size = 3.2, color = "white",
            fontface = "bold", family = "HiraginoSans-W3") +
  geom_text(data = daily_status %>% distinct(daily_label, n_tot),
            aes(x = daily_label, y = 1.06, label = paste0("n=", n_tot)),
            inherit.aes = FALSE, size = 2.8, color = "gray30") +
  scale_fill_manual(values = c("現役" = "#BDBDBD", "使用停止" = "#D62728"), name = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  labs(
    title = "日常利用スコア別 使用停止の割合",
    subtitle = paste0("使用停止（現時点の祭り利用=0）の13件中12件が「日常利用=全くない」に集中。\n",
                      "「ほとんどない（2）」からの使用停止は本データ上ゼロ"),
    x = "日常利用スコア", y = "植物資源レコードの割合"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "21f_daily_vs_discontinuation.png"), p21f_daily_status,
       width = 7, height = 5.5, dpi = 150)

write.csv(daily_status, file.path(OUTPUT_DIR, "daily_use_vs_discontinuation.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 図22: 植物ごとの代替可能性 × 日常利用 × 調達方法（植物別小図）
# ------------------------------------------------------------------------------
# 1植物分類群につき1パネル、点＝1サンプル（資源レコード＝祭り×資源名の
# 記録1件。1つの祭りで複数部位が記録されていれば複数点になる）。
# x = 代替可能性　y = 日常利用　色 = 調達方法（3分類・連続的に濃淡が変わる
# 配色だが凡例は離散カテゴリー。図16と同じ配色を再利用）。
#
# 【複数の調達方法をどう扱うか】
#   1レコード内で調達方法が複数併記される場合（「氏子・保存会採取／地域住民
#   提供」等）は、code_embeddedness() が既に「最も嵌入度の高い方法」を採用
#   している（=そのレコードが到達しうる最大の自給度）。したがって色は
#   平均や合成ではなく、この確定済みの離散カテゴリーをそのまま使う。
#   前バージョンで試した「祭り間の連続値平均」は、複数の調達方法を1つの
#   数値に潰してしまい実態が見えにくかったため、この案に差し替えた。
# ------------------------------------------------------------------------------

embed_colors_22 <- c(
  "3 共同体が自ら採取・栽培" = "#1A6A1A",
  "2 地域内から無償で入手"   = "#74C476",
  "1 購入・地域外に依存"     = "#D62728",
  "不明（調達方法未記録）"   = "gray70"
)

taxon_order_22 <- plant_festival %>%
  count(resource_taxon, name = "n_festivals") %>%
  arrange(desc(n_festivals)) %>%
  pull(resource_taxon)

sample_22 <- resource_df %>%
  filter(!is.na(resource_taxon), resource_taxon != "非植物資材",
         !is.na(subst_score), !is.na(daily_score)) %>%
  mutate(
    embed_label = case_when(
      embed_score == 3 ~ "3 共同体が自ら採取・栽培",
      embed_score == 2 ~ "2 地域内から無償で入手",
      embed_score == 1 ~ "1 購入・地域外に依存",
      TRUE             ~ "不明（調達方法未記録）"
    ),
    embed_label = factor(embed_label, levels = names(embed_colors_22))
  )

facet_n <- sample_22 %>% distinct(festival, resource_taxon) %>% count(resource_taxon, name = "n_fes")
sample_22 <- sample_22 %>%
  left_join(facet_n, by = "resource_taxon") %>%
  group_by(resource_taxon) %>%
  mutate(n_rec = n()) %>%
  ungroup() %>%
  mutate(taxon_label = paste0(resource_taxon, "（", n_fes, "祭り, ", n_rec, "点）"),
         taxon_label = factor(taxon_label,
           levels = unique(taxon_label[order(match(resource_taxon, taxon_order_22))])))

cat("
=== 図22 サンプル数（植物分類群別） ===
")
print(as.data.frame(sample_22 %>% distinct(resource_taxon, taxon_label, n_fes, n_rec) %>%
  arrange(match(resource_taxon, taxon_order_22))))

set.seed(20260903)
# 【2026-09-06改訂】調達方法による色分けを廃止（代替可能性×日常利用の
# 分布そのものに集中させるため）。調達方法は図22b（後述）で別途扱う。
p22 <- ggplot(sample_22, aes(x = subst_score, y = daily_score)) +
  geom_jitter(width = 0.16, height = 0.16, size = 2.2, alpha = 0.7, color = "#2C7FB8") +
  facet_wrap(~ taxon_label, ncol = 6) +
  scale_x_continuous(limits = c(0.5, 3.5), breaks = 1:3,
                     labels = c("1
代替可", "2
代替困難", "3
代替不可")) +
  scale_y_continuous(limits = c(0.5, 3.5), breaks = 1:3,
                     labels = c("1
日常的に
使う", "2
ほとんど
ない", "3
全く
ない")) +
  labs(
    title = "植物ごとの代替可能性・日常利用・調達方法",
    subtitle = paste0("1点＝1資源レコード（祭り×資源名）。同じ祭りで複数部位が記録されている場合は複数点。
",
                      "パネルの見出しは（利用祭り数, レコード点数）。点は重なりを避けるため位置を微小にずらしている（ジッター）。
",
                      "右上＝代替できず・日常利用が失われた祭礼専用資源"),
    x = "代替可能性", y = "日常利用"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 7.5),
        axis.text = element_text(size = 7))

n_panels <- n_distinct(sample_22$resource_taxon)
ggsave(file.path(OUTPUT_DIR, "22_subst_daily_embed.png"), p22,
       width = 13, height = ceiling(n_panels / 6) * 2.1 + 1.5, dpi = 150)

write.csv(
  sample_22 %>%
    mutate(pref = unname(FESTIVAL_PREF[festival])) %>%
    select(pref, festival, resource_taxon, resource_raw, subst_score, daily_score,
           embed_score, embed_label, method_cat, is_substitute_material),
  file.path(OUTPUT_DIR, "plant_use_profile.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図23: 調達方法の内訳 — 府県別・植物別
# ------------------------------------------------------------------------------
# 図16（祭り別）と同じ設計を、集計単位だけ府県／植物分類群に変えて再利用する。
# 府県ウェイトは使わない：ここでの問いは「観測された30祭りの範囲内で、
# 府県や植物ごとに調達方法がどう違うか」という記述であり、母集団への
# 一般化ではないため（母集団推定が要る分析は図03a・keystone_scoreで別途実施済み）。
# ------------------------------------------------------------------------------

embed_lv <- c("3: 共同体が自ら採取・栽培", "2: 地域内から無償で入手", "1: 購入・地域外に依存")
embed_pal <- c("3: 共同体が自ら採取・栽培" = "#1A6A1A",
              "2: 地域内から無償で入手"   = "#74C476",
              "1: 購入・地域外に依存"     = "#D62728")

embed_records <- resource_df %>%
  filter(!is.na(embed_score)) %>%
  mutate(pref = unname(FESTIVAL_PREF[festival]),
         embed_label = factor(embed_score, levels = c(3, 2, 1), labels = embed_lv))

# 共通のプロット関数（集計キーだけ差し替える）
plot_embed_breakdown <- function(df, group_var, title, subtitle, y_lab = NULL,
                                  order_desc = TRUE) {
  summary_tbl <- df %>%
    group_by({{ group_var }}) %>%
    summarise(mean_embed = mean(embed_score, na.rm = TRUE),
              n_total = n(), .groups = "drop") %>%
    arrange(if (order_desc) desc(mean_embed) else mean_embed)
  ord <- summary_tbl %>% pull({{ group_var }})

  long_tbl <- df %>%
    count({{ group_var }}, embed_label) %>%
    group_by({{ group_var }}) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    mutate("{{group_var}}" := factor({{ group_var }}, levels = ord))

  label_tbl <- summary_tbl %>%
    mutate("{{group_var}}" := factor({{ group_var }}, levels = ord))

  list(
    summary = summary_tbl,
    plot = ggplot(long_tbl, aes(x = {{ group_var }}, y = pct, fill = embed_label)) +
      geom_col(position = "stack", width = 0.7) +
      geom_text(data = label_tbl,
                aes(x = {{ group_var }}, y = 1.06,
                    label = sprintf("%.1f (n=%d)", mean_embed, n_total)),
                inherit.aes = FALSE, size = 3, color = "gray30",
                family = "HiraginoSans-W3") +
      coord_flip() +
      scale_fill_manual(values = embed_pal, name = "調達方法") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.16)) +
      labs(title = title, subtitle = subtitle, x = y_lab,
           y = "植物資源レコードの割合") +
      theme_bw(base_family = "HiraginoSans-W3") +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  )
}

# --- 23a: 府県別 ---
res_pref <- plot_embed_breakdown(
  embed_records, pref,
  title = "調達方法の内訳（府県別）",
  subtitle = "右の数値 = 嵌入度平均 (n=記録数)。府県ウェイトなし（観測された標本の記述）"
)
cat("
=== 調達方法（府県別） ===
"); print(as.data.frame(res_pref$summary))
ggsave(file.path(OUTPUT_DIR, "23a_embed_by_pref.png"), res_pref$plot,
       width = 9, height = 4.5, dpi = 150)

# --- 23b: 植物別 ---
res_taxon <- plot_embed_breakdown(
  embed_records, resource_taxon,
  title = "調達方法の内訳（植物別）",
  subtitle = "右の数値 = 嵌入度平均 (n=記録数)。府県ウェイトなし（観測された標本の記述）"
)
cat("
=== 調達方法（植物別） ===
"); print(as.data.frame(res_taxon$summary))
ggsave(file.path(OUTPUT_DIR, "23b_embed_by_plant.png"), res_taxon$plot,
       width = 9.5, height = max(6, nrow(res_taxon$summary) * 0.34), dpi = 150)

write.csv(res_pref$summary, file.path(OUTPUT_DIR, "embed_by_pref.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(res_taxon$summary, file.path(OUTPUT_DIR, "embed_by_plant.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 2026-09-06: 図23c（同じ植物が府県によって調達方法が変わるか）は削除。
# 府県の次元を外すと23b（植物別の調達方法）と同じ内容になるため。
# pref_x_plant（植物×府県別の嵌入度）はplant_x_landscape.csv等と同様の
# 生データとして embed_pref_x_plant.csv に残す。
pref_x_plant <- embed_records %>%
  group_by(resource_taxon, pref) %>%
  summarise(mean_embed = mean(embed_score), n = n(), .groups = "drop")

write.csv(
  pref_x_plant %>% arrange(resource_taxon, desc(mean_embed)),
  file.path(OUTPUT_DIR, "embed_pref_x_plant.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図28: 植物ごとの調達地の変化（結果5「調達地の変化」）
# ------------------------------------------------------------------------------
# change_cat（1=変化なし／2A=以前より近い／2B=以前より広い／3=使用停止）を
# これまで一度も可視化していなかったため新設する。
# 【現在使われていない資源も含める】主分析（resource_df）は current_use==1
# に絞っているが、この図は「資源基盤がどう変容したか」を見るものなので
# 使用停止（＝discontinued_resources.csvの13件）も対象に含める必要がある。
# そのためここだけ resource_df_full（current_useで絞る前）を使う。
# 対象は2記録以上でchange_catが記録された17分類群。植物の並びは
# TAXON_ORDERではなく「変化が大きい順」（以前より広い＋使用停止の割合が
# 高い順）——この図の主題そのものが変化の大きさなので、生活形順より
# 変化順の方が読みやすい。
# ------------------------------------------------------------------------------

CHANGE_PAL <- c(
  "以前より近い" = "#1A6A1A",  # 調達が身近になった＝改善
  "変化なし"     = "#BDBDBD",  # 中立
  "以前より広い" = "#FD8D3C",  # 遠方化＝負荷増
  "使用停止"     = "#D62728"   # 使用停止＝最も深刻
)
CHANGE_LEVELS <- names(CHANGE_PAL)

change_records <- resource_df_full %>%
  filter(!is.na(change_cat)) %>%
  mutate(change_cat = factor(as.character(change_cat), levels = CHANGE_LEVELS))

change_denom <- change_records %>%
  count(resource_taxon, name = "n_rec") %>%
  filter(n_rec >= 2)

cat("\n=== 図28 除外（記録2件未満、", sum(!change_records$resource_taxon %in% change_denom$resource_taxon),
    "件）===\n")

change_summary <- change_records %>%
  filter(resource_taxon %in% change_denom$resource_taxon) %>%
  count(resource_taxon, change_cat, .drop = FALSE) %>%
  left_join(change_denom, by = "resource_taxon") %>%
  mutate(pct = n / n_rec)

severity <- change_summary %>%
  filter(change_cat %in% c("以前より広い", "使用停止")) %>%
  group_by(resource_taxon) %>%
  summarise(severity = sum(pct), .groups = "drop") %>%
  right_join(change_denom, by = "resource_taxon") %>%
  mutate(severity = replace_na(severity, 0)) %>%
  arrange(severity)

cat("\n=== 植物ごとの調達地変化（変化が大きい順） ===\n")
print(as.data.frame(severity %>% arrange(desc(severity)) %>%
  transmute(resource_taxon, n_rec, 変化スコア = round(severity, 2))))

change_summary <- change_summary %>%
  mutate(taxon_label = paste0(resource_taxon, "（", n_rec, "件）"),
         taxon_label = factor(taxon_label,
           levels = paste0(severity$resource_taxon, "（", severity$n_rec, "件）")))

p28 <- ggplot(change_summary, aes(x = taxon_label, y = pct, fill = change_cat)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = CHANGE_PAL, name = "調達地の変化", drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "植物ごとの調達地の変化",
    subtitle = paste0("2記録以上ある", nrow(change_denom), "分類群。現在使われていない資源（使用停止）も含む\n",
                      "並びは変化が大きい順（「以前より広い」＋「使用停止」の割合が高い順）"),
    x = NULL, y = "資源レコードの割合"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "28_procurement_change_by_plant.png"), p28,
       width = 9, height = max(5, nrow(change_denom) * 0.4), dpi = 150)

write.csv(
  change_summary %>% select(resource_taxon, n_rec, change_cat, n, pct),
  file.path(OUTPUT_DIR, "procurement_change_by_plant.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図18: 生息地依存ネットワーク（祭り × 景観タイプ）
#   二部グラフ（bipartite）：祭り（左）― 景観タイプ（右）
#   ggplot で簡易的に可視化
# ==============================================================================

habitat_net <- resource_df %>%
  filter(!is.na(landscape_all), landscape_all != "なし") %>%
  select(-landscape_norm) %>%
  separate_rows(landscape_all, sep = "\\|") %>%
  rename(landscape_norm = landscape_all) %>%
  distinct(festival, landscape_norm) %>%
  group_by(landscape_norm) %>%
  mutate(n_festivals_using = n()) %>%
  ungroup()

# 景観タイプ別に依存祭り数を集計
habitat_summary <- habitat_net %>%
  group_by(landscape_norm) %>%
  summarise(
    n_festivals = n_distinct(festival),
    festivals   = paste(festival, collapse = "\n"),
    .groups = "drop"
  ) %>%
  arrange(desc(n_festivals))

cat("\n=== 生息地タイプ別依存祭り数 ===\n")
print(as.data.frame(habitat_summary))

# 二部グラフ用座標
hab_types  <- habitat_summary$landscape_norm
fest_using <- unique(habitat_net$festival)

node_df <- bind_rows(
  data.frame(
    name  = hab_types,
    type  = "habitat",
    x     = 1,
    y     = seq(1, length(hab_types)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    name  = fest_using,
    type  = "festival",
    x     = 3,
    y     = seq(1, length(fest_using)),
    stringsAsFactors = FALSE
  )
)

edge_df <- habitat_net %>%
  left_join(node_df %>% filter(type == "habitat") %>%
              select(name, y_hab = y), by = c("landscape_norm" = "name")) %>%
  left_join(node_df %>% filter(type == "festival") %>%
              select(name, y_fest = y), by = c("festival" = "name"))

# 2026-09-03: まとめ移行で景観カテゴリーが6種（森林/水田/湿地/畑/荒地/庭）
# から11種（二次林・人工林・竹林・神社林・海岸防災林 等に細分）に変わった際、
# この配色が更新されておらず、竹林(19件)・二次林(16件)・人工林(15件)という
# 最頻カテゴリーが無配色（デフォルトのNA色）になっていた。LANDSCAPE_PAL に
# 差し替える。
hab_colors_net <- LANDSCAPE_PAL

p18_network <- ggplot() +
  geom_segment(data = edge_df,
               aes(x = 1, xend = 3, y = y_hab, yend = y_fest,
                   color = landscape_norm),
               alpha = 0.35, linewidth = 0.6) +
  geom_point(data = node_df %>% filter(type == "habitat"),
             aes(x = x, y = y, color = name), size = 8, alpha = 0.9) +
  geom_point(data = node_df %>% filter(type == "festival"),
             aes(x = x, y = y), size = 4, color = "gray40") +
  geom_text(data = node_df %>% filter(type == "habitat"),
            aes(x = x - 0.15, y = y, label = name),
            hjust = 1, size = 3.2, family = "HiraginoSans-W3") +
  geom_text(data = node_df %>% filter(type == "festival"),
            aes(x = x + 0.1, y = y, label = name),
            hjust = 0, size = 2.8, family = "HiraginoSans-W3") +
  scale_color_manual(values = hab_colors_net, guide = "none") +
  scale_x_continuous(limits = c(0.3, 4.5)) +
  annotate("text", x = 1, y = max(node_df$y) + 0.7,
           label = "生息地タイプ", fontface = "bold", size = 3.5,
           family = "HiraginoSans-W3") +
  annotate("text", x = 3, y = max(node_df$y) + 0.7,
           label = "祭り", fontface = "bold", size = 3.5,
           family = "HiraginoSans-W3") +
  labs(
    title = "生息地依存ネットワーク",
    subtitle = "線 = その生息地タイプの植物を使用する関係"
  ) +
  theme_void(base_family = "HiraginoSans-W3") +
  # theme_void() は rect = element_blank() を含むため plot.background も透明になり、
  # ビューアによっては透明部分が黒く表示されて黒文字（デフォルト色）のラベルや
  # タイトルが見えなくなる。明示的に白背景を戻す。
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"))

ggsave(file.path(OUTPUT_DIR, "18_habitat_dependency_network.png"), p18_network,
       width = 11, height = 9, dpi = 150)

# ==============================================================================
# 図24: 植物 × 調達地の生態景観類型
# ------------------------------------------------------------------------------
# 24a 総合図：全30祭りを合わせた 植物×景観タイプ のヒートマップ
# 24b 府県別小図：同じヒートマップを府県ごとに分割（facet）
#
# 【解析単位】資源レコード（祭り×資源名）。1レコードが複数の景観に由来する
# 場合（例：「湿地|荒地」）は両方に计上する（図12・図18と同じ landscape_all
# の展開方式）。「なし」（景観不明）は除外。
# 【色】件数（カウント）の連続グラデーション。図23の調達方法（3分類・
# 自給↔購入という順序尺度）とは異なり、景観タイプに優劣の順序はないため、
# 発散配色ではなく単色系の濃淡（多いほど濃い青）を使う。府県ウェイトは
# 適用しない（観測された標本の記述）。
# ------------------------------------------------------------------------------

landscape_records <- resource_df %>%
  filter(!is.na(resource_taxon), resource_taxon != "非植物資材",
         !is.na(landscape_all), landscape_all != "なし") %>%
  select(-landscape_norm) %>%
  separate_rows(landscape_all, sep = "\\|") %>%
  rename(landscape_type = landscape_all) %>%
  mutate(pref = unname(FESTIVAL_PREF[festival]))

# 2026-09-06: 植物の並びを出現頻度順からTAXON_ORDER（生活形）順に変更
# （図03a/03b・図17bと同じ論理的順序に統一）。
taxon_order_24 <- levels(order_taxon(landscape_records$resource_taxon))
land_order_24  <- landscape_records %>% count(landscape_type, sort = TRUE) %>% pull(landscape_type)

cat("
=== 図24 対象レコード:", nrow(landscape_records), "件（",
    n_distinct(paste(landscape_records$festival, landscape_records$resource_raw)),
    "件の資源レコードが複数景観のため展開）===
")

# --- 24a: 総合図 ---
mat_24a <- landscape_records %>%
  count(resource_taxon, landscape_type) %>%
  mutate(resource_taxon = factor(resource_taxon, levels = rev(taxon_order_24)),
         landscape_type = factor(landscape_type, levels = land_order_24))

p24a <- ggplot(mat_24a, aes(x = landscape_type, y = resource_taxon, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 2.8, family = "HiraginoSans-W3", color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      name = "レコード数") +
  labs(
    title = "植物 × 調達地の生態景観類型（総合）",
    subtitle = paste0("全30祭り、資源レコード", n_distinct(paste(landscape_records$festival, landscape_records$resource_raw)),
                      "件。複数景観に由来する記録は両方に計上（合計は資源レコード数を超える）"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "24a_plant_x_landscape.png"), p24a,
       width = 9, height = max(6, n_distinct(mat_24a$resource_taxon) * 0.33), dpi = 150)

# --- 24b: 府県別小図 ---
# 全パネルで植物・景観の並び順を24aと統一する（比較のため）。
# セルが1件も無い府県×植物×景観の組み合わせは白のまま（0件ではなく描画しない）。
mat_24b <- landscape_records %>%
  count(pref, resource_taxon, landscape_type) %>%
  mutate(pref = factor(pref, levels = PREF_ORDER),
         resource_taxon = factor(resource_taxon, levels = rev(taxon_order_24)),
         landscape_type = factor(landscape_type, levels = land_order_24))

p24b <- ggplot(mat_24b, aes(x = landscape_type, y = resource_taxon, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), size = 2.3, family = "HiraginoSans-W3", color = "gray15") +
  facet_wrap(~ pref, ncol = 3) +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      name = "レコード数") +
  labs(
    title = "植物 × 調達地の生態景観類型（府県別）",
    subtitle = "植物・景観の並び順は24aと共通。府県ウェイトなし（観測された標本の記述、府県間で総数が異なる点に注意）",
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 40, hjust = 1, size = 6.5),
        axis.text.y = element_text(size = 6.5))

ggsave(file.path(OUTPUT_DIR, "24b_plant_x_landscape_by_pref.png"), p24b,
       width = 14, height = max(9, n_distinct(mat_24b$resource_taxon) * 0.42), dpi = 150)

write.csv(
  mat_24a %>% arrange(desc(n)),
  file.path(OUTPUT_DIR, "plant_x_landscape.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  mat_24b %>% arrange(pref, desc(n)),
  file.path(OUTPUT_DIR, "plant_x_landscape_by_pref.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図25: 関係者数 × 観光客数 × 植物資源種数 × 府県
# ------------------------------------------------------------------------------
# 図10（2×2祭りタイプで色分け）と同じ scale_df を使うが、色を府県に差し替える。
# x = 関係者数　y = 観光客数　点の大きさ = 植物資源種数　色 = 都道府県
# ------------------------------------------------------------------------------

PREF_PAL <- c(
  "滋賀県"   = "#1B9E77",
  "京都府"   = "#D95F02",
  "大阪府"   = "#7570B3",
  "兵庫県"   = "#E7298A",
  "奈良県"   = "#66A61E",
  "和歌山県" = "#E6AB02"
)

scale_df_pref <- scale_df %>%
  mutate(pref = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER)) %>%
  filter(!is.na(tourists))

p25 <- ggplot(scale_df_pref, aes(x = participants, y = tourists + 1)) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "gray75", linewidth = 0.5) +
  geom_hline(yintercept = 1001, linetype = "dashed", color = "gray75", linewidth = 0.5) +
  geom_point(aes(color = pref, size = n_resources), alpha = 0.85) +
  geom_text_repel(aes(label = festival), size = 2.7, max.overlaps = 25,
                  family = "HiraginoSans-W3") +
  scale_x_continuous(transform = "log10", labels = scales::comma,
                     breaks = c(10, 30, 100, 200, 500, 1000, 2000)) +
  scale_y_continuous(transform = "log10",
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c("0", "10", "100", "1千", "1万", "10万", "100万")) +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  scale_size_continuous(range = c(2.5, 9), name = "植物資源種数") +
  labs(
    title = "関係者数 × 観光客数 × 植物資源種数 × 都道府県",
    subtitle = paste0("点の大きさ＝植物資源種数、色＝都道府県　",
                      "縦破線: 関係者数200人、横破線: 観光客1000人（図10と同じ目安線）"),
    x = "祭り関係者数（人）[対数]",
    y = "観光客数（人）[対数]",
    caption = "雄琴学区ヨシ松明一斉点火・熊野速玉大社は観光客数不明のため除外"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "25_scale_x_tourists_x_resources_x_pref.png"), p25,
       width = 11, height = 7.5, dpi = 150)

write.csv(
  scale_df_pref %>% select(pref, festival, participants, tourists, n_resources),
  file.path(OUTPUT_DIR, "scale_x_tourists_x_resources_x_pref.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 図26: 府県別 平均植物資源種数（祭りあたり）
# ------------------------------------------------------------------------------
# 【図25の点の大きさについての確認】
# 図25の点の大きさ（植物資源種数）は各祭り自身の distinct(resource_taxon) で
# あり、他の祭りと合算した合計値ではない（1点＝1祭り、他祭りの値は混じらない）。
# 30祭り分を単純に足すと合計になるが、これは図25では使っていない。
# 「祭りあたりの平均」が意味を持つのは、複数の祭りを束ねる単位＝府県のレベル
# であるため、ここでは府県ごとに（その県の祭りの植物資源種数の単純平均）を示す。
#
# 【2026-09-06 改訂】旧版は「分類群ベース」と「資源名ベース（部位を分けた
# まま）」の2本を並べていた——独自の正規表現グルーピング（normalize_taxon）
# が部位違いの資源を過度に統合するケース（例：がんがら火祭りのアカマツの薪＋
# 肥松を同一taxonに統合）があり、両者の差を見せる意味があった。まとめ.xlsx
# 側で植物の種類・材質（taxon_kind）が人手コード済みになった今、この2指標は
# 同一の値になるため1本の棒に統合した。
# ------------------------------------------------------------------------------

div_by_pref <- resource_df %>%
  group_by(festival) %>%
  summarise(n_taxon = n_distinct(resource_taxon), .groups = "drop") %>%
  mutate(pref = factor(unname(FESTIVAL_PREF[festival]), levels = PREF_ORDER)) %>%
  group_by(pref) %>%
  summarise(n_festivals = n(), mean_taxon = mean(n_taxon), .groups = "drop") %>%
  arrange(desc(mean_taxon))

cat("
=== 府県別 平均植物資源種数（祭りあたり） ===
")
print(as.data.frame(div_by_pref %>% mutate(mean_taxon = round(mean_taxon, 2))))

p26 <- div_by_pref %>%
  mutate(pref = factor(pref, levels = rev(div_by_pref$pref))) %>%
  ggplot(aes(x = mean_taxon, y = pref)) +
  geom_col(fill = "#08519C", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", mean_taxon)), hjust = -0.2, size = 3.2,
            family = "HiraginoSans-W3") +
  geom_text(aes(x = -0.4, label = paste0("n=", n_festivals)),
            size = 2.8, color = "gray40", hjust = 1, family = "HiraginoSans-W3") +
  scale_x_continuous(limits = c(-1.2, 6.5), breaks = 0:6) +
  labs(
    title = "府県別 平均植物資源種数（祭りあたり）",
    subtitle = "県内の祭りごとの植物資源種数（taxon_kind、まとめ.xlsx人手コード）を単純平均。左端のn=はその県の祭り数",
    x = "祭りあたりの平均植物資源種数", y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

ggsave(file.path(OUTPUT_DIR, "26_mean_resources_by_pref.png"), p26,
       width = 8, height = 4.5, dpi = 150)

write.csv(div_by_pref, file.path(OUTPUT_DIR, "mean_resources_by_pref.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 図27: 資源をめぐる話題頻度・生産保全活動の交差分析
# ------------------------------------------------------------------------------
# 27a 話題頻度 × 管理活動レベル のクロス表
# 27b 話題・管理活動と、既出の変数（調達嵌入度・祭りタイプ・府県）との関係
#
# 【解釈上の注意】
# mgmt_score は旧14祭り（2026-06調査）と新16祭り（2026-08追加調査）で
# 分布が偏っている：旧14祭りは11/14が「全面的（3）」、新16祭りは13/16が
# 「採取・管理のみ（2）」。実際に生産活動の違いを反映している可能性もあるが、
# 聞き取り・コーディングの時期差（新しい調査ほど「計画栽培はしていない」と
# いう限定を明示的に聞き取れている）による可能性も排除できない。府県との
# 関係を見る際は、この2群がどの府県に対応するかも併せて確認すること。
# ==============================================================================

cat("
=== 話題スコア × 管理活動スコア クロス表 ===
")
print(table(topic = festival_engagement$topic_score, mgmt = festival_engagement$mgmt_score))
cat("Spearman相関（話題 vs 管理活動）:",
    round(cor(festival_engagement$topic_score, festival_engagement$mgmt_score,
              method = "spearman"), 3), "
")

# --- 27a: クロス表ヒートマップ ---
cross27a <- festival_engagement %>% count(topic_label, mgmt_label, .drop = FALSE)

p27a <- ggplot(cross27a, aes(x = mgmt_label, y = topic_label, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 5, color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      name = "祭り数") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, 8)) +
  labs(
    title = "資源についての話題頻度 × 生産保全活動レベル",
    subtitle = paste0("30祭り。Spearman相関 = ",
                      round(cor(festival_engagement$topic_score, festival_engagement$mgmt_score,
                                method = "spearman"), 2),
                      "（ほぼ無相関）\n",
                      "「よく話す」祭りが必ずしも「全面的な生産」ではない"),
    x = "生産保全活動レベル", y = "話題頻度"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank())

ggsave(file.path(OUTPUT_DIR, "27a_topic_x_management.png"), p27a,
       width = 8.5, height = 6.3, dpi = 150)

# --- 27b: 既出変数との関係 ---
engagement_join <- festival_engagement %>%
  left_join(embed_festival %>% select(festival, mean_embed), by = "festival") %>%
  left_join(scale_df %>% select(festival, festival_type), by = "festival")

cat("
=== 府県別 平均スコア ===
")
print(as.data.frame(engagement_join %>% group_by(pref) %>%
  summarise(n = n(), 話題平均 = round(mean(topic_score), 2),
            管理活動平均 = round(mean(mgmt_score), 2), .groups = "drop")))

cat("
=== 祭りタイプ別 平均スコア ===
")
print(as.data.frame(engagement_join %>% group_by(festival_type) %>%
  summarise(n = n(), 話題平均 = round(mean(topic_score), 2),
            管理活動平均 = round(mean(mgmt_score), 2), .groups = "drop")))

cat("
=== 嵌入度との相関（Spearman）===
")
cat("話題 vs 嵌入度:", round(cor(engagement_join$topic_score, engagement_join$mean_embed,
                              method = "spearman", use = "complete.obs"), 3), "
")
cat("管理活動 vs 嵌入度:", round(cor(engagement_join$mgmt_score, engagement_join$mean_embed,
                                method = "spearman", use = "complete.obs"), 3), "
")

set.seed(20260903)
p27b1 <- ggplot(engagement_join, aes(x = mgmt_score, y = mean_embed, color = pref)) +
  geom_jitter(width = 0.12, height = 0, size = 2.8, alpha = 0.85) +
  scale_x_continuous(breaks = 1:3, limits = c(0.6, 3.4)) +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  labs(title = "管理活動 × 調達嵌入度", x = "管理活動レベル", y = "調達嵌入度（祭り平均）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 10, face = "bold"), legend.position = "none")

p27b2 <- ggplot(engagement_join, aes(x = topic_score, y = mean_embed, color = pref)) +
  geom_jitter(width = 0.12, height = 0, size = 2.8, alpha = 0.85) +
  scale_x_continuous(breaks = 1:4, limits = c(0.6, 4.4)) +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  labs(title = "話題頻度 × 調達嵌入度", x = "話題頻度", y = "調達嵌入度（祭り平均）") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 10, face = "bold"))

p27b3 <- ggplot(engagement_join, aes(x = festival_type, y = mgmt_score)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = "gray90") +
  geom_jitter(aes(color = pref), width = 0.12, height = 0.05, size = 2.5, alpha = 0.85) +
  scale_y_continuous(breaks = 1:3, limits = c(0.6, 3.4)) +
  scale_color_manual(values = PREF_PAL, guide = "none") +
  labs(title = "祭りタイプ別の管理活動レベル", x = NULL, y = "管理活動レベル") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

p27b4 <- ggplot(engagement_join, aes(x = festival_type, y = topic_score)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = "gray90") +
  geom_jitter(aes(color = pref), width = 0.12, height = 0.05, size = 2.5, alpha = 0.85) +
  scale_y_continuous(breaks = 1:4, limits = c(0.6, 4.4)) +
  scale_color_manual(values = PREF_PAL, name = "都道府県") +
  labs(title = "祭りタイプ別の話題頻度", x = NULL, y = "話題頻度") +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

p27b <- (p27b1 + p27b2) / (p27b3 + p27b4) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "話題頻度・管理活動レベルと、調達嵌入度・祭りタイプ・府県との関係",
    subtitle = "色＝都道府県（左右2列で共通）。x軸はジッターで重なりを分散",
    theme = theme(plot.title = element_text(face = "bold", family = "HiraginoSans-W3"),
                  plot.subtitle = element_text(family = "HiraginoSans-W3"))
  )

ggsave(file.path(OUTPUT_DIR, "27b_topic_mgmt_vs_context.png"), p27b,
       width = 11, height = 9, dpi = 150)

write.csv(
  engagement_join %>%
    select(pref, festival, festival_type, topic_score, topic_label,
           mgmt_score, mgmt_label, mean_embed),
  file.path(OUTPUT_DIR, "topic_management_engagement.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 選定理由コーディング仕様書（Excel）
# ------------------------------------------------------------------------------
# code_reason()/REASON_RULES による10類型コーディングの完全な仕様書。
# 選定理由（結果2「植物の選定理由（要点）」）はまとめ.xlsx側でもコード化
# されていない唯一の主要自由記述項目であり、この帰納的コーディングの根拠を
# 誰でも検証できるようにするための出力。
#   シート1 コーディング表：タイプ・ラベル・定義・判定キーワード一覧
#   シート2 判定ロジック：適用対象・判定単位・多重ラベルの扱い・限界
#   シート3 適用結果（全件）：レコードごとの原文と付与タイプ
#   シート4 タイプ別件数：検算用の単純集計
# ==============================================================================

reason_type_defs <- tribble(
  ~type, ~jp_label, ~definition,
  "burn",   "燃焼特性",       "燃えやすさ・火力・持続時間・油分・煙など、燃焼そのものに関わる性質",
  "phys",   "物理・加工特性", "まっすぐさ・軽さ・強度・しなやかさ・太さ長さ等の寸法・加工しやすさなど、構造材としての物理的性質",
  "sens",   "感覚・美的",     "色・香り・見た目・音・緑・清浄感・装飾性など、五感に訴える性質",
  "avail",  "入手容易性",     "手に入りやすさ・地域に多い・身近・調達の容易さ",
  "byprod", "生業副産物",     "農林業の副産物であること・裏作・間伐材・不要材の循環利用",
  "trad",   "伝統・慣習",     "昔からの材料であること・伝統・由来・継承",
  "symb",   "象徴・宗教",     "縁起・神聖さ・魔除け・奉納・豊穣の象徴・伝承との結び付き",
  "subst",  "代替・制約",     "本命の資源が確保できない・高価・技術低下などによる代替選択（積極的選好ではない）",
  "social", "社会的機能",     "子供の参加・世代継承・安全性・村同士の競い合いなど、資源の物理的性質以外の社会的機能",
  "env",    "環境保全",       "水質浄化・環境保護政策・里山保全そのものを目的とする選択"
)

reason_kw_rows <- lapply(REASON_RULES, function(r) {
  tibble(type = r[1], keywords = str_split(r[2], "\\|")[[1]])
}) %>% bind_rows()

codebook_sheet1 <- reason_type_defs %>%
  left_join(reason_kw_rows %>% group_by(type) %>%
              summarise(keyword_list = paste(keywords, collapse = " ／ ")),
            by = "type") %>%
  transmute(タイプコード = type, ラベル = jp_label, 定義 = definition,
            判定キーワード = keyword_list) %>%
  bind_rows(tribble(
    ~タイプコード, ~ラベル, ~定義, ~判定キーワード,
    "(前処理)", "―",
    "原文が「不明」「理由なし」「未確認」で始まる場合は全類型NAとする",
    "―",
    "(誤検出回避)", "―",
    "「田遊び」（農耕儀礼の名称）を「田儀礼」に置換してから判定する。socialタイプの「遊び」との誤マッチを防ぐため",
    "―"
  ))

codebook_sheet2 <- tribble(
  ~項目, ~説明,
  "適用対象", "分析内容まとめ.xlsx 結果2「植物の選定理由（要点）」列（自由記述、まとめ側でもコード化されていない唯一の主要項目）",
  "判定単位", "1レコード（祭り×資源名）の選定理由テキスト1件",
  "判定方法", "10類型それぞれについて、対応するキーワード群のいずれかが原文に部分一致（str_detect、正規表現OR）すれば、そのタイプを付与する",
  "多重ラベル", "1レコードが複数タイプに該当する場合はすべて付与する（排他的分類ではない）",
  "該当なし", "定義された10類型のいずれのキーワードにも一致しない場合はNA（理由コーディングなし）として扱う",
  "府県ウェイト", "図17a/17b・図19bではタイプの出現割合を府県の抽出率に応じた事後層化ウェイトで補正している。ウェイトの定義自体はこの表の対象外（pref_weights()参照）",
  "限界", "キーワードは実際の原文から帰納的に作成した一覧であり、まとめ.xlsxが将来的に選定理由も統制語彙化した場合は本コーディングは不要になる。新しい表現パターンが今後の追加データで出現した場合、本表のキーワードでは拾えない可能性がある"
)

codebook_sheet3 <- resource_df %>%
  filter(!is.na(reason_raw)) %>%
  transmute(
    祭り名 = festival,
    植物分類群 = resource_taxon,
    使用部位 = part,
    選定理由_原文 = reason_raw,
    付与タイプ = ifelse(is.na(reason_types), "(該当なし)",
                    str_replace_all(reason_types, "\\|", " ／ ")),
    付与タイプ_日本語 = ifelse(is.na(reason_types), "(該当なし)",
      vapply(str_split(reason_types, "\\|"), function(ts)
        paste(unname(REASON_LABELS[ts]), collapse = " ／ "), character(1)))
  ) %>%
  arrange(祭り名, 植物分類群)

codebook_sheet4 <- resource_df %>%
  filter(!is.na(reason_types)) %>%
  separate_rows(reason_types, sep = "\\|") %>%
  count(reason_types, name = "件数") %>%
  left_join(reason_type_defs %>% select(type, jp_label), by = c("reason_types" = "type")) %>%
  transmute(タイプコード = reason_types, ラベル = jp_label, 件数) %>%
  arrange(desc(件数))

write_xlsx(
  list("コーディング表" = codebook_sheet1,
       "判定ロジック" = codebook_sheet2,
       "適用結果（全件）" = codebook_sheet3,
       "タイプ別件数（検算）" = codebook_sheet4),
  path = file.path(OUTPUT_DIR, "選定理由_コーディング仕様書.xlsx")
)

# ==============================================================================
# 調達方法（嵌入度）コーディング仕様書（Excel）
# ------------------------------------------------------------------------------
# EMBED_LEVELS による3段階（嵌入度）コーディングの完全な仕様書。
# 図22・図23で使われる嵌入度ラベルが「結果4 method_class の元カテゴリーを
# どうグルーピングしたか」を示す。選定理由と異なり、こちらはキーワード
# 推測ではなく元カテゴリー文字列との完全一致によるグルーピングである。
#   シート1 対応表：元カテゴリー（結果4 method_class）→ 嵌入度スコア・ラベル
#   シート2 判定ロジックの説明
#   シート3 各カテゴリーの実際の記録数（検算用）
# ==============================================================================

embed_type_labels <- c("3" = "3 共同体が自ら採取・栽培",
                       "2" = "2 地域内から無償で入手",
                       "1" = "1 購入・地域外に依存")

embed_codebook_sheet1 <- lapply(names(EMBED_LEVELS), function(lv) {
  tibble(元カテゴリー = EMBED_LEVELS[[lv]], 嵌入度スコア = as.integer(lv))
}) %>% bind_rows() %>%
  mutate(ラベル = unname(embed_type_labels[as.character(嵌入度スコア)])) %>%
  bind_rows(tibble(
    元カテゴリー = c("現行調達なし", "調達方法不明"),
    嵌入度スコア = NA_integer_,
    ラベル = "（分類対象外＝NA）"
  )) %>%
  select(元カテゴリー, 嵌入度スコア, ラベル) %>%
  arrange(desc(嵌入度スコア))

embed_codebook_sheet2 <- tribble(
  ~項目, ~説明,
  "適用対象", "分析内容まとめ.xlsx 結果4「調達方法」の【 】カテゴリー（method_class）。結果4のカテゴリー自体は統制語彙で、キーワード推測ではなく元の文字列との完全一致でグルーピングする",
  "複数併記の扱い", "1レコード内で調達方法が「／」区切りで複数併記される場合（例：「氏子・保存会採取／地域住民提供」）、code_embeddedness()はそのレコードが該当する複数カテゴリーのうち最も嵌入度スコアが高いもの（＝最も自給的な方法）を採用する",
  "状態注記の除去", "カテゴリー文字列に付く（旧来）（推定）等の括弧注記は判定前に除去し、括弧を除いた本体でグルーピングする",
  "対象外カテゴリー", "「現行調達なし」「調達方法不明」の2カテゴリーはどの嵌入度スコアにも対応しないためNAとする",
  "図22での扱い", "2026-09-06以降、図22（代替可能性×日常利用の散布図）では調達方法による色分けを廃止した。調達方法は図16・図23a/23b・図28で別途扱う"
)

embed_codebook_sheet3 <- resource_df %>%
  filter(!is.na(method_cat)) %>%
  count(method_cat, name = "件数") %>%
  arrange(desc(件数))

write_xlsx(
  list("対応表" = embed_codebook_sheet1,
       "判定ロジック" = embed_codebook_sheet2,
       "元カテゴリー別件数（検算）" = embed_codebook_sheet3),
  path = file.path(OUTPUT_DIR, "調達方法_コーディング仕様書.xlsx")
)

# 図19は削除

# ==============================================================================
# CSV出力
# ==============================================================================

write.csv(
  survey_df %>% select(festival, age_raw, participants_trend, tourists_trend,
                       belief, purpose, preservation, core_generation, challenges),
  file.path(OUTPUT_DIR, "survey_summary.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

write.csv(
  resource_df %>% select(festival, resource_raw, resource_taxon, part,
                         landscape_raw, landscape_all, landscape_norm,
                         method_class, method_cat, embed_score,
                         timing_raw, change_class, change_cat,
                         use_class, use_types, use_status,
                         subst_class, subst_score, is_substitute_material,
                         daily_class, daily_score, current_use,
                         reason_raw, reason_types),
  file.path(OUTPUT_DIR, "resource_detail.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

write.csv(
  resource_df_full %>% filter(current_use == 0) %>%
    select(festival, resource_taxon, resource_raw = resource_orig, part,
           daily_class, daily_note, change_class, change_note),
  file.path(OUTPUT_DIR, "discontinued_resources.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

write.csv(
  scale_df %>% select(festival, participants, tourists, n_resources, festival_type),
  file.path(OUTPUT_DIR, "scale_summary.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

write.csv(
  keystone_df %>% arrange(desc(keystone_score)),
  file.path(OUTPUT_DIR, "keystone_species.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

write.csv(
  embed_festival,
  file.path(OUTPUT_DIR, "embeddedness_by_festival.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# 景観分類の判定結果（原文つき）— classify_landscape() の妥当性確認用
write.csv(
  resource_df %>%
    select(festival, resource_raw, landscape_raw, landscape_all) %>%
    arrange(landscape_all, festival),
  file.path(OUTPUT_DIR, "landscape_mapping_check.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ==============================================================================
# 完了
# ==============================================================================

cat("\n========================================\n")
cat("分析完了。出力 (", OUTPUT_DIR, "):\n", sep = "")
cat("  01: 基本プロファイル（協力者年齢×植物資源種数）\n")
cat("  03a/03b: 植物利用頻度（府県ウェイト補正）と府県別利用率\n")
cat("  02,05〜14: 組織・信仰・目的・資源マトリクス 分析\n")
cat("  --- 生物文化多様性分析 ---\n")
cat("  15_cultural_keystone_species.png     文化的関鍵種\n")
cat("  16_procurement_embeddedness.png      文化-生態嵌入度\n")
cat("  17a_reason_types_overall.png         選定理由10類型の全体分布\n")
cat("  17b_reason_by_plant.png              主要植物ごとの選定理由構成\n")
cat("  18_habitat_dependency_network.png    生息地依存ネットワーク\n")
cat("  20_use_vs_substitutability.png      利用方法 × 代替可能性\n")
cat("  21a_daily_use_distribution.png     日常利用スコア全体分布\n")
cat("  21b_daily_vs_substitutability.png  日常利用 × 代替可能性\n")
cat("  21c_daily_by_resource.png          植物種別 日常利用スコア\n")
cat("  21d_daily_vs_tek.png               日常利用 × TEKタイプ\n")
cat("  10_scale_typology.png               2×2 規模・観光類型散布図\n")
cat("  --- CSV ---\n")
cat("  survey_summary.csv / resource_detail.csv / scale_summary.csv\n")
cat("  keystone_species.csv / embeddedness_by_festival.csv\n")
cat("========================================\n")
