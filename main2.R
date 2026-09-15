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

# 2026-09-12 追加：結果1・結果2・結果4・結果5（資源レコード層）を1シートに
# 統合・更新した raw_data_agg.xlsx。呼び出し元スクリプトが実行前に
# USE_RAW_DATA_AGG <- TRUE を設定すると、資源レコードをこちらから読む
# （受访者年齢=mt0・府県対応=mt3・話題/管理活動=mt6 は従来通り
# 分析内容まとめ.xlsx を使う。両ファイルとも祭り名30件が完全一致することを
# 確認済み）。未設定時（デフォルト）は既存の挙動を一切変えない。
if (!exists("USE_RAW_DATA_AGG")) USE_RAW_DATA_AGG <- FALSE
RAW_AGG_PATH <- "data_raw/raw_data_agg.xlsx"

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
NONPLANT_TAXA <- c("アルミ・灯油", "ワタ（綿）", "タオル（綿）", "布類（材質不明）")

# raw_data_agg.xlsx側の表記ゆれをTAXON_ORDERの既存カテゴリーに揃える
# （同じ分類群の言い換えと判断したもののみ。新規に追加された分類群は
# そのままTAXON_ORDER外の"extra"として末尾に表示される）
# 【2026-09-13改訂】data_raw/plant_category.xlsx（正式な分類・表示名一覧）
# で確認したところ、raw_data_agg.xlsx側の表記（稲→イネ、小麦→コムギ、
# 菜種→ナタネ類、麻→アサ、ヨシ→ヨシ類、ササ→ササ類、ツバキ→ツバキ類、
# スダジイ、その他の広葉樹類）がそのまま正式名称と一致するため、変換は
# 不要になった（TAXON_ORDERはplant_category.xlsxの並び順に更新済み）。
# 【2026-09-14改訂】raw_data_agg.xlsx側で「ツツジ」→「ツツジ類」、
# 「その他の広葉樹類」→「広葉樹」に表記変更された。これは表記揺れの
# 吸収ではなく正式名称そのものの変更のため、逆変換はせず、TAXON_ORDER・
# data_raw/plant_category.xlsx側を新表記に合わせて更新した（変換不要）。
TAXON_RENAME_AGG <- c()

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

# 【 】内の分類ラベルを取り出す。
# 【2026-09-10修正】旧実装は最初の【 】1個しか拾えなかったが、実データには
# 「【燃焼材】【充填材】」のように複数の【 】が「／」なしで連続するセルが
# 多数ある（method_classで162件中71件、use_classで162件中61件）ため、
# 2個目以降のカテゴリーが黙って失われていた（例：「充填材」が図19c等に
# 一切現れない原因）。str_match_allで全ての【 】を拾い、「|」区切りで返す。
bracket_cat <- function(x) {
  vapply(as.character(x), function(z) {
    if (is.na(z)) return(NA_character_)
    m <- str_match_all(z, "【([^】]+)】")[[1]]
    if (nrow(m) == 0) return(NA_character_)
    paste(m[, 2], collapse = "|")
  }, character(1), USE.NAMES = FALSE)
}

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
# 1つのセルに複数カテゴリーが並ぶ場合は最も高い嵌入度を採る（bracket_cat()が
# 【A】【B】を"A|B"として返すほか、稀に単一【 】内で「／」区切りされる
# 場合もあるため、区切り文字は「／」「|」の両方を受け付ける）。
# 「地域内採取（旧来）」は行為者を明示しないが、共同体による自給的な採取の
# 旧称と判断し③ではなく③の1つ上、level 3（自ら採取・栽培）に含める。
EMBED_LEVELS <- list(
  "3" = c("氏子・保存会採取", "氏子・保存会栽培", "協働採取", "協働栽培", "地域内採取"),
  "2" = c("地域住民提供", "地元農家提供", "地元農家委託栽培", "地域内寺社提供",
          "地域内事業者提供", "副産物・再利用", "寄付・奉納"),
  "1" = c("地域外購入", "地域内購入", "購入", "外部業者委託", "外部協力者提供",
          "外部協力者採取", "外部協力者仲介", "地域外農家提供",
          "地域外農家委託栽培", "農家提供")
)

# 複数カテゴリーが併記される場合に「最も嵌入度が高い（＝最も自給的な）
# カテゴリー」を代表として選ぶ共通ヘルパー。code_embeddedness・
# code_method_type の両方がこれを使うことで、1レコードに複数の調達方式が
# 併記されていても両者が矛盾しない（同じカテゴリーを勝者として選ぶ）。
winning_method_cat <- function(cs) {
  if (is.na(cs)) return(NA_character_)
  parts <- str_trim(str_split(cs, "[／|]")[[1]])
  parts <- str_replace_all(parts, "[（(].*?[）)]", "")   # （旧来）（推定）を落とす
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
    for (lv in names(EMBED_LEVELS))
      if (winner %in% EMBED_LEVELS[[lv]]) return(as.integer(lv))
    NA_integer_
  }, integer(1), USE.NAMES = FALSE)
}

# 調達方式の類型（結果4 の【 】カテゴリーを、嵌入度の強弱（EMBED_LEVELS、
# 自給↔購入の順序尺度）ではなく行為の種類で5つに分類する、別の軸）。
# 2026-09-08追加、2026-09-10修正（bracket_cat()の複数【 】対応に伴い、
# 複数の調達方式が併記されるレコードをwinning_method_cat()で
# code_embeddednessと同じ代表カテゴリーに解決するよう変更。あわせて
# EMBED_LEVELSにはあるがどの類型にも属さず落ちていた「副産物・再利用」を
# ③提供・奉納に追加——他者からの供与という点で提供系に最も近いため）。
# 「現行調達なし」「調達方法不明」はどの類型にも対応しないためNA
# （EMBED_LEVELSと同じ対象外扱い）。
METHOD_TYPE_LEVELS <- list(
  "① 採取"       = c("氏子・保存会採取", "協働採取", "外部協力者採取", "地域内採取"),
  "② 栽培"       = c("氏子・保存会栽培", "協働栽培"),
  "③ 提供・奉納" = c("地元農家提供", "地域外農家提供", "農家提供", "地域住民提供",
                     "地域内事業者提供", "地域内寺社提供", "外部協力者提供", "寄付・奉納",
                     "副産物・再利用"),
  "④ 購入"       = c("地域内購入", "地域外購入", "購入"),
  "⑤ 委託"       = c("地元農家委託栽培", "地域外農家委託栽培", "外部業者委託")
)
METHOD_TYPE_ORDER <- names(METHOD_TYPE_LEVELS)
METHOD_TYPE_PAL <- setNames(
  c("#05527E", "#7C90AF", "#E3E3E3", "#D38171", "#C1261B"),
  METHOD_TYPE_ORDER
)

code_method_type <- function(x) {
  cat_str <- bracket_cat(x)
  vapply(cat_str, function(cs) {
    winner <- winning_method_cat(cs)
    if (is.na(winner)) return(NA_character_)
    hit <- METHOD_TYPE_ORDER[vapply(METHOD_TYPE_LEVELS, function(v) winner %in% v, logical(1))]
    if (!length(hit)) return(NA_character_)
    hit[1]
  }, character(1), USE.NAMES = FALSE)
}

# 利用方法（結果2 の【 】カテゴリー）。bracket_cat()が返す「／」「|」
# 区切りの複数カテゴリー（【A】【B】のように連続する【 】も含む）と、
# （旧来）（代替材）（代替試行・不採用）（推定）という状態注記を分離する。
code_use <- function(x) {
  cs <- bracket_cat(x)
  vapply(cs, function(z) {
    if (is.na(z)) return(NA_character_)
    parts <- str_trim(str_split(z, "[／|]")[[1]])
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
# 【2026-09-13改訂】data_raw/plant_category.xlsx（正式な分類・表示順一覧、
# 30分類群）の並び順（分類：農作物→タケ・ササ類→草本→針葉樹→広葉樹→
# つる性木本、各分類内も表中の順）をそのまま採用する。同ファイルに
# 含まれない分類群（吉祥草・食材各種）は末尾の「その他」に追加した。
# 実データに存在しない分類群が含まれていても実害はない（描画時に
# intersect() で絞り込む）。新しい taxon_kind が増えた場合はここに追記する。
TAXON_ORDER <- c(
  # ---- 農作物 ----
  "イネ（うるち米）", "イネ（もち米）", "イネ（赤米）", "コムギ", "ナタネ類", "アサ",
  # ---- タケ・ササ類 ----
  "タケ類", "ササ類",
  # ---- 草本 ----
  "ヒオウギ", "ヨシ類", "ススキ",
  # ---- 針葉樹 ----
  "アカマツ", "クロマツ", "スギ", "ヒノキ",
  # ---- 広葉樹 ----
  "ウメ", "クリ", "クロモジ", "コバノミツバツツジ", "サカキ", "スダジイ", "シキミ",
  "ソヨゴ", "ツツジ類", "ツバキ類", "ヌルデ", "ハンノキ", "広葉樹",
  # ---- つる性木本 ----
  "フジ", "ツヅラフジ",
  # ---- その他（plant_category.xlsxに含まれない分類群） ----
  "吉祥草", "食材各種"
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

# 日常利用スコアのラベル・配色（図03a・図19d・図21a/e/fで共通使用）
daily_label_lv <- c("1 日常的に使う", "2 ほとんどない", "3 全くない")
daily_colors   <- c("1 日常的に使う" = "#2CA02C",
                    "2 ほとんどない"  = "#FF7F0E",
                    "3 全くない"      = "#D62728")

# 代替可能性スコアのラベル・配色（図17b・図20で共通使用）
subst_label_lv <- c("代替可（1）", "代替困難（2）", "代替不可（3）")
subst_colors_20 <- c(
  "代替可（1）"    = "#4DAF4A",
  "代替困難（2）"  = "#FF7F00",
  "代替不可（3）"  = "#E41A1C"
)

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
if (USE_RAW_DATA_AGG) {

  # --- raw_data_agg.xlsx（結果1・2・4・5を統合済みの資源レコード表）---
  agg_raw <- suppressMessages(read_excel(RAW_AGG_PATH, sheet = 1, col_names = TRUE))
  # 【2026-09-14改訂】「部位・材料名」列（値は"部位"/"材料名"）が結果1の3列目に
  # 追加され、19列→20列になった。図19d（植物×使用部位）で部位/材料名を
  # 分けて表示する際、この列を正とする（従来のハードコードした判定リストは
  # 廃止）。
  stopifnot(ncol(agg_raw) == 20)
  names(agg_raw) <- c(
    "festival", "taxon_kind", "part_category", "part", "daily_class", "daily_note",
    "current_use", "in_scope", "use_class", "use_note", "reason_raw",
    "subst_class", "subst_note", "method_class", "method_note", "timing_raw",
    "change_class", "change_note", "landscape_raw", "landscape_note"
  )

  resource_raw <- agg_raw %>%
    mutate(across(c(festival, taxon_kind, part, part_category, daily_class, use_class,
                    reason_raw, subst_class, method_class, change_class, landscape_raw),
                  matome_clean),
           taxon_kind    = ifelse(taxon_kind %in% names(TAXON_RENAME_AGG),
                                  unname(TAXON_RENAME_AGG[taxon_kind]), taxon_kind),
           resource_orig = part,
           current_use   = suppressWarnings(as.integer(current_use)),
           in_scope      = suppressWarnings(as.integer(in_scope)),
           taxon_matome  = NA_character_) %>%
    filter(in_scope == 1) %>%
    select(festival, taxon_kind, part, part_category, method_class, method_note, timing_raw,
           resource_orig, daily_class, daily_note, current_use,
           use_class, use_note, reason_raw, subst_class, subst_note,
           change_class, change_note, landscape_raw, taxon_matome)

  cat("\n=== raw_data_agg.xlsxから資源レコードを読み込み ===\n")
  cat("除外（集計対象外 in_scope=0）:", sum(agg_raw$in_scope == 0, na.rm = TRUE), "件\n")

} else {

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

}

# 【2026-09-14追加】USE_RAW_DATA_AGGを使わない従来経路には「部位・材料名」
# 列（part_category）が存在しないため、図19d用に簡易ヒューリスティックで
# 代用する（raw_data_agg.xlsx側は結果1の実データ列をそのまま使うので対象外）。
if (!"part_category" %in% names(resource_raw)) {
  PART_BODY_SET_FALLBACK <- c("根", "枝", "葉", "花", "稈", "地上部", "ツル")
  resource_raw <- resource_raw %>%
    mutate(part_category = ifelse(part %in% PART_BODY_SET_FALLBACK, "部位", "材料名"))
}

.unmapped_use <- resource_raw %>% filter(is.na(use_class))
cat("結果2（利用方法等）に対応しない記録:", nrow(.unmapped_use), "件",
    if (nrow(.unmapped_use) > 0) paste0("（", paste(unique(.unmapped_use$taxon_kind), collapse = "、"), "）") else "", "\n")

if (!USE_RAW_DATA_AGG) {
  .unmapped_taxon <- resource_raw %>% filter(is.na(taxon_matome), !(taxon_kind %in% NONPLANT_TAXA))
  if (nrow(.unmapped_taxon) > 0) {
    cat("結果3の資源グループに対応しない記録:", nrow(.unmapped_taxon), "件\n")
    print(as.data.frame(.unmapped_taxon %>% select(festival, taxon_kind, resource_orig)))
  }
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
    method_type    = code_method_type(method_class),
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
#   raw_prev   素の割合          = 使用祭り数 / 現在資源記録が残る祭り数
#                                  （2026-09-15改訂。以前は固定30だったが、
#                                  raw_data_agg.xlsxの更新で資源記録が0件に
#                                  なった祭りは分母からも除くようにした）
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

# 【2026-09-15追加】raw_data_agg.xlsxの更新で資源記録が0件になった祭り
# （festival_profileでn_resourcesがNAになるもの）は、図-1の分母（祭り数）
# からも除く。固定の30ではなく、実際に資源記録が残っている祭りの数を
# 分母とする（usage_matrix/estimate_prevalenceに個別に design を渡す）。
fest_design_active <- fest_design %>% filter(festival %in% unique(resource_df$festival))

# --- 推定関数 ---------------------------------------------------------------
# u_mat: 祭り(行) × 植物(列) の 0/1 利用行列
usage_matrix <- function(df, unit_col, design = fest_design) {
  tab <- table(factor(df$festival, levels = design$festival),
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


# ==============================================================================
# 図03: 植物の利用頻度 — 抽出の府県偏りを補正した推定
# ------------------------------------------------------------------------------
# 03a 素の利用祭り数（部位重複を除いた祭り数）と母集団推定割合の比較
# 03b 府県別の利用率ヒートマップ（順位が動く理由を示す）
# ==============================================================================

u_taxon  <- usage_matrix(plant_festival, "resource_taxon", design = fest_design_active)
prev_tax <- estimate_prevalence(u_taxon, design = fest_design_active)

# 【2026-09-15追加】植物別に行ラベルへ表示する「祭り数」は、図全体で
# 図-1（raw_n、祭り×植物単位で重複除去）の口径に統一する。図-5・図-6・
# 図-7はそれぞれ独自の対象範囲（理由が記録された記録、調達方式が分類
# できた記録、調達地変化が記録された記録）を持つため、その範囲内だけで
# 祭り数を数え直すと図ごとに数字が微妙にずれる（例：ある祭りの記録だけ
# 調達方式が「不明」で図-6の対象から漏れる、等）。表示上の混乱を避ける
# ため、ラベルの祭り数は常にこのplant_fes_denom（＝図-1のraw_n）を使う。
plant_fes_denom <- prev_tax %>% transmute(resource_taxon = taxon, n_fes = raw_n)

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

# --- 03a: 素の利用の広がり + 日常利用の内訳（右パネル） --------------------
# 【2026-09-06改訂】全34分類群を表示する（従来はraw_n>=2でフィルタしていた）。
# 【2026-09-08改訂】母集団推定（府県ウェイト補正）の赤点・誤差棒・矢印は
#   補正の不確実性そのものが焦点になり本図の主題（利用の広がり）をぼかす
#   ため撤去した（補正値自体はコンソール出力とplant_prevalence_weighted.csv
#   にそのまま残る）。代わりに右側へ日常利用スコア（daily_label_lv・
#   daily_colors、図21aと共通定義）の内訳を狭い帯グラフで添え、「多くの
#   祭りで使われる植物ほど日常生活でも使われ続けているか」を一目で見られる
#   ようにする（patchworkで左右結合、図01と同じ方式）。
prev_plot <- prev_tax %>%
  mutate(taxon = order_taxon(taxon))
# coord_flipなしの横棒(geom_point+y=taxon)なので、上から見たい順に並べるには
# 逆順にする（factorの最初の水準が下に来るため）。
prev_plot <- prev_plot %>% mutate(taxon = factor(taxon, levels = rev(levels(taxon))))

daily_share_03 <- resource_df %>%
  filter(!is.na(daily_score)) %>%
  mutate(daily_label = case_when(daily_score == 1 ~ "1 日常的に使う",
                                 daily_score == 2 ~ "2 ほとんどない",
                                 TRUE              ~ "3 全くない")) %>%
  count(resource_taxon, daily_label) %>%
  complete(resource_taxon = levels(prev_plot$taxon), daily_label = daily_label_lv,
           fill = list(n = 0)) %>%
  group_by(resource_taxon) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(pct = ifelse(is.nan(pct), NA_real_, pct),
         resource_taxon = factor(resource_taxon, levels = levels(prev_plot$taxon)),
         daily_label = factor(daily_label, levels = daily_label_lv)) %>%
  # 【2026-09-14改訂】complete()で失われるfactorの並び順を明示的に
  # 再度並べ替える（行の物理的な並びがposition_stackの積み上げ順を
  # 左右するため、factorの水準を設定し直すだけでは不十分）。
  arrange(resource_taxon, daily_label)

p03a_main <- ggplot(prev_plot, aes(y = taxon)) +
  geom_segment(aes(x = 0, xend = raw_prev, y = taxon, yend = taxon),
               color = "#4472C4", linewidth = 0.9) +
  geom_point(aes(x = raw_prev), color = "#4472C4", size = 2.8) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0, 0.03))) +
  labs(
    title = "植物ごとの利用の広がり",
    subtitle = paste0("解析単位＝祭り×植物分類群（同一祭り内の部位重複は1件に集約） ／ 全",
                      nrow(prev_plot), "分類群"),
    x = paste0("その植物を使用する火祭りの割合（", nrow(fest_design_active), "祭りのうち）"), y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

p03a_daily <- ggplot(daily_share_03, aes(x = pct, y = resource_taxon, fill = daily_label)) +
  geom_col(position = "stack", width = 0.72, na.rm = TRUE) +
  scale_fill_manual(values = daily_colors, name = "日常利用", drop = FALSE) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(title = "日常利用", x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

# guides="collect"で右パネルの凡例を個別表示せず、拼図全体で1つにまとめる。
# 末尾の & theme(...) はpatchwork全体（collectされた凡例含む）に効くため、
# 凡例が右パネル内で途切れず、拼図全体の中央下に表示される。
p03a <- (p03a_main + p03a_daily +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect") +
  patchwork::plot_annotation(
    caption = paste0("右：日常利用スコアの内訳（緑＝日常的に使う、橙＝ほとんどない、赤＝全くない）。",
                     "母集団推定割合（府県ウェイト補正）は plant_prevalence_weighted.csv を参照。"),
    theme = theme(plot.caption = element_text(family = "HiraginoSans-W3"))
  )) &
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "03a_plant_prevalence_weighted.png"), p03a,
       width = 11, height = max(5, nrow(prev_plot) * 0.42), dpi = 150)

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

# --- 17b: 植物（個別分類群）ごとの理由構成（ウェイト付き）-------------------
# 【2026-09-06 改訂】結果3の粗い資源グループ（9分類）ではなく、taxon_kind
#   （人手コード済みの個別植物、TAXON_ORDERで表示順を統一）を行に使う。
# 【2026-09-08 改訂】理由が1件でも記録されていれば表示する（全植物を含める）。
#   従来の「記録数2件未満は除外」は割合の安定性を優先した閾値だったが、
#   n=1の行も「その1件の記録では何が挙げられたか」という情報として残す。
# 【選定理由】結果2「植物の選定理由（要点）」に code_reason() の10類型を適用。
# 【着色】行ごとに割合の高い上位3セルだけをグラデーションで着色する
#   （固定の分位点しきい値だと行によって着色数がばらつくため、順位方式に
#   変更）。0%のセルは白、上位3に入らない非0セルは薄灰にして区別する。

REASON_MIN_N <- 1

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

p17b_main <- ggplot(reason_grid, aes(x = rlabel, y = taxon_label)) +
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
    subtitle = paste0("理由が記録された全", nrow(taxon_denom), "分類群。",
                      "縦軸はTAXON_ORDER（生活形）順（n=1の分類群を含む）\n",
                      "セル＝その植物を使う祭りのうちその理由が語られた割合",
                      "（行ごとの割合、府県ウェイト補正後）\n",
                      "着色は行ごとの上位3セルのみ。白＝0%、薄灰＝上位3外の非0セル\n",
                      "1単位が複数類型を持つため行の合計は100%を超える"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

# --- 右パネル：代替可能性の内訳（狭い積み上げ棒、図03aと同じpatchwork方式）---
# 【2026-09-09追加】解析単位は資源レコード（resource_df、府県ウェイトなし）
# ——左の理由ヒートマップ（祭り×植物、府県ウェイト付き）とは単位が異なる
# 点に注意。行の並びは左パネルと同じtaxon_label（label_order_17b）。
subst_share_17b <- resource_df %>%
  filter(resource_taxon %in% taxon_denom$resource_taxon, !is.na(subst_score)) %>%
  mutate(subst_label = factor(subst_score, levels = 1:3, labels = subst_label_lv)) %>%
  count(resource_taxon, subst_label) %>%
  complete(resource_taxon = taxon_denom$resource_taxon, subst_label = subst_label_lv,
           fill = list(n = 0)) %>%
  group_by(resource_taxon) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(pct = ifelse(is.nan(pct), NA_real_, pct)) %>%
  left_join(taxon_denom %>% select(resource_taxon, n_fes), by = "resource_taxon") %>%
  mutate(taxon_label = factor(paste0(resource_taxon, "（", n_fes, "祭り）"), levels = rev(label_order_17b)))

p17b_subst <- ggplot(subst_share_17b, aes(x = pct, y = taxon_label, fill = subst_label)) +
  geom_col(position = "stack", width = 0.72, na.rm = TRUE) +
  scale_fill_manual(values = subst_colors_20, name = "代替可能性", drop = FALSE) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(title = "代替可能性", x = NULL, y = NULL) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

p17b <- (p17b_main + p17b_subst +
  patchwork::plot_layout(widths = c(3, 1), guides = "collect") +
  patchwork::plot_annotation(
    caption = paste0("右：代替可能性の内訳（緑＝代替可、橙＝代替困難、赤＝代替不可）。",
                     "資源レコード単位の割合（府県ウェイトなし、左の理由ヒートマップとは解析単位が異なる）"),
    theme = theme(plot.caption = element_text(family = "HiraginoSans-W3"))
  )) &
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "17b_reason_by_plant.png"), p17b,
       width = 13, height = max(6.5, nrow(taxon_denom) * 0.42), dpi = 150)

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

# ==============================================================================
# 図19c: 植物 × 用途（利用方法）
# ------------------------------------------------------------------------------
# 解析単位は資源レコード（use_long、1レコードが複数用途を持つ場合は展開
# 済み）。【2026-09-08改訂】セルの色・数値は件数ではなく行（植物）ごとの
# 割合——「その植物の資源レコードのうち何%がこの用途を持つか」に変更
# （図17bと同じ「行内で百分率化」の考え方。分母は展開前の資源レコード数
# なので、複数用途を持つ記録がある行は合計が100%を超えうる）。
# 着色は図24aと同じ連続グラデーション（割合が高いほど濃い青、0%は白）、
# 行=植物はTAXON_ORDER（生活形）順、列=用途は出現頻度順、府県ウェイトは
# 適用しない（観測された標本の記述）。
# ------------------------------------------------------------------------------

taxon_order_19c <- levels(order_taxon(use_long$resource_taxon))
use_order_19c   <- use_long %>% count(use_cat, sort = TRUE) %>% pull(use_cat)

cat("\n=== 図19c 対象レコード:", nrow(use_long), "件（",
    n_distinct(paste(use_long$festival, use_long$resource_raw)),
    "件の資源レコードが複数用途のため展開）===\n")

taxon_denom_19c <- use_long %>%
  distinct(festival, resource_raw, resource_taxon) %>%
  count(resource_taxon, name = "n_taxon")

mat_19c <- use_long %>%
  count(resource_taxon, use_cat) %>%
  left_join(taxon_denom_19c, by = "resource_taxon") %>%
  mutate(pct = n / n_taxon,
         resource_taxon = factor(resource_taxon, levels = rev(taxon_order_19c)),
         use_cat = factor(use_cat, levels = use_order_19c))

p19c <- ggplot(mat_19c, aes(x = use_cat, y = resource_taxon, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)),
            size = 2.6, family = "HiraginoSans-W3", color = "gray15") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08519C", na.value = "white",
                      labels = scales::percent, name = "その植物の資源レコードに\n占める割合") +
  labs(
    title = "植物 × 用途（利用方法）",
    subtitle = paste0("資源レコード", n_distinct(paste(use_long$festival, use_long$resource_raw)),
                      "件。セルの色・数値は行（植物）ごとの割合\n",
                      "（その植物の資源レコードのうち何%がこの用途を持つか。複数用途を持つ記録は両方に計上のため行の合計が100%を超えることがある）\n",
                      "行はTAXON_ORDER（生活形）順、列は出現頻度順"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "19c_plant_x_use.png"), p19c,
       width = 10, height = max(6, n_distinct(mat_19c$resource_taxon) * 0.33), dpi = 150)

write.csv(mat_19c %>% arrange(desc(pct)) %>%
            select(resource_taxon, use_cat, n, n_taxon, pct),
          file.path(OUTPUT_DIR, "plant_x_use.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 図19d: 植物 × 使用部位
# ------------------------------------------------------------------------------
# 解析単位は資源レコード（resource_df、部位を分けたまま）。件数そのもの
# ではなく「その植物の部位記録のうち何%がこの部位か」という行内比率を
# 円の大きさで表す（松の記録の40%が幹、なら幹のバブルが最大）。
# 【2026-09-09改訂】結果2「使用部位等」の文字列"NA"は欠測ではなく、
# 「特定の部位を区別せず全体を使う／部位という概念が当てはまらない」
# ケースを表すとの確認を得たため、除外せず「全体/NA」という1カテゴリー
# として列に含める（他の部位と同列に扱う）。行=植物はTAXON_ORDER順、
# 列=部位は出現頻度順、府県ウェイトは適用しない（観測された標本の記述）。
# ==============================================================================

part_df <- resource_df %>%
  mutate(part = ifelse(is.na(part) | part %in% c("", "NA"), "全体/NA", part))

# 行に使う植物リストはresource_df全体（「全体/NA」を含めるため全レコードが対象）。
part_taxon_order <- levels(order_taxon(resource_df$resource_taxon))
# 【2026-09-14改訂】列を「部位」（植物の解剖学的な部分）と「材料名」
# （収穫・加工後の製品としての呼び名）の2グループに分け、部位を左側、
# 材料名を右側に配置する（各グループ内は出現頻度順）。結果1に追加された
# 「部位・材料名」列（part_category）を正とする（旧・ハードコード判定は廃止）。
part_order_body     <- part_df %>% filter(part_category == "部位") %>%
  count(part, sort = TRUE) %>% pull(part)
part_order_material <- part_df %>% filter(part_category == "材料名") %>%
  count(part, sort = TRUE) %>% pull(part)
part_order <- c(part_order_body, part_order_material)

cat("\n=== 図19d 対象レコード:", nrow(part_df), "件 /",
    n_distinct(part_df$resource_taxon), "分類群（全",
    n_distinct(resource_df$resource_taxon), "分類群中）。",
    "うち「全体/NA」（特定の部位を区別しない）が",
    sum(part_df$part == "全体/NA"), "件===\n")

mat_19d <- part_df %>%
  count(resource_taxon, part, name = "n") %>%
  group_by(resource_taxon) %>%
  mutate(n_taxon = sum(n), pct = n / n_taxon) %>%
  ungroup() %>%
  mutate(resource_taxon = factor(resource_taxon, levels = rev(part_taxon_order)),
         part = factor(part, levels = part_order))

p19d <- ggplot(mat_19d, aes(x = part, y = resource_taxon, size = pct)) +
  geom_point(color = "#08519C", alpha = 0.75) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)),
            size = 2.4, family = "HiraginoSans-W3", color = "gray20", vjust = -1.4) +
  scale_size_area(max_size = 11, labels = scales::percent,
                  name = "その植物の部位記録に占める割合") +
  # drop=FALSE：部位データが1件もない植物も空行として軸に残す（全植物を表示）
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "植物 × 使用部位",
    subtitle = paste0("全", length(part_taxon_order), "分類群、資源レコード", nrow(part_df), "件。",
                      "円の大きさ＝その植物の部位記録のうちこの部位が占める割合\n",
                      "（例：50%ならその植物の部位記録の半分がこの部位）\n",
                      "「全体/NA」＝特定の部位を区別せず全体を使う、または部位が未記録。行はTAXON_ORDER順、列は出現頻度順\n",
                      "記録数が少ない分類群（特にn=1）は割合が不安定な点に注意"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray92"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "19d_plant_x_part.png"), p19d,
       width = 10, height = max(6, length(part_taxon_order) * 0.38), dpi = 150)

write.csv(mat_19d %>% arrange(desc(pct)), file.path(OUTPUT_DIR, "plant_x_part.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 図23: 調達方法の内訳 — 府県別・植物別
# ------------------------------------------------------------------------------
# 図16（祭り別）と同じ設計を、集計単位だけ府県／植物分類群に変えて再利用する。
# 府県ウェイトは使わない：ここでの問いは「観測された30祭りの範囲内で、
# 府県や植物ごとに調達方法がどう違うか」という記述であり、母集団への
# 一般化ではないため（母集団推定が要る分析は図03a・keystone_scoreで別途実施済み）。
# ------------------------------------------------------------------------------

# --- 23c: 調達方式の内訳（植物別、行為類型） --------------------------------
# 【2026-09-08追加】図16・23a/23bの3段階「嵌入度」（自給↔購入の強弱、順序
# 尺度）とは別の軸として、行為の種類（採取・栽培・提供／奉納・購入・委託の
# 5類型、METHOD_TYPE_LEVELS）で植物ごとの内訳を見る。府県ウェイトなし
# （観測された標本の記述、23a/23bと同じ方針）。
# 【2026-09-08改訂】並びは記録数順ではなく、他の植物別の図（図03a・17b・
# 19c・19d・23b・29）と揃えてTAXON_ORDER（生活形）順にする。
method_type_records <- resource_df %>%
  filter(!is.na(method_type)) %>%
  mutate(method_type = factor(method_type, levels = METHOD_TYPE_ORDER))

taxon_method_order <- intersect(rev(levels(order_taxon(method_type_records$resource_taxon))),
                                 unique(method_type_records$resource_taxon))

# 【2026-09-08改訂】x軸（coord_flip前はy軸）を0-100%に限定するため、
# 記録数はバー右の余白ではなく行ラベルに埋め込む（図17b・19cと同じ方式）。
# 【2026-09-15改訂】％計算の分母（n_total＝資源記録数）と、行ラベルに
# 表示する数字は別物とする。行ラベルは全図共通のplant_fes_denom（＝
# 図-1のraw_n）を表示し、％の分母には従来通り資源記録数（n_total）を
# 使う。
# 【2026-09-15再改訂】ラベルの祭り数はこの図の対象範囲（method_typeが
# 分類できた記録）だけで数え直すのではなく、常に図-1と同じ数字にする
# （例：ある祭りの唯一の記録が「調達方法不明」でこの図の対象から漏れて
# いても、図-1ではその祭りをこの植物の使用祭りとして数えているため、
# ラベルの祭り数は図-1に揃える）。
taxon_n_23c <- method_type_records %>%
  filter(resource_taxon %in% taxon_method_order) %>%
  count(resource_taxon, name = "n_total")

taxon_label_order_23c <- taxon_n_23c %>%
  left_join(plant_fes_denom, by = "resource_taxon") %>%
  mutate(resource_taxon = factor(resource_taxon, levels = taxon_method_order)) %>%
  arrange(resource_taxon) %>%
  mutate(taxon_label = paste0(resource_taxon, "（n=", n_fes, "）")) %>%
  pull(taxon_label)

method_type_long <- method_type_records %>%
  count(resource_taxon, method_type, .drop = FALSE) %>%
  filter(resource_taxon %in% taxon_method_order) %>%
  left_join(taxon_n_23c, by = "resource_taxon") %>%
  left_join(plant_fes_denom, by = "resource_taxon") %>%
  mutate(pct = n / n_total,
         taxon_label = factor(paste0(resource_taxon, "（n=", n_fes, "）"),
                              levels = taxon_label_order_23c))

cat("\n=== 調達方式（植物別、行為類型）===\n")
print(as.data.frame(method_type_long %>% filter(n > 0) %>%
  arrange(resource_taxon, desc(pct)) %>%
  select(resource_taxon, method_type, n, pct)))

p23c <- ggplot(method_type_long, aes(x = taxon_label, y = pct, fill = method_type)) +
  # position_stack(reverse=TRUE)：デフォルトだと積み上げ順が凡例の並び
  # （①→⑤）と逆になるため、バー内の並びを凡例と一致させる。
  geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = METHOD_TYPE_PAL, name = "調達方式（行為類型）", drop = FALSE) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "調達方式の内訳（植物別、行為類型）",
    subtitle = paste0("図16・図23a/23bの3段階嵌入度（自給↔購入）とは別の軸\n",
                      "採取・栽培・提供／奉納・購入・委託の5類型（結果4のカテゴリーを再分類）\n",
                      "「現行調達なし」「調達方法不明」のレコードは対象外。府県ウェイトなし。行はTAXON_ORDER順"),
    x = NULL, y = "植物資源レコードの割合"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "23c_method_type_by_plant.png"), p23c,
       width = 9.5, height = max(6, n_distinct(method_type_long$taxon_label) * 0.34), dpi = 150)

write.csv(method_type_long %>% filter(n > 0) %>%
            arrange(desc(n_total), resource_taxon) %>%
            select(resource_taxon, method_type, n, n_total, pct),
          file.path(OUTPUT_DIR, "method_type_by_plant.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 図28: 植物ごとの調達地の変化（結果5「調達地の変化」）
# ------------------------------------------------------------------------------
# change_cat（1=変化なし／2A=以前より近い／2B=以前より広い／3=使用停止）を
# これまで一度も可視化していなかったため新設する。
# 【現在使われていない資源も含める】主分析（resource_df）は current_use==1
# に絞っているが、この図は「資源基盤がどう変容したか」を見るものなので
# 使用停止（＝discontinued_resources.csvの13件）も対象に含める必要がある。
# そのためここだけ resource_df_full（current_useで絞る前）を使う。
# 【2026-09-09改訂】change_catが記録された植物は全て表示する（n=1分類群
# も含む。他の図と同じ「全植物」方針）。植物の並びはTAXON_ORDERではなく
# 「変化が大きい順」（以前より広い＋使用停止の割合が高い順）——この図の
# 主題そのものが変化の大きさなので、生活形順より変化順の方が読みやすい。
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
  count(resource_taxon, name = "n_rec")

cat("\n=== 図28 対象:", nrow(change_denom), "分類群（change_catが記録された全植物。n_rec=1が",
    sum(change_denom$n_rec == 1), "分類群）===\n")

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
    subtitle = paste0("全", nrow(change_denom), "分類群（現在使われていない資源＝使用停止も含む）。",
                      "並びは変化が大きい順\n",
                      "（「以前より広い」＋「使用停止」の割合が高い順）\n",
                      "記録数が少ない分類群（特にn=1）は割合が不安定な点に注意"),
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
# 図28b: 生態景観類型ごとの調達地の変化
# ------------------------------------------------------------------------------
# 【2026-09-09追加】図28（植物別）と同じ調達地の変化（change_cat）を、
# 植物ではなく調達地の生態景観類型（結果5、図24aと同じlandscape_all）で
# 集計し直す。「二次林から採る植物ほど以前より広い範囲を探しに行くように
# なったか」等、景観タイプと調達難易度の関係を見るための図。
# 図24aと同じく1レコードが複数景観に由来する場合は両方に計上する
# （separate_rowsで展開）。図28と同じくresource_df_full を使い、
# 現在使われていない資源（使用停止）も対象に含める。
# ------------------------------------------------------------------------------

landscape_change_records <- resource_df_full %>%
  filter(!is.na(change_cat), !is.na(landscape_all), landscape_all != "なし") %>%
  select(-landscape_norm) %>%
  separate_rows(landscape_all, sep = "\\|") %>%
  rename(landscape_type = landscape_all) %>%
  mutate(change_cat = factor(as.character(change_cat), levels = CHANGE_LEVELS))

landscape_change_denom <- landscape_change_records %>%
  count(landscape_type, name = "n_rec")

cat("\n=== 図28b 対象:", nrow(landscape_change_records), "件（",
    n_distinct(paste(landscape_change_records$festival, landscape_change_records$resource_raw)),
    "件の資源が複数景観のため展開）/", nrow(landscape_change_denom), "景観類型 ===\n")

landscape_change_summary <- landscape_change_records %>%
  count(landscape_type, change_cat, .drop = FALSE) %>%
  left_join(landscape_change_denom, by = "landscape_type") %>%
  mutate(pct = n / n_rec)

landscape_severity <- landscape_change_summary %>%
  filter(change_cat %in% c("以前より広い", "使用停止")) %>%
  group_by(landscape_type) %>%
  summarise(severity = sum(pct), .groups = "drop") %>%
  right_join(landscape_change_denom, by = "landscape_type") %>%
  mutate(severity = replace_na(severity, 0)) %>%
  arrange(severity)

cat("\n=== 生態景観類型ごとの調達地変化（変化が大きい順） ===\n")
print(as.data.frame(landscape_severity %>% arrange(desc(severity)) %>%
  transmute(landscape_type, n_rec, 変化スコア = round(severity, 2))))

landscape_change_summary <- landscape_change_summary %>%
  mutate(land_label = paste0(landscape_type, "（", n_rec, "件）"),
         land_label = factor(land_label,
           levels = paste0(landscape_severity$landscape_type, "（", landscape_severity$n_rec, "件）")))

p28b <- ggplot(landscape_change_summary, aes(x = land_label, y = pct, fill = change_cat)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = CHANGE_PAL, name = "調達地の変化", drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "生態景観類型ごとの調達地の変化",
    subtitle = paste0("資源レコード", nrow(landscape_change_records), "件（",
                      n_distinct(paste(landscape_change_records$festival, landscape_change_records$resource_raw)),
                      "件の資源が複数景観のため展開）。現在使われていない資源（使用停止）も含む\n",
                      "並びは変化が大きい順（「以前より広い」＋「使用停止」の割合が高い順）"),
    x = NULL, y = "資源レコードの割合"
  ) +
  theme_bw(base_family = "HiraginoSans-W3") +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "28b_procurement_change_by_landscape.png"), p28b,
       width = 9, height = max(5, nrow(landscape_change_denom) * 0.5), dpi = 150)

write.csv(
  landscape_change_summary %>% select(landscape_type, n_rec, change_cat, n, pct),
  file.path(OUTPUT_DIR, "procurement_change_by_landscape.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

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

