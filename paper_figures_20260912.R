# ==============================================================================
# paper_20260912: raw_data_agg.xlsx（結果1・2・4・5統合版）を使って
# 論文用12図のうち、資源レコード層のデータに依存する8図を再生成する。
# ------------------------------------------------------------------------------
# 対応関係（2026-09-12 調査結果）:
#   raw_data_agg.xlsxがカバー = 結果1(日常利用)・結果2(利用方法/選定理由/
#   代替可能性)・結果4(調達方法)・結果5(調達地変化/景観) → 図2,3,4,5,7,8,10,11
#   （図2・図5は「府県ウェイト」も必要。これは分析内容まとめ.xlsxの結果3
#   （祭り→府県対応）から取得し続ける。変更なしと確認済み）
#   raw_data_agg.xlsxがカバーしない = 受协力者年齢(結果0/mt0) → 図1、
#   話題頻度/保全管理活動(結果6/mt6) → 図12。これらは既存の確認済み画像を
#   そのままコピーする。
#   図6・図9は今回不要のため出力しない。
# 実装: main2.R に追加した USE_RAW_DATA_AGG トグルでraw_data_agg.xlsxから
# 資源レコードを読み込み、既存の paper_figures.R（8cm版の作図ロジック）を
# そのまま再利用する（PAPER_DIR/FINAL_DIRを新しい出力先に差し替え）。
# ==============================================================================

USE_RAW_DATA_AGG <- TRUE
PAPER_DIR <- "data_proc/20260912_scratch"
FINAL_DIR <- "data_proc/paper_20260912"

source("paper_figures.R")

# --- 図12：元データ（結果6・mt6）がraw_data_agg.xlsxにないため、
#     既存の確認済み版をコピーする。図1は右パネル（資源数）がresource_df
#     （raw_data_agg由来、USE_RAW_DATA_AGGの影響を受ける）に依存するため
#     コピーせず、paper_figures.R内で新データから生成し直す。 ---
OLD_FINAL_DIR <- "data_proc/20260902/paper_figures_8cm"
for (f in c("図-12_27a_topic_x_management.png")) {
  ok <- file.copy(file.path(OLD_FINAL_DIR, f), file.path(FINAL_DIR, f), overwrite = TRUE)
  cat("copied (unchanged data):", f, "->", ok, "\n")
}

# --- 図6・図9：今回は不要 ---
for (f in c("図-6_03b_plant_prevalence_by_pref.png",
            "図-9_29_plant_x_method_x_change.png")) {
  p <- file.path(FINAL_DIR, f)
  if (file.exists(p)) { file.remove(p); cat("removed (not needed):", f, "\n") }
}

cat("\n=== paper_20260912 完了:", length(list.files(FINAL_DIR, pattern = "\\.png$")),
    "枚 ===\n")
print(sort(list.files(FINAL_DIR, pattern = "\\.png$")))
