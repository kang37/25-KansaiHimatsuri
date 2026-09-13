# ==============================================================================
# paper_20260912: raw_data_agg.xlsx（結果1・2・4・5統合版）を使って
# 論文用12図のうち、資源レコード層のデータに依存する8図を再生成する。
# ------------------------------------------------------------------------------
# 対応関係（2026-09-12 調査結果。図番号は2026-09-14に914本文用原稿の
# 図番号へ合わせて振り直し済み。旧→新: 旧図1→図2、旧図2→図1、旧図7→図6、
# 旧図8→図7、旧図10→その他図-1、旧図11→図8、旧図12→その他図-2）:
#   raw_data_agg.xlsxがカバー = 結果1(日常利用)・結果2(利用方法/選定理由/
#   代替可能性)・結果4(調達方法)・結果5(調達地変化/景観) → 図1,3,4,5,6,7,8,
#   その他図-1
#   （図1・図5は「府県ウェイト」も必要。これは分析内容まとめ.xlsxの結果3
#   （祭り→府県対応）から取得し続ける。変更なしと確認済み）
#   raw_data_agg.xlsxがカバーしない = 受协力者年齢(結果0/mt0) → 図2、
#   話題頻度/保全管理活動(結果6/mt6) → その他図-2。これらは既存の確認済み
#   画像をそのままコピーする。
#   03b_plant_prevalence_by_pref・29_plant_x_method_x_changeは今回不要の
#   ため出力しない（コード内ではそれぞれ旧番号「図-6」「図-9」のまま
#   残置しているが、914本文用原稿の図番号とは無関係で、単に除外対象を
#   示す内部識別名に過ぎない）。
# 実装: main2.R に追加した USE_RAW_DATA_AGG トグルでraw_data_agg.xlsxから
# 資源レコードを読み込み、既存の paper_figures.R（8cm版の作図ロジック）を
# そのまま再利用する（PAPER_DIR/FINAL_DIRを新しい出力先に差し替え）。
# ==============================================================================

USE_RAW_DATA_AGG <- TRUE
PAPER_DIR <- "data_proc/20260912_scratch"
FINAL_DIR <- "data_proc/paper_20260912"
# 【2026-09-14改訂】PNG・CSVはFINAL_DIRに、SVGは別フォルダ（SVG_DIR）に
# 分けて保存する（save_final()がSVG_DIRの有無を見て書き出し先を切り替える）。
SVG_DIR  <- "data_proc/paper_20260912_svg"
dir.create(SVG_DIR, showWarnings = FALSE, recursive = TRUE)

source("paper_figures.R")

# --- その他図-2（旧図12）：元データ（結果6・mt6）はraw_data_agg.xlsxに
#     ないためデータ自体は変わらないが、2026-09-13にラベル文言・フォント
#     サイズを見直したためもうコピーせず、paper_figures.R内で（同じデータ
#     のまま）都度描き直す。 ---

# --- 図6・図9：今回は不要（FINAL_DIR・SVG_DIRとも削除） ---
for (base in c("図-6_03b_plant_prevalence_by_pref",
               "図-9_29_plant_x_method_x_change")) {
  for (p in c(file.path(FINAL_DIR, paste0(base, ".png")),
              file.path(SVG_DIR, paste0(base, ".svg")))) {
    if (file.exists(p)) { file.remove(p); cat("removed (not needed):", basename(p), "\n") }
  }
}

cat("\n=== paper_20260912 完了:", length(list.files(FINAL_DIR, pattern = "\\.png$")),
    "枚（PNG/CSV:", FINAL_DIR, "、SVG:", SVG_DIR, "） ===\n")
print(sort(list.files(FINAL_DIR, pattern = "\\.png$")))
