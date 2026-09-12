# ==============================================================================
# 滋賀県 土地利用データ（国土数値情報 L03-b-c-21_5236, 令和2年）と
# 植物資源利用・保全措置の関係 — 独立分析
# ------------------------------------------------------------------------------
# 【位置づけ】main2.R・conservation_measures_analysis.R とは独立した新規
# ファイル。main2.R には一切手を加えない。
#
# 【データ】
#   data_raw/L03-b-c-21_5236-jgd2011_GML/L03-b-c-21_5236.shp
#   国土数値情報「土地利用細分メッシュ」令和2年版、100mメッシュ、CRS=JGD2011
#   （EPSG:6668）。コード定義は製品仕様書第3.1版より確認済み：
#     0100=田 0200=その他農用地 0500=森林 0600=荒地 0700=建物用地
#     0901=道路 0902=鉄道 1000=その他用地 1100=河川地及び湖沼
#     1400=海浜 1500=海水域 1600=ゴルフ場
#
# 【重要な制約：メッシュ5236のカバー範囲】
# 1次メッシュ5236は 緯度[34.667, 35.333)・経度[136.0, 137.0) をカバーする。
# 本研究で対象とする滋賀県の11祭りのうち、以下の理由で3件は本メッシュ
# だけでは分析できない：
#   ・雄琴学区ヨシ松明一斉点火（大津市雄琴、約東経135.93）
#       → 経度が136.0を下回り完全にメッシュ外。西隣のメッシュ5235
#         （経度135.0〜136.0）が必要。
#   ・勝部の火祭り（守山市、東経135.9905）
#       → 地点そのものは辛うじてメッシュ外側の直近だが、半径500m/1kmの
#         バッファーの大半が136.0の境界線を越えてメッシュ外に出るため、
#         周辺土地利用の集計として信頼できない。
#   ・松明を次世代に送る会（近江八幡市、特定の単一社寺なし。「それぞれ
#     地域の神社」で構成される分散型組織）
#       → 単一の代表地点を定義できないため点バッファー分析の対象外。
# 上記3件を除く8祭りについてのみ分析する。5235を追加取得すれば
# 雄琴・勝部の2件は追加できる（松明を次世代に送る会は地点定義の問題の
# ため追加取得でも解決しない）。
#
# 【座標の出典】
# data_raw/近畿地方火祭り.xlsx（156件のマスターDB、緯度経度列）と一致する
# ものはそこから採用。同DBに存在しない信楽・雄琴は個別にWeb検索で確認した
# 神社所在地の座標を用いた（出典はコード内コメント参照、確度は前者よりやや
# 低い）。
# ==============================================================================

suppressMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
})

OUTPUT_DIR <- "data_proc/landuse_gis"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

LANDUSE_CODES <- c(
  "0100" = "田", "0200" = "その他農用地", "0500" = "森林", "0600" = "荒地",
  "0700" = "建物用地", "0901" = "道路", "0902" = "鉄道", "1000" = "その他用地",
  "1100" = "河川地及び湖沼", "1400" = "海浜", "1500" = "海水域", "1600" = "ゴルフ場"
)

# ------------------------------------------------------------------------------
# 1. 対象8祭りの座標（メッシュ5236でカバーされるもののみ）
# ------------------------------------------------------------------------------
festival_points <- tribble(
  ~festival,               ~lat,     ~lon,      ~coord_source,
  "巽神社松明",             35.1078,  136.1706, "近畿地方火祭り.xlsx（糠塚町 巽神社）",
  "太郎坊宮の火祭り",       35.1149,  136.1829, "近畿地方火祭り.xlsx（太郎坊宮阿賀神社）",
  "信楽の火祭り",           34.8787,  136.0557, "Web検索確認（新宮神社公式サイト、甲賀市信楽町長野）",
  "近江八幡左義長祭り",     35.1411,  136.0896, "近畿地方火祭り.xlsx（日牟禮八幡宮）",
  "八幡祭り",               35.1411,  136.0896, "近畿地方火祭り.xlsx（日牟禮八幡宮、左義長祭りと同一社）",
  "王の浜若宮神社",         35.1717,  136.0974, "近畿地方火祭り.xlsx（白王町王之浜 若宮神社）",
  "小田神社",               35.1148,  136.0469, "近畿地方火祭り.xlsx（小田町 小田神社）",
  "大嶋奥津嶋神社",         35.1709,  136.0815, "近畿地方火祭り.xlsx（北津田町 大嶋奥津嶋神社）"
)

cat("=== 分析対象", nrow(festival_points), "祭り（滋賀県11祭り中、詳細は本ファイル冒頭コメント参照）===\n")
cat("除外3件: 雄琴学区ヨシ松明一斉点火（メッシュ外）、勝部の火祭り（境界上）、",
    "松明を次世代に送る会（単一地点なし）\n\n")

pts_sf <- st_as_sf(festival_points, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(6668)  # JGD2011、シェープファイルと同一CRSに揃える

# ------------------------------------------------------------------------------
# 2. 土地利用ポリゴンの読み込み（対象8地点の周辺のみ、bboxで絞り込み）
# ------------------------------------------------------------------------------
cat("土地利用データを読み込み中...\n")
t0 <- Sys.time()
# 【2026-09-13 追記】wkt_filter が本環境（sf 3.13.0 / GDAL 3.8.5）でこの
# シェープファイルに対して常に0件を返す問題を確認したため、範囲絞り込みなし
# の全件読み込みに変更した（1,120,000件・約6秒・923MBで実用上問題ない）。
landuse <- st_read(
  "data_raw/L03-b-c-21_5236-jgd2011_GML/L03-b-c-21_5236.shp",
  quiet = TRUE
)
cat("読み込み完了:", nrow(landuse), "メッシュ、", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "秒\n")

# 【重要】実データの空間範囲を検査する。ファイル名が示す1次メッシュ「5236」
# の理論上の範囲は 経度[136.0,137.0)・緯度[34.667,35.333) だが、実際に
# ダウンロードされたファイルの中身がそれを完全にカバーしているとは限らない
# （国土数値情報の配布ツールで範囲選択した場合、1次メッシュ全体ではなく
# 選択範囲と交差する2次メッシュだけが入ることがある）。対象地点がこの
# 実データ範囲に入っているかを機械的に検査し、入っていなければ空の結果を
# 静かに返す代わりにエラーで止める。
data_bbox <- st_bbox(landuse)
cat(sprintf("\n実データの空間範囲: 経度[%.4f, %.4f) 緯度[%.4f, %.4f)\n",
            data_bbox["xmin"], data_bbox["xmax"], data_bbox["ymin"], data_bbox["ymax"]))

pts_bbox <- st_bbox(pts_sf)
out_of_range <- festival_points$festival[
  festival_points$lon < data_bbox["xmin"] | festival_points$lon > data_bbox["xmax"] |
  festival_points$lat < data_bbox["ymin"] | festival_points$lat > data_bbox["ymax"]
]
if (length(out_of_range) > 0) {
  stop(
    "対象地点が実データの範囲外です: ", paste(out_of_range, collapse = "、"), "\n",
    "  ファイル名が示す1次メッシュ5236の理論範囲（経度136.0-137.0）とは異なり、\n",
    "  実際のデータは経度", round(data_bbox["xmin"], 3), "以東しかカバーしていません。\n",
    "  国土数値情報のダウンロードサイトで「1次メッシュ5236」を選択し直すか、\n",
    "  範囲選択（都道府県境等）ではなく1次メッシュ単位でのダウンロードを試してください。\n",
    "  滋賀県中西部（近江八幡・守山・東近江・甲賀信楽等）をカバーするには、\n",
    "  このファイルに加えて西隣の1次メッシュ「5235」（経度135.0-136.0）も必要な可能性があります。"
  )
}

names(landuse)[1:4] <- c("meshcode", "landuse_code", "survey_date", "flag")
landuse <- landuse %>%
  mutate(landuse_code = sprintf("%04d", as.integer(landuse_code)),
         landuse_label = unname(LANDUSE_CODES[landuse_code]))

cat("\n=== 絞り込み範囲内の土地利用構成（全体、参考） ===\n")
print(sort(table(landuse$landuse_label), decreasing = TRUE))

# ------------------------------------------------------------------------------
# 3. 座標系を平面直角座標系（メートル単位、近畿圏=第VI系 EPSG:6675）に変換し、
#    各地点にバッファーを作成
# ------------------------------------------------------------------------------
pts_proj <- st_transform(pts_sf, 6675)
landuse_proj <- st_transform(landuse, 6675)

BUFFER_RADII <- c(500, 1000)  # メートル。花背松上げ等の聞き取りで「半径1km以内」
                               # という言及があったため1kmを主要な尺度とし、
                               # 感度分析として500mも併記する。

compute_composition <- function(radius_m) {
  buf <- st_buffer(pts_proj, radius_m)
  buf$buf_area <- as.numeric(st_area(buf))
  inter <- st_intersection(landuse_proj, buf)
  inter$area <- as.numeric(st_area(inter))
  inter_df <- st_drop_geometry(inter) %>%
    group_by(festival, landuse_label) %>%
    summarise(area = sum(area), .groups = "drop") %>%
    left_join(st_drop_geometry(buf) %>% select(festival, buf_area), by = "festival") %>%
    mutate(pct = area / buf_area, radius_m = radius_m)
  inter_df
}

cat("\nバッファー交差処理中（500m・1000m）...\n")
comp_all <- bind_rows(lapply(BUFFER_RADII, compute_composition))

cat("\n=== 祭りごとの土地利用構成（半径1km）===\n")
print(as.data.frame(comp_all %>% filter(radius_m == 1000) %>%
  select(festival, landuse_label, pct) %>%
  pivot_wider(names_from = landuse_label, values_from = pct, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))))

write.csv(comp_all, file.path(OUTPUT_DIR, "landuse_composition_by_festival.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 4. 植物資源利用・保全措置との突き合わせ
# ------------------------------------------------------------------------------
# conservation_measures_analysis.R の出力（結果6ベースの保全段階・嵌入度・
# 景観多様性）を読み込む。無ければメッセージを出して土地利用の可視化のみ行う。
cons_path <- "data_proc/conservation_measures/conservation_tier_by_festival.csv"
if (file.exists(cons_path)) {
  cons <- read.csv(cons_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  joined <- comp_all %>% filter(radius_m == 1000) %>%
    select(festival, landuse_label, pct) %>%
    pivot_wider(names_from = landuse_label, values_from = pct, values_fill = 0) %>%
    left_join(cons %>% select(festival, conservation_tier, tier_label, mean_embed, n_habitats),
              by = "festival")

  cat("\n=== 土地利用構成 × 保全段階・嵌入度（半径1km）===\n")
  print(as.data.frame(joined %>%
    select(festival, tier_label, mean_embed, any_of(c("森林", "田", "その他農用地", "河川地及び湖沼"))) %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))))

  # 森林率 vs 嵌入度（自給度）
  if ("森林" %in% names(joined)) {
    ct <- cor.test(joined$森林, joined$mean_embed, method = "spearman", exact = FALSE)
    cat("\n森林率 vs 嵌入度 の相関（Spearman）: rho =", round(ct$estimate, 3),
        " p =", round(ct$p.value, 3), " n =", nrow(joined), "\n")
  }
  # 農地率（田+その他農用地）vs 嵌入度
  ag_cols <- intersect(c("田", "その他農用地"), names(joined))
  if (length(ag_cols) > 0) {
    joined$agri_pct <- rowSums(joined[ag_cols], na.rm = TRUE)
    ct2 <- cor.test(joined$agri_pct, joined$mean_embed, method = "spearman", exact = FALSE)
    cat("農地率(田+その他農用地) vs 嵌入度 の相関（Spearman）: rho =", round(ct2$estimate, 3),
        " p =", round(ct2$p.value, 3), "\n")
  }

  write.csv(joined, file.path(OUTPUT_DIR, "landuse_x_conservation.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  # --- 図：森林率 × 嵌入度、色=保全段階 ---
  TIER_COLORS <- c(
    "A: 生態系・環境保全と明示的に紐づく措置" = "#1A6A1A",
    "B: 祭礼専用の計画的栽培"                 = "#74C476",
    "C: 受動的な資源管理（経験知ベース）"     = "#FDB863",
    "D: 計画的な措置なし"                     = "#D62728",
    "分類不能（要目視確認）"                   = "gray60"
  )
  if ("森林" %in% names(joined)) {
    p1 <- ggplot(joined, aes(x = 森林, y = mean_embed)) +
      geom_point(aes(color = tier_label, size = n_habitats), alpha = 0.9) +
      geom_text_repel(aes(label = festival), size = 3, family = "HiraginoSans-W3",
                       max.overlaps = 20) +
      scale_x_continuous(labels = scales::percent, limits = c(0, NA)) +
      scale_y_continuous(breaks = 1:3, limits = c(0.8, 3.2)) +
      scale_color_manual(values = TIER_COLORS, name = "保全措置の段階", drop = FALSE) +
      scale_size_continuous(name = "景観タイプ数\n(聞き取りベース)") +
      labs(
        title = "神社周辺1km圏内の森林率 × 調達嵌入度",
        subtitle = paste0("国土数値情報 土地利用細分メッシュ（令和2年、滋賀県分の一部）。",
                          "対象8祭り（メッシュ5236でカバーされる範囲のみ）"),
        x = "半径1km圏内に占める森林の割合（実測）", y = "嵌入度（平均自給度、聞き取りベース）"
      ) +
      theme_bw(base_family = "HiraginoSans-W3") +
      theme(plot.title = element_text(face = "bold"))
    ggsave(file.path(OUTPUT_DIR, "forest_pct_x_embed.png"), p1, width = 10, height = 7, dpi = 150)
  }

  # --- 図：各祭りの土地利用構成（積み上げ棒、1km）---
  LANDUSE_COLORS <- c(
    "森林" = "#238B45", "その他農用地" = "#A1D99B", "田" = "#3182BD",
    "河川地及び湖沼" = "#6BAED6", "荒地" = "#969696", "建物用地" = "#FD8D3C",
    "その他用地" = "#DE77AE", "道路" = "#525252", "鉄道" = "#252525",
    "海浜" = "#FEE08B", "海水域" = "#4575B4", "ゴルフ場" = "#66C2A4"
  )
  p2 <- comp_all %>% filter(radius_m == 1000) %>%
    left_join(cons %>% select(festival, mean_embed), by = "festival") %>%
    mutate(festival = forcats::fct_reorder(festival, mean_embed)) %>%
    ggplot(aes(x = festival, y = pct, fill = landuse_label)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = LANDUSE_COLORS, name = "土地利用") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "神社周辺1km圏内の土地利用構成（嵌入度の低い順）",
         subtitle = "国土数値情報 土地利用細分メッシュ（令和2年）",
         x = NULL, y = "面積割合") +
    theme_bw(base_family = "HiraginoSans-W3") +
    theme(plot.title = element_text(face = "bold"), legend.position = "right")
  ggsave(file.path(OUTPUT_DIR, "landuse_composition_stacked.png"), p2, width = 10, height = 6, dpi = 150)

  cat("\n  forest_pct_x_embed.png            森林率×嵌入度\n")
  cat("  landuse_composition_stacked.png   土地利用構成（積み上げ棒）\n")
} else {
  cat("\n注意:", cons_path, "が見つかりません。先に conservation_measures_analysis.R を",
      "実行してください。土地利用データ自体の集計結果は上記の通り出力済みです。\n")
}

cat("\n完了。出力先:", OUTPUT_DIR, "\n")
