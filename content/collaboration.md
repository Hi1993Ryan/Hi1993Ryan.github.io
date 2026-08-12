---
title: Collaboration Network
url: collaboration/
wideContent: true
ShowShareButtons: false
description: "Interactive map of co-author institutions."
summary: "Institution-level collaboration map."
---

按**合作机构**汇总合著关系；以重庆大学为枢纽，**航线式大圆航线**连向各合作机构。点击节点可查看合著论文与机构链接。国外机构仅显示英文名称。

Institution-level collaboration map with **airline-style great-circle routes** from Chongqing University. Click a marker for papers and links. Foreign institutions are labeled in English only.

{{< rawhtml >}}
<iframe
  class="collab-map-frame"
  src="/collab-map/index.html"
  title="Collaboration network map"
  loading="lazy"
  referrerpolicy="no-referrer-when-downgrade"></iframe>
{{< /rawhtml >}}

数据与脚本在仓库 `collab-map/`：改 `data/papers.csv` / `data/institutions.csv` 后运行 `Rscript collab-map/build_collab_map.R` 即可更新地图。
