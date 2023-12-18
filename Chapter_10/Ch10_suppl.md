第十章: 潛在成長模型分析（附錄）
================

\#讀取與擷取資料

``` r
# 本程式碼需自行到學術調查研究資料庫下載 KIT 資料後方能順利執行
# 下載某波資料壓縮檔解壓後，會有 data1.csv
# 此處假設各波資料（data1.csv）有相同變項命名與格式，並被集中在底下資料夾中，且適當命名
folder_path <- "../Data/KITdata/"

# 將檔案名都存於 file_list
# 五波資料，假設命名為 W1.csv,W2.csv,...,W5,CSV
file_list <- list.files(folder_path, pattern = "W[1-5].csv", full.names = TRUE)

# 定義擷取單一檔案資料的函數，會加總記憶變項，擷取編號與母親教育程度
# 記憶的各變項，5是拒答，9999是遺漏值
extract_data_from_file <- function(file) { 
   data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE) |> 
           dplyr::select(coga01:cogc07, baby_id, pfa0202, relationship) |>
           dplyr::mutate(score = across(coga01:cogc07, ~if_else(. > 4, NA_real_, .)) |>           
           rowSums(na.rm = FALSE)) |>
           dplyr::select(!(coga01:cogc07))
   return(data) 
}

# 利用定義函數，搭配 lapply，處理每個資料，會形成物件列
extracted_data_list <- lapply(file_list, FUN=extract_data_from_file) 
```

``` r
#利用編號與母親教育程度，將五波資料接成寬形
merged_data <- plyr::join_all(extracted_data_list, by = c("baby_id", "pfa0202"))
```

``` r
# 重新命名變項，避免 join 產生的重複變項名稱
names(merged_data)[-c(1,2)] <- paste(names(merged_data)[-c(1,2)], rep(1:5, rep(2,5)), sep='.')
```

``` r
#看一下各個資料有幾筆，幾個變項
extracted_data_list |> glimpse()
```

    List of 5
     $ :'data.frame':   6588 obs. of  4 variables:
      ..$ baby_id     : chr [1:6588] "N111001213" "C340101165" "N232001184" "N236801005" ...
      ..$ pfa0202     : int [1:6588] 2 4 2 2 2 5 5 4 3 2 ...
      ..$ relationship: int [1:6588] 3 3 2 3 2 3 2 3 2 3 ...
      ..$ score       : num [1:6588] 29 23 27 25 27 29 32 25 23 24 ...
     $ :'data.frame':   6739 obs. of  4 variables:
      ..$ baby_id     : chr [1:6739] "C340001002" "C340001003" "C340001004" "C340001005" ...
      ..$ pfa0202     : int [1:6739] 5 5 4 5 5 5 4 5 5 3 ...
      ..$ relationship: int [1:6739] 3 2 3 3 3 3 2 3 3 2 ...
      ..$ score       : num [1:6739] 36 22 34 31 37 31 34 31 34 30 ...
     $ :'data.frame':   6874 obs. of  4 variables:
      ..$ baby_id     : chr [1:6874] "C340001002" "C340001004" "C340001005" "C340001006" ...
      ..$ pfa0202     : int [1:6874] 5 4 5 2 9999 5 5 4 5 9999 ...
      ..$ relationship: int [1:6874] 3 3 3 3 3 3 3 2 3 3 ...
      ..$ score       : num [1:6874] 57 58 50 54 57 58 50 53 55 51 ...
     $ :'data.frame':   6866 obs. of  4 variables:
      ..$ baby_id     : chr [1:6866] "C340001002" "C340001003" "C340001004" "C340001005" ...
      ..$ pfa0202     : int [1:6866] 5 5 4 5 9999 5 5 4 5 9999 ...
      ..$ relationship: int [1:6866] 3 2 3 3 3 3 3 2 3 3 ...
      ..$ score       : num [1:6866] 64 42 61 60 62 61 59 57 57 58 ...
     $ :'data.frame':   6775 obs. of  4 variables:
      ..$ baby_id     : chr [1:6775] "C340001002" "C340001004" "C340001005" "C340001006" ...
      ..$ pfa0202     : int [1:6775] 5 4 5 3 5 5 5 4 5 5 ...
      ..$ relationship: int [1:6775] 3 3 3 3 3 3 2 2 3 3 ...
      ..$ score       : num [1:6775] 63 68 63 57 73 65 58 59 61 59 ...

``` r
glimpse(merged_data)
```

    Rows: 6,588
    Columns: 12
    $ baby_id        <chr> "N111001213", "C340101165", "N232001184", "N236801005",…
    $ pfa0202        <int> 2, 4, 2, 2, 2, 5, 5, 4, 3, 2, 1, 5, 5, 5, 2, 5, 3, 5, 4…
    $ relationship.1 <int> 3, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3…
    $ score.1        <dbl> 29, 23, 27, 25, 27, 29, 32, 25, 23, 24, 24, 21, 25, 29,…
    $ relationship.2 <int> NA, NA, 2, 3, 2, 3, NA, NA, 2, 3, 2, 3, 3, 3, 3, 3, NA,…
    $ score.2        <dbl> NA, NA, 30, 30, 31, 33, NA, NA, 29, 31, 33, 32, 28, 35,…
    $ relationship.3 <int> NA, 3, 2, 3, 2, 3, NA, NA, 2, 3, 2, 3, 2, 3, NA, NA, NA…
    $ score.3        <dbl> NA, 44, 46, 47, 58, 51, NA, NA, 58, 50, 46, 53, 49, 61,…
    $ relationship.4 <int> NA, 3, 3, 3, 2, 3, NA, NA, 3, 3, 2, 3, NA, 3, 3, NA, NA…
    $ score.4        <dbl> NA, 54, 58, 55, 59, 58, NA, NA, 61, 61, 53, 59, NA, 64,…
    $ relationship.5 <int> NA, NA, 3, 3, 2, 3, NA, NA, 2, 3, 2, 3, 2, 3, 3, NA, NA…
    $ score.5        <dbl> NA, NA, 69, 63, 61, 65, NA, NA, 65, 69, 60, 69, 69, 79,…

\#資料整理，去除無效資料

``` r
#保留沒有遺漏值的資料
filtered_data  <- merged_data[complete.cases(merged_data),]

#五波填答人必須是同一個，利用計算五波的 SD 看看是否一致
row_sd <- 
  filtered_data |> 
  select(contains("relationship")) |> 
  pmap_dbl(~ sd(c(...), na.rm = TRUE))

#看一下有多少筆是同一人填的，多少筆不是
table(row_sd==0)
```

<table>
<thead>
<tr>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
699
</td>
<td style="text-align:right;">
4078
</td>
</tr>
</tbody>
</table>

``` r
#將 row_sd 加入 filtered_data 以便過濾
#選出 row_sd 等於 0 的行（五波填答人同一個）並刪除包含 'relationship' 的變項
filtered_data <- filtered_data |> 
    mutate(row_sd = row_sd) |>
    filter(row_sd == 0) |> 
    select(-contains("relationship"),-row_sd) 


#變項更名，重新製造識別碼 ，
names(filtered_data) <- c('識別碼','母親教育程度','月_03','月_06','月_12','月_18','月_24')

#確認一下資料
head(filtered_data)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
識別碼
</th>
<th style="text-align:right;">
母親教育程度
</th>
<th style="text-align:right;">
月_03
</th>
<th style="text-align:right;">
月_06
</th>
<th style="text-align:right;">
月_12
</th>
<th style="text-align:right;">
月_18
</th>
<th style="text-align:right;">
月_24
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
N236801005
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
N120401113
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
N111001417
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
65
</td>
</tr>
<tr>
<td style="text-align:left;">
N122701032
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
69
</td>
</tr>
<tr>
<td style="text-align:left;">
N233301050
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
N123401431
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
69
</td>
</tr>
</tbody>
</table>

# 隨機抽取 2,000 筆資料

``` r
set.seed(20230716)
sampled_data <- 
  filtered_data |> 
  sample_n(min(2000, nrow(filtered_data))) |> 
  mutate(
    識別碼 = factor(paste0("S",1001:3000)),
    母親教育程度 = factor(母親教育程度) |> 
      fct_recode(專科以下="1",
                 專科以下="2",
                 專科以下="3",
                 專科以下="4",
                 大學以上="5",
                 大學以上="6")
  )

#看一下抽取結果
head(sampled_data)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
識別碼
</th>
<th style="text-align:left;">
母親教育程度
</th>
<th style="text-align:right;">
月_03
</th>
<th style="text-align:right;">
月_06
</th>
<th style="text-align:right;">
月_12
</th>
<th style="text-align:right;">
月_18
</th>
<th style="text-align:right;">
月_24
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
S1001
</td>
<td style="text-align:left;">
大學以上
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
S1002
</td>
<td style="text-align:left;">
大學以上
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
65
</td>
</tr>
<tr>
<td style="text-align:left;">
S1003
</td>
<td style="text-align:left;">
大學以上
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
S1004
</td>
<td style="text-align:left;">
專科以下
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
68
</td>
</tr>
<tr>
<td style="text-align:left;">
S1005
</td>
<td style="text-align:left;">
大學以上
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
65
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
S1006
</td>
<td style="text-align:left;">
大學以上
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
68
</td>
</tr>
</tbody>
</table>

# 輸出資料

``` r
write.csv(sampled_data, 'KIT_memory_20230716.csv', row.names=FALSE, quote=FALSE)
```
