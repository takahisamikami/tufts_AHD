

if (!require("pacman")) { install.packages("pacman") }
pacman::p_load( pipeR, dplyr, tidyverse, pdftools, tabulizer, data.table, lubridate )
if (!require("remotes")) { install.packages("remotes") }
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

df_rotation = openxlsx::read.xlsx("/Users/TakahisaMikami/drive/0.NeuroEducation/Tufts_education/AHD/AHD_participants/attendance_reports/participants_schedule.xlsx")

date_list <- c("d1104", "d1111", "d1118", "d1202") # , "d1209"
path_prefix <- "/Users/TakahisaMikami/drive/0.NeuroEducation/Tufts_education/AHD/AHD_participants/attendance_reports/participants_"
file_list <- paste0(path_prefix, date_list, ".csv") %>>% as.list()
# filename <- file_vec[1]


func_convert_name <- function( df ) {
  df %>>%
    dplyr::filter( ! str_detect( name, "Suski|Agosto|Yerstein|PRO|12819396952|16177506418|Yakov|SUNY|OBH|Lerner|Kornbluth|tufts|ivia|Grace|Xuemei|iPhone|Ofori|RESPAR|paulgross" ) ) %>>% 
    dplyr::mutate( 
      name = case_when(
        str_detect( name, "ikami" ) ~ "Taka", 
        str_detect( name, "Taka" ) ~ "Taka", 
        str_detect( name, "Valeria" ) ~ "Valeria", 
        str_detect( name, "Firas" ) ~ "Firas", 
        str_detect( name, "asaman" ) ~ "Yasaman", 
        str_detect( name, "asmine" ) ~ "Yasaman", 
        str_detect( name, "anizhe" ) ~ "Manizhe", 
        str_detect( name, "amelia" ) ~ "Camelia", 
        str_detect( name, "afail" ) ~ "Rafail",
        str_detect( name, "vanna" ) ~ "Ivanna",
        str_detect( name, "Aya" ) ~ "Aya",
        str_detect( name, "ikita" ) ~ "Nikita",
        str_detect( name, "aby" ) ~ "Gaby",
        str_detect( name, "hsan" ) ~ "Ehsan",
        str_detect( name, "mniyah" ) ~ "Omniyah",
        str_detect( name, "az" ) ~ "Naz",
        str_detect( name, "arc" ) ~ "Marc",
        str_detect( name, "libay" ) ~ "Alibay",
        str_detect( name, "naman" ) ~ "Naman",
        str_detect( name, "Naman" ) ~ "Naman",
        str_detect( name, "enzheng" ) ~ "Wenzheng",
        str_detect( name, "ustafa" ) ~ "Mustafa",
        str_detect( name, "rsalan" ) ~ "Arsalan",
        TRUE ~ name
      )
    ) 
}

    # dplyr::filter( name == "Gaby") %>>% 


func_ahd_tidy <- function( filename_i ) { 
  # temp_union = 
  data.table::fread( filename_i ) %>>% 
    dplyr::select( name = 1, t_join = 3, t_leave = 4 ) %>>% 
    func_convert_name() %>>% 
    dplyr::mutate( 
      across( c( t_join, t_leave ), ~ lubridate::mdy_hms(.x) )
    ) %>>% 
    group_by(name) %>%
    mutate(indx = c(0, cumsum(as.numeric(lead(t_join)) >
                                cummax(as.numeric(t_leave)))[-n()])) %>%
    group_by(name, indx) %>%
    summarise(t_join = first(t_join), t_leave = last(t_leave)) %>>% 
    dplyr::ungroup() 
}

df_ahd_list <- file_list %>>% lapply( func_ahd_tidy )
    
i = 1
df = df_ahd_list[[i]]

func_ahd_ModifyEndTime <- function( i ) { 
  df_leave <- df_ahd_list[[i]] %>>% 
    dplyr::group_by( name ) %>>% 
    dplyr::summarise( max_t_leave = max(t_leave) )
  median_t_leave <- median(sort(df_leave$max_t_leave))
  df_ahd_list[[i]] %>>% 
    dplyr::mutate(
      t_leave = if_else( t_leave > median_t_leave, median_t_leave, t_leave ) ) %>>%
    
    dplyr::mutate( interval = time_length(interval(t_join, t_leave), unit = "minutes") ) %>>% 
    dplyr::arrange(name) %>>%
    dplyr::group_by(name) %>>% 
    dplyr::summarise( 
      t_join = min(t_join), 
      t_leave = max(t_leave),
      t_total = sum(interval), 
      n_login = n()
    ) %>>% 
    dplyr::ungroup() %>>% 
    dplyr::mutate(
      join_rate = t_total / max(t_total)
    ) %>>% 
  dplyr::right_join( df_rotation[c("name", date_list[i])], by = "name" ) %>>% 
    dplyr::rename( rotation = ncol(.) ) %>>% 
    dplyr::mutate( across( t_total:join_rate, ~ replace_na( .x, 0 ) ) )
}

df_ahd_tidy <- 
  purrr::map_df( 1:4, func_ahd_ModifyEndTime ) %>>% 
  as.data.frame() %>>% 
  dplyr::arrange( name )

df_ahd_tidy$res_id <- as.factor( match(df_ahd_tidy$name, sample(unique(df_ahd_tidy$name))) )
df_rotation_mean <- df_ahd_tidy %>>% 
  dplyr::group_by(rotation) %>>% 
  dplyr::summarise( rate_mean = mean(join_rate) ) %>>% 
  dplyr::arrange( rate_mean )


df_ahd_tidy %>>% 
  dplyr::mutate( rotation = fct_relevel( rotation, df_rotation_mean$rotation ) ) %>>%
  ggplot( aes( x = rotation, y = join_rate, color = res_id ) ) + # , group = cov_int )
  # geom_boxplot() +
  # geom_line( aes( linetype = cov_int ), position = pd ) + 
  geom_point( size = 1.5 ) +  # , position = pd
  labs( # title = paste( "1-year mortality" ),alpha
    x = "Rotation", 
    y = "Attendance (%)" ) + 
  labs( colour = "Residents" ) + # shape="", 
  
  # 
  theme(
    plot.title = element_text(size = 12, hjust = 0, vjust = 3), # , face = "bold"
    # legend.position= c(0.15, 0.7), # "top", # c( 0,1 ), # .2, .9 
    legend.justification = c(0, 0), # 1, 0.5  # https://blog.atusy.net/2018/11/10/ggplot2-legend-pos-n-just/
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(colour = "transparent", fill = "white"),
    legend.text = element_text(size=8),
    legend.title = element_text(size=8), 
    legend.key.height = unit(.3, "cm"), # https://github.com/tidyverse/ggplot2/wiki/legend-attributes
    # axis.title.x = element_text(size=14),　#x軸タイトルを上詰め(0)、黒色、サイズ１４で
    # axis.title.y = element_text(vjust = 0.5,angle=0,size=14),#vjustで上下の位置調整、英語の場合はangle = 90で
    # axis.text.x = element_text(size=12),#x軸の目盛りに対応した文字サイズを12で
    # axis.text.y = element_text(size=12,colour="black"),
    # axis.ticks.x = element_line(size = 0),#x軸の目盛り線をゼロにして消す
    # axis.ticks.x = element_line(color = c( rep(NA, len - 1), rep("black", len) ) ), 
    axis.text.x = element_text(angle = 45, hjust =1, size = 10), 
    axis.line.x = element_line(colour = "black", size = .5),
    axis.line.y = element_line(colour = "black", size = .5),
    axis.line = element_line(size = 1
    )
  )




rotations_vec <- df_rotation %>>% dplyr::select( starts_with("d") ) %>>% as.vector() %>>% unlist() %>>% unique() %>>% subset( !is.na(.))

df_ahd_tidy_list[[4]] %>>% 
  dplyr::left_join( df_rotation[c("name", date_list[4])], by = "name" ) %>>% 
  dplyr::group_by( d1202 ) %>>% 
  dplyr::summarise( mean(t_total) )
  


# 





# tidy_ahdattendance <- function( filename_i ) { 
#   data.table::fread( filename_i ) %>>% 
#     dplyr::select( name = 1, t_join = 3, t_leave = 4, duration = 5 ) %>>% 
#     tidyr::separate( t_join, into = c("d_join", "t_join"), sep = " ") %>>%
#     tidyr::separate( t_leave, into = c("d_leave", "t_leave"), sep = " ") %>>%
#     dplyr::select( -d_leave ) %>>%
#     dplyr::mutate( 
#       across( c( d_join ), ~ lubridate::mdy( str_replace_all( .x, "/", "-" )  ) ),
#       across( c( t_join, t_leave ), ~ lubridate::hms( .x )  ), 
#       t_join = if_else( t_join < hours( 1 ) | t_join > hours( 12 ) , hours( 1 ), t_join ),
#       t_leave = if_else( t_leave > hours( 5 ), hours( 5 ), t_leave ), 
#       duration =  as.numeric(as.difftime(t_leave - t_join)) /60 
#     ) %>>%  # ---> temp
#     func_convert_name() %>>% 
#     # dplyr::filter( !str_detect( name, "Tufts" ) )
#     dplyr::mutate(
#       across( c( t_join, t_leave ), ~ as.numeric(seconds( .x ) / 60) - 60 )
#     ) %>>% 
#     dplyr::arrange( name ) %>>% 
#     dplyr::group_by( name ) %>>% 
#     dplyr::summarise( 
#       total_duration = sum(duration),
#       dt_join = min(t_join),
#       dt_leave = max(t_leave),
#       n_login = n() 
#       
#     ) %>>% 
#     as_tibble() %>>% 
#     dplyr::filter( ! str_detect( name, "Suski|Agosto|Yerstein|PRO|12819396952|16177506418|Yakov|SUNY|OBH|Lerner|Kornbluth|tufts|ivia|Grace|Xuemei|iPhone" ) ) %>>% 
#     dplyr::arrange( name )
#   }
  


  
  

###








d1 <- 
  data.table::fread(filename) 


d1 %>% 
  dplyr::select( "Name (Original Name)", "Join Time"  ) %>% head()


colnames( d1 )
head( d1 )

# df <- head( d1 )
d2 <- d1 %>>% 
  dplyr::select( name = 1, t_join = 3, t_leave = 4, duration = 5 ) %>>% 
  tidyr::separate( t_join, into = c("d_join", "t_join"), sep = " ") %>>%
  tidyr::separate( t_leave, into = c("d_leave", "t_leave"), sep = " ") %>>%
  dplyr::select( -d_leave ) %>>%
  dplyr::mutate( 
    across( c( d_join ), ~ lubridate::mdy( str_replace_all( .x, "/", "-" )  ) ),
    across( c( t_join, t_leave ), ~ lubridate::hms( .x )  ), 
    t_join = if_else( t_join < hours( 1 ) | t_join > hours( 12 ) , hours( 1 ), t_join ),
    t_leave = if_else( t_leave > hours( 4 ), hours( 4 ), t_leave ), 
    duration =  as.numeric(as.difftime(t_leave - t_join)) /60 
    ) 

d2 %>>% 
  dplyr::filter( str_detect( name, "and" ) )

d1_ <- d2 %>>%
  dplyr::mutate( 
    name = case_when(
      str_detect( name, "ikami" ) ~ "Taka", 
      str_detect( name, "Taka" ) ~ "Taka", 
      str_detect( name, "Valeria" ) ~ "Valeria", 
      str_detect( name, "Firas" ) ~ "Firas", 
      str_detect( name, "asaman" ) ~ "Yasaman", 
      str_detect( name, "anizhe" ) ~ "Manizhe", 
      str_detect( name, "amelia" ) ~ "Camelia", 
      str_detect( name, "afail" ) ~ "Rafail",
      str_detect( name, "vanna" ) ~ "Ivanna",
      str_detect( name, "ikita" ) ~ "Nikita",
      str_detect( name, "aby" ) ~ "Gaby",
      str_detect( name, "hsan" ) ~ "Ehsan",
      str_detect( name, "mniyah" ) ~ "Omniyah",
      str_detect( name, "az" ) ~ "Naz",
      str_detect( name, "arc" ) ~ "Marc",
      str_detect( name, "libay" ) ~ "Alibay",
      str_detect( name, "enzheng" ) ~ "Wenzheng",
      str_detect( name, "ustafa" ) ~ "Mustafa",
      str_detect( name, "rsalan" ) ~ "Arsalan",
      
      TRUE ~ name
    )
    ) %>>% 
  dplyr::filter( !str_detect( name, "Tufts" ) )

d1_ %>>% head()
d1 %>>% dplyr::filter( str_detect( name, "ibay" ) )


  
d1_ %>>% 
  dplyr::mutate(
    across( c( t_join, t_leave ), ~ as.numeric(seconds( .x ) / 60) - 60 )
  ) %>>% 
  dplyr::group_by( name ) %>>% 
  dplyr::summarise( 
    total_duration = sum(duration),
    dt_join = min(t_join),
    dt_leave = max(t_leave),
    n_login = n() 
    
    ) %>>% 
  as.data.frame() 

%>>% 
  dplyr::left_join( df_rotation, by = "name" ) %>>% 
  dplyr::filter( ! str_detect( name, "Suski|Agosto|Yerstein|PRO|12819396952|16177506418" ) )





# dplyr::mutate( join_time = lubridate::ymd_hms(join_time) )
  

# pdf.text <- pdftools::pdf_text("/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2/RITE_2021_Mikami.pdf")
# tabel <- extract_tables('/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2/RITE_2021_Mikami.pdf', pages = 1)
# 
# 
# tabel <- extract_tables('/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2/RITE_2021_Mikami.pdf', pages = 2)
# 
# tabel[[1]][-c(1:2), -c(4)] %>>% 
#   as.data.frame() %>>% 
#   tidyr::separate( V2, sep = " ", into = c("V2_1", "V2_2") ) %>>% 
#   dplyr::mutate( 
#     V2_2 = stringr::str_remove(V2_2, "%"), 
#     across( V2_1:V4, as.numeric )
#     )
# 


##################################################################################################
# Table1
##################################################################################################


table.list <- 
  list.files("/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2", pattern = ".pdf", full.names = T) %>>% 
  lapply( extract_tables, pages = 2)

tab1_area <- table.list[[1]][[1]][,1][-c(1:2)]

tabel[[1]] %>>% dim()


func_extract_tab1 <- 
  function( et ) {
    et[[1]][-c(1:2), -c(4)] %>>% 
      as.data.frame() %>>% 
      tidyr::separate( V2, sep = " ", into = c("V2_1", "V2_2") ) %>>% 
      dplyr::mutate( 
        V2_2 = stringr::str_remove(V2_2, "%"), 
        across( V2_1:V4, as.numeric )
      )
  }


table.list %>>% lapply( func_extract_tab1 ) %>>%
  purrr::map2( seq(length(.)), ~ .x %>>% dplyr::mutate( id = .y ) ) %>>% 
  bind_rows() %>>% 
  dplyr::group_by( V1 ) %>>% 
  dplyr::summarise( n_q = mean( V2_1 ), mean( V2_2 ), mean( V3 ), mean( V4 ) ) %>>% 
  dplyr::arrange( match( V1, tab1_area ) )



# tab2 <- extract_tables('/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2/RITE_2021_Mikami.pdf', pages = c(3,4))
# tab2

# tab2_1 <- tab2[[1]]
# fix1.row <- which(tab2_1[,1] == "Adult Neuroimmunologic/Demyelinating Disease and Neuroinfectious")
# fix1.row <- which(stringr::str_detect(tab2_1[,1], "Neuroimmunologic"))
# # tab2_1[ tab2_1.fix1.row + 1, ][1] %>>% stringr::str_sub( 1, 2)
# fix1.str_to_spl <- tab2_1[ fix1.row + 1, ][1] %>>% stringr::str_extract( pattern = "[^0-9]+" )
# fix1.row.spl <- tab2_1[ tab2_1.fix1.row + 1, ][1] %>>% stringr::str_split( fix1.str_to_spl )
# tab2_1[ fix1.row, 1] <- paste(tab2_1[ fix1.row, 1], fix1.str_to_spl, fix1.row.spl[[1]][2])
# tab2_1[ fix1.row, 2] <- tab2_1[ fix1.row + 1,2]
# tab2_1[ fix1.row, 3] <- fix1.row.spl[[1]][1]
# tab2_1
# 
# 
# fix2.row <- which(stringr::str_detect(tab2_1[,1], "Headache and Pain"))
# # tab2_1[ tab2_1.fix2.row + 1, ][1] %>>% stringr::str_sub( 1, 2)
# fix2.str_to_spl <- tab2_1[ fix2.row + 1, ][1] %>>% stringr::str_extract( pattern = "[^0-9]+" )
# fix2.row.spl <- tab2_1[ fix2.row + 1, ][1] %>>% stringr::str_split( fix2.str_to_spl )
# tab2_1[ fix2.row, 1] <- paste(tab2_1[ fix2.row, 1], fix2.str_to_spl, fix2.row.spl[[1]][2])
# tab2_1[ fix2.row, 2] <- tab2_1[ fix2.row + 1,2]
# tab2_1[ fix2.row, 3] <- fix2.row.spl[[1]][1]
# tab2_1



##################################################################################################
# Table2
##################################################################################################

func_extract_tab2 <- function( tab2.l ) {
  # tab2.l <- extract_tables('/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2/RITE_2021_Mikami.pdf', pages = c(3,4), method = "lattice")
  
  tab2.l_1 <- rbind(tab2.l[[1]][-c(1),], tab2.l[[2]][-c(1),])
  tab2.l_1.o <- tab2.l_1[seq(nrow(tab2.l_1)) %% 2 == 1,]
  cat_big <- tab2.l_1.o[,1] %>>% stringr::str_replace("Adult ", "") %>>% stringr::str_split(" ") %>>% purrr::map(1) %>>% unlist()
  tab2.l_1.t <- tab2.l_1[seq(nrow(tab2.l_1)) %% 2 == 0,] %>>% as.data.frame() %>>% 
    dplyr::mutate(
      cat_big = cat_big
    ) %>>% 
    dplyr::group_split( cat_big ) %>>%
    purrr::map(
      ~ .x %>>% 
        purrr::map( ~ stringr::str_replace( .x, "and\r", "and" ) ) %>>% 
        purrr::map( ~ stringr::str_split( .x, "\r" ) ) %>>% 
        purrr::flatten_df()
    ) %>>% 
    bind_rows() %>>% as.data.frame() %>>% 
    dplyr::mutate( 
      V3 = stringr::str_remove(V3, "%"),
      across(V2:V5, as.numeric)
    ) %>>% 
    dplyr::select(
      Category = cat_big, Subcategory = V1, n_question = V2, Correct_pct = V3, Pctl_class = V4, Pctl_all = V5
    ) 
  
  unique(tab2.l_1.t$Subcategory)
  
  
  
  tab2 <- tab2.l_1.t %>>% 
    dplyr::mutate(
      Subcategory = case_when(
        Subcategory == "Neuroscience/Mechanism of Disease, Neuroanatomy, and Neuropathology" ~ "Basic",
        Subcategory == "Clinical Aspects" ~ "Clinical",
        Subcategory == "Neuroimaging and Neurophysiology" ~ "Imaging_Phys",
        Subcategory == "Clinical Treatment" ~ "Treatment",
        Subcategory == "Contemporary Issues" ~ "Comtemporary",
        Subcategory == "Other Diagnostic Procedures" ~ "Diagnosis",
        Subcategory == "Genetic and Developmental Disorders" ~ "Genetic",
        Subcategory == "Behavioral/Neurocognitive Disorders and Movement Disorders" ~ "Behavioral/Neurocognitive/Movement",
        Subcategory == "Vascular Neurology and Neurocritical Care" ~ "Vascular",
        Subcategory == "Neuromuscular/Spinal Cord Disorders and Other*" ~ "Neuromuscular/Spinal",
        Subcategory == "Epilepsy and Episodic Disorders and Sleep Disorders" ~ "Epilepsy",
        Subcategory == "Neuroimmunologic/Demyelinating Disease and Neuroinfectious Disease" ~ "Neuroimmunologic/Demyelinating",
        Subcategory == "Headache and Pain Disorders, Neuro-oncology, andNeuro-ophthalmologic/Neuro-otologic Disorders" ~ "Headache",
        TRUE ~ "NA"
      )
    )
  return( tab2 )
}


tab2.l <- tab2.list[[1]]

tab2.list <- 
  list.files("/Users/TakahisaMikami/drive/Documents/TuftsNeurology/PGY2", pattern = ".pdf", full.names = T) %>>% 
  lapply( extract_tables, pages = c(3,4), method = "lattice")

tab2.list[[1]] %>>% func_extract_tab2()

tab2.list %>>%
  lapply( func_extract_tab2 ) %>>%
  purrr::map2( seq(length(.)), ~ .x %>>% dplyr::mutate( id = .y ) ) %>>% 
  bind_rows() %>>% 
  dplyr::group_by( Category, Subcategory ) %>>% 
  dplyr::summarise( n_question = mean( n_question ), Correct_pct = mean( Correct_pct ), Pctl_class = mean( Pctl_class ), Pctl_all = mean( Pctl_all ) ) %>>% as.data.frame()




tab2_1.fix2.row <- which(tab2_1[,1] == "44Neuro-ophthalmologic/Neuro-otologic Disorders42 69%")
# tab2_1[ tab2_1.fix1.row + 1, ][1] %>>% stringr::str_sub( 1, 2)
tab2_1.fix2.row.spl <- tab2_1[ tab2_1.fix2.row + 1, ][1] %>>% stringr::str_split( "Disease")
tab2_1[ tab2_1.fix2.row, 1] <- paste(tab2_1[ tab2_1.fix2.row, 1], "Disease", tab2_1.fix2.row.spl[[1]][2])
tab2_1[ tab2_1.fix2.row, 2] <- tab2_1[ tab2_1.fix2.row + 1,2]
tab2_1[ tab2_1.fix2.row, 3] <- tab2_1.fix2.row.spl[[1]][1]

tab2_1[,1] %>>% stringr::str_extract( pattern = "[0-9]+%" )
tab2_1 %>>% 
  as.data.frame() %>>% 
  dplyr::mutate(
    V1_1 = stringr::str_extract( V1, pattern = "[0-9]+" ), 
    V1_2 = stringr::str_extract( V1, pattern = "[0-9]+%" ) %>>% stringr::str_remove( '%' ),
    V1 = stringr::str_extract( V1, pattern = "[^0-9]+" ),
    across(V2:V1_2, as.numeric)
  ) %>>% 
  dplyr::filter( !is.na( V3 ) )

  





transact <- read_csv('https://raw.githubusercontent.com/rsquaredacademy/datasets/master/transact.csv')

vacation_start    <- as_date('2017-04-19')
vacation_end      <- as_date('2017-04-25')
course_start    <- as_date('2017-04-12')
course_end      <- as_date('2017-04-21')
course_duration <- course_end - course_start
course_duration

course_interval   <- interval(course_start, course_end)
vacation_interval <- interval(vacation_start, vacation_end)
int_overlaps(course_interval, vacation_interval)


transact %>%
  mutate(
    inv_due_interval = interval(Invoice, Due),
    due_next         = Due + days(1),
    due_pay_interval = interval(due_next, Payment),
    overlaps         = int_overlaps(inv_due_interval, due_pay_interval)
  ) %>%
  select(Invoice, Due, Payment, overlaps)



my.start <- ymd("2018-08-01")
my.end <- ymd("2018-08-31")

df <- data.frame(start = c("2018-07-15", "2018-07-20", "2018-08-15", "2018-08-20", "2018-09-01"), 
                 end   = c("2018-07-20", "2018-08-05", "2018-08-19", "2018-09-15", "2018-09-15"))



df %>%
  mutate(
    pmin = pmin(my.end, end), 
    overlap = pmax(
      pmin(my.end, end) -
        pmax(my.start, start) + 
        1, 0) 
      )







temp_v <- temp %>>% 
  dplyr::filter( name == "Valeria")


temp_union = data.table::fread( filename_i ) %>>% 
  dplyr::select( name = 1, t_join = 3, t_leave = 4 ) %>>% 
  dplyr::filter( name == "Gaby") %>>% 
  dplyr::mutate( 
    across( c( t_join, t_leave ), ~ lubridate::mdy_hms(.x) ),
    interval = lubridate::interval(t_join, t_leave)
  )

temp_union %>%
  group_by(name) %>%
  mutate(indx = c(0, cumsum(as.numeric(lead(t_join)) >
                              cummax(as.numeric(t_leave)))[-n()])) %>%
  group_by(name, indx) %>%
  summarise(start = first(t_join), end = last(t_leave))



lubridate::union(temp_union$interval[1], temp_union$interval[2])
foverlaps(temp_v,data[[2]], type="any", nomatch=0)

temp_union %>>% 
  dplyr::group_by( name ) %>>% 
  dplyr::summarise( total = lubridate::union( interval ) )

across( c( d_join ), ~ lubridate::mdy( str_replace_all( .x, "/", "-" )  ) ),
across( c( t_join, t_leave ), ~ lubridate::hms( .x )  ), 

temp_v %>>% 
  

foverlaps(temp_v,data[[2]], type="any", nomatch=0)




data <- data.table( id = seq(1,21),
                    type = as.character(c(1,2,2,2,2,2,2,2,1,1,1,1,1,2,1,2,1,1,1,1,1)),
                    start_dt = as.Date(c("2015-01-09", "2015-04-14", "2015-06-19", "2015-10-30", "2016-03-01", "2016-05-24", 
                                         "2016-08-03", "2017-08-18", "2017-08-18", "2018-02-01", "2018-05-07", "2018-08-09", 
                                         "2019-01-31", "2019-03-22", "2019-05-16", "2019-11-04", "2019-11-04", "2020-02-06",
                                         "2020-05-28", "2020-08-25", "2020-12-14")),
                    end_dt   = as.Date(c("2017-07-24", "2015-05-04", "2015-08-27", "2015-11-19", "2016-03-21", "2016-06-09", 
                                         "2017-07-18", "2019-02-21", "2018-01-23", "2018-04-25", "2018-07-29", "2019-01-15", 
                                         "2019-04-24", "2019-09-13", "2019-10-13", "2020-12-23", "2020-01-26", "2020-04-29", 
                                         "2020-08-19", "2020-11-16", "2021-03-07")))


#split into two frames
data = split(data,by="type")

# key the second frame
setkey(data[[2]], start_dt, end_dt)

# create the rows that have overlaps
overlap = foverlaps(data[[1]],data[[2]], type="any", nomatch=0)

# get the overlapping time periods
overlap[, .(start_dt = max(start_dt,i.start_dt), end_dt=min(end_dt,i.end_dt)), by=1:nrow(overlap)][,type:=3]



library(data.table)
alldates <- data.table(date = seq(min(data$start_dt), max(data$end_dt), by = "day"))
data[alldates, on = .(start_dt <= date, end_dt >= date)] %>%
  .[, .N, by = .(start_dt, type) ] %>%
  .[ !is.na(type), ] %>%
  dcast(start_dt ~ type, value.var = "N") %>%
  .[, r := do.call(rleid, .SD), .SDcols = setdiff(colnames(.), "start_dt") ] %>%
  .[, .(type = fcase(is.na(`1`[1]), "2", is.na(`2`[1]), "1", TRUE, "3"),
        start_dt = min(start_dt), end_dt = max(start_dt)), by = r ]





a<-"2015-12-13 09:00:00"           
b<-"2015-12-13 12:00:00"
c<-interval(a,b)

d<-"2015-12-13 09:00:00"           
e<-"2015-12-13 10:00:00"
f<-interval(d,e)
h<-lubridate::intersect(c,f)
lubridate::union(c,f)
h
