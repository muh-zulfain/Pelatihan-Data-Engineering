library(tidyverse)
library(writexl)
library(readxl)
transaction<-read_xlsx("D:/Pelatihan Data Enginering/membership.xlsx",
                    sheet = "transaction")
head(transaction)
membership<-read_xlsx("D:/Pelatihan Data Enginering/membership.xlsx",
                     sheet = "membership")
head(membership)
product<-read_xlsx("D:/Pelatihan Data Enginering/membership.xlsx",
                     sheet = "product")
head(product)
subproduct<-read_xlsx("D:/Pelatihan Data Enginering/membership.xlsx",
                  sheet = "subproduct")
head(subproduct)
library(lubridate)
# Filter ------------------------------------------------------------------

trx_tbl <- filter(transaction, Qty <= 10)
trx_tbl
filter(product, str_detect(ProductName, "A"))

filter(transaction, Qty >=5 & Qty <=9)

filter(transaction, between(Qty,5,9))
#filter(transaction, Qty_low <=Qty & Qty<=Qty_high)
filter(transaction, Qty %in% c(2,5,8))

filter(membership, MemberID == 433907)

filter(transaction, Qty<=10 & as_date(TransactionDatetime) >= as_date("2019-07-01"))

#Pip operator
transaction%>%
  filter(Qty<=10)%>%
  filter(as_date(TransactionDatetime)>=as_date("2019-07-01"))

# Slice (memilih menurut baris)

trx_tbl %>% 
  slice(1:100)

trx_tbl %>% 
  slice(c(1,4,5,2,10))

trx_tbl %>% 
  slice_max(TransactionDatetime)

# Arrange (mengurutkan)

trx_tbl %>% 
  filter(month(TransactionDatetime) == 1) %>% 
  arrange(TransactionDatetime)

trx_tbl %>% 
  filter(month(TransactionDatetime) == 1) %>% 
  arrange(desc(TransactionDatetime))

# Distinct (menghapus data duplikat)

trx_tbl %>% 
  distinct(FKMemberID)

trx_tbl %>% 
  distinct(FKMemberID, .keep_all = TRUE)

trx_tbl %>%
  distinct(FKMemberID, Qty, .keep_all = TRUE)

# Select (menselek data berdasarkan variabel)
names(trx_tbl)
trx_tbl %>% 
  select(FKMemberID, Qty, PricePerUnit, TransactionDatetime)

trx_tbl %>% 
  select(-c(ProductID, SubProductID, TransactionDatetime))

# Mutate (mengganti data kolom atau membuat data kategori)

membership <- membership %>% 
  mutate(JoinDate = as_date(JoinDate), 
         YoungestDepDOB = as_date(YoungestDepDOB))
membership

membership %>%
  mutate(DepCategory=if_else(Dependant<=2, "Sedikit", "Banyak"))

membership%>%
  mutate(DepCategory = case_when(Dependant<=2 ~"sedikit",
                                 Dependant<=3 ~ "Cukup",
                                 TRUE ~ "Banyak"))

# Transmute ---------------------------------------------------------------

membership%>%
  transmute(memberid=MemberID,
    DepCategory = case_when(Dependant<=2 ~"sedikit",
                                 Dependant<=3 ~ "Cukup",
                                 TRUE ~ "Banyak"))

membership %>% 
  transmute(MemberID, 
            Tenure = as.duration(JoinDate %--% as_date("2019-12-31"))/dmonths(1))

trx_tbl<-trx_tbl%>%
  mutate(Monetary = Qty*PricePerUnit,
         TransactionDate = as_date(TransactionDatetime))
view(trx_tbl)

# Rename ------------------------------------------------------------------

membership %>% 
  rename(JumlahAnak = Dependant,
         memberid=MemberID)

# Grouping

trx_tbl%>%
  group_by(FKMemberID)%>%
  summarise(totalmonetary = sum(Monetary),
            lasttrx=max(TransactionDatetime))

# Inner Join --------------------------------------------------------------

trx_tbl %>% 
  inner_join(subproduct, by = "SubProductID")

band_members
band_instruments

band_members %>% 
  inner_join(band_instruments, by = "name")

# Full (Outer) Join -------------------------------------------------------

band_members %>% 
  full_join(band_instruments, by = "name")

# Left Join ---------------------------------------------------------------

band_members %>% 
  left_join(band_instruments, by = "name")

# Right Join --------------------------------------------------------------

band_members %>% 
  right_join(band_instruments, by = "name")

# Anti Join ---------------------------------------------------------------

band_members %>% 
  anti_join(band_instruments, by = "name")

# Tenure ------------------------------------------------------------------

member <- membership %>% 
  mutate(YoungestAge = as.duration(YoungestDepDOB %--% as_date("2019-12-31"))/dyears(1), 
         Tenure = as.duration(JoinDate %--% as_date("2019-12-31"))/dmonths(1))
member

#Recency -----------------------------------------------------------------
  
  recency <- trx_tbl %>% 
  group_by(FKMemberID) %>% 
  summarise(LastTrx = max(TransactionDatetime)) %>% 
  mutate(LastTrx = as_date(LastTrx), 
         Recency = as.duration(LastTrx %--% as_date("2019-12-31"))/ddays(1))

# Frequency ---------------------------------------------------------------

freq <- trx_tbl %>% 
  distinct(FKMemberID, TransactionDatetime) %>% 
  count(FKMemberID, name = "Frequency")

# Monetary & Consumption --------------------------------------------------

monetary <- trx_tbl %>% 
  left_join(subproduct, by = "SubProductID") %>% 
  group_by(FKMemberID) %>% 
  summarise(Monetary = sum(Qty*PricePerUnit), 
            Consumption = sum(Qty*Weight))
monetary

# Product -----------------------------------------------------------------

product_trx <- trx_tbl %>% 
  left_join(product, by = "ProductID") %>% 
  mutate(ProductName = str_remove_all(ProductName, "Product ")) %>% 
  select(FKMemberID, Qty, ProductName) %>% 
  group_by(FKMemberID, ProductName) %>% 
  summarise(TotalQty = sum(Qty)) %>% 
  pivot_wider(id_cols = c(FKMemberID, ProductName), names_from = ProductName, values_from = TotalQty, values_fill = 0)

# Member bertransaksi 6 bulan terakhir ------------------------------------

last6mo_trx <- trx_tbl %>% 
  filter(between(TransactionDate, as_date("2019-07-01"), as_date("2019-12-31"))) %>% 
  distinct(FKMemberID) %>% 
  mutate(Last6Mo = 1)

# Data Akhir --------------------------------------------------------------

final_dt <- member %>% 
  inner_join(recency, by = c("MemberID" = "FKMemberID")) %>% 
  inner_join(freq, by = c("MemberID" = "FKMemberID")) %>% 
  inner_join(monetary, by = c("MemberID" = "FKMemberID")) %>% 
  inner_join(product_trx, by = c("MemberID" = "FKMemberID")) %>% 
  left_join(last6mo_trx, by = c("MemberID" = "FKMemberID")) %>% 
  select(MemberID, Dependant, YoungestAge, Tenure, Recency, Frequency, Consumption, Monetary, Last6Mo, A, B) %>% 
  mutate(Last6Mo = case_when(is.na(Last6Mo) ~ 0, 
                             TRUE ~ 1))

View(final_dt)

# Ekspor Data -------------------------------------------------------------

library(writexl)

final_dt %>% 
  write_xlsx("D:/Pelatihan Data Enginering/final data prep.xlsx")
