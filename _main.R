library(angstromATE)
pathData = "/Users/gredigcsulb/Library/CloudStorage/OneDrive-SharedLibraries-csulb/Gredig Molecular Thin Film Lab - Erin - Erin/RAW/ATE"

pathData = "/Users/gredigcsulb/Library/CloudStorage/OneDrive-csulb/RAW Docs/ATE_Log/Dep"
fileList = dir(pathData, pattern='xml$', recursive=TRUE)
fileList = file.path(pathData, fileList)
basename(fileList)

for (f in fileList) {
  q = ATE.status(f)
  print(q$msg)
}


library(xml2)
library(dplyr)
for (i in 50) {
  f = fileList[i]
  df <- xmlParse(f)
  x <- xmlToList(df)
  xPhase <- x$Layers$RecipeLayer
  xp = unlist(xPhase)
  a <- xp[grepl('RecipeActionBaseClass.ActionName',names(xp))]
  print(a)
}


f = fileList[48]
f
grepl('_Status_',f)
df <- xmlParse(f)
x <- xmlToList(df)
xp <- x$RecipeElements
9*34
df1 <- as.data.frame(do.call(rbind, xp))


# d = ATE.import(f)
m1 = data.frame()
for (f in fileList[10:20]) {
  cat(".")
  m = ATE.info(f)
  if (length(m)>0) {
    m2 = as.data.frame(do.call(cbind, m))
    m1 = rbind(m1, m2)
  }
}
cat("\n")

head(d)
head(m)

r = data.frame()
r2 = data.frame()
j = 0
f = fileList[1]
for(f in fileList) {
  #file.exists(f)
  j=j+1

  d = ATE.import(f)
  m1 <- which(d$SubstrateShutterOpen==TRUE)
  if (length(m1)==0) next
  d1 <- d[m1,]
  d2 = data.frame(
    time = conv2seconds(d1$Elapsed.Time),
    thickness = d1$Radak.3.ActualThickness,
    T.CuPc = d1$Radak.3.ActualTemperature,
    Tdep = d1$Substrate.Heater.ActualTemperature,
    pressure = d1$ChamberPressure,
    tooling.factor = d1$Physical.Sensor.2.ToolingFactor
  )
  d2$time = d2$time - min(d2$time)
  d2$no = j
  r = rbind(r, d2)

  q = ATE.info(f)
  r2 = rbind(r2, q)
  cat(".")
}
cat("\n")

library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)

r2 %>% select(-c("material","filename","date.dep")) %>%
  filter(!is.na(time.dep)) %>%
  cor() -> res
corrplot(res,type = "upper",order = "hclust")

r1 <- r %>% filter(no != 3)

r1 %>%
  mutate(TF = factor(tooling.factor)) %>%
  ggplot(aes(time, thickness, col=TF)) +
  geom_point() +
  theme_bw()

r1 %>%
  mutate(TF = factor(tooling.factor)) %>%
  ggplot(aes(time, T.CuPc, col=TF)) +
  geom_point() +
  theme_bw()

