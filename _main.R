library(angstromATE)
pathData = "/Users/gredigcsulb/Library/CloudStorage/OneDrive-SharedLibraries-csulb/Gredig Molecular Thin Film Lab - Erin - Erin/RAW/ATE"
fileList = dir(pathData, pattern='csv$', recursive=TRUE)
fileList = file.path(pathData, fileList)


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

