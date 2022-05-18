```{r setup}
library(here)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
```

### LAI
```{r topography to points}
library(raster)
plots <-st_read("C:/Users/Admin/Documents/ArcGIS/Projects/Sedgewick_SHIFT/sedgewick_plots.shp")
slope <-raster("C:/Users/Admin/Documents/ArcGIS/Projects/Sedgewick_SHIFT/Slope_Sedgwi1_1.tif")
aspect <-raster("C:/Users/Admin/Documents/ArcGIS/Projects/Sedgewick_SHIFT/Aspect_1.tif")
elevation <-raster("C:/Users/Admin/Documents/ArcGIS/Projects/Sedgewick_SHIFT/DEM  3 meters.tif")

plots_topo <-plots %>%
  mutate(aspect = raster::extract(aspect, plots),
         slope = raster::extract(slope, plots),
         elevation = raster::extract(elevation, plots)) %>%
  mutate(aspectcos = case_when(aspect<0 ~ 0,
                               aspect==270.000000 
                               |aspect==90.000000 ~ 0,
                               aspect>-1 & aspect<361 
                               ~ cos(aspect*pi/180))) %>%
  mutate(tree_group = case_when(Tree_id>2019 & Tree_id<2032 ~ 1,
                                (Tree_id>2008 & Tree_id<2012) 
                                |(Tree_id>2300 & Tree_id<2385) ~ 2,
                                (Tree_id>2000 & Tree_id<2009) 
                                |(Tree_id>2011 & Tree_id<2017) 
                                |(Tree_id==1478) ~ 3,
                                Tree_id>2085 & Tree_id<2094 ~4)) %>%
  rename(solarrad=T0)
detach("package:raster")

LAI_all <-as.data.frame(read.csv(here("all_trees_april_CSV.csv")))
LAI_ESW <-left_join(LAI_all, plots_topo, by=c("Tree.Num"="TreeNum")) %>%
  select(Tree_id, Tree.Num, Direction, Leaf.Area.Index..LAI., 
         aspectcos, solarrad, slope, elevation, Date.and.Time) %>%
  filter(Direction!="O", Direction!="Round", Direction!="N") %>%
  rename(LAI=Leaf.Area.Index..LAI.) %>%
  rename(TreeFactor=Tree.Num) %>%
  drop_na(Tree_id, LAI, Direction)
LAI_ESW$LAI <-as.numeric(LAI_ESW$LAI)
LAI_ESW$Direction <-as.factor(LAI_ESW$Direction)
LAI_ESW$TreeFactor <-as.factor(LAI_ESW$TreeFactor)

LAI_Avg <-group_by(LAI_ESW, TreeFactor) %>%
  summarize(meanLAI=mean(LAI)) %>%
  left_join(plots_topo, by=c("TreeFactor"="TreeNum")) %>%
  select(TreeFactor, meanLAI, elevation, slope, aspectcos, solarrad, tree_group) %>%
  mutate(tree_group = as.factor(tree_group))

mutate(LAI_Avg, TreeFactor=as.character(TreeFactor)) %>%
write.csv(here("LAI_Avg.csv"))
```

```{r exploring LAI}
# all LAI measurements distribution
hist(LAI_ESW$LAI)

# mean E/W/S LAI distribution
hist(LAI_Avg$meanLAI)

# E/W/S LAI spread for each tree and in groups
ggplot(LAI_ESW, aes(LAI, TreeFactor, col=Direction)) + geom_point() +
  theme(text=element_text(size=7))

ggplot(LAI_Avg,aes(x=tree_group,y=meanLAI)) + geom_boxplot(alpha=0.2) + geom_jitter(width=0.25)


# LAI vs slope/aspect/elevation
ggplot(LAI_Avg, aes(slope, meanLAI))+geom_point()

ggplot(LAI_Avg, aes(aspectcos, meanLAI, color=tree_group))+geom_point()

ggplot(LAI_Avg, aes(elevation, meanLAI, color=tree_group))+geom_point()

ggplot(LAI_Avg, aes(solarrad, meanLAI, color=tree_group))+geom_point()

# Linear Models
LAI_lm1 <-lm(meanLAI~elevation, data=LAI_Avg) #R2=0.185
LAI_mlm1 <-lmer(meanLAI~elevation+slope+solarrad+(1|tree_group),
                data=LAI_Avg) #is singular

summary(LAI_lm1)
```

### Water Content
```{r loading in data}
turgid_dry_wt <-as.data.frame(read.csv(here("RWC-4-25.csv"))) %>%
  dplyr::select(ï..Date:dry_wt_g)

fresh_dry_wt <-as.data.frame(read.csv(here("LFM-4-25.csv"))) %>%
  select(ï..Date:dry_wt_g)
fresh_dry_wt$Tree_id <- as.integer(fresh_dry_wt$Tree_id)

## WC taken from first leaf of each tree measured 3/15 
turgid_315 <-rename(turgid_dry_wt, Date=ï..Date) %>%
  filter(Type=="leaf") %>%
  mutate(turgid_wc=(wet_wt_g-dry_wt_g)/wet_wt_g)
fresh_315 <-rename(fresh_dry_wt, Date=ï..Date) %>%
  filter(Type=="leaves", 
           Time=="PD", Age=="new") %>%
  mutate(fresh_wc=(wet_wt_g-dry_wt_g)/wet_wt_g)

hist(fresh_315$fresh_wc)
hist(turgid_315$turgid_wc)

```
### Relating LAI & WC


