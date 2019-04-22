# загрузка пакетов
library('R.utils')               # gunzip() для распаковки архивов 
library('sp')                    # функция spplot()
library('ggplot2')               # функция ggplot()
library('RColorBrewer')          # цветовые палитры
require('rgdal')                 # функция readOGR()
library('broom')                 # функция tidy()
require('dplyr')                 # функция join()
library('scales')                # функция pretty_breaks()
library('mapproj')
install.packages('gpclib', type = 'source')
library('gpclib')
library('maptools')


gpclibPermit()

ShapeFileURL <- "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_RUS_shp.zip"
if (!file.exists('./data')) dir.create('./data')
if (!file.exists('./data/gadm36_RUS_shp.zip')) {
  download.file(ShapeFileURL, destfile = './data/gadm36_RUS_shp.zip')
}
# распаковать архив
unzip('./data/gadm36_RUS_shp.zip', exdir = './data/gadm36_RUS_shp')
# посмотреть список файлов распакованного архива
dir('./data/gadm36_RUS_shp')

# прочитать данные уровней 0, 1, 2
Regions0 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_0.shp")
Regions1 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_1.shp")
Regions2 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_2.shp")

# делаем фактор из имён областей (т.е. нумеруем их)
Regions1@data$NAME_1 <- as.factor(Regions1@data$NAME_1)

# загружаем статистику с показателями по регионам
fileURL <- 'https://raw.githubusercontent.com/KseniaKubirikova/pract4/master/laba4_prom.csv'
stat.Regions <- read.csv2(fileURL, stringsAsFactors = F)
stat.Regions$Resourse <- as.numeric(stat.Regions$Resourse)

# вносим данные в файл карты
Regions1@data <- merge(Regions1@data, stat.Regions,
                       by.x = 'NAME_1', by.y = 'Region')

# задаём палитру
mypalette <- colorRampPalette(c('whitesmoke', 'green3'))


spplot(Regions1, 'Resourse', main = 'Добыча полезных ископаемых за 2017 год (млн.руб)',
       col.regions = mypalette(10), # цветовая шкала
       # (10 градаций)
       col = 'green4', # цвет контурных линий
       par.settings = list(axis.line = list(col = NA)) # без
       # осей
)

rm(Regions1, stat.Regions)


# Количество населения в Ивановской области на 1 января 2018
gpclibPermit()

Regions2@data$NAME_2 <- as.factor(Regions2@data$NAME_2)


fileURL <- 'https://raw.githubusercontent.com/KseniaKubirikova/pract4/master/lab4_naselie.csv'
stat.Regions <- read.csv2(fileURL, stringsAsFactors = F)
stat.Regions$Naselenie_2017 <- as.numeric(stat.Regions$Naselenie_2017)
Regions <- readOGR(dsn = './data/gadm36_RUS_shp', # папка
                   layer = 'gadm36_RUS_2') # уровень 
Regions@data$id <- Regions@data$NAME_2
Regions <- Regions[grepl('^RU.AD.', Regions$HASC_2), ]
Regions.points <- fortify(Regions, region = 'id')
Regions.df <- merge(Regions.points, Regions@data, by = 'id')
stat.Regions$id <- stat.Regions$Region
Regions.df <- merge(Regions.df,
                    stat.Regions[, c('id',
                                     'Naselenie_2017')],
                    by = 'id')

centroids.df <- as.data.frame(coordinates(Regions))
centroids.df$id <- Regions@data$id
colnames(centroids.df) <- c('long', 'lat', 'id')


gp <- ggplot() +
  geom_polygon(data = Regions.df,
               aes(long, lat, group = group,
                   fill = Naselenie_2017)) +
  geom_path(data = Regions.df,
            aes(long, lat, group = group),
            color = 'coral4') +
  coord_map(projection = 'gilbert') +
  scale_fill_distiller(palette = 'OrRd',
                       direction = 1,
                       breaks = pretty_breaks(n = 5)) +
  labs(x = 'Долгота', y = 'Широта',
       title = "Количество населения в Республике Адыгея на 1 января 2018") +
  geom_text(data = centroids.df,
            aes(long, lat, label = id))
gp
