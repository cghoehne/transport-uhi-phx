myDir = 'X:/GitHub Projects/transport-uhi-phx/data/aster/processed/'

pipe = QgsRasterPipe()

for layer in iface.mapCanvas().layers():
   extent = layer.extent()
   width, height = layer.width(), layer.height()
   #renderer = layer.renderer()
   provider=layer.dataProvider()
   crs = layer.crs().toWkt() 
   pipe.set(provider.clone())
   #pipe.set(renderer.clone())
   file_writer = QgsRasterFileWriter(myDir + layer.name() + ".tif")
   file_writer.writeRaster(pipe,
                        width,
                        height,
                        extent,
                        layer.crs())
