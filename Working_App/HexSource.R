library(data.table)
library(shiny)

plugins <- {list(vgplugin = 
                   htmltools::htmlDependency(
                     name = "leaflet.vectorgrid",
                     version = "1.3.0",
                     src = "htmlwidgets",
                     script = "lfx-vgrid-prod.js"
                   ),
                 sliderplugin = htmltools::htmlDependency(
                   name = "leaflet.slider",
                   version = "1.0.0",
                   stylesheet = "lfx-slider.css",
                   src = "htmlwidgets",
                   script = "lfx-slider.js"
                 )
)
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

addHexMap <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map
}

jscode_hex <- paste0('window.LeafletWidget.methods.addHexTiles1 = function(server,layerID) {
      
      var map = this;
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                stroke: true,
                weight: 0.5,
                fillColor: "#919191",
                color: "#919191",
                fill: true,
                fillOpacity: 1,
                opacity: 1
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(server,
        vectorTileOptions("bec_hex", layerID, true,
                          "tilePane", "ID", "ID")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_hex", "Hex");
      
      Shiny.addCustomMessageHandler("newCol1",function(dat){
        var hexID = dat["NewID"];
        var Cols = dat["Col"];

        hexID.forEach((ID,i) => {
          let styleNew = {
            weight: 0.5,
            color: Cols[i],
            fillColor: Cols[i],
            fillOpacity: 1,
            fill: true
          };
          subzLayer.setFeatureStyle(ID, styleNew);
        });
      });
      
      Shiny.addCustomMessageHandler("resetMap",function(hexID){
        let styleNew = {
            stroke: true,
                weight: 0.5,
                fillColor: "#919191",
                color: "#919191",
                fill: true,
                fillOpacity: 1,
                opacity: 1
          };
        hexID.forEach((ID,i) => {
          subzLayer.setFeatureStyle(ID, styleNew);
        });
      });

    };')

leafletjs_hex <-  tags$head(
  tags$script(HTML(
    jscode_hex
  ))
)

# window.LeafletWidget.methods.addHexTiles2 = function(IDs,Colour,server,layerID) {
#   var subzoneColors = {};
#   IDs.forEach((id,i) => {
#     const col = Colour[i];
#     subzoneColors[id] = col;
#   });
#   
#   var map = this;
#   console.log(server);
#   console.log(layerID);
#   var vectorTileOptions=function(layerName, layerId, activ,
#                                  lfPane, colorMap, prop, id) {
#     return {
#       vectorTileLayerName: layerName,
#       interactive: true, // makes it able to trigger js events like click
#       vectorTileLayerStyles: {
#         [layerId]: function(properties, zoom) {
#           return {
#             weight: 0,
#             fillColor: colorMap[properties[prop]],
#             fill: true,
#             fillOpacity: 1
#           }
#         }
#       },
#       pane : lfPane,
#       getFeatureId: function(f) {
#         return f.properties[id];
#       }
#     }
#     
#   };
#   
#   var subzLayer = L.vectorGrid.protobuf(server,
#                                         vectorTileOptions("bec_hex", layerID, true,
#                                                           "tilePane", subzoneColors, "ID", "ID")
#   )
#   console.log(subzLayer);
#   this.layerManager.addLayer(subzLayer, "tile", "bec_hex", "Hex");
#   
#   Shiny.addCustomMessageHandler("newCol2",function(dat){
#     var hexID = dat["NewID"];
#     var Cols = dat["Col"];
#     
#     hexID.forEach((ID,i) => {
#       let styleNew = {
#         weight: 0,
#         fillColor: Cols[i],
#         fillOpacity: 1,
#         fill: true
#       };
#       subzLayer.setFeatureStyle(ID, styleNew);
#     });
#   });
#   
# };
# 
# window.LeafletWidget.methods.addHexTiles3 = function(IDs,Colour,server,layerID) {
#   var subzoneColors = {};
#   IDs.forEach((id,i) => {
#     const col = Colour[i];
#     subzoneColors[id] = col;
#   });
#   
#   var map = this;
#   console.log(server);
#   console.log(layerID);
#   var vectorTileOptions=function(layerName, layerId, activ,
#                                  lfPane, colorMap, prop, id) {
#     return {
#       vectorTileLayerName: layerName,
#       interactive: true, // makes it able to trigger js events like click
#       vectorTileLayerStyles: {
#         [layerId]: function(properties, zoom) {
#           return {
#             weight: 0,
#             fillColor: colorMap[properties[prop]],
#             fill: true,
#             fillOpacity: 1
#           }
#         }
#       },
#       pane : lfPane,
#       getFeatureId: function(f) {
#         return f.properties[id];
#       }
#     }
#     
#   };
#   
#   var subzLayer = L.vectorGrid.protobuf(server,
#                                         vectorTileOptions("bec_hex", layerID, true,
#                                                           "tilePane", subzoneColors, "ID", "ID")
#   )
#   console.log(subzLayer);
#   this.layerManager.addLayer(subzLayer, "tile", "bec_hex", "Hex");
#   
#   Shiny.addCustomMessageHandler("newCol3",function(dat){
#     var hexID = dat["NewID"];
#     var Cols = dat["Col"];
#     
#     hexID.forEach((ID,i) => {
#       let styleNew = {
#         weight: 0,
#         fillColor: Cols[i],
#         fillOpacity: 1,
#         fill: true
#       };
#       subzLayer.setFeatureStyle(ID, styleNew);
#     });
#   });
#   
# };