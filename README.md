# sujuiko-network-prepare

A simple Shiny app that prepares network data for [sujuiko](https://github.com/datarttu/sujuikoDB).

- Digiroad `dr_linkki` street links are read from a Geopackage and transformed into `node.csv` and `link.csv`.
- Jore route exports containing route variants and stops used by them are read from one or more `.txt` files and transformed into `stop.csv`, `route_version.csv` and `stop_on_route.csv`.

## Deployment

To deploy the Shiny app and expose it through port 3838:

```
git clone https://github.com/datarttu/sujuiko-network-prepare.git
cd sujuiko-network-prepare
docker build -t sujuiko-network-prepare-app .
docker run --rm -d -p 3838:3838 sujuiko-network-prepare-app
```

## TO DO

### link, node (from Digiroad)

- [x] Upload single .gpkg file
- [x] Select link layer from available layers
- [x] Validate required fields and geometry type
- [x] Transform links
- [x] Create nodes
- [x] Report counts & print heads of link and node
- [x] Download link.csv and node.csv

### stop, route_version, stop_on_route (from Jore files)

- [x] Upload multiple .txt files
- [x] Validate single Jore file: date range from name & txt contents
- [x] Transform single Jore file into stop, route_version, stop_on_route
- [x] Combine files
- [x] Combine stops into unique ones
- [ ] Validate combined route_version and stop_on_route: no overlaps by route_id and validity periods
- [x] Report counts & print heads of stop, route_version and stop_on_route
- [x] Download stop.csv, route_version.csv and stop_on_route.csv
