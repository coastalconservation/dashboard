<h2 align="center"> 
  
<img src="https://github.com/coastalconservation/.github/blob/main/photos/cc-hexlogo-lowquality.png?raw=true" alt="Coastal Conservation Capstone group logo: hex sticker with rocky coastline and lighthouse illustration" width="300">

<h1 align="center">

California Ranges of Intertidal Species Portal (CRISP)

## Product Description
The interactive web application is a comprehensive priority monitoring assessment of rocky intertidal species, with range edges near Point Conception, at risk of suitable habitat loss due to changing environmental conditions. The assessment compiles analyses of their habitat ranges, contemporary range shifts over time, and projected habitat suitability utilizing MARINe biodiversity survey data. The application is hosted on the Nature Conservancy's (TNC) Geospatial Hub and is intended to be primarily utilized by internal staff and partners, who may implement the product for priority monitoring, grant proposals, and public education purposes. It addresses the need for open-source, reproducible science that is user-friendly and accessible to a broad audience. Overall, the application provides a foundational framework for further analyses to investigate the ecological and biological factors that influence rocky intertidal species near the  Jack and Laura Dangermond Preserve.

## Repository Structure
```bash
dashboard
├── crisp
│   ├── global.R
│   ├── server.R
│   ├── ui.R
│   ├── data
│   │   ├── analyses_results
│   │   ├── spatial_data
│   │   ├── species_info
│   │   └── species_model_rasters
│   ├── text
│   │   └── text.md files
│   └── www
│       ├── carousel_images
│       ├── diagrams
│       └──  logos
│
├── .gitignore
│
├── LICENSE
│
├── README.md
│
├── dashboard.Rproj
│
└── session_info.txt

```

## Usage

Welcome to the CRISP (Coastal Range Interactive Shifts Platform) dashboard, an interactive tool designed to explore range dynamics of intertidal species along the California coast, with a special focus on the ecologically significant Point Conception region.

The sidebar on the left guides you through the different components of the analysis. Each tab offers a unique lens into the spatial and temporal dynamics of species range shifts:

- `Home`: An introduction to the dashboard, including navigation tips and a brief overview of the project.
- `Background`: Context on species range shifts and the importance of the Point Conception biogeographic boundary.
- `Range Edges`: Explore how the northern and southern boundaries (range edges) of intertidal species are identified across the California coastline.
- `Contemporary Range Shifts`: Analyze how species’ core ranges have shifted from 2000 to 2024, focusing on species whose range edges are near Point Conception.
- `Projected Shifts`: Visualize habitat suitability under both current and future climate scenarios to assess potential range changes.
- `Priority Monitoring Assessment`: Identify species that are most likely to undergo range expansions or contractions.
- `Data and Limitations`: Learn about the data sources, methods, and key assumptions behind the analysis, as well as known limitations.
- `Acknowledgements`: Credits to the project team, collaborators, and data providers.

Below is more information about how to use the analysis tabs: 

### Range Edges

![](https://github.com/coastalconservation/.github/blob/main/photos/dashboard1.png)

Two leaflet maps of California’s coastline divide and group survey sites into 100 kilometer coastal segments, illustrating the number of rocky intertidal species with northern and southern range edges in each segment. A color gradient legend is utilized to indicate the number of species with range edges in each segment, highlighting Point Conception as a biodiversity hotspot. Next to each map, an interactive data table displays the scientific name, common name, latitude, and image of rocky intertidal species that have a range edge when a specific coastal segment is clicked. At the bottom, a static map zoomed in on the northern and southern Point Conception coastal segments emphasizes key takeaways about the species residing near the biogeographic barrier.

### Contemporary Range Shifts

![](https://github.com/coastalconservation/.github/blob/main/photos/dashboard2.png)

At the top, a dropdown widget allows users to select a species or species group. Below, a range extent plot illustrates the contemporary range shift of the species or species group every five years, with Point Conception highlighted by a horizontal red dashed line. The range extent plot examines species distribution over time by showing where 95% of observations are located near the northern range edge and 5% near the southern range edge, highlighting shifts in range boundaries around Point Conception. For geographic context, users can hover over the y-axis to uncover coastal landmarks every 200 kilometers, along with a static map on the right that displays the California coastline with distances in kilometers from the southern border. Below the plots, there is a summary of the key takeaways regarding intertidal species range extent movements.

### Projected Shifts 

![](https://github.com/coastalconservation/.github/blob/main/photos/dashboard3.png)

On the top left, a dropdown widget allows users to select a species, and the corresponding habitat suitability maps will appear. The first row illustrates the percent change in habitat suitability between the present and 2050, highlighting areas of habitat loss or gain. The second row shows the current habitat suitability based on existing environmental conditions, generated from raster data provided by Bio-ORACLE and biodiversity survey data pertaining to rocky intertidal species found at their northern range edges. The second row also shows the projected habitat suitability for the year 2050 under a moderate climate scenario (SSP2-4.5). A color gradient legend is used to portray differences in measurements, highlighting possible shifts in suitable habitat for rocky intertidal species.

### Priority Monitoring Assessment

A dropdown widget allows users to select a monitoring priority level based on an additive assessment of the three previous analyses. An interactive table will update accordingly, displaying the species name, common name, cumulative score, and an image associated with the selected priority level. Higher scores (e.g., decreasing population combined with declining suitable habitat near Point Conception) represent higher monitoring priority.



## Data 

All data used to support the interactive web application is found within the `data` folder in this repository. It is an aggregation of processed data generated by analysis done in the [coastal_species_analysis](https://github.com/coastalconservation/coastal_species_analysis) repository within this organization. 


## Performance and Limitations

The data used for the dashboard analyses are static, covering the period from 2000 to 2024. No future updates or maintenance to the dataset are planned; however, the dashboard is intended to serve as a framework that can be adapted and applied to data from 2025 onward. To optimize performance and minimize computation time, abundance trend plots, habitat suitability maps, and performance metric visualizations will be pre-generated and uploaded as static images.


## Authors and Contributors 

#### Authors 

- Amanda Overbye  [GitHub](https://github.com/Aoverbye) | [Website](https://aoverbye.github.io/) | [LinkedIn](https://www.linkedin.com/in/amanda-overbye-3a6364161/) 
- Ian Morris-Sibaja  [GitHub](https://github.com/imsibaja) | [Website](https://imsibaja.github.io/) | [LinkedIn](https://www.linkedin.com/in/imsibaja/) 
- Jordan Sibley  [GitHib](https://github.com/jordancsibley) | [Website](https://jordancsibley.github.io/) | [LinkedIn](https://www.linkedin.com/in/jordancsibley/)  
- Matteo Torres  [GitHub](https://github.com/matteo-torres) | [Website](https://matteo-torres.github.io/) | [LinkedIn](https://www.linkedin.com/in/matteo-torres-876a62234/)

#### Client 

Dr. Erica Nielsen  | Anthony W. LaFetra Point Conception Research Fellow | The Nature Conservancy in California | erica.nielsen@tnc.org

#### Advisor 

Dr. Bruce Kendall | Bren School Professor; Associate Dean | [Bren page](https://bren.ucsb.edu/people/bruce-kendall)
