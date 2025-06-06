<h2 align="center"> 
  
<img src="https://github.com/coastalconservation/.github/blob/main/photos/cc-hexlogo-lowquality.png?raw=true" alt="Coastal Conservation Capstone group logo: hex sticker with rocky coastline and lighthouse illustration" width="300">

<h1 align="center">

California Ranges of Intertidal Species Portal (CRISP)

## Table of Contents 
[Product Description](#product-description)

[Repository Structure](#repository-structure)

[Usage](#usage)

[Data](#data)

[Performance and Limitations](#performance-and-limitations) 

[Authors and Contributors](#authors-and-contributors) 




## Product Description
Welcome to the [California Range of Intertidal Species Portal (CRISP)](https://tnc-dangermond.shinyapps.io/crisp/) dashboard. The interactive web application is a comprehensive priority monitoring assessment of rocky intertidal species, with range edges near Point Conception, at risk of suitable habitat loss due to changing environmental conditions. The assessment compiles analyses of their habitat ranges, contemporary range shifts over time, and projected habitat suitability utilizing MARINe coastal biodiversity survey data. The application is hosted on The Nature Conservancy's (TNC) Dangermond Preserve Geospatial Hub and is intended to be primarily utilized by internal staff and partners, who may implement the product for priority monitoring, grant proposals, and public education purposes. It addresses the need for open-source, reproducible science that is user-friendly and accessible to a broad audience. Overall, the application provides a foundational framework for further analyses to investigate the ecological and biological factors that influence rocky intertidal species near the  Jack and Laura Dangermond Preserve.

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

The analyses hosted on CRISP are organized chronologically into separate tabs that collectively form the priority monitoring assessment. Each tab offers a unique lens into further understanding the project’s scope:

- `Home`: An introduction to the dashboard, including navigation tips and a brief overview of the project.
- `Background`: Context on species range shifts and the importance of the Point Conception biogeographic boundary.
- `Range Edges`: Explore how the northern and southern boundaries (range edges) of intertidal species are identified across the California coastline.
- `Contemporary Range Shifts`: Analyze how species’ core ranges have shifted from 2000 to 2024, focusing on species whose range edges are near Point Conception.
- `Projected Shifts`: Visualize habitat suitability under both current and future climate scenarios to assess potential range changes.
- `Priority Monitoring Assessment`: Identify species that are most likely to undergo range expansions or contractions.
- `Data and Limitations`: Learn about the data sources, methods, and key assumptions behind the analysis, as well as known limitations.
- `Acknowledgements`: Credits to the project team, collaborators, and data providers.

The following sections describe the visualizations and results that can be explored through each analysis found on CRISP.

### Range Edges

![](https://github.com/coastalconservation/.github/blob/main/photos/dashboard1.png)

Two interactive Leaflet maps of California’s coastline divide and group MARINe survey sites into 100-kilometer coastal segments, illustrating the number of rocky intertidal species with northern and southern range edges in each segment. A color gradient legend is utilized to indicate the number of species with range edges in each segment, highlighting Point Conception as a biodiversity hotspot. Next to each map, an interactive data table displays the scientific name, common name, range edge latitude, and an image of each rocky intertidal species that reaches the limit of its geographic range when a specific coastal segment is selected. At the bottom, a static map zoomed in on the Northern and Southern Point Conception coastal segments is accompanied by key takeaways from the analysis about the intertidal species or species groups residing near the biogeographic barrier.

### Contemporary Range Shifts

![](https://github.com/coastalconservation/.github/blob/main/photos/dashboard2.png)

At the top, a dropdown widget enables users to select one of the 59 species or species groups previously defined as having range edges at Northern and Southern Point Conception. Below, an image of the selected species is displayed alongside a diagram of the analysis for visual context. The analysis showcases a range extent plot that illustrates the contemporary range shift of the selected species or species group every five years, highlighting Point Conception with a horizontal red dashed line. The plot functions as a dumbbell chart, depicting the movement of the northern and southern boundaries in kilometers along the California coastline. For geographic context, users hover over the y-axis of the plot to reveal coastal landmarks every 200 kilometers, as well as a reference map of the California coast that indicates distances from the southern border. At the bottom, a summary presents key takeaways from the analysis regarding the intertidal species or species groups that reside near the biogeographic barrier.

### Projected Shifts 

![](https://github.com/coastalconservation/.github/blob/main/photos/dashboard3.png)

At the top, a dropdown widget enables users to select one of the 59 species or species groups previously defined as having range edges at Northern and Southern Point Conception. Below, an image of the selected species is displayed alongside the habitat change detection map. This map is a product of the current and projected habitat suitability rasters, emphasizing areas along the coast where habitat is either gained or lost for the selected species or species group. Further down, users can view the current habitat suitability based on existing environmental conditions, utilizing raster data sourced from Bio-ORACLE and biodiversity coastal surveys. It also presents the projected habitat suitability for the year 2050 under a moderate climate scenario (SSP2-4.5), applying an ensemble model approach. A color gradient legend illustrates the differences in measurements, effectively highlighting potential shifts in suitable habitat for rocky intertidal species.  At the bottom, a summary presents key takeaways from the analysis regarding the intertidal species or species groups that reside near the biogeographic barrier.

### Priority Monitoring Assessment

A picker widget allows users to select a monitoring priority level, based on an additive assessment of the three previous analyses for both species that experience a range extension or contraction. An interactive data table updates accordingly, displaying the species name, common name, cumulative score, and an image associated with the selected priority level. Higher scores indicate a higher monitoring priority. At the bottom, a summary presents key takeaways from the analysis regarding the intertidal species or species groups that reside near the biogeographic barrier.

## Data 

All data used to support the interactive web application is found within the `data` folder in this repository. It is an aggregation of processed data generated by analysis done in the [coastal_species_analysis](https://github.com/coastalconservation/coastal_species_analysis) repository within this organization. 


## Performance and Limitations

The data used for the dashboard analyses are static, covering the period from 2000 to 2024. No future updates or maintenance for the dataset are planned; however, the dashboard is intended to serve as a framework that can be adapted and applied to data from 2025 onward. To optimize performance and minimize computation time, habitat suitability models will be pre-run and uploaded as raster images layered on top of a Leaflet map.

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
