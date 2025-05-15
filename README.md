# "Dexter" text mining analysis

## What is "Dexter" text mining analysis?
"Dexter" Text Mining Analysis is an R-based application that explores and analyzes the scripts of Season 1 (for more info, see the `Disclaimer` below) using a wide range of text mining packages 
- from basic ones like tm and tidytext, to more advanced tools like topicmodels and skmeans.
The project also features a local Shiny web app that showcases all the visualizations from the analysis, along with explanations and interpretations of key findings.

Learn the basics of text mining through:
- Word clouds and word frequency analysis  
- Sentiment analysis  
- Topic modeling (LDA)  
- Document segmentation using clustering methods (e.g., k-means, spherical k-means)  
- And other natural language processing techniques in R.

And one more thing: *tonight's the night*...

## Build Prerequisites
"Dexter" text mining analysis's build environment requires a couple of R packages in order to work, but before that make sure that you have downloaded and using:
- **R 3.6.0+** from [CRAN](https://cran.rstudio.com/) (located at the top, in the `Download and install R` tab);
- **RStudio Desktop** from [Posit](https://posit.co/download/rstudio-desktop/) (in the middle of the website you can find downloads for Windows, macOS, Ubuntu etc.);
- **Java Development Kit (JDK ≥ 8)** - required for `qdap` R package
> [!NOTE]
> Installing the JDK is not required to run the Shiny web app. If you're only interested in viewing the app's content, this step is optional.

## Installation
Clone the repository to your computer with:
```sh
git clone https://github.com/BorysSzk/dexter-text-mining.git
```
If you don't have Git installed, you can download the repository as a ZIP file from the repository page.

After cloning/downloading the repository, set your working directory in R to the project folder (`dexter-text-mining-main` folder) in R studio console, for example:
```sh
setwd(C:/Users/user1/Desktop/dexter-text-mining-main)
```

> [!IMPORTANT]
> **If you are only interested in running the Shiny web app (showcase of the analysis)** go to `Running "Dexter" text mining analysis` part down below

**If you are interested in the whole project, including R code of the text mining**:
Run the following command to install the packages:
```sh
source("install-packages.R")
```

## Running "Dexter" text mining analysis
1. Make sure your working directory is the project directory.
2. Load the project workspace, in the RStudio console with:
```sh
load('project.RData')
```
3. Next, start the web app, in the RStudio console run:
```sh
shiny::runApp()
```
The app will launch at [http://127.0.0.1:7216](http://127.0.0.1:7216).
> [!TIP]
> The app will open in the RStudio browser - at the top of the browser there is a button `Open in browser` which will open the web app in the default browser of your system

## Disclaimer
This project is a non-commercial, educational analysis of publicly available scripts from Season 1 of the TV series *Dexter*.  
The logo and audio used in this project are the intellectual property of their respective owners (Showtime, original creators, and composers).  
They are included here strictly for illustrative and educational purposes.

No copyright infringement is intended. This project is not affiliated with, endorsed by, or sponsored by Showtime or any related entities.

## Tech Stack
* R + RStudio
* Text mining: tm, tidytext, textstem, qdap, hunspell
* PDF/text input: pdftools, stringr, dplyr
* Visualisation: ggplot2, wordcloud, wordcloud2, dendextend
* NLP & ML: topicmodels, lsa, skmeans, dbscan, factoextra
* Web app: shiny
