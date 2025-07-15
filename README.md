# JTNA - Jamovi Transition Network Analysis Module

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version: 1.2.0](https://img.shields.io/badge/Version-1.2.0-blue.svg)]()
[![Jamovi](https://img.shields.io/badge/Jamovi-Compatible-green.svg)](https://www.jamovi.org/)

A comprehensive **Jamovi plugin** for performing **Transition Network Analysis (TNA)** to study relational dynamics and behavioral patterns in sequential data. This module provides an intuitive graphical interface for advanced network analysis techniques within the Jamovi statistical software environment.

## 🎯 Features

### **Core TNA Analysis**
- **Build TNA Models**: Create transition networks from sequential behavioral data
- **Centrality Measures**: Calculate betweenness, closeness, degree, and eigenvector centrality
- **Network Visualization**: Generate publication-ready transition network plots
- **Statistical Validation**: Bootstrap confidence intervals and permutation tests

### **🆕 Sequence Analysis** 
- **Sequence Visualization**: Plot behavioral sequences with multiple display options
- **Flexible Geometry**: Choose between bar charts and area plots
- **Scaling Options**: Display as distributions or proportions
- **Customizable Styling**: Full control over colors, themes, and appearance

### **Group Analysis**
- **Multi-group Comparisons**: Analyze TNA patterns across different groups
- **Comparative Visualizations**: Side-by-side network and sequence comparisons
- **Group-specific Statistics**: Separate centrality measures and network metrics

### **Advanced Features**
- **Interactive Plotting**: Dynamic network visualizations with customizable layouts
- **Export Options**: High-quality publication-ready outputs
- **Comprehensive Statistics**: Detailed network metrics and behavioral insights
- **User-friendly Interface**: Intuitive GUI designed for researchers

## 📊 Analysis Types

| Analysis | Description | Key Features |
|----------|-------------|--------------|
| **TNA** | Individual transition network analysis | Network plots, centrality measures, histograms, sequences |
| **Group TNA** | Multi-group comparison analysis | Comparative networks, group statistics, side-by-side visualizations |

## 🚀 Installation

### Method 1: Direct Installation (Recommended)
1. Download the latest `.jmo` file from the [releases page](https://github.com/mohsaqr/JTNA1.2/releases)
2. Open **Jamovi**
3. Navigate to **Modules → Jamovi library → Sideload**
4. Select the downloaded `JTNA_1.2.0.jmo` file
5. Restart Jamovi to activate the module

### Method 2: Development Installation
```r
# Install jmvtools if not already installed
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

# Load library and set Jamovi path
library(jmvtools)
options(jamovi_home="/path/to/your/jamovi")

# Navigate to module directory and install
setwd("path/to/TNAModule")
jmvtools::install()
```

## 📁 Project Structure

```
JTNA/
├── TNAModule/                    # Main module directory
│   ├── R/                       # R processing scripts
│   │   ├── TNA.b.R             # TNA analysis implementation
│   │   ├── TNA.h.R             # TNA interface definitions
│   │   ├── GroupTNA.b.R        # Group TNA implementation
│   │   └── GroupTNA.h.R        # Group TNA interface
│   │
│   ├── jamovi/                  # UI definition files
│   │   ├── TNA.a.yaml          # TNA analysis options
│   │   ├── TNA.u.yaml          # TNA user interface
│   │   ├── GroupTNA.a.yaml     # Group TNA options
│   │   └── GroupTNA.u.yaml     # Group TNA interface
│   │
│   ├── DESCRIPTION             # Package metadata
│   ├── NAMESPACE               # Function exports
│   └── JTNA_1.2.0.jmo         # Compiled module file
│
└── README.md                   # This file

```

## 🎮 Quick Start

### 1. Load Your Data
- Import your sequential behavioral data into Jamovi
- Ensure data includes participant IDs, timestamps, and behavioral codes

### 2. Run TNA Analysis
- Go to **Analyses → JTNA → TNA**
- Select your variables (participant, time, behavior)
- Configure analysis options
- Generate network plots and statistics

### 3. Explore Sequence Analysis
- In the same analysis, navigate to **Sequence Analysis**
- Choose visualization type (bar/area)
- Customize colors and styling
- Generate sequence plots

### 4. Compare Groups (Optional)
- Use **Analyses → JTNA → Group TNA** for multi-group analysis
- Specify grouping variable
- Compare networks and sequences across groups

## 📖 Documentation & Tutorials

- **TNA Basics**: [Introduction to Transition Network Analysis](https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html)
- **Frequency-based TNA**: [Advanced TNA Techniques](https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html)
- **Clustering with TNA**: [Pattern Discovery Methods](https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html)

## 📚 Citation

If you use JTNA in your research, please cite:

```bibtex
@inproceedings{saqr2024transition,
  title={Transition Network Analysis: A Novel Method for Analyzing Sequential Data},
  author={Saqr, Mohammed and López-Pernas, Sonsoles and Tikka, Santtu},
  booktitle={Proceedings of the Conference},
  year={2024},
  doi={10.1145/3706468.3706513}
}
```

## 👥 Authors & Contributors
- **Dylan Girault** - Lead Developer   
- **Mohammed Saqr** - Development & Research
- **Santtu Tikka** - Statistical Methods
- **Sonsoles López-Pernas** - Research & Development

**Maintainer**: Mohammed Saqr (saqr@saqr.me)

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](TNAModule/LICENSE) file for details.

## 🐛 Bug Reports & Feature Requests

- **Issues**: [GitHub Issues](https://github.com/mohsaqr/JTNA1.2/issues)
- **Discussions**: [GitHub Discussions](https://github.com/mohsaqr/JTNA1.2/discussions)

## 🔄 Version History

### Version 1.2.0 (Latest)
- ✨ **New**: Sequence Analysis feature with bar and area plot options
- ✨ **New**: Enhanced customization options for plots
- 🔧 **Improved**: User interface and workflow
- 🐛 **Fixed**: Various stability improvements

