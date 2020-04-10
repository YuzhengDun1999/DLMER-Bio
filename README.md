# DLMER-Bio
DLMER-Bio is a R package for deep learning model evalution. Currently, we can calculate information content, example-based precision, recall, average precision, average recall, exact match, S-min, F1-measure, AUC.  The package is used for for handling multi-labeled biomedical big-data. This package contains two main files. Info_Content.R is used for calculating information content of each label. evaluate.R is used for calculating the evaluation of the model. ROC.R is used for calculating ROC and AUC. We are still working on this project, the version now is 1.


Source Repository
-----------------
DLMER-Bio's source-code repository is hosted here on GitHub:

https://github.com/YuzhengDun1999/DLMER-Bio

Input Format
-----------------

The input to DLMER-Bio is composed of two text files:

* sample_count.txt: The first column contains the path to specific node identifier of hierarchical biome tree (see supplementary information)(e.g. root-Host-associated-Human-Skin). The second column is the total number of this category (e.g., 4848). Row example: root-Host-associated-Human-Skin:4848.
* result.txt: The format of each row is: 'ID'|'predicted labels' 'ID'|'true labels' 'probability of each labels'. Each label is delimited by comma. Example: gi:939342212:ref:WP_054888161.1|root,Host,associated,Human,Skin	gi:902540833:ref:WP_049608939.1|root,Host,associated,Human,Skin	1,0.91,0.88,0.79,0.74

Note: This package supports basic evaluation and advanced evaluation. The input format required for basic evaluation is that all results are not hierarchical and the data format required for advanced evaluation requires hierarchical input data. Only the input format of the two evaluation methods is different, and the rest are completely adaptive by the program. The hierarchical prediction results can be converted from the Non-hierarchical results using the data preprocessing scripts provided by us. The usage is `python preprocessing.py -i input_file -o output_file`. For example input and output data, see `data_unlayered.txt` and `data_layered.txt`.

Output Format
-----------------

The output to DLMER-Bio is composed of two text files:

* IC.txt: This file is generated by Info_Content.R. It contains the information content of each label. The first column contains the label(e.g. Host). The second column is the information content of this label (e.g., 0.298381701906727). Row example: Host,0.298381701906727.
* father.txt: This file is generated by Info_Content.R. It contains the information content of each label. The first column contains the label(e.g. Host). The second column is the father node of this label (e.g., root). Row example: Host, root.
* AUC and TOC are computed by ROC.R.
* Other results of evaluation are printed as output.csv.

Running the Test
-----------------
* For testing, go to main.R, revise the first line: setwd('G:/source_tracking/evaluation'). change the path to the your data path.
* execute source('evaluate.R') then execute source('main.R'), and execute source('ROC.R')

Test Result
-----------------
Test results are in the output.csv. We choose 0 to 1 with a skip of 0.01 as the threshold.

Notes
-----------------

The information content is used for calculating S-min which computes the semantic distance between real and predicted annotations based on information content of the classes. You can still use evaluate.R to calculate other measure.

Authors
---------

| Name                     | Email                                             |                                                              |
| :----------------------- | :------------------------------------------------ | :----------------------------------------------------------- |
| Yuzheng Dun (maintainer) | yuzhengdun@hust.edu.cn                            | Research Assistant, School of Mathematics and Statistics, Huazhong University of Science & Technology |
| Hugo Zha                 | hugozha@hust.edu.cn                               | Ph.D. Candidate, School of Life Science and Technology, Huazhong University of Science & Technology |
| Hui Chong                | chonghui@hust.edu.cn<br />ch37915405887@gmail.com | Research Assistant, School of Mathematics and Statistics, Huazhong University of Science & Technology |
| Kang Ning                | ningkang@hust.edu.cn                              | Professor, School of Life Science and Technology, Huazhong University of Science & Technology |
Supplementary Information
-----------------

hierarchical biome tree:

```
root
├── Engineered
│   └── Wastewater
│       ├── Activated_Sludge
│       ├── Industrial_wastewater
│       │   ├── Agricultural_wastewater
│       │   ├── Mine_water
│       │   └── Petrochemical
│       ├── Nutrient_removal
│       │   ├── Biological_phosphorus_removal
│       │   │   ├── Activated_sludge
│       │   │   └── Bioreactor
│       │   ├── Dissolved_organics_(aerobic)
│       │   ├── Dissolved_organics_(anaerobic)
│       │   └── Nitrogen_removal
│       └── Water_and_sludge
├── Environmental
│   ├── Aquatic
│   │   ├── Freshwater
│   │   │   ├── Groundwater
│   │   │   │   ├── Biofilm
│   │   │   │   └── Contaminated
│   │   │   └── Lake
│   │   ├── Marine
│   │   │   ├── Coastal
│   │   │   │   └── Sediment
│   │   │   ├── Cold_seeps
│   │   │   ├── Hydrothermal_vents
│   │   │   │   ├── Black_smokers
│   │   │   │   ├── Diffuse_flow
│   │   │   │   └── Microbial_mats
│   │   │   ├── Intertidal_zone
│   │   │   │   ├── Coral_reef
│   │   │   │   ├── Estuary
│   │   │   │   ├── Mangrove_swamp
│   │   │   │   ├── Microbialites
│   │   │   │   └── Salt_marsh
│   │   │   ├── Marginal_Sea
│   │   │   ├── Neritic_zone
│   │   │   ├── Oceanic
│   │   │   │   ├── Abyssal_plane
│   │   │   │   ├── Aphotic_zone
│   │   │   │   ├── Benthic
│   │   │   │   ├── Oil
│   │   │   │   │   ├── contaminated
│   │   │   │   │   ├── contaminated_sediment
│   │   │   │   │   └── contaminated_sediments
│   │   │   │   └── Photic_zone
│   │   │   ├── Oil_seeps
│   │   │   ├── Pelagic
│   │   │   ├── Volcanic
│   │   │   └── Wetlands
│   │   ├── Non
│   │   │   └── marine_Saline_and_Alkaline
│   │   │       ├── Alkaline
│   │   │       ├── Hypersaline
│   │   │       └── Salt_crystallizer_pond
│   │   └── Thermal_springs
│   │       └── Hot_(42
│   │           └── 90C)
│   └── Terrestrial
│       └── Soil
│           ├── Agricultural
│           ├── Boreal_forest
│           ├── Crop
│           │   └── Agricultural_land
│           ├── Desert
│           ├── Grasslands
│           ├── Loam
│           │   └── Forest_soil
│           ├── Mine_drainage
│           ├── Permafrost
│           ├── Sand
│           ├── Silt
│           ├── Tropical_rainforest
│           └── Uranium_contaminated
├── Host
│   └── associated
│       ├── Human
│       │   ├── Circulatory_system
│       │   │   └── Blood
│       │   ├── Digestive_system
│       │   │   ├── Fecal
│       │   │   ├── Foregut
│       │   │   │   └── Rumen
│       │   │   ├── Intestine
│       │   │   ├── Large_intestine
│       │   │   │   └── Cecum
│       │   │   ├── Oral
│       │   │   │   ├── Periodontal_pockets
│       │   │   │   ├── Saliva
│       │   │   │   ├── Subgingival_plaque
│       │   │   │   ├── Supragingival_plaque
│       │   │   │   ├── Throat
│       │   │   │   ├── buccal_mucosa
│       │   │   │   └── tongue_dorsum
│       │   │   ├── Oral_cavity
│       │   │   │   └── Buccal_mucosa
│       │   │   └── Stomach
│       │   ├── Lympathic_system
│       │   │   └── Lymph_nodes
│       │   ├── Reproductive_system
│       │   │   ├── Female
│       │   │   └── Vagina
│       │   │       └── posterior_fornix
│       │   ├── Respiratory_system
│       │   │   ├── Nasopharyngeal
│       │   │   │   ├── Nasal_cavity
│       │   │   │   └── Pharynx
│       │   │   └── Pulmonary_system
│       │   │       └── Sputum
│       │   └── Skin
│       │       └── Naris
│       ├── Insecta
│       ├── Mammals
│       └── Plants
│           ├── Phylloplane
│           │   └── Endophytes
│           ├── Rhizome
│           ├── Rhizoplane
│           ├── Rhizosphere
│           │   └── Epiphytes
│           └── Root
└── Mixed
```
