# MasterThesis

## Environment and modules
This project requires a linux environment with several modules. The python version is 3.10.18 and a env.yml is available for easy replication with conda.
The project 

## Project structure

```text
.
├── .git      
├── AutoTest                # Directory containing the auto testing feature
├── Clustering              # Directory containing all the clusterign related files
├── Datasets                # Stores the Datasets
├── Files                   # File Manager for a unified interaction
├── Lizard                  # Lizard related files
├── LLM                     # LLM related files
├── Metrics                 # Statistics building and code for defining the differnent thresholds (IQR, outliers ...) and scoring for the student report
├── Pipeline                # Pipeline directory defining the different steps and step structure
├── Routers                 # FastAPI routers
├── Services                # Services folder containing mainly the dataset changing serivce
├── Static                  # Static files with Jinja2 templates, CSS and JS files
├── Tree_Sitter             # Custom metrics extraction and AST related files
├── __pycache__
├── .gitignore
├── config.py               # Main configuration file with notable prompts, texts, OLLAMA server config
└── main.py                 # Main file to launch the prototype
```
## Launching the program
To launch the program use the following command:
```code
uvicorn main:app --reload
```

## Dataset .zip structure
The ingestion support Moodle extracted datasets. The .zip file must contain all files at the root of the archive (don't check the download all files in folders option)

