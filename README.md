# zotero_migration

This is an R project for migrating Zotero data.

## Project Structure
- R/ : R scripts
- data/ : Input/output data
- tests/ : Test scripts

## Setup
1. Open this project in RStudio or VS Code.
2. Install required packages as needed.

## Usage
Add your R scripts to the R/ directory.

## Migration Steps

### Phase 0 – Define the Target Model (Before Touching Data)
Goal: Avoid rework by deciding how MacFamilyTree concepts map to Zotero.
- Decide Zotero item types (e.g., Archival document → Manuscript or Document)
- Define field mapping (e.g., MacFamilyTree Title → Zotero title)
- Decide attachment policy: Copy files into Zotero storage (per project requirement)
- Decide ID strategy (e.g., generate stable IDs like MFT-000123)

### Phase 1 – Export Data from MacFamilyTree
Goal: Get all sources into a machine-readable format.
- Export sources to CSV or GEDCOM, including titles, fields, and filenames
- Create a snapshot and inventory files

### Phase 2 – Data Audit & Cleaning (RStudio)
Goal: Fix issues before Zotero sees the data.
- Load data into R, normalize paths, detect problems
- Create a validation report

### Phase 3 – Zotero Import Strategy
Goal: Choose the safest ingestion path.
- Recommended: Zotero CSV + Zotero API for metadata and attachments

### Phase 4 – Prepare Zotero-Compatible Metadata (R)
Goal: Create a Zotero-ready dataset.
- Transform fields, create Zotero CSV with required columns

### Phase 5 – Import Metadata into Zotero
Goal: Create Zotero items without attachments yet.
- Import CSV into a dedicated collection, verify mappings

### Phase 6 – Attach Files Programmatically (R + Zotero API)
Goal: Link each Zotero item to its document.
- Use API to attach files (copy into Zotero storage), log results

### Phase 7 – Verification & Quality Control
Goal: Ensure scholarly reliability.
- Automated and manual checks, diff reports

### Phase 8 – Post-Migration Enhancements (Optional)
- Add relations, normalize names, backups

### Phase 9 – Documentation & Reproducibility
Goal: Future-proof the work.
- Write README, version scripts, keep raw data immutable

### Deliverables Checklist
- Original MacFamilyTree export
- Cleaned CSV
- Zotero-import CSV
- R scripts
- Migration log
- Zotero collection with attachments

## License
MIT License
