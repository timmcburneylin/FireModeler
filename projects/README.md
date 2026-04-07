Each project lives under `projects/<project_name>/`.

Expected structure:
- `projects/<project_name>/data/raw/`
- `projects/<project_name>/data/intermediate/`
- `projects/<project_name>/data/outputs/`
- `projects/<project_name>/data/external/`

Shared assets such as modeling templates live at the repo root in `templates/`,
not inside each project folder.

Example:
- `projects/TR_LionsBurn/data/raw/SNAP/TR_LionsBurn_OS.csv`
