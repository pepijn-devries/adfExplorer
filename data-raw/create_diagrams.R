if (requireNamespace(c("DiagrammeR", "DiagrammeRsvg"))) {
  library(DiagrammeR)
  library(DiagrammeRsvg)
  grViz(
    "
digraph 'adfExplorer old' {
layout = 'dot';
splines = spline;
node [fontname = Helvetica shape = box style = filled fillcolor = white];

subgraph cluster_memory {
style = filled;
fontname = Helvetica;
fillcolor = lightcyan;
label = 'memory';
{
rank = 'same'; 
adf_mem [label = 'ADF file copy'];
file_mem [label = 'File data'];
}
}

subgraph cluster_local_drive {
style = filled;
fontname = Helvetica;
fillcolor = lightcyan;
{
rank = 'same';
adf_local [label = 'ADF file'];
file_cop [label = 'Copy of file'];
}
label = 'local drive';
}

adf_local -> adf_mem   [label = 'Copy entire virtual disk to memory'];
adf_mem   -> file_mem  [label = 'Extract file data to memory'];
file_mem  -> file_cop  [label = 'Write file data to local drive']
}
") |>
    export_svg() |>
    writeLines("man/figures/diagram_old.svg")
  grViz(
    "
digraph 'adfExplorer old' {
layout = 'dot';
splines = spline;
node [fontname = Helvetica shape = box style = filled fillcolor = white];

subgraph cluster_memory {
style = filled;
fontname = Helvetica;
fillcolor = lightcyan;
label = 'memory';
{
rank = 'same'; 
adf_con [label = 'Connection to ADF'];
file_con [label = 'Connection to virtual file'];
}
}

subgraph cluster_local_drive {
style = filled;
fontname = Helvetica;
fillcolor = lightcyan;
{
rank = 'same';
adf_local [label = 'ADF file'];
file_cop [label = 'copy of file'];
}
label = 'local drive';
}

adf_local -> adf_con   [label = 'Copy file pointer to memory'];
adf_con   -> file_con  [label = 'Create pointer to virtual file'];
file_con  -> file_cop  [label = 'Write file directly from\nthe connection to local drive']
}
") |>
    export_svg() |>
    writeLines("man/figures/diagram_new.svg")

} else message("Install packages in requireNameSpace and try again")
